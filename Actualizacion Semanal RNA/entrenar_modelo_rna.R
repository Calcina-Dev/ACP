# ==============================================================================
# PIPELINE DE ENTRENAMIENTO: RED NEURONAL ARTIFICIAL (RNA) - AJUSTE SEMANAL
# Agrícola Cerro Prieto (ACP) • Proyecciones & Control de Cosecha
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(readxl)
  library(nnet)
})

Sys.setenv(TZ = "America/Lima")
cat("==============================================================================\n")
cat("🚀 INICIANDO CONSTRUCCIÓN DE MATRIZ Y ENTRENAMIENTO DE LA RNA\n")
cat("==============================================================================\n\n")

ruta_rdata <- "d:/ACP/Actualizacion Semanal RNA/datos_maestros_cosecha_2026.RData"
if (!file.exists(ruta_rdata)) {
  ruta_rdata <- "d:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData"
}

cat("📂 [1/6] Cargando datos maestros de cosecha y conteo...\n")
load(ruta_rdata)

df_cosecha[["Kg"]] <- df_cosecha[["Kg Total"]]

# ------------------------------------------------------------------------------
# 1. AGREGACIÓN DE COSECHA A NIVEL TURNO x SEMANA (2021-2026)
# ------------------------------------------------------------------------------
cat("📊 [2/6] Agregando cosecha y extrayendo señales de primeros días (Lun-Mar)...\n")

limpiar_variedad_vec <- function(x) {
  v <- toupper(trimws(as.character(x)))
  chartr("ÁÉÍÓÚÜ", "AEIOUU", v)
  case_when(
    grepl("EB\\s*9\\s*[-_.]\\s*2", v, ignore.case = TRUE) ~ "MAGICA",
    grepl("^MEGA\\s*CRISP|^MEGACRISP", v, ignore.case = TRUE) ~ "MEGA CRISP",
    grepl("^MILAGRO", v, ignore.case = TRUE) ~ "MILAGROS",
    TRUE ~ v
  )
}

df_turn_week <- df_cosecha %>%
  filter(Campaña >= 2021, !coalesce(Es_Testblock, FALSE)) %>%
  mutate(
    wd = wday(Fecha, week_start = 1),
    Semana = as.numeric(Semana),
    Campaña = as.numeric(Campaña),
    Modulo_chr = as.character(Módulo),
    Turno_chr = as.character(Turno),
    Variedad_clean = limpiar_variedad_vec(Variedad),
    Tipo_Cultivo = ifelse(coalesce(Tipo_Cosecha, "") == "Orgánico" | grepl("org", tolower(coalesce(Variedad, ""))), "Organico", "Convencional")
  ) %>%
  group_by(Campaña, Semana, Modulo_chr, Turno_chr, Variedad_clean, Tipo_Cultivo) %>%
  summarise(
    Area_Ha = max(coalesce(Área, 0), na.rm = TRUE),
    Plantas = max(coalesce(Plantas, 0), na.rm = TRUE),
    Kg_Lun_Mar = sum(Kg[wd %in% c(1, 2)], na.rm = TRUE),
    Kg_Mie_Sab = sum(Kg[wd %in% c(3, 4, 5, 6)], na.rm = TRUE),
    Kg_Total_Sem = sum(Kg, na.rm = TRUE),
    Dias_Cosechados_LunMar = n_distinct(wd[wd %in% c(1, 2)]),
    Ultimo_Dia_Semana = max(wd, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(Area_Ha > 0.1, Kg_Total_Sem > 0) %>%
  mutate(
    Rend_Total_Sem = Kg_Total_Sem / Area_Ha,
    Rend_Lun_Mar = Kg_Lun_Mar / Area_Ha,
    Tiene_Lun_Mar = as.numeric(Kg_Lun_Mar > 0),
    Prop_Avance_Lun_Mar = pmin(1, Kg_Lun_Mar / pmax(1, Kg_Total_Sem)),
    Plantas_Ha = ifelse(Area_Ha > 0 & Plantas > 0, Plantas / Area_Ha, 3500)
  )

# Calcular Lags temporales S-1 y S-2
cat("⏳ [3/6] Calculando inercias temporales (Lags S-1 y S-2)...\n")
df_turn_week <- df_turn_week %>%
  arrange(Modulo_chr, Turno_chr, Variedad_clean, Campaña, Semana) %>%
  group_by(Modulo_chr, Turno_chr, Variedad_clean, Campaña) %>%
  mutate(
    Semana_Lag1 = lag(Semana),
    Semana_Lag2 = lag(Semana, 2),
    Rend_S_1 = ifelse(Semana == Semana_Lag1 + 1, lag(Rend_Total_Sem), NA_real_),
    Rend_S_2 = ifelse(Semana == Semana_Lag2 + 2, lag(Rend_Total_Sem, 2), NA_real_),
    Ultimo_Dia_S1 = ifelse(Semana == Semana_Lag1 + 1, lag(Ultimo_Dia_Semana), NA_real_),
    Semana_Ant_No_Finalizo = as.numeric(!is.na(Ultimo_Dia_S1) & Ultimo_Dia_S1 >= 5 & Tiene_Lun_Mar == 1)
  ) %>%
  ungroup() %>%
  mutate(
    Rend_S_1 = coalesce(Rend_S_1, Rend_Total_Sem), # Fallback si es primera semana
    Rend_S_2 = coalesce(Rend_S_2, Rend_S_1),
    Ratio_S1_S2 = pmin(3, pmax(0.33, Rend_S_1 / pmax(10, Rend_S_2)))
  )

# ------------------------------------------------------------------------------
# 2. INGESTAR METEOROLOGÍA SEMANAL (FILTRADO 7 Y 9)
# ------------------------------------------------------------------------------
cat("☀️ [4/6] Integrando variables meteorológicas (Filtrado 7 & 9)...\n")
path_meteo <- "d:/ACP/Variables_Filtrados_7_9.xlsx"

get_modulo_filtrado <- function(m) {
  num <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", as.character(m))))
  if (is.na(num)) return("Filtrado 9")
  if (floor(num) %in% c(1, 2, 3, 4, 5, 13, 14)) "Filtrado 7" else "Filtrado 9"
}

df_turn_week$Zona_Meteo <- sapply(df_turn_week$Modulo_chr, get_modulo_filtrado)

if (file.exists(path_meteo)) {
  s7 <- suppressMessages(read_excel(path_meteo, sheet = "Variables_Meteo_Filtrado7"))[, 1:6]
  s9 <- suppressMessages(read_excel(path_meteo, sheet = "Variables_Meteo_Filtrado9"))[, 1:6]
  
  d7_col <- s7[[1]]
  s7_clean <- s7 %>%
    mutate(
      Fecha = make_date(as.numeric(Año), as.numeric(format(d7_col, "%m")), as.numeric(format(d7_col, "%d"))),
      Semana = pmin(isoweek(Fecha), 52),
      AñoISO = isoyear(Fecha),
      Zona_Meteo = "Filtrado 7",
      Variable = ifelse(Variable == "Temperatura Exterior (°C)", "Temp_Prom", trimws(as.character(Variable)))
    ) %>%
    filter(!is.na(Fecha), !is.na(Valor))
  
  d9_col <- s9[[2]]
  s9_clean <- s9 %>%
    mutate(
      Fecha = make_date(as.numeric(Año), as.numeric(format(d9_col, "%m")), as.numeric(format(d9_col, "%d"))),
      Semana = pmin(isoweek(Fecha), 52),
      AñoISO = isoyear(Fecha),
      Zona_Meteo = "Filtrado 9",
      Variable = ifelse(Variable == "Temperatura Exterior (°C)", "Temp_Prom", trimws(as.character(Variable)))
    ) %>%
    filter(!is.na(Fecha), !is.na(Valor))
  
  meteo_all <- bind_rows(s7_clean, s9_clean)
  
  # Resumen semanal de temperatura promedio y radiación
  meteo_semanal <- meteo_all %>%
    filter(Variable %in% c("Temp_Prom", "Radiación (W/m²)", "Temperatura Máxima (°C)")) %>%
    mutate(Var_Norm = case_when(
      Variable == "Temp_Prom" ~ "Temp_Prom",
      Variable == "Temperatura Máxima (°C)" ~ "Temp_Max",
      TRUE ~ "Radiacion"
    )) %>%
    group_by(Zona_Meteo, AñoISO, Semana, Var_Norm) %>%
    summarise(Valor_Sem = mean(Valor, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = Var_Norm, values_from = Valor_Sem, values_fill = NA_real_)
  
  # Calcular promedio histórico de temperatura por semana y zona para obtener la anomalía
  meteo_base <- meteo_semanal %>%
    group_by(Zona_Meteo, Semana) %>%
    summarise(Temp_Prom_Base = mean(Temp_Prom, na.rm = TRUE), .groups = "drop")
  
  meteo_semanal <- meteo_semanal %>%
    left_join(meteo_base, by = c("Zona_Meteo", "Semana")) %>%
    mutate(Anomalia_Temp = Temp_Prom - Temp_Prom_Base)
  
  df_turn_week <- df_turn_week %>%
    left_join(
      meteo_semanal %>% select(Zona_Meteo, Campaña = AñoISO, Semana, Temp_Prom, Temp_Max, Radiacion, Anomalia_Temp),
      by = c("Zona_Meteo", "Campaña", "Semana")
    )
} else {
  df_turn_week$Temp_Prom <- NA_real_
  df_turn_week$Temp_Max <- NA_real_
  df_turn_week$Radiacion <- NA_real_
  df_turn_week$Anomalia_Temp <- NA_real_
}

# Imputar medias si faltan datos meteorológicos
df_turn_week <- df_turn_week %>%
  mutate(
    Temp_Prom = coalesce(Temp_Prom, 20.5),
    Temp_Max = coalesce(Temp_Max, 25.0),
    Radiacion = coalesce(Radiacion, 220),
    Anomalia_Temp = coalesce(Anomalia_Temp, 0.0)
  )

# ------------------------------------------------------------------------------
# 3. INTEGRAR CONTEO FENOLÓGICO DESFASADO (FLOR Y BOTÓN)
# ------------------------------------------------------------------------------
cat("🌸 [5/6] Integrando conteo floral desfasado por maduración (~6 semanas)...\n")

if (exists("df_conteo_historico") && is.data.frame(df_conteo_historico) && nrow(df_conteo_historico) > 0) {
  df_conteo_resumen <- df_conteo_historico %>%
    filter(toupper(trimws(coalesce(Fundo, "ACP"))) == "ACP") %>%
    mutate(
      Campaña = as.numeric(Campaña),
      Semana = as.numeric(Semana),
      Modulo_chr = trimws(as.character(Módulo)),
      Turno_chr = trimws(as.character(Turno)),
      Variedad_clean = limpiar_variedad_vec(Variedad),
      Organos_Flor = coalesce(Boton, 0) + coalesce(Flor, 0),
      Muestra = coalesce(MuestraF, 15)
    ) %>%
    group_by(Campaña, Semana, Modulo_chr, Turno_chr, Variedad_clean) %>%
    summarise(
      Flor_Planta = sum(Organos_Flor, na.rm = TRUE) / pmax(1, sum(Muestra, na.rm = TRUE)),
      Verde_Planta = sum(coalesce(Verde, 0), na.rm = TRUE) / pmax(1, sum(Muestra, na.rm = TRUE)),
      .groups = "drop"
    )
  
  # Cruce con desfase de 6 semanas hacia atrás (semana de floración para la cosecha actual)
  df_turn_week <- df_turn_week %>%
    mutate(Sem_Flor = Semana - 6) %>%
    left_join(
      df_conteo_resumen %>% select(Campaña, Sem_Flor = Semana, Modulo_chr, Turno_chr, Variedad_clean, Carga_Flor_Lag = Flor_Planta, Fruto_Verde_Lag = Verde_Planta),
      by = c("Campaña", "Sem_Flor", "Modulo_chr", "Turno_chr", "Variedad_clean")
    ) %>%
    select(-Sem_Flor)
} else {
  df_turn_week$Carga_Flor_Lag <- NA_real_
  df_turn_week$Fruto_Verde_Lag <- NA_real_
}

# Fallback si el conteo faltó
media_flor_var <- df_turn_week %>%
  group_by(Variedad_clean) %>%
  summarise(Media_Flor = mean(Carga_Flor_Lag, na.rm = TRUE), .groups = "drop")

df_turn_week <- df_turn_week %>%
  left_join(media_flor_var, by = "Variedad_clean") %>%
  mutate(
    Carga_Flor_Lag = coalesce(Carga_Flor_Lag, Media_Flor, 15.0),
    Fruto_Verde_Lag = coalesce(Fruto_Verde_Lag, 10.0)
  ) %>%
  select(-Media_Flor)

# ------------------------------------------------------------------------------
# 4. NORMALIZACIÓN Y PREPARACIÓN DEL VECTOR DE CARACTERÍSTICAS
# ------------------------------------------------------------------------------
cat("🧠 [6/6] Preparando vector de características y entrenando Ensemble de RNA...\n")

# Variedades principales
top_vars <- c("ROCIO", "VENTURA", "BILOXI", "MAGICA", "SEKOYA POP", "SEKOYA BEAUTY", "IMPERIAL", "MADEIRA")

df_modelo <- df_turn_week %>%
  mutate(
    # One-hot encoding de variedades principales
    Var_ROCIO   = as.numeric(Variedad_clean == "ROCIO"),
    Var_VENTURA = as.numeric(Variedad_clean == "VENTURA"),
    Var_BILOXI  = as.numeric(Variedad_clean == "BILOXI"),
    Var_MAGICA  = as.numeric(Variedad_clean == "MAGICA"),
    Var_SPOP    = as.numeric(Variedad_clean == "SEKOYA POP"),
    Var_SBEAUTY = as.numeric(Variedad_clean == "SEKOYA BEAUTY"),
    Var_IMPERIAL= as.numeric(Variedad_clean == "IMPERIAL"),
    Var_MADEIRA = as.numeric(Variedad_clean == "MADEIRA"),
    Var_OTROS   = as.numeric(!Variedad_clean %in% top_vars),
    Es_Organico = as.numeric(Tipo_Cultivo == "Organico"),
    Modulo_Num  = suppressWarnings(as.numeric(gsub("[^0-9.]", "", Modulo_chr))),
    Modulo_Num  = coalesce(Modulo_Num, 1)
  ) %>%
  filter(!is.na(Rend_Total_Sem), Rend_Total_Sem > 0)

# Escaladores Min-Max para entrenamiento neuronal
feature_cols <- c(
  "Rend_S_1", "Rend_S_2", "Ratio_S1_S2", "Rend_Lun_Mar", "Tiene_Lun_Mar",
  "Prop_Avance_Lun_Mar", "Semana_Ant_No_Finalizo", "Semana", "Area_Ha",
  "Plantas_Ha", "Temp_Prom", "Temp_Max", "Radiacion", "Anomalia_Temp",
  "Carga_Flor_Lag", "Fruto_Verde_Lag", "Es_Organico", "Modulo_Num",
  "Var_ROCIO", "Var_VENTURA", "Var_BILOXI", "Var_MAGICA", "Var_SPOP",
  "Var_SBEAUTY", "Var_IMPERIAL", "Var_MADEIRA", "Var_OTROS"
)

# Partición Temporal: Train (2021-2025) y Test (2026)
train_raw <- df_modelo %>% filter(Campaña <= 2025)
test_raw  <- df_modelo %>% filter(Campaña == 2026)

cat(sprintf("   Dataset Total: %d filas\n", nrow(df_modelo)))
cat(sprintf("   • Train (2021-2025): %d observaciones\n", nrow(train_raw)))
cat(sprintf("   • Test  (2026):      %d observaciones\n", nrow(test_raw)))

# Calcular estadísticas de escalado sobre el conjunto de entrenamiento
escaladores <- list()
for (col in c(feature_cols, "Rend_Total_Sem")) {
  vals <- train_raw[[col]]
  q_low <- quantile(vals, 0.01, na.rm = TRUE)
  q_high <- quantile(vals, 0.99, na.rm = TRUE)
  if (q_high == q_low) q_high <- q_low + 1
  escaladores[[col]] <- list(min = as.numeric(q_low), max = as.numeric(q_high))
}

escalar_df <- function(df, esc, cols) {
  df_esc <- df
  for (col in cols) {
    e <- esc[[col]]
    vals <- df_esc[[col]]
    df_esc[[col]] <- pmin(1, pmax(0, (vals - e$min) / (e$max - e$min)))
  }
  df_esc
}

desescalar_vec <- function(vals_norm, esc, col = "Rend_Total_Sem") {
  e <- esc[[col]]
  vals_norm * (e$max - e$min) + e$min
}

train_scaled <- escalar_df(train_raw, escaladores, c(feature_cols, "Rend_Total_Sem"))
test_scaled  <- escalar_df(test_raw, escaladores, c(feature_cols, "Rend_Total_Sem"))

# ------------------------------------------------------------------------------
# 5. ENTRENAMIENTO DE ENSEMBLE DE REDES NEURONALES (BAGGED MULTI-LAYER PERCEPTRON)
# ------------------------------------------------------------------------------
cat("\n🤖 Entrenando Ensemble de 10 Redes Neuronales (Bagged MLP)...\n")
set.seed(2026)

formula_rna <- as.formula(paste("Rend_Total_Sem ~", paste(feature_cols, collapse = " + ")))

num_redes <- 10
lista_modelos <- list()

for (i in 1:num_redes) {
  # Bagging: muestra aleatoria con reemplazo del 85% de los datos de train
  idx_bag <- sample(nrow(train_scaled), size = round(0.85 * nrow(train_scaled)), replace = TRUE)
  train_bag <- train_scaled[idx_bag, ]
  
  # Alternar tamaños ocultos y regularización decay para diversidad en el ensemble
  size_i <- ifelse(i %% 2 == 0, 10, 8)
  decay_i <- ifelse(i %% 3 == 0, 0.005, 0.002)
  
  mod_i <- nnet(
    formula_rna,
    data = train_bag,
    size = size_i,
    decay = decay_i,
    maxit = 350,
    linout = TRUE,
    trace = FALSE,
    MaxNWts = 2000
  )
  lista_modelos[[i]] <- mod_i
  cat(sprintf("   ✔ Red %02d/%02d entrenada (Hidden: %d neuronas, Decay: %.4f)\n", i, num_redes, size_i, decay_i))
}

# ------------------------------------------------------------------------------
# 6. EVALUACIÓN Y VALIDACIÓN CRUZADA EN TEST (CAMPAÑA 2026)
# ------------------------------------------------------------------------------
cat("\n📈 Evaluando Ensemble en Test independiente (Campaña 2026)...\n")

mat_preds_test <- matrix(0, nrow = nrow(test_scaled), ncol = num_redes)
for (i in 1:num_redes) {
  mat_preds_test[, i] <- predict(lista_modelos[[i]], newdata = test_scaled)
}

pred_ensemble_norm <- rowMeans(mat_preds_test)
pred_ensemble_kg <- desescalar_vec(pred_ensemble_norm, escaladores, "Rend_Total_Sem")
real_kg <- test_raw$Rend_Total_Sem

cor_test <- cor(pred_ensemble_kg, real_kg)
r2_test  <- 1 - sum((real_kg - pred_ensemble_kg)^2) / sum((real_kg - mean(real_kg))^2)
mae_test <- mean(abs(real_kg - pred_ensemble_kg))
rmse_test <- sqrt(mean((real_kg - pred_ensemble_kg)^2))
media_real <- mean(real_kg)
mape_aprox <- (mae_test / media_real) * 100

cat("==============================================================================\n")
cat("🎯 RESULTADOS FINALES DEL MODELO RNA EN TEST (CAMPAÑA 2026):\n")
cat(sprintf("   • Correlación (Pred vs Real): %0.4f\n", cor_test))
cat(sprintf("   • R² Score (Campaña 2026):    %0.4f (%0.1f%% de varianza explicada)\n", r2_test, r2_test * 100))
cat(sprintf("   • MAE:                        %0.2f kg/ha\n", mae_test))
cat(sprintf("   • RMSE:                       %0.2f kg/ha\n", rmse_test))
cat(sprintf("   • Rendimiento medio real:     %0.2f kg/ha\n", media_real))
cat(sprintf("   • Error porcentual (MAE/Med): %0.1f%%\n", mape_aprox))
cat("==============================================================================\n\n")

# ------------------------------------------------------------------------------
# 7. EXPORTAR ARTEFACTO COMPLETO DEL MODELO (.RDS)
# ------------------------------------------------------------------------------
objeto_rna <- list(
  modelos = lista_modelos,
  num_modelos = num_redes,
  feature_cols = feature_cols,
  escaladores = escaladores,
  top_vars = top_vars,
  metricas_test_2026 = list(
    r2 = r2_test,
    cor = cor_test,
    mae = mae_test,
    rmse = rmse_test,
    mape = mape_aprox
  ),
  fecha_entrenamiento = Sys.time()
)

ruta_modelo_out <- "d:/ACP/Actualizacion Semanal RNA/modelo_rna_ajuste.rds"
saveRDS(objeto_rna, ruta_modelo_out)
cat(sprintf("💾 Modelo exportado con éxito a: %s\n", ruta_modelo_out))
cat("🎉 Pipeline completado satisfactoriamente.\n")
