suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(lubridate)
})

load("G:/Unidades compartidas/Compartido Proyecciones/3.- Arándano/datos_maestros_cosecha_2026.RData")

cat("=== ANALISIS DE FRECUENCIA VS RENDIMIENTO REAL ACP ===\n\n")

# Analizar eventos de cosecha a nivel de lote/turno en df_cosecha
df_c <- df_cosecha %>%
  filter(
    as.character(Campaña) == "2026",
    coalesce(`Kg Total`, 0) > 0,
    !coalesce(Es_Testblock, FALSE)
  ) %>%
  mutate(
    Fecha_d = as.Date(Fecha),
    Módulo_c = as.character(Módulo),
    Turno_c = as.character(Turno),
    Variedad_c = as.character(Variedad)
  )

# Eventos de cosecha por turno y fecha
eventos <- df_c %>%
  group_by(Módulo_c, Turno_c, Variedad_c, Fecha_d) %>%
  summarise(
    Kg = sum(`Kg Total`, na.rm = TRUE),
    Area = first(na.omit(Área)),
    Rend_Pase = ifelse(!is.na(Area) && Area > 0, Kg / Area, NA_real_),
    .groups = "drop"
  ) %>%
  arrange(Módulo_c, Turno_c, Fecha_d) %>%
  group_by(Módulo_c, Turno_c) %>%
  mutate(
    Dias_Frec = as.numeric(Fecha_d - lag(Fecha_d))
  ) %>%
  ungroup() %>%
  filter(!is.na(Dias_Frec), Dias_Frec >= 2, Dias_Frec <= 21, !is.na(Rend_Pase), Rend_Pase > 0)

cat(sprintf("Total eventos con frecuencia calculada: %d\n", nrow(eventos)))

# Correlación general
cor_gen <- cor(eventos$Dias_Frec, eventos$Rend_Pase, use = "complete.obs")
cat(sprintf("Correlación de Pearson global (Días Frecuencia vs Rendimiento del Pase): %.3f\n\n", cor_gen))

# Resumen por rango de frecuencia
res_rango <- eventos %>%
  mutate(Rango_Frec = case_when(
    Dias_Frec <= 4 ~ "1. Corta (2-4 d)",
    Dias_Frec <= 6 ~ "2. Normal-Corta (5-6 d)",
    Dias_Frec <= 8 ~ "3. Estándar (7-8 d)",
    Dias_Frec <= 11 ~ "4. Extendida (9-11 d)",
    TRUE ~ "5. Muy Larga (12+ d)"
  )) %>%
  group_by(Rango_Frec) %>%
  summarise(
    N_Pases = n(),
    Dias_Medio = round(mean(Dias_Frec), 1),
    Rend_Medio = round(mean(Rend_Pase), 1),
    Kg_Medio = round(mean(Kg), 0),
    Rend_Por_Dia = round(mean(Rend_Pase / Dias_Frec), 1),
    .groups = "drop"
  )

print(res_rango)

# Análisis por variedad
cat("\n=== POR VARIEDAD ===\n")
res_var <- eventos %>%
  group_by(Variedad_c) %>%
  summarise(
    N = n(),
    Frec_Prom = round(mean(Dias_Frec), 1),
    Rend_Prom = round(mean(Rend_Pase), 1),
    Correlacion = round(cor(Dias_Frec, Rend_Pase), 3),
    Elasticidad_Regresion = round(coef(lm(Rend_Pase ~ Dias_Frec))[2], 2),
    .groups = "drop"
  ) %>%
  filter(N >= 20) %>%
  arrange(desc(N))

print(res_var)
