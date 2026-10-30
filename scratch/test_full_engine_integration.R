# Reemplazar la linea de sap_sem
load("G:/Unidades compartidas/Compartido Proyecciones/3.- Ar\u00e1ndano/datos_maestros_cosecha_2026.RData")
suppressPackageStartupMessages(library(dplyr))

limpiar_variedad <- function(x) {
  v <- toupper(trimws(as.character(x)))
  gsub("\\s+", " ", v)
}
norm_codigo_valvula <- function(x) {
  x <- toupper(trimws(as.character(x)))
  gsub("\\s+", "", x)
}

sem_sel <- 44
sem_ant <- 43
dias_nombres <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
dias_cols_kg <- c("Kg_Lunes", "Kg_Martes", "Kg_Miércoles", "Kg_Jueves", "Kg_Viernes", "Kg_Sábado")
dias_ajustables_nom <- c("Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
dia_inicio_nom <- "Martes"
dia_inicio_idx <- 2

desplazar_dia_frecuencia <- function(dia_base_vec, frec_vec, dias_nombres, dia_inicio_idx, dia_inicio_nom) {
  idx_base <- match(dia_base_vec, dias_nombres)
  idx_target <- pmin(6L, pmax(1L, idx_base + as.integer(frec_vec) - 7L))
  dia_target <- ifelse(is.na(idx_target), dia_inicio_nom, dias_nombres[idx_target])
  ifelse(is.na(idx_target) | idx_target < dia_inicio_idx, dia_inicio_nom, dia_target)
}

# 1. Catálogo
valvs_cat <- df_valvulas %>%
  mutate(
    codigo_norm = norm_codigo_valvula(coalesce(codigo_norm, codigo)),
    Sector = trimws(as.character(Sector)),
    Turno_num = suppressWarnings(as.numeric(Turno)),
    Módulo = case_when(
      Módulo %in% c("13.1", "13.2") ~ "13",
      Módulo %in% c("14.1", "14.2") ~ "14",
      Módulo == "9" & between(Turno_num, 1, 11)  ~ "9.2",
      Módulo == "9" & Turno_num >= 12            ~ "9.1",
      Módulo == "11" & between(Turno_num, 1, 12) ~ "11.1",
      Módulo == "11" & Turno_num >= 13           ~ "11.2",
      TRUE                                       ~ trimws(as.character(Módulo))
    ),
    Turno = trimws(as.character(Turno)),
    Variedad = limpiar_variedad(Variedad),
    Área = suppressWarnings(as.numeric(Área))
  ) %>%
  mutate(Frecuencia_Dias = ifelse(Variedad == "ROCIO", 3L, 7L))

# 2. Histórico Semana Anterior (Semana 43)
df_ant_valvs <- df_hist_oficial_wide %>%
  filter(Semana == sem_ant) %>%
  mutate(
    codigo_norm = norm_codigo_valvula(Codigo_Valv),
    Turno_num = suppressWarnings(as.numeric(Turno_Norm)),
    Módulo = case_when(
      Módulo_Norm %in% c("13.1", "13.2") ~ "13",
      Módulo_Norm %in% c("14.1", "14.2") ~ "14",
      Módulo_Norm == "9" & between(Turno_num, 1, 11)  ~ "9.2",
      Módulo_Norm == "9" & Turno_num >= 12            ~ "9.1",
      Módulo_Norm == "11" & between(Turno_num, 1, 12) ~ "11.1",
      Módulo_Norm == "11" & Turno_num >= 13           ~ "11.2",
      TRUE                                            ~ as.character(Módulo_Norm)
    ),
    Turno = as.character(Turno_Norm),
    Variedad = limpiar_variedad(Variedad)
  )

mat_kg <- as.matrix(df_ant_valvs[, intersect(dias_cols_kg, names(df_ant_valvs))])
cols_presentes <- intersect(dias_cols_kg, names(df_ant_valvs))
max_col_idx <- max.col(mat_kg, ties.method = "first")
has_kg <- rowSums(mat_kg, na.rm = TRUE) > 0
nombres_cols_map <- sub("^Kg_", "", cols_presentes)
df_ant_valvs$Dia_7d <- ifelse(has_kg, nombres_cols_map[max_col_idx], NA_character_)

valv_7d_map <- df_ant_valvs %>%
  filter(!is.na(Dia_7d)) %>%
  arrange(desc(Kg_Total_Sem)) %>%
  group_by(codigo_norm) %>%
  slice(1) %>%
  ungroup() %>%
  select(codigo_norm, Dia_7d, Kg_Total_Ant = Kg_Total_Sem, Ha_Total_Ant = Ha_Total_Sem)

turno_dia_habitual <- df_ant_valvs %>%
  filter(!is.na(Dia_7d)) %>%
  group_by(Módulo, Turno, Dia_7d) %>%
  summarise(Total_Kg = sum(Kg_Total_Sem, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(Total_Kg)) %>%
  group_by(Módulo, Turno) %>%
  slice(1) %>%
  ungroup() %>%
  select(Módulo, Turno, Dia_Habitual = Dia_7d)

rend_ant_turno <- df_ant_valvs %>%
  group_by(Módulo, Turno, Variedad) %>%
  summarise(
    Kg_Ant_Turno = sum(Kg_Total_Sem, na.rm = TRUE),
    Ha_Ant_Turno = sum(Ha_Total_Sem, na.rm = TRUE),
    Rend_Ant_Turno = ifelse(Ha_Ant_Turno > 0, Kg_Ant_Turno / Ha_Ant_Turno, 0),
    Ultimo_Dia_Cosecha_Ant = {
      sub_mat <- mat_kg[cur_group_rows(), , drop = FALSE]
      c_sums <- colSums(sub_mat, na.rm = TRUE)
      dias_con_kg <- names(which(c_sums > 0))
      if (length(dias_con_kg) > 0) sub("^Kg_", "", tail(dias_con_kg, 1)) else NA_character_
    },
    .groups = "drop"
  )

# 3. Rutas de Campo
rc <- readRDS("D:/ACP/Rutas de Cosecha/rutas_cosecha_mapa.rds")
rc_valv_avance <- rc %>%
  filter(Semana == sem_sel) %>%
  mutate(codigo_norm = norm_codigo_valvula(codigo)) %>%
  group_by(codigo_norm) %>%
  summarise(
    Ha_Prog = sum(coalesce(Ha_Total_Prog, 0), na.rm = TRUE),
    Ha_Lunes_Rc = sum(coalesce(Ha_Lunes, 0), na.rm = TRUE),
    Ha_Martes_Rc = sum(coalesce(Ha_Martes, 0), na.rm = TRUE),
    .groups = "drop"
  )

# 4. SAP
sap_sem <- df_cosecha %>%
  filter(Semana == sem_sel, Campaña == "2026") %>%
  mutate(
    Turno_num = suppressWarnings(as.numeric(Turno)),
    Módulo = case_when(
      Módulo %in% c("13.1", "13.2") ~ "13",
      Módulo %in% c("14.1", "14.2") ~ "14",
      Módulo == "9" & between(Turno_num, 1, 11)  ~ "9.2",
      Módulo == "9" & Turno_num >= 12            ~ "9.1",
      Módulo == "11" & between(Turno_num, 1, 12) ~ "11.1",
      Módulo == "11" & Turno_num >= 13           ~ "11.2",
      TRUE                                       ~ as.character(Módulo)
    ),
    Turno = as.character(Turno),
    Variedad = limpiar_variedad(Variedad),
    wday_num = lubridate::wday(Fecha, week_start = 1)
  )

kg_real_turno <- sap_sem %>%
  group_by(Módulo, Turno, Variedad) %>%
  summarise(
    Kg_Real_Sem = sum(`Kg Total`, na.rm = TRUE),
    Kg_Lunes_Real = sum(`Kg Total`[wday_num == 1], na.rm = TRUE),
    Kg_Martes_Real = sum(`Kg Total`[wday_num == 2], na.rm = TRUE),
    Sap_Cosecho_Lunes = any(wday_num == 1 & `Kg Total` > 0),
    Sap_Cosecho_Martes = any(wday_num == 2 & `Kg Total` > 0),
    .groups = "drop"
  )

# 5. Proyecciones
proy_sem <- df_proyecciones %>%
  filter(Semana == sem_sel, Campaña == "2026") %>%
  mutate(
    Turno_num = suppressWarnings(as.numeric(Turno)),
    Módulo = case_when(
      Módulo %in% c("13.1", "13.2") ~ "13",
      Módulo %in% c("14.1", "14.2") ~ "14",
      Módulo == "9" & between(Turno_num, 1, 11)  ~ "9.2",
      Módulo == "9" & Turno_num >= 12            ~ "9.1",
      Módulo == "11" & between(Turno_num, 1, 12) ~ "11.1",
      Módulo == "11" & Turno_num >= 13           ~ "11.2",
      TRUE                                       ~ as.character(Módulo)
    ),
    Turno = as.character(Turno),
    Variedad = limpiar_variedad(Variedad)
  ) %>%
  group_by(Módulo, Turno, Variedad) %>%
  summarise(Proy_Kg = sum(Proyección, na.rm = TRUE), .groups = "drop")

# 6. Tabla Maestro de Válvulas
valvulas_calc <- valvs_cat %>%
  left_join(rc_valv_avance, by = "codigo_norm") %>%
  mutate(
    Ha_Prog      = coalesce(Ha_Prog, 0),
    Ha_Lunes_Rc  = coalesce(Ha_Lunes_Rc, 0),
    Ha_Martes_Rc = coalesce(Ha_Martes_Rc, 0),
    Ha_Faltante  = pmax(0, Área - Ha_Prog)
  ) %>%
  left_join(valv_7d_map, by = "codigo_norm") %>%
  left_join(turno_dia_habitual, by = c("Módulo", "Turno")) %>%
  mutate(
    Dia_Base_Ant      = coalesce(Dia_7d, Dia_Habitual),
    Dia_Habitual_Calc = desplazar_dia_frecuencia(Dia_Base_Ant, Frecuencia_Dias, dias_nombres, dia_inicio_idx, dia_inicio_nom),
    Tiene_Ciclo_Previo = !is.na(Dia_7d)
  )

turno_avance_ha <- valvulas_calc %>%
  group_by(Módulo, Turno, Variedad) %>%
  summarise(
    Ha_Prog_Turno     = sum(Ha_Prog, na.rm = TRUE),
    Ha_Lunes_Turno    = sum(Ha_Lunes_Rc, na.rm = TRUE),
    Ha_Martes_Turno   = sum(Ha_Martes_Rc, na.rm = TRUE),
    Ha_Faltante_Turno = sum(Ha_Faltante, na.rm = TRUE),
    Area_Total_Turno  = sum(Área, na.rm = TRUE),
    Valvulas_Total    = n(),
    Frecuencia_Dias   = first(Frecuencia_Dias),
    .groups           = "drop"
  )

rend_turno <- turno_avance_ha %>%
  left_join(kg_real_turno, by = c("Módulo", "Turno", "Variedad")) %>%
  left_join(proy_sem, by = c("Módulo", "Turno", "Variedad")) %>%
  left_join(rend_ant_turno, by = c("Módulo", "Turno", "Variedad")) %>%
  mutate(
    Kg_Real_Sem        = coalesce(Kg_Real_Sem, 0),
    Kg_Lunes_Real      = coalesce(Kg_Lunes_Real, 0),
    Kg_Martes_Real     = coalesce(Kg_Martes_Real, 0),
    Proy_Kg            = coalesce(Proy_Kg, 0),
    Rend_Ant_Turno     = coalesce(Rend_Ant_Turno, 0),
    Sap_Cosecho_Lunes  = coalesce(Sap_Cosecho_Lunes, FALSE),
    Sap_Cosecho_Martes = coalesce(Sap_Cosecho_Martes, FALSE),
    Rend_Proy          = ifelse(Area_Total_Turno > 0 & Proy_Kg > 0, Proy_Kg / Area_Total_Turno, 0),
    
    Cosecho_Lunes  = Sap_Cosecho_Lunes | (Ha_Lunes_Turno > 0) | (Kg_Lunes_Real > 0),
    Cosecho_Martes = Sap_Cosecho_Martes | (Ha_Martes_Turno > 0) | (Kg_Martes_Real > 0),
    Cosecha_Iniciada = (Cosecho_Lunes | Cosecho_Martes | Kg_Real_Sem > 0 | Ha_Prog_Turno > 0),
    
    Turno_Es_Remanente_Ant = Cosecha_Iniciada & 
                             coalesce(Ultimo_Dia_Cosecha_Ant, "") %in% c("Sábado", "Viernes"),
    
    Rend_Lunes = case_when(
      Cosecho_Lunes & Kg_Lunes_Real > 0 & Ha_Lunes_Turno > 0 ~ Kg_Lunes_Real / Ha_Lunes_Turno,
      Cosecho_Lunes & Kg_Lunes_Real > 0 & Area_Total_Turno > 0 ~ Kg_Lunes_Real / Area_Total_Turno,
      TRUE ~ 0
    ),
    Rend_Martes = case_when(
      Cosecho_Martes & Kg_Martes_Real > 0 & Ha_Martes_Turno > 0 ~ Kg_Martes_Real / Ha_Martes_Turno,
      Cosecho_Martes & Kg_Martes_Real > 0 & Area_Total_Turno > 0 ~ Kg_Martes_Real / Area_Total_Turno,
      TRUE ~ 0
    ),
    Rend_Real_Obs = case_when(
      Rend_Lunes > 0 ~ Rend_Lunes,
      Rend_Martes > 0 ~ Rend_Martes,
      Ha_Prog_Turno > 0 & Kg_Real_Sem > 0 ~ Kg_Real_Sem / Ha_Prog_Turno,
      TRUE ~ 0
    ),
    
    Rend_Turno_Aplicar = case_when(
      Cosecha_Iniciada & Rend_Ant_Turno > 0 ~ Rend_Ant_Turno,
      Cosecha_Iniciada & Rend_Real_Obs > 0 ~ Rend_Real_Obs,
      !Cosecha_Iniciada & Rend_Proy > 0 & Rend_Ant_Turno > 0 ~ pmax(0.70 * Rend_Ant_Turno, pmin(1.30 * Rend_Ant_Turno, Rend_Proy)),
      !Cosecha_Iniciada & Rend_Proy > 0 ~ Rend_Proy,
      Rend_Ant_Turno > 0 ~ Rend_Ant_Turno,
      TRUE ~ 0
    ),
    
    Permite_Segunda_Pasada_Corta = (Frecuencia_Dias <= 4L) & (Cosecho_Lunes | Cosecho_Martes)
  )

valvulas_calc <- valvulas_calc %>%
  left_join(
    rend_turno %>% select(
      Módulo, Turno, Variedad, Rend_Turno_Aplicar, Rend_Ant_Turno, Rend_Real_Obs, Rend_Proy,
      Cosecha_Iniciada, Turno_Es_Remanente_Ant, Cosecho_Lunes, Cosecho_Martes,
      Permite_Segunda_Pasada_Corta, Ultimo_Dia_Cosecha_Ant
    ),
    by = c("Módulo", "Turno", "Variedad")
  ) %>%
  mutate(
    Valvula_Cosecho_Lunes = (Ha_Lunes_Rc > 0 | (Cosecho_Lunes & is.na(Dia_7d))),
    Valvula_Cosecho_Martes = (Ha_Martes_Rc > 0 | (Cosecho_Martes & is.na(Dia_7d))),
    
    Valvula_Es_Remanente = Turno_Es_Remanente_Ant & (Valvula_Cosecho_Lunes | Valvula_Cosecho_Martes),
    
    Ha_Cosechada_Lunes = case_when(
      Ha_Lunes_Rc > 0 ~ Ha_Lunes_Rc,
      Valvula_Cosecho_Lunes ~ Área,
      TRUE ~ 0
    ),
    Ha_Cosechada_Martes = case_when(
      Ha_Martes_Rc > 0 ~ Ha_Martes_Rc,
      Valvula_Cosecho_Martes ~ Área,
      TRUE ~ 0
    ),
    Ha_Remanente_Cerrada = Ha_Cosechada_Lunes + Ha_Cosechada_Martes
  )

# A) PASADA REGULAR
reg_regular <- valvulas_calc %>%
  mutate(
    Ha_Regular = case_when(
      Valvula_Es_Remanente & Ha_Remanente_Cerrada >= 0.90 * Área ~ 0,
      Valvula_Es_Remanente ~ pmax(0, Área - Ha_Remanente_Cerrada),
      Permite_Segunda_Pasada_Corta & Ha_Prog >= 0.90 * Área ~ 0,
      Cosecha_Iniciada ~ Ha_Faltante,
      TRUE ~ Área
    ),
    
    Dia_Asignado = case_when(
      Turno_Es_Remanente_Ant ~ "Sábado",
      Cosecha_Iniciada & Ha_Prog > 0 & Ha_Prog < 0.90 * Área ~ dia_inicio_nom,
      TRUE ~ Dia_Habitual_Calc
    ),
    
    Kg_Asignado = round(Ha_Regular * Rend_Turno_Aplicar, 2)
  ) %>%
  filter(Kg_Asignado > 0, Dia_Asignado %in% dias_ajustables_nom)

# B) SEGUNDA PASADA PARA REMANENTES
reg_2da_remanente <- valvulas_calc %>%
  filter(Valvula_Es_Remanente, Ha_Remanente_Cerrada > 0) %>%
  mutate(
    Ha_2da = pmin(Área, Ha_Remanente_Cerrada),
    Dia_Asignado = case_when(
      Valvula_Cosecho_Lunes ~ "Viernes",
      Valvula_Cosecho_Martes ~ "Sábado",
      TRUE ~ "Viernes"
    ),
    Factor_Crecimiento = 4 / 7,
    Kg_Asignado = round(Ha_2da * (Rend_Turno_Aplicar * Factor_Crecimiento), 2)
  ) %>%
  filter(Kg_Asignado > 0, Dia_Asignado %in% dias_ajustables_nom)

# C) SEGUNDA PASADA PARA ROCIO
reg_2da_ciclo_corto <- valvulas_calc %>%
  filter(Permite_Segunda_Pasada_Corta, !Valvula_Es_Remanente) %>%
  mutate(
    Dia_Asignado = case_when(
      Cosecho_Lunes ~ "Jueves",
      Cosecho_Martes ~ "Viernes",
      TRUE ~ "Viernes"
    ),
    Kg_Asignado = round(Área * Rend_Turno_Aplicar, 2)
  ) %>%
  filter(Kg_Asignado > 0, Dia_Asignado %in% dias_ajustables_nom)

auto_ediciones <- bind_rows(
  reg_regular %>% select(Sector, Módulo, Turno, Variedad, Dia_Asignado, Kg_Asignado),
  reg_2da_remanente %>% select(Sector, Módulo, Turno, Variedad, Dia_Asignado, Kg_Asignado),
  reg_2da_ciclo_corto %>% select(Sector, Módulo, Turno, Variedad, Dia_Asignado, Kg_Asignado)
) %>%
  group_by(Sector, Módulo, Turno, Variedad, Campo = Dia_Asignado) %>%
  summarise(Valor = round(sum(Kg_Asignado, na.rm = TRUE), 2), .groups = "drop")

cat("=== TOTAL EDICIONES GENERADAS ===", nrow(auto_ediciones), "\n")
cat("Total Kg Auto:", format(round(sum(auto_ediciones$Valor), 1), big.mark = ","), "\n\n")

cat("=== FILTRADO PARA MODULO 4 TURNOS 7 Y 8 ===\n")
auto_ediciones %>% filter(Módulo == "4", Turno %in% c("7", "8")) %>% as.data.frame() %>% print()
