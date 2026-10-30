suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
})

load("G:/Unidades compartidas/Compartido Proyecciones/3.- Arándano/datos_maestros_cosecha_2026.RData")

app_env <- new.env()
sys.source("d:/ACP/Actualizacion Semanal/app.R", envir = app_env)

# Simulate executing the adjustment on M4
sem_sel <- 44
sem_ant <- 43
es_campana_act <- TRUE
dias_nombres <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
dias_cols_kg <- c("Kg_Lunes", "Kg_Martes", "Kg_Miércoles", "Kg_Jueves", "Kg_Viernes", "Kg_Sábado")
dia_inicio_nom <- "Martes"
dia_inicio_idx <- 2
dias_ajustables_nom <- dias_nombres[2:6]

# Frecuencias
vars_todas <- sort(unique(app_env$limpiar_variedad(df_valvulas$Variedad)))
frecuencias_var <- tibble(
  Variedad = vars_todas,
  Frecuencia_Dias = ifelse(vars_todas == "ROCIO", 3L, 7L)
)

# 1. Historico S-1
df_ant_valvs <- df_hist_oficial_wide %>%
  filter(Semana == sem_ant) %>%
  mutate(codigo_norm = app_env$norm_codigo_valvula(Codigo_Valv))

cols_presentes <- intersect(dias_cols_kg, names(df_ant_valvs))
mat_kg <- as.matrix(df_ant_valvs[, cols_presentes])
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

rend_ant_turno <- df_ant_valvs %>%
  mutate(
    Turno_num = suppressWarnings(as.numeric(Turno_Norm)),
    Módulo = case_when(
      es_campana_act & Módulo_Norm %in% c("13.1", "13.2") ~ "13",
      es_campana_act & Módulo_Norm %in% c("14.1", "14.2") ~ "14",
      Módulo_Norm == "9" & between(Turno_num, 1, 11)  ~ "9.2",
      Módulo_Norm == "9" & Turno_num >= 12            ~ "9.1",
      Módulo_Norm == "11" & between(Turno_num, 1, 12) ~ "11.1",
      Módulo_Norm == "11" & Turno_num >= 13           ~ "11.2",
      TRUE ~ as.character(Módulo_Norm)
    ),
    Turno = as.character(Turno_Norm),
    Variedad = app_env$limpiar_variedad(Variedad)
  ) %>%
  group_by(Módulo, Turno, Variedad) %>%
  summarise(
    Kg_Ant_Turno = sum(Kg_Total_Sem, na.rm = TRUE),
    Ha_Ant_Turno = sum(Ha_Total_Sem, na.rm = TRUE),
    Rend_Ant_Turno = ifelse(Ha_Ant_Turno > 0, Kg_Ant_Turno / Ha_Ant_Turno, 0),
    Ultimo_Dia_Cosecha_Ant = {
      dias_con_kg <- names(which(colSums(mat_kg[cur_group_rows(), , drop = FALSE], na.rm = TRUE) > 0))
      if (length(dias_con_kg) > 0) sub("^Kg_", "", tail(dias_con_kg, 1)) else NA_character_
    },
    .groups = "drop"
  )

# 2. Rutas
rc_df <- app_env$cargar_rutas_cosecha()
rc_actual <- rc_df %>%
  filter(as.character(Campaña) == "2026", Semana == sem_sel, Tipo_Registro == "CosechaCampo") %>%
  mutate(
    codigo_norm = app_env$norm_codigo_valvula(codigo),
    Ha_Lunes_rc = coalesce(Ha_Lunes, 0),
    Ha_Martes_rc = coalesce(Ha_Martes, 0),
    Ha_Prog_valv = pmax(coalesce(Ha_Total_Prog, 0), Ha_Lunes_rc + Ha_Martes_rc),
    Kg_Prog_valv = coalesce(Kg_Total_Prog, 0)
  )

valv_rc_map <- rc_actual %>%
  group_by(codigo_norm) %>%
  summarise(
    Ha_Prog = sum(Ha_Prog_valv, na.rm = TRUE),
    Kg_Prog = sum(Kg_Prog_valv, na.rm = TRUE),
    Ha_Lunes_Rc = sum(Ha_Lunes_rc, na.rm = TRUE),
    Ha_Martes_Rc = sum(Ha_Martes_rc, na.rm = TRUE),
    .groups = "drop"
  )

# 3. SAP
df_cosecha_sem <- app_env$df_cosecha_inicial %>%
  filter(as.character(Campaña) == "2026", Semana == sem_sel, coalesce(Tipo_Cosecha, "Comercial") == "Comercial")

kg_real_turno <- df_cosecha_sem %>%
  mutate(
    Módulo = trimws(as.character(Módulo)),
    Turno = trimws(as.character(Turno)),
    Variedad = app_env$limpiar_variedad(Variedad),
    wday_num = lubridate::wday(Fecha, week_start = 1)
  ) %>%
  group_by(Módulo, Turno, Variedad) %>%
  summarise(
    Kg_Real_Sem        = sum(`Kg Total`, na.rm = TRUE),
    Kg_Lunes_Real      = sum(`Kg Total`[wday_num == 1], na.rm = TRUE),
    Kg_Martes_Real     = sum(`Kg Total`[wday_num == 2], na.rm = TRUE),
    Sap_Cosecho_Lunes  = any(wday_num == 1 & `Kg Total` > 0),
    Sap_Cosecho_Martes = any(wday_num == 2 & `Kg Total` > 0),
    .groups            = "drop"
  )

proy_turno <- app_env$df_proyecciones %>%
  filter(as.character(Campaña) == "2026", Semana == sem_sel) %>%
  mutate(
    Módulo = trimws(as.character(Módulo)),
    Turno = trimws(as.character(Turno)),
    Variedad = app_env$limpiar_variedad(Variedad)
  ) %>%
  group_by(Módulo, Turno, Variedad) %>%
  summarise(Proy_Kg = sum(Proyección, na.rm = TRUE), .groups = "drop")

# 4. Válvulas
valvulas_calc <- df_valvulas %>%
  filter(as.character(Módulo) == "4") %>%
  mutate(
    codigo_norm = app_env$norm_codigo_valvula(codigo),
    Módulo = as.character(Módulo),
    Turno = as.character(Turno),
    Variedad = app_env$limpiar_variedad(Variedad)
  ) %>%
  left_join(valv_7d_map, by = "codigo_norm") %>%
  left_join(valv_rc_map, by = "codigo_norm") %>%
  left_join(frecuencias_var, by = "Variedad") %>%
  mutate(
    Ha_Prog         = coalesce(Ha_Prog, 0),
    Kg_Prog         = coalesce(Kg_Prog, 0),
    Ha_Lunes_Rc     = coalesce(Ha_Lunes_Rc, 0),
    Ha_Martes_Rc    = coalesce(Ha_Martes_Rc, 0),
    Frecuencia_Dias = coalesce(Frecuencia_Dias, 7L),
    Ha_Faltante     = pmax(0, Área - Ha_Prog),
    Tiene_Ciclo_Previo_Valv = !is.na(Dia_7d),
    Dia_Habitual_Calc = app_env$desplazar_dia_frecuencia(Dia_7d, Frecuencia_Dias, dias_nombres, dia_inicio_idx, dia_inicio_nom)
  )

# Avance turno
turno_avance_ha <- valvulas_calc %>%
  group_by(Módulo, Turno, Variedad) %>%
  summarise(
    Ha_Prog_Turno     = sum(Ha_Prog, na.rm = TRUE),
    Kg_Prog_Turno     = sum(Kg_Prog, na.rm = TRUE),
    Ha_Lunes_Turno    = sum(Ha_Lunes_Rc, na.rm = TRUE),
    Ha_Martes_Turno   = sum(Ha_Martes_Rc, na.rm = TRUE),
    Ha_Faltante_Turno = sum(Ha_Faltante, na.rm = TRUE),
    Area_Total_Turno  = sum(Área, na.rm = TRUE),
    Valvulas_Total    = n(),
    Frecuencia_Dias   = first(Frecuencia_Dias),
    .groups           = "drop"
  )

aplicar_castigo_rend <- function(rend_ref, rend_s1) {
  brecha <- ifelse(rend_ref > 0, (rend_ref - rend_s1) / rend_ref, 0)
  pct_castigo <- ifelse(brecha > 0.25, 0.15, 0.10)
  ifelse(rend_s1 > 0 & rend_ref > rend_s1, pmax(rend_s1, rend_ref * (1 - pct_castigo)), rend_ref)
}

rend_turno <- turno_avance_ha %>%
  left_join(kg_real_turno, by = c("Módulo", "Turno", "Variedad")) %>%
  left_join(proy_turno, by = c("Módulo", "Turno", "Variedad")) %>%
  left_join(rend_ant_turno, by = c("Módulo", "Turno", "Variedad")) %>%
  mutate(
    Kg_Real_Sem        = coalesce(Kg_Real_Sem, 0),
    Kg_Lunes_Real      = coalesce(Kg_Lunes_Real, 0),
    Kg_Martes_Real     = coalesce(Kg_Martes_Real, 0),
    Proy_Kg            = coalesce(Proy_Kg, 0),
    Ha_Ant_Turno       = coalesce(Ha_Ant_Turno, 0),
    Kg_Ant_Turno       = coalesce(Kg_Ant_Turno, 0),
    Rend_Ant_Turno     = coalesce(Rend_Ant_Turno, 0),
    Sap_Cosecho_Lunes  = coalesce(Sap_Cosecho_Lunes, FALSE),
    Sap_Cosecho_Martes = coalesce(Sap_Cosecho_Martes, FALSE),
    Rend_Proy          = ifelse(Area_Total_Turno > 0 & Proy_Kg > 0, Proy_Kg / Area_Total_Turno, 0),
    es_remanente_minimo = (Ha_Prog_Turno == 0) & (Kg_Real_Sem > 0) & 
                          ((Kg_Real_Sem / pmax(1, Proy_Kg)) < 0.15 | (Kg_Real_Sem / pmax(0.1, Area_Total_Turno)) < 50),
    Cosecho_Lunes     = Sap_Cosecho_Lunes | (Ha_Lunes_Turno > 0) | (Kg_Lunes_Real > 0),
    Cosecho_Martes    = Sap_Cosecho_Martes | (Ha_Martes_Turno > 0) | (Kg_Martes_Real > 0),
    Tiene_Cosecha_Act = !es_remanente_minimo & (Cosecho_Lunes | Cosecho_Martes | Kg_Real_Sem > 0 | Ha_Prog_Turno > 0),
    Ha_Recorrida_Turno = case_when(
      Ha_Prog_Turno > 0   ~ Ha_Prog_Turno,
      es_remanente_minimo ~ round(pmin(Area_Total_Turno, Kg_Real_Sem / pmax(1, Rend_Proy)), 2),
      Kg_Real_Sem > 0     ~ Area_Total_Turno,
      TRUE                ~ 0
    ),
    Prop_Avance_Turno = ifelse(Area_Total_Turno > 0, pmin(1, Ha_Recorrida_Turno / Area_Total_Turno), 0),
    Semana_Ant_No_Finalizo = (Ha_Ant_Turno > 0) &
                             (Ha_Ant_Turno < 0.95 * Area_Total_Turno) &
                             (coalesce(Ultimo_Dia_Cosecha_Ant, "") %in% c("Sábado", "Viernes")) &
                             (Cosecho_Lunes | Cosecho_Martes),
    Turno_Completo_Lun_Mar = !Semana_Ant_No_Finalizo & Tiene_Cosecha_Act &
                             ((Ha_Recorrida_Turno >= 0.95 * Area_Total_Turno) | (Area_Total_Turno - Ha_Recorrida_Turno <= 0.15)),
    Es_Ciclo_Corto         = (Variedad == "ROCIO") | (Frecuencia_Dias <= 4L),
    Ha_Con_Kg_Sap = ifelse(Kg_Lunes_Real > 0, ifelse(Ha_Lunes_Turno > 0, Ha_Lunes_Turno, Area_Total_Turno), 0) +
                    ifelse(Kg_Martes_Real > 0, ifelse(Ha_Martes_Turno > 0, Ha_Martes_Turno, Area_Total_Turno), 0),
    Rend_Sem_Actual_Bruto = case_when(
      Kg_Real_Sem > 0 & Ha_Con_Kg_Sap > 0      ~ Kg_Real_Sem / Ha_Con_Kg_Sap,
      Kg_Real_Sem > 0 & Ha_Recorrida_Turno > 0 ~ Kg_Real_Sem / Ha_Recorrida_Turno,
      TRUE                                     ~ 0
    ),
    Rend_Sem_Actual = case_when(
      Rend_Sem_Actual_Bruto > 0 ~ Rend_Sem_Actual_Bruto,
      Rend_Ant_Turno > 0        ~ Rend_Ant_Turno,
      TRUE                      ~ Rend_Proy
    ),
    Rend_Base_Turno = case_when(
      Semana_Ant_No_Finalizo ~ ifelse(Rend_Ant_Turno > 0, Rend_Ant_Turno, Rend_Proy),
      Turno_Completo_Lun_Mar & Es_Ciclo_Corto ~ Rend_Sem_Actual,
      !Tiene_Cosecha_Act ~ aplicar_castigo_rend(Rend_Proy, Rend_Ant_Turno),
      Tiene_Cosecha_Act & Prop_Avance_Turno > 0.30 ~ aplicar_castigo_rend(Rend_Sem_Actual, Rend_Ant_Turno),
      Tiene_Cosecha_Act & Prop_Avance_Turno <= 0.30 ~ aplicar_castigo_rend(Rend_Proy, Rend_Ant_Turno),
      TRUE ~ Rend_Proy
    ),
    Rend_Turno_Aplicar = ifelse(Turno_Completo_Lun_Mar & !Es_Ciclo_Corto, 0, Rend_Base_Turno)
  )

valvulas_calc <- valvulas_calc %>%
  left_join(
    rend_turno %>% select(
      Módulo, Turno, Variedad, Rend_Turno_Aplicar, Rend_Base_Turno, Rend_Ant_Turno, Rend_Sem_Actual, Rend_Proy,
      Tiene_Cosecha_Act, Semana_Ant_No_Finalizo, Turno_Completo_Lun_Mar, Es_Ciclo_Corto,
      Cosecho_Lunes, Cosecho_Martes, Kg_Lunes_Real, Kg_Martes_Real, Ultimo_Dia_Cosecha_Ant, Prop_Avance_Turno
    ),
    by = c("Módulo", "Turno", "Variedad")
  ) %>%
  mutate(
    Valvula_Cosecho_Lunes  = (Ha_Lunes_Rc > 0) | (Semana_Ant_No_Finalizo & Cosecho_Lunes & is.na(Dia_7d) & Ha_Prog == 0),
    Valvula_Cosecho_Martes = (Ha_Martes_Rc > 0) | (Semana_Ant_No_Finalizo & !Cosecho_Lunes & Cosecho_Martes & is.na(Dia_7d) & Ha_Prog == 0),
    Valvula_Cerro_En_Lun_Mar = Semana_Ant_No_Finalizo & (Valvula_Cosecho_Lunes | Valvula_Cosecho_Martes),
    Ha_Cosechada_Lunes = case_when(
      Ha_Lunes_Rc > 0       ~ Ha_Lunes_Rc,
      Valvula_Cosecho_Lunes ~ Área,
      TRUE                  ~ 0
    ),
    Ha_Cosechada_Martes = case_when(
      Ha_Martes_Rc > 0       ~ Ha_Martes_Rc,
      Valvula_Cosecho_Martes ~ Área,
      TRUE                   ~ 0
    ),
    Ha_Cerrada_Lun_Mar = pmin(Área, Ha_Cosechada_Lunes + Ha_Cosechada_Martes)
  )

# Components with Ha_Asignada
reg_ruta_lun <- valvulas_calc %>%
  filter(Ha_Lunes_Rc > 0, Kg_Lunes_Real == 0) %>%
  mutate(Dia_Asignado = "Lunes", Kg_Asignado = round(Ha_Lunes_Rc * coalesce(Rend_Base_Turno, 0), 2), Ha_Asignada = Ha_Lunes_Rc, Es_Ruta_Sin_Kg = TRUE) %>%
  filter(Kg_Asignado > 0, Dia_Asignado %in% dias_ajustables_nom)

reg_ruta_mar <- valvulas_calc %>%
  filter(Ha_Martes_Rc > 0, Kg_Martes_Real == 0) %>%
  mutate(Dia_Asignado = "Martes", Kg_Asignado = round(Ha_Martes_Rc * coalesce(Rend_Base_Turno, 0), 2), Ha_Asignada = Ha_Martes_Rc, Es_Ruta_Sin_Kg = TRUE) %>%
  filter(Kg_Asignado > 0, Dia_Asignado %in% dias_ajustables_nom)

reg_regular <- valvulas_calc %>%
  filter(!Turno_Completo_Lun_Mar) %>%
  mutate(
    Ha_Regular = case_when(
      Semana_Ant_No_Finalizo & Valvula_Cerro_En_Lun_Mar & Ha_Cerrada_Lun_Mar >= 0.90 * Área ~ 0,
      Semana_Ant_No_Finalizo & Valvula_Cerro_En_Lun_Mar ~ pmax(0, Área - Ha_Cerrada_Lun_Mar),
      Semana_Ant_No_Finalizo ~ Área,
      Tiene_Cosecha_Act ~ Ha_Faltante,
      TRUE ~ Área
    ),
    Dia_Asignado = case_when(
      Semana_Ant_No_Finalizo & Tiene_Ciclo_Previo_Valv ~ Dia_Habitual_Calc,
      Semana_Ant_No_Finalizo & !Tiene_Ciclo_Previo_Valv & !Valvula_Cerro_En_Lun_Mar ~ dia_inicio_nom,
      Semana_Ant_No_Finalizo ~ dia_inicio_nom,
      Tiene_Cosecha_Act & Ha_Prog > 0 & Ha_Prog < 0.90 * Área ~ dia_inicio_nom,
      TRUE ~ Dia_Habitual_Calc
    ),
    Kg_Asignado = round(Ha_Regular * coalesce(Rend_Turno_Aplicar, 0), 2),
    Ha_Asignada = Ha_Regular,
    Es_Ruta_Sin_Kg = FALSE
  ) %>%
  filter(Kg_Asignado > 0, Dia_Asignado %in% dias_ajustables_nom)

reg_2da_remanente <- valvulas_calc %>%
  filter(Semana_Ant_No_Finalizo, Valvula_Cerro_En_Lun_Mar, Ha_Cerrada_Lun_Mar > 0) %>%
  mutate(
    Ha_2da = Ha_Cerrada_Lun_Mar,
    Dia_Cierre_Idx = ifelse(Valvula_Cosecho_Lunes, 1L, 2L),
    Dia_Asignado = case_when(
      Valvula_Cosecho_Lunes  ~ "Viernes",
      Valvula_Cosecho_Martes ~ "Sábado",
      TRUE                   ~ "Viernes"
    ),
    Dia_Nuevo_Idx = match(Dia_Asignado, dias_nombres),
    Dias_Crecimiento = pmax(1L, Dia_Nuevo_Idx - Dia_Cierre_Idx),
    Factor_Crecimiento = pmin(1, Dias_Crecimiento / pmax(1, Frecuencia_Dias)),
    Kg_Asignado = round(Ha_2da * coalesce(Rend_Turno_Aplicar, 0) * Factor_Crecimiento, 2),
    Ha_Asignada = Ha_2da,
    Es_Ruta_Sin_Kg = FALSE
  ) %>%
  filter(Kg_Asignado > 0, Dia_Asignado %in% dias_ajustables_nom)

reg_2da_ciclo_corto <- valvulas_calc %>%
  filter(!Semana_Ant_No_Finalizo, Es_Ciclo_Corto, (Turno_Completo_Lun_Mar | (Ha_Lunes_Rc > 0 | Ha_Martes_Rc > 0))) %>%
  mutate(
    Ha_2da_Corta = case_when(Turno_Completo_Lun_Mar ~ Área, TRUE ~ pmin(Área, Ha_Lunes_Rc + Ha_Martes_Rc)),
    Dia_Base_Corta = case_when(Ha_Lunes_Rc > 0 ~ "Lunes", Ha_Martes_Rc > 0 ~ "Martes", Cosecho_Lunes ~ "Lunes", Cosecho_Martes ~ "Martes", TRUE ~ "Lunes"),
    Dia_Asignado = as.character(app_env$desplazar_dia_frecuencia(Dia_Base_Corta, Frecuencia_Dias, dias_nombres, dia_inicio_idx, dia_inicio_nom)),
    Kg_Asignado = round(Ha_2da_Corta * coalesce(Rend_Turno_Aplicar, 0), 2),
    Ha_Asignada = Ha_2da_Corta,
    Es_Ruta_Sin_Kg = FALSE
  ) %>%
  filter(Kg_Asignado > 0, Dia_Asignado %in% dias_ajustables_nom)

registros_todos <- bind_rows(
  reg_ruta_lun %>% select(Sector, Módulo, Turno, Variedad, Dia_Asignado, Kg_Asignado, Ha_Asignada),
  reg_ruta_mar %>% select(Sector, Módulo, Turno, Variedad, Dia_Asignado, Kg_Asignado, Ha_Asignada),
  reg_regular %>% select(Sector, Módulo, Turno, Variedad, Dia_Asignado, Kg_Asignado, Ha_Asignada),
  reg_2da_remanente %>% select(Sector, Módulo, Turno, Variedad, Dia_Asignado, Kg_Asignado, Ha_Asignada),
  reg_2da_ciclo_corto %>% select(Sector, Módulo, Turno, Variedad, Dia_Asignado, Kg_Asignado, Ha_Asignada)
)

auto_ediciones <- registros_todos %>%
  group_by(Sector, Módulo, Turno, Variedad, Campo = Dia_Asignado) %>%
  summarise(
    Valor = round(sum(Kg_Asignado, na.rm = TRUE), 2),
    Ha_Asignada = round(sum(Ha_Asignada, na.rm = TRUE), 2),
    .groups = "drop"
  )

cat("\n--- DETALLE DE DIAS AJUSTADOS EN MODULO 4 (VALOR Y HA ASIGNADA) ---\n")
auto_ediciones %>%
  arrange(suppressWarnings(as.numeric(Turno)), Campo) %>%
  print(n = 30)

cat("\n--- TOTAL HA AJUSTADA POR TURNO EN MODULO 4 ---\n")
auto_ediciones %>%
  group_by(Módulo, Turno, Variedad) %>%
  summarise(
    Kg_Ajustados_Total = sum(Valor),
    Ha_Ajustada_Real   = sum(Ha_Asignada),
    .groups = "drop"
  ) %>%
  arrange(suppressWarnings(as.numeric(Turno))) %>%
  print()
