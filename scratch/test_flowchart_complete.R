load("G:/Unidades compartidas/Compartido Proyecciones/3.- Ar\u00e1ndano/datos_maestros_cosecha_2026.RData")
suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

limpiar_variedad <- function(x) toupper(trimws(gsub("\\s+", " ", as.character(x))))
norm_codigo_valvula <- function(x) toupper(trimws(gsub("\\s+", "", as.character(x))))

desplazar_dia_frecuencia <- function(dia_base_vec, frec_vec, dias_nombres, dia_inicio_idx, dia_inicio_nom) {
  idx_base <- match(dia_base_vec, dias_nombres)
  idx_target <- pmin(6L, pmax(1L, idx_base + as.integer(frec_vec) - 7L))
  dia_target <- ifelse(is.na(idx_target), dia_inicio_nom, dias_nombres[idx_target])
  ifelse(is.na(idx_target) | idx_target < dia_inicio_idx, dia_inicio_nom, dia_target)
}

sem_sel <- 44
sem_ant <- 43
es_campana_act <- TRUE
dias_nombres <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
dias_cols_kg <- c("Kg_Lunes", "Kg_Martes", "Kg_Miércoles", "Kg_Jueves", "Kg_Viernes", "Kg_Sábado")
dia_inicio_nom <- "Martes"
dia_inicio_idx <- 2
dias_ajustables_nom <- dias_nombres[dia_inicio_idx:6]

# 1. Histórico Semana Anterior (S-1)
df_ant_valvs <- df_hist_oficial_wide %>%
  filter(Semana == sem_ant) %>%
  mutate(codigo_norm = norm_codigo_valvula(Codigo_Valv))

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

turno_dia_habitual <- df_ant_valvs %>%
  filter(!is.na(Dia_7d)) %>%
  mutate(
    Turno_num = suppressWarnings(as.numeric(Turno_Norm)),
    Módulo = case_when(
      es_campana_act & Módulo_Norm %in% c("13.1", "13.2") ~ "13",
      es_campana_act & Módulo_Norm %in% c("14.1", "14.2") ~ "14",
      Módulo_Norm == "9" & between(Turno_num, 1, 11)  ~ "9.2",
      Módulo_Norm == "9" & Turno_num >= 12            ~ "9.1",
      Módulo_Norm == "11" & between(Turno_num, 1, 12) ~ "11.1",
      Módulo_Norm == "11" & Turno_num >= 13           ~ "11.2",
      TRUE                                            ~ as.character(Módulo_Norm)
    ),
    Turno = as.character(Turno_Norm)
  ) %>%
  group_by(Módulo, Turno, Dia_7d) %>%
  summarise(Total_Kg = sum(Kg_Total_Sem, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(Total_Kg)) %>%
  group_by(Módulo, Turno) %>%
  slice(1) %>%
  ungroup() %>%
  select(Módulo, Turno, Dia_Habitual = Dia_7d)

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
      TRUE                                            ~ as.character(Módulo_Norm)
    ),
    Turno    = as.character(Turno_Norm),
    Variedad = limpiar_variedad(Variedad)
  ) %>%
  group_by(Módulo, Turno, Variedad) %>%
  summarise(
    Kg_Ant_Turno   = sum(Kg_Total_Sem, na.rm = TRUE),
    Ha_Ant_Turno   = sum(Ha_Total_Sem, na.rm = TRUE),
    Rend_Ant_Turno = ifelse(Ha_Ant_Turno > 0, Kg_Ant_Turno / Ha_Ant_Turno, 0),
    Ultimo_Dia_Cosecha_Ant = {
      sub_mat <- mat_kg[cur_group_rows(), , drop = FALSE]
      c_sums <- colSums(sub_mat, na.rm = TRUE)
      d_con <- names(which(c_sums > 0))
      if (length(d_con) > 0) sub("^Kg_", "", tail(d_con, 1)) else NA_character_
    },
    .groups = "drop"
  )

# 2. Rutas de Campo Semana Actual
rc <- readRDS("D:/ACP/Rutas de Cosecha/rutas_cosecha_mapa.rds")
rc_valv_avance <- rc %>%
  filter(Semana == sem_sel) %>%
  mutate(codigo_norm = norm_codigo_valvula(codigo)) %>%
  group_by(codigo_norm) %>%
  summarise(
    Ha_Prog      = sum(coalesce(Ha_Total_Prog, 0), na.rm = TRUE),
    Kg_Prog      = sum(coalesce(Kg_Total_Prog, 0), na.rm = TRUE),
    Ha_Lunes_Rc  = sum(coalesce(Ha_Lunes, 0), na.rm = TRUE),
    Ha_Martes_Rc = sum(coalesce(Ha_Martes, 0), na.rm = TRUE),
    .groups      = "drop"
  )

# 3. Cosecha SAP Semana Actual
kg_real_turno <- df_cosecha %>%
  filter(Semana == sem_sel, Campaña == "2026") %>%
  mutate(
    Turno_num = suppressWarnings(as.numeric(Turno)),
    Módulo = case_when(
      es_campana_act & Módulo %in% c("13.1", "13.2") ~ "13",
      es_campana_act & Módulo %in% c("14.1", "14.2") ~ "14",
      Módulo == "9" & between(Turno_num, 1, 11)  ~ "9.2",
      Módulo == "9" & Turno_num >= 12            ~ "9.1",
      Módulo == "11" & between(Turno_num, 1, 12) ~ "11.1",
      Módulo == "11" & Turno_num >= 13           ~ "11.2",
      TRUE                                       ~ trimws(as.character(Módulo))
    ),
    Turno    = trimws(as.character(Turno)),
    Variedad = limpiar_variedad(Variedad),
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

# 4. Proyección Oficial Semana Actual
proy_turno <- df_proyecciones %>%
  filter(Semana == sem_sel, Campaña == "2026") %>%
  mutate(
    Módulo = case_when(
      es_campana_act & Módulo_join %in% c("13.1", "13.2") ~ "13",
      es_campana_act & Módulo_join %in% c("14.1", "14.2") ~ "14",
      TRUE ~ trimws(as.character(Módulo_join))
    ),
    Turno    = trimws(as.character(Turno_join)),
    Variedad = limpiar_variedad(Variedad_join)
  ) %>%
  group_by(Módulo, Turno, Variedad) %>%
  summarise(Proy_Kg = sum(Proyección, na.rm = TRUE), .groups = "drop")

# 5. Catálogo de Válvulas
valvulas_calc <- df_valvulas %>%
  mutate(
    codigo_norm = norm_codigo_valvula(coalesce(codigo_norm, codigo)),
    Sector      = trimws(as.character(Sector)),
    Turno_num   = suppressWarnings(as.numeric(Turno)),
    Módulo      = case_when(
      es_campana_act & Módulo %in% c("13.1", "13.2") ~ "13",
      es_campana_act & Módulo %in% c("14.1", "14.2") ~ "14",
      Módulo == "9" & between(Turno_num, 1, 11)  ~ "9.2",
      Módulo == "9" & Turno_num >= 12            ~ "9.1",
      Módulo == "11" & between(Turno_num, 1, 12) ~ "11.1",
      Módulo == "11" & Turno_num >= 13           ~ "11.2",
      TRUE                                       ~ trimws(as.character(Módulo))
    ),
    Turno       = trimws(as.character(Turno)),
    Variedad    = limpiar_variedad(Variedad),
    Área        = suppressWarnings(as.numeric(Área)),
    Frecuencia_Dias = ifelse(Variedad == "ROCIO", 3L, 7L)
  ) %>%
  left_join(rc_valv_avance, by = "codigo_norm") %>%
  mutate(
    Ha_Prog      = coalesce(Ha_Prog, 0),
    Kg_Prog      = coalesce(Kg_Prog, 0),
    Ha_Lunes_Rc  = coalesce(Ha_Lunes_Rc, 0),
    Ha_Martes_Rc = coalesce(Ha_Martes_Rc, 0),
    Ha_Faltante  = pmax(0, Área - Ha_Prog)
  ) %>%
  left_join(valv_7d_map, by = "codigo_norm") %>%
  left_join(turno_dia_habitual, by = c("Módulo", "Turno")) %>%
  mutate(
    Dia_Base_Ant            = coalesce(Dia_7d, Dia_Habitual),
    Dia_Habitual_Calc       = desplazar_dia_frecuencia(Dia_Base_Ant, Frecuencia_Dias, dias_nombres, dia_inicio_idx, dia_inicio_nom),
    Tiene_Ciclo_Previo_Valv = !is.na(Dia_7d)
  )

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

# Función auxiliar vectorizada para castigar rendimiento (Fallas #5 y #6: 10% si dif <=25%, 15% si dif >25%, piso en Rend_S1)
aplicar_castigo_rend <- function(rend_ref, rend_s1) {
  tiene_s1 <- !is.na(rend_s1) & (rend_s1 > 0)
  es_mayor <- tiene_s1 & (rend_ref > rend_s1)
  delta_pct <- ifelse(es_mayor, (rend_ref - rend_s1) / pmax(1, rend_ref), 0)
  pct_castigo <- ifelse(delta_pct > 0.25, 0.15, 0.10)
  ifelse(es_mayor, pmax(rend_s1, rend_ref * (1 - pct_castigo)), rend_ref)
}

# Evaluación del Flujograma a Nivel de Turno
rend_turno <- turno_avance_ha %>%
  left_join(kg_real_turno, by = c("Módulo", "Turno", "Variedad")) %>%
  left_join(proy_turno, by = c("Módulo", "Turno", "Variedad")) %>%
  left_join(rend_ant_turno, by = c("Módulo", "Turno", "Variedad")) %>%
  mutate(
    Kg_Real_Sem        = coalesce(Kg_Real_Sem, 0),
    Kg_Lunes_Real      = coalesce(Kg_Lunes_Real, 0),
    Kg_Martes_Real     = coalesce(Kg_Martes_Real, 0),
    Proy_Kg            = coalesce(Proy_Kg, 0),
    Kg_Ant_Turno       = coalesce(Kg_Ant_Turno, 0),
    Ha_Ant_Turno       = coalesce(Ha_Ant_Turno, 0),
    Rend_Ant_Turno     = coalesce(Rend_Ant_Turno, 0),
    Sap_Cosecho_Lunes  = coalesce(Sap_Cosecho_Lunes, FALSE),
    Sap_Cosecho_Martes = coalesce(Sap_Cosecho_Martes, FALSE),
    Rend_Proy          = ifelse(Area_Total_Turno > 0 & Proy_Kg > 0, Proy_Kg / Area_Total_Turno, 0),

    # Filtro de error/rezago mínimo en SAP (Falla #4)
    es_remanente_minimo = (Ha_Prog_Turno == 0) & (Kg_Real_Sem > 0) &
                          ((Kg_Real_Sem / pmax(1, Proy_Kg)) < 0.15 | (Kg_Real_Sem / pmax(0.1, Area_Total_Turno)) < 50),

    # Hectáreas recorridas efectivas en la semana actual (Falla #4: Rutas o estimación desde SAP)
    Rend_Base_Est_Ha = ifelse(Rend_Ant_Turno > 0, Rend_Ant_Turno, pmax(1, Rend_Proy)),
    Ha_Recorrida_Turno = case_when(
      Ha_Prog_Turno > 0   ~ Ha_Prog_Turno,
      es_remanente_minimo ~ 0,
      Kg_Real_Sem > 0     ~ pmin(Area_Total_Turno, Kg_Real_Sem / Rend_Base_Est_Ha),
      TRUE                ~ 0
    ),

    Prop_Avance_Turno = ifelse(Area_Total_Turno > 0, pmin(1, Ha_Recorrida_Turno / Area_Total_Turno), 0),

    Cosecho_Lunes  = !es_remanente_minimo & (Sap_Cosecho_Lunes | (Ha_Lunes_Turno > 0) | (Kg_Lunes_Real > 0)),
    Cosecho_Martes = !es_remanente_minimo & (Sap_Cosecho_Martes | (Ha_Martes_Turno > 0) | (Kg_Martes_Real > 0)),
    Tiene_Cosecha_Act = !es_remanente_minimo & (Cosecho_Lunes | Cosecho_Martes | Kg_Real_Sem > 0 | Ha_Prog_Turno > 0),

    # NODO 1: ¿Finalizó el turno la Semana Anterior (S-1)? (Falla #1)
    # NO finalizó si en S-1 quedó inconcluso (<95% del área) Y cerró en Viernes/Sábado, y hoy cosechó Lunes/Martes
    Semana_Ant_No_Finalizo = Tiene_Cosecha_Act &
                             (Ha_Ant_Turno > 0 & Ha_Ant_Turno < 0.95 * Area_Total_Turno) &
                             (coalesce(Ultimo_Dia_Cosecha_Ant, "") %in% c("Sábado", "Viernes")),

    # NODO 2: (Si S-1 sí finalizó) ¿Se cosechó TODO el turno entre Lunes y Martes?
    Turno_Completo_Lun_Mar = !Semana_Ant_No_Finalizo & Tiene_Cosecha_Act &
                             ((Ha_Recorrida_Turno >= 0.95 * Area_Total_Turno) | (Area_Total_Turno - Ha_Recorrida_Turno <= 0.15)),

    # NODO 3: ¿Es Rocío (o frecuencia corta <= 4 días)?
    Es_Ciclo_Corto = (Variedad == "ROCIO") | (Frecuencia_Dias <= 4L),

    # Rendimiento de la Semana Actual (con fallback Falla #4 si Rutas > 0 pero SAP aún en 0 kg)
    Rend_Sem_Actual = case_when(
      Kg_Real_Sem > 0 & Ha_Recorrida_Turno > 0 ~ Kg_Real_Sem / Ha_Recorrida_Turno,
      Ha_Prog_Turno > 0 & Kg_Prog_Turno > 0    ~ Kg_Prog_Turno / Ha_Prog_Turno,
      Ha_Prog_Turno > 0 & Rend_Ant_Turno > 0   ~ Rend_Ant_Turno,
      Ha_Prog_Turno > 0 & Rend_Proy > 0        ~ Rend_Proy,
      TRUE                                     ~ 0
    ),

    # NODO 5, 6 y 7: Cálculo del Rendimiento a Aplicar según rama del Flujograma
    Rend_Turno_Aplicar = case_when(
      # Rama Izquierda (Nodo 1 = NO finalizó S-1): Usa Rendimiento S-1 sin los kilos del lunes
      Semana_Ant_No_Finalizo ~ ifelse(Rend_Ant_Turno > 0, Rend_Ant_Turno, ifelse(Rend_Sem_Actual > 0, Rend_Sem_Actual, Rend_Proy)),

      # Rama Derecha -> Nodo 2 = SÍ (Todo cosechado Lun/Mar) -> Nodo 3 = SÍ (Rocío): Usa Rend Sem Actual
      Turno_Completo_Lun_Mar & Es_Ciclo_Corto ~ ifelse(Rend_Sem_Actual > 0, Rend_Sem_Actual, ifelse(Rend_Ant_Turno > 0, Rend_Ant_Turno, Rend_Proy)),

      # Rama Derecha -> Nodo 2 = SÍ -> Nodo 3 = NO (No es Rocío): No ajustar (0)
      Turno_Completo_Lun_Mar & !Es_Ciclo_Corto ~ 0,

      # Rama Derecha -> Nodo 4 = NO (Sin Kilos / Sin Cosecha en Sem Actual) -> Nodo 5 (Comparar Proy vs S-1)
      !Tiene_Cosecha_Act ~ ifelse(
        Rend_Proy > 0,
        aplicar_castigo_rend(Rend_Proy, Rend_Ant_Turno),
        Rend_Ant_Turno
      ),

      # Rama Derecha -> Nodo 4 = SÍ (Con Cosecha Parcial) -> Nodo 6 = SÍ (Se cosechó > 30%) -> Nodo 7 (Comparar Actual vs S-1)
      Tiene_Cosecha_Act & (Prop_Avance_Turno > 0.30) ~ {
        ref_act <- ifelse(Rend_Sem_Actual > 0, Rend_Sem_Actual, ifelse(Rend_Ant_Turno > 0, Rend_Ant_Turno, Rend_Proy))
        aplicar_castigo_rend(ref_act, Rend_Ant_Turno)
      },

      # Rama Derecha -> Nodo 4 = SÍ (Con Cosecha Parcial) -> Nodo 6 = NO (Se cosechó <= 30%) -> Nodo 7 (Comparar Proy vs S-1)
      Tiene_Cosecha_Act & (Prop_Avance_Turno <= 0.30) ~ {
        ref_proy <- ifelse(Rend_Proy > 0, Rend_Proy, ifelse(Rend_Sem_Actual > 0, Rend_Sem_Actual, Rend_Ant_Turno))
        aplicar_castigo_rend(ref_proy, Rend_Ant_Turno)
      },

      TRUE ~ 0
    )
  )

cat("Resumen de clasificación de turnos por rama del flujograma:\n")
rend_turno %>%
  mutate(
    Rama = case_when(
      Semana_Ant_No_Finalizo ~ "1. Rama Izq: S-1 No Finalizó (Remanente)",
      Turno_Completo_Lun_Mar & Es_Ciclo_Corto ~ "2A. Rama Der: 100% Lun/Mar -> Rocío (2da Pasada)",
      Turno_Completo_Lun_Mar & !Es_Ciclo_Corto ~ "2B. Rama Der: 100% Lun/Mar -> No Rocío (No Ajustar)",
      !Tiene_Cosecha_Act ~ "3A. Rama Der: Sin Kilos (Proy vs S-1)",
      Tiene_Cosecha_Act & Prop_Avance_Turno > 0.30 ~ "3B-1. Rama Der: Con Kilos >30% (Actual vs S-1)",
      Tiene_Cosecha_Act & Prop_Avance_Turno <= 0.30 ~ "3B-2. Rama Der: Con Kilos <=30% (Proy vs S-1)"
    )
  ) %>%
  count(Rama) %>%
  as.data.frame() %>%
  print()
