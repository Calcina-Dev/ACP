suppressMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
})

cat("=================================================================\n")
cat("TESTING FULL END-TO-END FIX FOR MÓDULO 6 (Turnos 3 y 4)\n")
cat("=================================================================\n\n")

hist_wide <- readRDS("D:/ACP/Rutas de Cosecha/historico_oficial_wide_2026.rds")
rc_map <- readRDS("D:/ACP/Rutas de Cosecha/rutas_cosecha_mapa.rds")
df_valvulas <- readRDS("D:/ACP/Rutas de Cosecha/valvulas_normalizadas.rds")

norm_codigo_valvula <- function(x) toupper(gsub("[^A-Za-z0-9]", "", as.character(x)))
limpiar_variedad <- function(x) toupper(trimws(as.character(x)))

desplazar_dia_frecuencia <- function(dia_base_vec, frec_vec, dias_nombres, dia_inicio_idx, dia_inicio_nom, dia_fin_idx = 6L, dia_fin_nom = "Sábado") {
  idx_base <- match(dia_base_vec, dias_nombres)
  frec_int <- as.integer(frec_vec)
  desp <- ifelse(frec_int <= 4L, frec_int, frec_int - 7L)
  idx_target <- pmin(dia_fin_idx, pmax(1L, idx_base + desp))
  dia_target <- ifelse(is.na(idx_target), dia_inicio_nom, dias_nombres[idx_target])
  dia_res <- ifelse(is.na(idx_target) | idx_target < dia_inicio_idx, dia_inicio_nom, dia_target)
  idx_res <- match(dia_res, dias_nombres)
  ifelse(!is.na(idx_res) & idx_res > dia_fin_idx, dia_fin_nom, dia_res)
}

sem_sel <- 45
sem_ant <- 44
dia_inicio_nom <- "Viernes"
dia_fin_nom <- "Viernes"
dias_nombres <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
dias_ajustables_nom <- c("Viernes")
dia_inicio_idx <- match(dia_inicio_nom, dias_nombres)
dia_fin_idx <- match(dia_fin_nom, dias_nombres)
dias_previos_nom <- c("Lunes", "Martes", "Miércoles", "Jueves")
cols_ha_prev <- paste0("Ha_", dias_previos_nom)
cols_kg_prev <- paste0("Kg_", dias_previos_nom)

rc_sem <- rc_map %>% filter(as.numeric(Semana) == sem_sel)

rc_valv_avance <- if (nrow(rc_sem) > 0 && "codigo" %in% names(rc_sem)) {
  rc_temp <- rc_sem %>% mutate(codigo_norm = norm_codigo_valvula(codigo))
  dias_cols_all <- paste0("Ha_", dias_nombres)
  dias_cols_ex  <- intersect(dias_cols_all, names(rc_temp))
  if (length(dias_cols_ex) > 0) {
    mat_ha <- as.matrix(rc_temp[, dias_cols_ex])
    has_any <- rowSums(mat_ha > 0, na.rm = TRUE) > 0
    max_idx <- max.col(mat_ha, ties.method = "first")
    dias_noms_sub <- sub("^Ha_", "", dias_cols_ex)
    rc_temp$Dia_Ruta_Sem <- ifelse(has_any, dias_noms_sub[max_idx], NA_character_)
  } else {
    rc_temp$Dia_Ruta_Sem <- NA_character_
  }
  
  ha_prev_vec <- numeric(nrow(rc_temp))
  kg_prev_vec <- numeric(nrow(rc_temp))
  for (d in dias_previos_nom) {
    col_h <- paste0("Ha_", d)
    col_k <- paste0("Kg_", d)
    val_h <- if (col_h %in% names(rc_temp)) coalesce(rc_temp[[col_h]], 0) else 0
    val_k <- if (col_k %in% names(rc_temp)) coalesce(rc_temp[[col_k]], 0) else 0
    es_ejecutado <- (d %in% c("Lunes", "Martes"))
    ha_prev_vec <- ha_prev_vec + ifelse(es_ejecutado, val_h, 0)
    kg_prev_vec <- kg_prev_vec + ifelse(es_ejecutado, val_k, 0)
  }
  rc_temp$Ha_Prev_Valv <- ha_prev_vec
  rc_temp$Kg_Prev_Valv <- kg_prev_vec
  
  rc_temp %>%
    group_by(codigo_norm) %>%
    summarise(
      Ha_Prog      = sum(coalesce(Ha_Prev_Valv, 0), na.rm = TRUE),
      Kg_Prog      = sum(coalesce(Kg_Prev_Valv, 0), na.rm = TRUE),
      Ha_Lunes_Rc  = sum(coalesce(Ha_Lunes, 0), na.rm = TRUE),
      Kg_Lunes_Rc  = if ("Kg_Lunes" %in% names(.)) sum(coalesce(Kg_Lunes, 0), na.rm = TRUE) else 0,
      Ha_Martes_Rc = sum(coalesce(Ha_Martes, 0), na.rm = TRUE),
      Kg_Martes_Rc = if ("Kg_Martes" %in% names(.)) sum(coalesce(Kg_Martes, 0), na.rm = TRUE) else 0,
      Dia_Ruta_Sem = first(na.omit(Dia_Ruta_Sem)),
      .groups      = "drop"
    )
} else tibble()

cols_ha_sap_exist <- intersect(cols_ha_prev, names(hist_wide))
cols_kg_sap_exist <- intersect(cols_kg_prev, names(hist_wide))

sap_valv_avance <- if (nrow(hist_wide) > 0) {
  hist_wide %>%
    filter(as.numeric(Semana) == sem_sel) %>%
    mutate(
      codigo_norm = norm_codigo_valvula(Codigo_Valv),
      Ha_Sap_Prev = if (length(cols_ha_sap_exist) > 0) rowSums(across(all_of(cols_ha_sap_exist)), na.rm = TRUE) else 0,
      Kg_Sap_Prev = if (length(cols_kg_sap_exist) > 0) rowSums(across(all_of(cols_kg_sap_exist)), na.rm = TRUE) else 0
    ) %>%
    group_by(codigo_norm) %>%
    summarise(
      Ha_Prog_Sap      = sum(coalesce(Ha_Sap_Prev, 0), na.rm = TRUE),
      Kg_Prog_Sap      = sum(coalesce(Kg_Sap_Prev, 0), na.rm = TRUE),
      Ha_Lunes_Sap     = sum(coalesce(Ha_Lunes, 0), na.rm = TRUE),
      Kg_Lunes_Sap     = if ("Kg_Lunes" %in% names(.)) sum(coalesce(Kg_Lunes, 0), na.rm = TRUE) else 0,
      Ha_Martes_Sap    = sum(coalesce(Ha_Martes, 0), na.rm = TRUE),
      Kg_Martes_Sap    = if ("Kg_Martes" %in% names(.)) sum(coalesce(Kg_Martes, 0), na.rm = TRUE) else 0,
      .groups          = "drop"
    )
} else tibble()

cols_kg_all <- paste0("Kg_", dias_nombres)
df_ant_valvs <- hist_wide %>% filter(as.numeric(Semana) == sem_ant)
df_ant_valvs$Kg_Total_Ant <- rowSums(df_ant_valvs[, intersect(cols_kg_all, names(df_ant_valvs))], na.rm = TRUE)
df_ant_valvs$Ha_Total_Ant <- rowSums(df_ant_valvs[, intersect(paste0("Ha_", dias_nombres), names(df_ant_valvs))], na.rm = TRUE)

rend_ant_turno <- df_ant_valvs %>%
  group_by(Módulo = as.character(Módulo_Norm), Turno = as.character(Turno_Norm), Variedad = limpiar_variedad(Variedad)) %>%
  summarise(
    Kg_Ant_Turno   = sum(Kg_Total_Ant, na.rm = TRUE),
    Ha_Ant_Turno   = sum(Ha_Total_Ant, na.rm = TRUE),
    Rend_Ant_Turno = ifelse(Ha_Ant_Turno > 0, Kg_Ant_Turno / Ha_Ant_Turno, 0),
    Ultimo_Dia_Cosecha_Ant = {
      tot_dia <- colSums(across(any_of(cols_kg_all)), na.rm = TRUE)
      dias_con_kg <- names(tot_dia)[tot_dia > 0]
      if (length(dias_con_kg) > 0) sub("^Kg_", "", tail(dias_con_kg, 1)) else NA_character_
    },
    .groups = "drop"
  )

df_cosecha_sem <- hist_wide %>% filter(as.numeric(Semana) == sem_sel)
df_cosecha_sem$Kg_Total_Sem <- rowSums(df_cosecha_sem[, intersect(cols_kg_all, names(df_cosecha_sem))], na.rm = TRUE)

kg_real_turno <- df_cosecha_sem %>%
  group_by(Módulo = as.character(Módulo_Norm), Turno = as.character(Turno_Norm), Variedad = limpiar_variedad(Variedad)) %>%
  summarise(
    Kg_Real_Sem        = sum(Kg_Total_Sem, na.rm = TRUE),
    Kg_Lunes_Real      = sum(Kg_Lunes, na.rm = TRUE),
    Kg_Martes_Real     = sum(Kg_Martes, na.rm = TRUE),
    Sap_Cosecho_Previo = any(Kg_Total_Sem > 0),
    Sap_Cosecho_Lunes  = any(Kg_Lunes > 0),
    Sap_Cosecho_Martes = any(Kg_Martes > 0),
    .groups            = "drop"
  )

valv_7d_map <- df_ant_valvs %>%
  filter(Kg_Total_Ant > 0) %>%
  mutate(
    codigo_norm = norm_codigo_valvula(Codigo_Valv),
    Dia_7d = {
      mat_kg <- as.matrix(select(., any_of(cols_kg_all)))
      cols_presentes <- intersect(cols_kg_all, names(.))
      if (ncol(mat_kg) > 0) {
        max_idx <- max.col(mat_kg, ties.method = "first")
        sub("^Kg_", "", cols_presentes[max_idx])
      } else NA_character_
    }
  ) %>%
  select(codigo_norm, Dia_7d, Kg_Total_Ant, Ha_Total_Ant)

turno_dia_habitual <- df_ant_valvs %>%
  filter(Kg_Total_Ant > 0) %>%
  group_by(Módulo = as.character(Módulo_Norm), Turno = as.character(Turno_Norm)) %>%
  summarise(
    Dia_Habitual = {
      tot_dia <- colSums(across(any_of(cols_kg_all)), na.rm = TRUE)
      dias_con_kg <- names(tot_dia)[tot_dia > 0]
      if (length(dias_con_kg) > 0) sub("^Kg_", "", tail(dias_con_kg, 1)) else "Jueves"
    },
    .groups = "drop"
  )

valvulas_sub <- df_valvulas %>%
  filter(as.character(Módulo) == "6", as.character(Turno) %in% c("3", "4")) %>%
  mutate(
    codigo_norm = norm_codigo_valvula(codigo),
    Turno       = trimws(as.character(Turno)),
    Variedad    = limpiar_variedad(Variedad),
    Área        = suppressWarnings(as.numeric(Área)),
    Frecuencia_Dias = 7L,
    Dia_Auto_Valv   = NA_character_
  ) %>%
  left_join(rc_valv_avance, by = "codigo_norm") %>%
  left_join(sap_valv_avance, by = "codigo_norm") %>%
  mutate(
    Ha_Prog      = case_when(
      coalesce(Ha_Prog_Sap, 0) > 0                        ~ pmin(Área, Ha_Prog_Sap),
      coalesce(Kg_Prog_Sap, 0) > 0                        ~ Área,
      (dia_inicio_idx > 1 & coalesce(Ha_Lunes_Rc, 0) > 0) ~ pmin(Área, Ha_Lunes_Rc),
      (dia_inicio_idx > 2 & coalesce(Ha_Martes_Rc, 0) > 0) ~ pmin(Área, Ha_Martes_Rc),
      TRUE                                                ~ 0
    ),
    Kg_Prog      = case_when(
      coalesce(Kg_Prog_Sap, 0) > 0 ~ Kg_Prog_Sap,
      (dia_inicio_idx > 1 & coalesce(Kg_Lunes_Rc, 0) > 0) ~ Kg_Lunes_Rc,
      (dia_inicio_idx > 2 & coalesce(Kg_Martes_Rc, 0) > 0) ~ Kg_Martes_Rc,
      TRUE                         ~ 0
    ),
    Ha_Lunes_Rc   = pmax(coalesce(Ha_Lunes_Rc, 0), coalesce(Ha_Lunes_Sap, 0)),
    Ha_Martes_Rc  = pmax(coalesce(Ha_Martes_Rc, 0), coalesce(Ha_Martes_Sap, 0)),
    Kg_Lunes_Valv = coalesce(Kg_Lunes_Sap, 0) + ifelse(coalesce(Kg_Lunes_Sap, 0) == 0, coalesce(Kg_Lunes_Rc, 0), 0),
    Kg_Martes_Valv = coalesce(Kg_Martes_Sap, 0) + ifelse(coalesce(Kg_Martes_Sap, 0) == 0, coalesce(Kg_Martes_Rc, 0), 0),
    Ha_Faltante   = pmax(0, Área - Ha_Prog)
  ) %>%
  left_join(valv_7d_map, by = "codigo_norm") %>%
  left_join(turno_dia_habitual, by = c("Módulo", "Turno")) %>%
  mutate(
    Kg_Total_Ant            = coalesce(Kg_Total_Ant, 0),
    Ha_Total_Ant            = coalesce(Ha_Total_Ant, 0),
    Dia_Base_Ant            = coalesce(Dia_7d, Dia_Habitual),
    Dia_Natural_Valv        = case_when(
      !is.na(Dia_Ruta_Sem) & nzchar(Dia_Ruta_Sem)   ~ Dia_Ruta_Sem,
      !is.na(Dia_Auto_Valv) & nzchar(Dia_Auto_Valv) ~ Dia_Auto_Valv,
      !is.na(Dia_Base_Ant) & nzchar(Dia_Base_Ant)   ~ desplazar_dia_frecuencia(Dia_Base_Ant, Frecuencia_Dias, dias_nombres, 1L, "Lunes", 6L, "Sábado"),
      TRUE                                          ~ dia_inicio_nom
    ),
    idx_natural             = match(Dia_Natural_Valv, dias_nombres),
    idx_natural             = ifelse(is.na(idx_natural), dia_inicio_idx, idx_natural),
    idx_asig_rango          = pmin(dia_fin_idx, pmax(dia_inicio_idx, idx_natural)),
    Dia_Habitual_Calc       = dias_nombres[idx_asig_rango],
    Tiene_Ciclo_Previo_Valv = !is.na(Dia_7d) | (!is.na(Dia_Auto_Valv) & nzchar(Dia_Auto_Valv)) | (!is.na(Dia_Ruta_Sem) & nzchar(Dia_Ruta_Sem))
  )

turno_avance_ha <- valvulas_sub %>%
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
  ) %>%
  mutate(
    Ha_Manual_1ra          = 0,
    Ha_Manual_2da          = 0,
    Ha_Faltante_Turno_Neto = pmax(0, Area_Total_Turno - Ha_Prog_Turno - Ha_Manual_1ra),
    Turno_Completo_1ra     = (Ha_Faltante_Turno_Neto <= 0.05)
  )

rend_turno <- turno_avance_ha %>%
  left_join(kg_real_turno, by = c("Módulo", "Turno", "Variedad")) %>%
  left_join(rend_ant_turno, by = c("Módulo", "Turno", "Variedad")) %>%
  mutate(
    Proy_Kg = 12000,
    Factor_Floral = 1.0,
    Tipo_Efecto_Floral = "Neutro",
    Delta_Floral_Pct = 0,
    Kg_Real_Sem        = coalesce(Kg_Real_Sem, 0),
    Kg_Lunes_Real      = coalesce(Kg_Lunes_Real, 0),
    Kg_Martes_Real     = coalesce(Kg_Martes_Real, 0),
    Esta_Proyectado    = TRUE,
    Ha_Ant_Turno       = coalesce(Ha_Ant_Turno, 0),
    Kg_Ant_Turno       = coalesce(Kg_Ant_Turno, 0),
    Rend_Ant_Turno     = coalesce(Rend_Ant_Turno, 0),
    Sap_Cosecho_Previo = coalesce(Sap_Cosecho_Previo, FALSE),
    Sap_Cosecho_Lunes  = coalesce(Sap_Cosecho_Lunes, FALSE),
    Sap_Cosecho_Martes = coalesce(Sap_Cosecho_Martes, FALSE),
    Rend_Proy          = ifelse(Area_Total_Turno > 0 & Proy_Kg > 0, Proy_Kg / Area_Total_Turno, 0),
    
    es_remanente_minimo = (Ha_Prog_Turno == 0) & (Kg_Real_Sem > 0) & 
      ((Kg_Real_Sem / pmax(1, Proy_Kg)) < 0.15 | (Kg_Real_Sem / pmax(0.1, Area_Total_Turno)) < 50),
    
    Cosecho_Lunes     = Sap_Cosecho_Lunes | (Ha_Lunes_Turno > 0) | (Kg_Lunes_Real > 0),
    Cosecho_Martes    = Sap_Cosecho_Martes | (Ha_Martes_Turno > 0) | (Kg_Martes_Real > 0),
    Tiene_Cosecha_Act = !es_remanente_minimo & (Sap_Cosecho_Previo | Cosecho_Lunes | Cosecho_Martes | Kg_Real_Sem > 0 | Ha_Prog_Turno > 0),
    
    Ha_Recorrida_Turno = case_when(
      Ha_Prog_Turno > 0   ~ Ha_Prog_Turno,
      es_remanente_minimo ~ round(pmin(Area_Total_Turno, Kg_Real_Sem / pmax(1, Rend_Proy)), 2),
      Kg_Real_Sem > 0     ~ round(pmin(Area_Total_Turno, Kg_Real_Sem / pmax(1, Rend_Proy)), 2),
      TRUE                ~ 0
    ),
    
    Prop_Avance_Turno = ifelse(Area_Total_Turno > 0, pmin(1, Ha_Recorrida_Turno / Area_Total_Turno), 0),
    
    Semana_Ant_No_Finalizo = (Ha_Ant_Turno > 0) &
      (Ha_Ant_Turno < 0.95 * Area_Total_Turno) &
      (coalesce(Ultimo_Dia_Cosecha_Ant, "") %in% c("Sábado", "Viernes")) &
      (Cosecho_Lunes | Cosecho_Martes),
    
    Turno_Completo_Lun_Mar = !Semana_Ant_No_Finalizo & Tiene_Cosecha_Act &
      (Ha_Faltante_Turno <= 0.05 | Area_Total_Turno - Ha_Recorrida_Turno <= 0.05),
    Es_Ciclo_Corto         = (Variedad == "ROCIO") | (Frecuencia_Dias <= 4L),
    
    Rend_Sem_Actual = ifelse(Ha_Recorrida_Turno > 0, Kg_Real_Sem / Ha_Recorrida_Turno, Rend_Proy),
    Rend_Turno_Aplicar = Rend_Sem_Actual,
    Motivo_Ajuste = sprintf("Avance >30%% (Rend act sem: %s kg/ha)", round(Rend_Sem_Actual))
  )

valvulas_calc <- valvulas_sub %>%
  left_join(rend_turno, by = c("Módulo", "Turno", "Variedad")) %>%
  mutate(
    Valvula_Cerro_En_Lun_Mar = FALSE,
    Ha_Cerrada_Lun_Mar = 0
  )

reg_regular <- valvulas_calc %>%
  filter(coalesce(Esta_Proyectado, FALSE), !Turno_Completo_Lun_Mar, !coalesce(Turno_Completo_1ra, FALSE)) %>%
  mutate(
    Ha_Regular = Ha_Faltante,
    Dia_Asignado = Dia_Habitual_Calc
  ) %>%
  group_by(Módulo, Turno, Variedad) %>%
  mutate(
    Rend_Valv_Regular = coalesce(Rend_Turno_Aplicar, 0),
    Kg_Asignado = round(Ha_Regular * Rend_Valv_Regular, 2),
    Ha_Asignada = Ha_Regular,
    Num_Pasada = 1L
  ) %>%
  ungroup() %>%
  filter(Kg_Asignado > 0, Ha_Regular > 0, Dia_Asignado %in% dias_ajustables_nom)

cat("\n=== RESUMEN POR TURNO EN VIERNES ===\n")
resumen <- reg_regular %>%
  group_by(Turno) %>%
  summarise(
    Valvulas_Asignadas = n(),
    Ha_Asignadas = sum(Ha_Asignada),
    Kg_Asignados = sum(Kg_Asignado)
  )
print(as.data.frame(resumen))
