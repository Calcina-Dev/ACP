suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
})

load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")

limpiar_variedad <- function(x) {
  if (is.null(x) || length(x) == 0) return(character(0))
  x_str <- toupper(trimws(as.character(x)))
  v <- as.character(chartr("ÁÉÍÓÚÜ", "AEIOUU", x_str))
  as.character(ifelse(grepl("EB\\s*9\\s*[-_.]\\s*2", v, ignore.case = TRUE), "MAGICA", v))
}

norm_codigo_valvula <- function(x) {
  sub("^M0*([0-9]+)[-_]T0*([0-9]+)[-_]V0*([0-9]+)$", "M\\1-T\\2-V\\3", toupper(trimws(as.character(x))))
}

desplazar_dia_2da_pasada <- function(dia_base_vec, frec_vec, dias_nombres, dia_inicio_idx, dia_inicio_nom, dia_fin_idx = 6L, dia_fin_nom = "Sábado") {
  idx_base <- match(dia_base_vec, dias_nombres)
  frec_int <- as.integer(frec_vec)
  desp <- ifelse(idx_base == 1L, ifelse(!is.na(frec_int) & frec_int <= 3L, 3L, 4L), 3L)
  idx_target <- idx_base + desp
  es_valido <- !is.na(idx_base) & idx_base %in% c(1L, 2L) &
               !is.na(idx_target) & idx_target >= dia_inicio_idx & idx_target <= dia_fin_idx
  ifelse(es_valido, dias_nombres[idx_target], NA_character_)
}

sem_sel <- 46
dia_inicio_nom <- "Miércoles"
dia_fin_nom    <- "Jueves"
dias_nombres   <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
dia_inicio_idx <- match(dia_inicio_nom, dias_nombres)
dia_fin_idx    <- match(dia_fin_nom, dias_nombres)
dias_previos_nom <- dias_nombres[1:(dia_inicio_idx - 1)]
dias_ajustables_nom <- dias_nombres[dia_inicio_idx:dia_fin_idx]

rutas_mapa <- readRDS("D:/ACP/Rutas de Cosecha/rutas_cosecha_mapa.rds")

rc_sem <- if (!is.null(rutas_mapa) && nrow(rutas_mapa) > 0) {
  rutas_mapa %>% filter(as.numeric(Semana) == sem_sel)
} else tibble()

rc_temp <- rc_sem %>% mutate(codigo_norm = norm_codigo_valvula(codigo))
ha_prev_vec <- numeric(nrow(rc_temp))
kg_prev_vec <- numeric(nrow(rc_temp))
for (d in dias_previos_nom) {
  col_h <- paste0("Ha_", d)
  col_k <- paste0("Kg_", d)
  val_h <- if (col_h %in% names(rc_temp)) coalesce(rc_temp[[col_h]], 0) else 0
  val_k <- if (col_k %in% names(rc_temp)) coalesce(rc_temp[[col_k]], 0) else 0
  ha_prev_vec <- ha_prev_vec + val_h
  kg_prev_vec <- kg_prev_vec + val_k
}
rc_temp$Ha_Prev_Valv <- ha_prev_vec
rc_temp$Kg_Prev_Valv <- kg_prev_vec

rc_valv_avance <- rc_temp %>%
  filter(coalesce(suppressWarnings(as.numeric(Num_Pasada)), 1) == 1) %>%
  group_by(codigo_norm) %>%
  summarise(
    Ha_Prog_Rc   = sum(coalesce(Ha_Prev_Valv, 0), na.rm = TRUE),
    Kg_Prog_Rc   = sum(coalesce(Kg_Prev_Valv, 0), na.rm = TRUE),
    Ha_Lunes_Rc  = sum(coalesce(Ha_Lunes, 0), na.rm = TRUE),
    Kg_Lunes_Rc  = if ("Kg_Lunes" %in% names(.)) sum(coalesce(Kg_Lunes, 0), na.rm = TRUE) else 0,
    Ha_Martes_Rc = sum(coalesce(Ha_Martes, 0), na.rm = TRUE),
    Kg_Martes_Rc = if ("Kg_Martes" %in% names(.)) sum(coalesce(Kg_Martes, 0), na.rm = TRUE) else 0,
    .groups      = "drop"
  )

valvs_con_2da_rc <- if (nrow(rc_sem) > 0 && 'codigo' %in% names(rc_sem)) {
  rc_sem %>%
    filter(coalesce(suppressWarnings(as.numeric(Num_Pasada)), 1) == 2) %>%
    mutate(codigo_norm = norm_codigo_valvula(codigo)) %>%
    filter(coalesce(Ha_Total_Prog, 0) > 0 | coalesce(Kg_Total_Prog, 0) > 0) %>%
    pull(codigo_norm) %>%
    unique()
} else character(0)

df_cosecha_sem <- df_cosecha %>%
  filter(as.character(Campaña) == "2026", Semana == sem_sel, !coalesce(Es_Testblock, FALSE)) %>%
  mutate(
    Turno_num = suppressWarnings(as.numeric(Turno)),
    Módulo = trimws(as.character(Módulo)),
    Turno    = trimws(as.character(Turno)),
    Variedad = limpiar_variedad(Variedad),
    wday_num = lubridate::wday(Fecha, week_start = 1)
  )

kg_real_turno <- df_cosecha_sem %>%
  group_by(Módulo, Turno, Variedad) %>%
  summarise(
    Kg_Real_Sem             = sum(`Kg Total`[wday_num < dia_inicio_idx], na.rm = TRUE),
    Kg_Total_Completo_Sem   = sum(`Kg Total`, na.rm = TRUE),
    Kg_Lunes_Real           = sum(`Kg Total`[wday_num == 1], na.rm = TRUE),
    Kg_Martes_Real          = sum(`Kg Total`[wday_num == 2], na.rm = TRUE),
    Sap_Cosecho_Previo      = any(wday_num < dia_inicio_idx & `Kg Total` > 0),
    Sap_Cosecho_Lunes       = any(wday_num == 1 & `Kg Total` > 0),
    Sap_Cosecho_Martes      = any(wday_num == 2 & `Kg Total` > 0),
    Valvulas_Cosechadas_Sap = n_distinct(Válvula[wday_num < dia_inicio_idx & `Kg Total` > 0]),
    .groups                 = "drop"
  )

proy_sem <- df_proyecciones %>% filter(as.numeric(Semana) == sem_sel)
proy_turno <- proy_sem %>%
  mutate(
    Módulo = trimws(as.character(Módulo)),
    Turno = trimws(as.character(Turno)),
    Variedad = limpiar_variedad(Variedad)
  ) %>%
  group_by(Módulo, Turno, Variedad) %>%
  summarise(Proy_Kg = sum(Proyección, na.rm = TRUE), .groups = "drop")

df_v <- df_valvulas %>%
  mutate(
    codigo_norm = norm_codigo_valvula(coalesce(codigo_norm, codigo)),
    Turno_num   = suppressWarnings(as.numeric(Turno)),
    Sector      = trimws(as.character(Sector)),
    Módulo      = trimws(as.character(Módulo)),
    Turno       = trimws(as.character(Turno)),
    Variedad    = limpiar_variedad(Variedad),
    Área        = suppressWarnings(as.numeric(Área))
  ) %>%
  left_join(rc_valv_avance, by = "codigo_norm") %>%
  mutate(
    Ha_Prog = case_when(
      !is.null(rc_sem) && nrow(rc_sem) > 0 ~ pmin(Área, coalesce(Ha_Prog_Rc, 0)),
      TRUE ~ 0
    ),
    Kg_Prog = case_when(
      Ha_Prog > 0 & coalesce(Kg_Prog_Rc, 0) > 0 ~ Kg_Prog_Rc,
      TRUE ~ 0
    ),
    Ha_Lunes_Rc = coalesce(Ha_Lunes_Rc, 0),
    Ha_Martes_Rc = coalesce(Ha_Martes_Rc, 0),
    Ha_Faltante = pmax(0, Área - Ha_Prog)
  )

resumen_man_turno <- tibble(Módulo = character(), Turno = character(), Variedad = character(), Ha_Manual_1ra = numeric(), Ha_Manual_2da = numeric())
ha_real_2da_map   <- tibble(Módulo = character(), Turno = character(), Variedad = character(), Ha_Real_2da = numeric())

turno_avance_ha <- df_v %>%
  group_by(Módulo, Turno, Variedad) %>%
  summarise(
    Ha_Prog_Turno     = sum(Ha_Prog, na.rm = TRUE),
    Kg_Prog_Turno     = sum(Kg_Prog, na.rm = TRUE),
    Ha_Lunes_Turno    = sum(Ha_Lunes_Rc, na.rm = TRUE),
    Ha_Martes_Turno   = sum(Ha_Martes_Rc, na.rm = TRUE),
    Ha_Faltante_Turno = sum(Ha_Faltante, na.rm = TRUE),
    Area_Total_Turno  = sum(Área, na.rm = TRUE),
    Valvulas_Total    = n(),
    Frecuencia_Dias   = 3L,
    Ha_Fresca_Turno   = 0,
    Kg_Fresca_Turno   = 0,
    Ha_Cierre_Turno   = 0,
    Kg_Cierre_Turno   = 0,
    .groups           = "drop"
  ) %>%
  left_join(resumen_man_turno, by = c("Módulo", "Turno", "Variedad")) %>%
  left_join(ha_real_2da_map, by = c("Módulo", "Turno", "Variedad")) %>%
  mutate(
    Ha_Manual_1ra          = coalesce(Ha_Manual_1ra, 0),
    Ha_Manual_2da          = coalesce(Ha_Manual_2da, 0),
    Ha_Faltante_Turno_Neto = pmax(0, Area_Total_Turno - Ha_Prog_Turno - Ha_Manual_1ra),
    Turno_Completo_1ra     = (Ha_Faltante_Turno_Neto <= 0.05),
    Ha_Real_2da            = coalesce(Ha_Real_2da, 0),
    Ha_Faltante_2da_Neto   = pmax(0, Area_Total_Turno - Ha_Real_2da - Ha_Manual_2da),
    Turno_Completo_2da     = (Ha_Faltante_2da_Neto <= 0.05)
  )

rend_turno <- turno_avance_ha %>%
  left_join(kg_real_turno, by = c("Módulo", "Turno", "Variedad")) %>%
  left_join(proy_turno, by = c("Módulo", "Turno", "Variedad")) %>%
  mutate(
    Kg_Real_Sem        = coalesce(Kg_Real_Sem, 0),
    Kg_Lunes_Real      = coalesce(Kg_Lunes_Real, 0),
    Kg_Martes_Real     = coalesce(Kg_Martes_Real, 0),
    Proy_Kg            = coalesce(Proy_Kg, 0),
    Esta_Proyectado    = (Proy_Kg > 0),
    Ha_Ant_Turno       = 0,
    Kg_Ant_Turno       = 0,
    Rend_Ant_Turno     = 0,
    Sap_Cosecho_Previo = coalesce(Sap_Cosecho_Previo, FALSE),
    Sap_Cosecho_Lunes  = coalesce(Sap_Cosecho_Lunes, FALSE),
    Sap_Cosecho_Martes = coalesce(Sap_Cosecho_Martes, FALSE),
    Rend_Proy          = ifelse(Area_Total_Turno > 0 & Proy_Kg > 0, Proy_Kg / Area_Total_Turno, 0),
    es_remanente_minimo = FALSE,
    Cosecho_Lunes     = Sap_Cosecho_Lunes | (Ha_Lunes_Turno > 0) | (Kg_Lunes_Real > 0),
    Cosecho_Martes    = Sap_Cosecho_Martes | (Ha_Martes_Turno > 0) | (Kg_Martes_Real > 0),
    Tiene_Cosecha_Act = !es_remanente_minimo & (Sap_Cosecho_Previo | Cosecho_Lunes | Cosecho_Martes | Kg_Real_Sem > 0 | Ha_Prog_Turno > 0),
    Ha_Recorrida_Turno = case_when(
      Ha_Prog_Turno > 0   ~ Ha_Prog_Turno,
      TRUE                ~ 0
    ),
    Prop_Avance_Turno = ifelse(Area_Total_Turno > 0, pmin(1, Ha_Recorrida_Turno / Area_Total_Turno), 0),
    Semana_Ant_No_Finalizo = FALSE,
    Turno_Completo_Lun_Mar = !Semana_Ant_No_Finalizo & Tiene_Cosecha_Act &
      (Ha_Faltante_Turno <= 0.05 | Area_Total_Turno - Ha_Recorrida_Turno <= 0.05),
    Es_Ciclo_Corto         = (Variedad == "ROCIO") | (Frecuencia_Dias <= 4L),
    Rend_Sem_Actual = ifelse(Ha_Recorrida_Turno > 0, Kg_Real_Sem / Ha_Recorrida_Turno, Rend_Proy),
    Rend_Lunes_Turno = ifelse(Ha_Lunes_Turno > 0, Kg_Lunes_Real / Ha_Lunes_Turno, NA_real_),
    Rend_Martes_Turno = ifelse(Ha_Martes_Turno > 0, Kg_Martes_Real / Ha_Martes_Turno, NA_real_),
    Rend_Fresco_Turno = 0,
    Prop_Fresca_Turno = 0,
    Tiene_Cosecha_Fresca = FALSE,
    Rend_Base_Turno = Rend_Sem_Actual,
    Rend_Base_Con_Floral = Rend_Base_Turno,
    Rend_Turno_Aplicar = Rend_Base_Con_Floral,
    Factor_Floral = 1.0,
    Tipo_Efecto_Floral = "Neutro",
    Delta_Floral_Pct = 0,
    Ultimo_Dia_Cosecha_Ant = NA_character_,
    Motivo_Ajuste = "Ajuste"
  )

valvulas_calc <- df_v %>%
  left_join(
    rend_turno %>% select(
      Módulo, Turno, Variedad, Rend_Turno_Aplicar, Rend_Base_Turno, Rend_Base_Con_Floral, Rend_Ant_Turno, Rend_Sem_Actual, Rend_Proy,
      Rend_Lunes_Turno, Rend_Martes_Turno, Rend_Fresco_Turno, Prop_Fresca_Turno, Tiene_Cosecha_Fresca,
      Esta_Proyectado, Tiene_Cosecha_Act, Semana_Ant_No_Finalizo, Turno_Completo_Lun_Mar, Es_Ciclo_Corto,
      Cosecho_Lunes, Cosecho_Martes, Kg_Lunes_Real, Kg_Martes_Real, Kg_Real_Sem, Ha_Prog_Turno,
      Area_Total_Turno, Ha_Manual_1ra, Ha_Manual_2da, Ha_Faltante_Turno_Neto, Turno_Completo_1ra, Ha_Real_2da, Ha_Faltante_2da_Neto, Turno_Completo_2da,
      Ultimo_Dia_Cosecha_Ant, Prop_Avance_Turno, Factor_Floral, Tipo_Efecto_Floral, Delta_Floral_Pct, Motivo_Ajuste
    ),
    by = c("Módulo", "Turno", "Variedad")
  ) %>%
  mutate(
    Ha_Prog_Turno = coalesce(Ha_Prog_Turno, 0),
    Kg_Real_Sem   = coalesce(Kg_Real_Sem, 0),
    Valvula_Cosecho_Lunes  = (Ha_Lunes_Rc > 0),
    Valvula_Cosecho_Martes = (Ha_Martes_Rc > 0),
    Valvula_Cerro_En_Lun_Mar = FALSE,
    Valvula_Tiene_2da_Real   = codigo_norm %in% valvs_con_2da_rc,
    
    Ha_Cosechada_Lunes = case_when(
      Ha_Lunes_Rc > 0                                                                          ~ Ha_Lunes_Rc,
      Valvula_Cosecho_Lunes                                                                    ~ Área,
      !Semana_Ant_No_Finalizo & Cosecho_Lunes & !Cosecho_Martes & Ha_Prog_Turno == 0            ~ Área,
      !Semana_Ant_No_Finalizo & Cosecho_Lunes & Cosecho_Martes & Ha_Prog_Turno == 0            ~ round(Área * (coalesce(Kg_Lunes_Real, 0) / pmax(1, Kg_Real_Sem)), 2),
      !Semana_Ant_No_Finalizo & Turno_Completo_Lun_Mar & !Cosecho_Martes & Ha_Prog_Turno == 0   ~ Área,
      TRUE                                                                                     ~ 0
    ),
    Ha_Cosechada_Martes = case_when(
      Ha_Martes_Rc > 0                                                                         ~ Ha_Martes_Rc,
      Valvula_Cosecho_Martes                                                                   ~ Área,
      !Semana_Ant_No_Finalizo & !Cosecho_Lunes & Cosecho_Martes & Ha_Prog_Turno == 0            ~ Área,
      !Semana_Ant_No_Finalizo & Cosecho_Lunes & Cosecho_Martes & Ha_Prog_Turno == 0            ~ round(Área * (coalesce(Kg_Martes_Real, 0) / pmax(1, Kg_Real_Sem)), 2),
      TRUE                                                                                     ~ 0
    ),
    Ha_Cerrada_Lun_Mar = pmin(Área, Ha_Cosechada_Lunes + Ha_Cosechada_Martes),
    Rend_Valv_Lunes = Rend_Sem_Actual,
    Rend_Valv_Martes = Rend_Sem_Actual,
    Frecuencia_Dias = 3L,
    Pct_Pasada_Lun = 60,
    Pct_Pasada_Mar = 60
  )

rectif_martes_ids <- character(0)
reg_2da_lun <- valvulas_calc %>%
  filter(
    coalesce(Esta_Proyectado, FALSE),
    !Semana_Ant_No_Finalizo,
    Es_Ciclo_Corto,
    Ha_Cosechada_Lunes > 0,
    !coalesce(Turno_Completo_2da, FALSE),
    !coalesce(Valvula_Tiene_2da_Real, FALSE)
  ) %>%
  group_by(Módulo, Turno, Variedad) %>%
  mutate(
    tot_2da_lun = sum(Ha_Cosechada_Lunes, na.rm = TRUE),
    Ha_2da_Corta = if_else(
      tot_2da_lun > coalesce(Ha_Faltante_2da_Neto, tot_2da_lun) & tot_2da_lun > 0,
      round(Ha_Cosechada_Lunes * (coalesce(Ha_Faltante_2da_Neto, tot_2da_lun) / tot_2da_lun), 2),
      Ha_Cosechada_Lunes
    ),
    Dia_Base_Frec  = ifelse(codigo_norm %in% rectif_martes_ids, 'Martes', 'Lunes'),
    Dia_Asignado   = as.character(desplazar_dia_2da_pasada(Dia_Base_Frec, Frecuencia_Dias, dias_nombres, dia_inicio_idx, dia_inicio_nom, dia_fin_idx, dia_fin_nom)),
    Pct_Rend       = if_else(limpiar_variedad(Variedad) == 'ROCIO', coalesce(Pct_Pasada_Lun, 60) / 100, 1.0),
    Rend_2da_Aplicar = round(Rend_Valv_Lunes * Pct_Rend, 2),
    Kg_Asignado    = round(Ha_2da_Corta * Rend_2da_Aplicar, 2),
    Ha_Asignada    = Ha_2da_Corta,
    Num_Pasada     = 2L,
    Es_Ruta_Sin_Kg = FALSE,
    Motivo_Ajuste  = sprintf('2da pasada Rocío (Rend Lunes: %s kg/ha • %d%% sin floración)', 
                             format(round(Rend_Valv_Lunes), big.mark = ','), round(Pct_Rend * 100))
  ) %>%
  ungroup() %>%
  filter(Kg_Asignado > 0, Ha_Asignada > 0, Dia_Asignado %in% dias_ajustables_nom)

cat("\nreg_2da_lun for M10:\n")
print(reg_2da_lun %>% filter(Módulo == "10") %>% select(codigo_norm, Turno, Dia_Asignado, Ha_Asignada, Kg_Asignado, Num_Pasada))
