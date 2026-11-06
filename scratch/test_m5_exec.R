suppressMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
})

# Load the real app data
hist_wide <- readRDS("D:/ACP/Rutas de Cosecha/historico_oficial_wide_2026.rds")
rc_map <- readRDS("D:/ACP/Rutas de Cosecha/rutas_cosecha_mapa.rds")
df_valvulas <- readRDS("D:/ACP/Rutas de Cosecha/valvulas_normalizadas.rds")

norm_codigo_valvula <- function(x) toupper(gsub("[^A-Za-z0-9]", "", as.character(x)))
limpiar_variedad <- function(x) toupper(trimws(as.character(x)))

# Let's run the exact logic from app.R for week 45, Sector 8, Módulo 5, Turno 11 & 12
sem_sel <- 45
sem_ant <- 44
dia_inicio_nom <- "Jueves"
dia_fin_nom <- "Sábado"
dias_nombres <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
dia_inicio_idx <- match(dia_inicio_nom, dias_nombres)
dia_fin_idx <- match(dia_fin_nom, dias_nombres)
dias_ajustables_nom <- dias_nombres[dia_inicio_idx:dia_fin_idx]
dias_previos_nom <- if (dia_inicio_idx > 1) dias_nombres[1:(dia_inicio_idx - 1)] else character(0)
cols_ha_prev <- paste0("Ha_", dias_previos_nom)
cols_kg_prev <- paste0("Kg_", dias_previos_nom)

rc_sem <- rc_map %>% filter(as.numeric(Semana) == sem_sel)

# Avance RC
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
    es_ejecutado <- (d %in% c("Lunes", "Martes")) | (val_k > 0)
    ha_prev_vec <- ha_prev_vec + ifelse(es_ejecutado, val_h, 0)
    kg_prev_vec <- kg_prev_vec + val_k
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

# Avance SAP
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

# Harvest S-1 for Rend_Ant_Turno
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

cat("=== S-1 for M5 T11 & T12 ===\n")
print(rend_ant_turno %>% filter(Módulo == "5", Turno %in% c("11", "12")))

# Harvest Sem Actual for Kg_Real_Sem
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

cat("\n=== Kg Real Sem for M5 T11 & T12 ===\n")
print(kg_real_turno %>% filter(Módulo == "5", Turno %in% c("11", "12")))
