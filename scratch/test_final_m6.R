library(dplyr)

cat("=================================================================\n")
cat("TESTING MÓDULO 6 CON APP.R MODIFICADO\n")
cat("=================================================================\n\n")

# Load real app data
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
      coalesce(Kg_Prog_Sap, 0) > 0                        ~ Kg_Prog_Sap,
      (dia_inicio_idx > 1 & coalesce(Kg_Lunes_Rc, 0) > 0) ~ Kg_Lunes_Rc,
      (dia_inicio_idx > 2 & coalesce(Kg_Martes_Rc, 0) > 0) ~ Kg_Martes_Rc,
      TRUE                                                ~ 0
    ),
    Ha_Lunes_Rc   = pmax(coalesce(Ha_Lunes_Rc, 0), coalesce(Ha_Lunes_Sap, 0)),
    Ha_Martes_Rc  = pmax(coalesce(Ha_Martes_Rc, 0), coalesce(Ha_Martes_Sap, 0)),
    Kg_Lunes_Valv = coalesce(Kg_Lunes_Sap, 0) + ifelse(coalesce(Kg_Lunes_Sap, 0) == 0, coalesce(Kg_Lunes_Rc, 0), 0),
    Kg_Martes_Valv = coalesce(Kg_Martes_Sap, 0) + ifelse(coalesce(Kg_Martes_Sap, 0) == 0, coalesce(Kg_Martes_Rc, 0), 0),
    Ha_Faltante   = pmax(0, Área - Ha_Prog)
  )

cat("=== VÁLVULAS MÓDULO 6 TURNOS 3 Y 4 ===\n")
res_valvs <- valvulas_sub %>%
  group_by(Turno) %>%
  summarise(
    Area_Total = sum(Área),
    Ha_Prog_Total = sum(Ha_Prog),
    Ha_Faltante_Total = sum(Ha_Faltante)
  )
print(as.data.frame(res_valvs))
