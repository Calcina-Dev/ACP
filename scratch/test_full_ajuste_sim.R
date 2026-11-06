suppressMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
})

# Load RDS files
rc_map <- readRDS("D:/ACP/Rutas de Cosecha/rutas_cosecha_mapa.rds")
hist_wide <- readRDS("D:/ACP/Rutas de Cosecha/historico_oficial_wide_2026.rds")
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

test_simulacion <- function(dia_inicio_nom, dia_fin_nom) {
  cat("\n======================================================\n")
  cat(sprintf("SIMULACION DESDE: %s  HASTA: %s\n", dia_inicio_nom, dia_fin_nom))
  cat("======================================================\n")
  
  sem_sel <- 45
  sem_ant <- 44
  dias_nombres <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
  dia_inicio_idx <- match(dia_inicio_nom, dias_nombres)
  dia_fin_idx <- match(dia_fin_nom, dias_nombres)
  dias_ajustables_nom <- dias_nombres[dia_inicio_idx:dia_fin_idx]
  dias_previos_nom <- if (dia_inicio_idx > 1) dias_nombres[1:(dia_inicio_idx - 1)] else character(0)
  cols_ha_prev <- paste0("Ha_", dias_previos_nom)
  cols_kg_prev <- paste0("Kg_", dias_previos_nom)
  
  rc_sem <- rc_map %>% filter(as.numeric(Semana) == sem_sel)
  
  # RC avance con fix
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
  } else tibble(codigo_norm = character(), Ha_Prog = numeric(), Kg_Prog = numeric(), Ha_Lunes_Rc = numeric(), Kg_Lunes_Rc = numeric(), Ha_Martes_Rc = numeric(), Kg_Martes_Rc = numeric(), Dia_Ruta_Sem = character())
  
  # SAP avance
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
  
  # History S-1
  cols_kg_all <- paste0("Kg_", dias_nombres)
  df_ant_valvs <- hist_wide %>% filter(as.numeric(Semana) == sem_ant)
  df_ant_valvs$Kg_Total_Ant <- rowSums(df_ant_valvs[, intersect(cols_kg_all, names(df_ant_valvs))], na.rm = TRUE)
  df_ant_valvs$Ha_Total_Ant <- rowSums(df_ant_valvs[, intersect(paste0("Ha_", dias_nombres), names(df_ant_valvs))], na.rm = TRUE)
  
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
  
  # Turno 3 calc
  valvs_t3 <- df_valvulas %>%
    filter(Módulo == "2", Turno == "3") %>%
    mutate(
      codigo_norm = norm_codigo_valvula(codigo),
      Área        = as.numeric(Área),
      Frecuencia_Dias = 7L
    ) %>%
    left_join(rc_valv_avance, by = "codigo_norm") %>%
    left_join(sap_valv_avance, by = "codigo_norm") %>%
    left_join(valv_7d_map, by = "codigo_norm") %>%
    mutate(
      Ha_Prog = case_when(
        coalesce(Ha_Prog_Sap, 0) > 0 ~ pmin(Área, Ha_Prog_Sap),
        coalesce(Kg_Prog_Sap, 0) > 0 ~ Área,
        (dia_inicio_idx > 1 & coalesce(Ha_Lunes_Rc, 0) > 0) ~ pmin(Área, Ha_Lunes_Rc),
        (dia_inicio_idx > 2 & coalesce(Ha_Martes_Rc, 0) > 0) ~ pmin(Área, Ha_Martes_Rc),
        coalesce(Ha_Prog, 0) > 0 & coalesce(Kg_Prog, 0) > 0 ~ pmin(Área, Ha_Prog),
        TRUE ~ 0
      ),
      Ha_Faltante = pmax(0, Área - Ha_Prog),
      
      # DIA NATURAL Y CLAMPING
      Dia_Natural_Valv = case_when(
        !is.na(Dia_Ruta_Sem) & nzchar(Dia_Ruta_Sem) ~ Dia_Ruta_Sem,
        !is.na(Dia_7d) & nzchar(Dia_7d)             ~ desplazar_dia_frecuencia(Dia_7d, Frecuencia_Dias, dias_nombres, 1L, "Lunes", 6L, "Sábado"),
        TRUE                                        ~ dia_inicio_nom
      ),
      idx_natural = match(Dia_Natural_Valv, dias_nombres),
      idx_natural = ifelse(is.na(idx_natural), dia_inicio_idx, idx_natural),
      # Posicionamiento en el rango disponible:
      idx_asig_rango = pmin(dia_fin_idx, pmax(dia_inicio_idx, idx_natural)),
      Dia_Asignado   = dias_nombres[idx_asig_rango]
    )
  
  cat("Valvulas T3 status:\n")
  print(valvs_t3 %>% select(codigo, Área, Ha_Prog, Ha_Faltante, Dia_Natural_Valv, Dia_Asignado))
  
  # Pending valves to adjust:
  valvs_ajustar <- valvs_t3 %>% filter(Ha_Faltante > 0)
  cat("\nValvulas a ajustar:\n")
  print(valvs_ajustar %>% select(codigo, Ha_Faltante, Dia_Asignado))
}

test_simulacion("Viernes", "Viernes")
test_simulacion("Jueves", "Viernes")
test_simulacion("Miércoles", "Sábado")
