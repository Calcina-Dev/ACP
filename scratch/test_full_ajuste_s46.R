suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
})

load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")

sem_sel <- 46
dia_inicio_nom <- "Miércoles"
dia_fin_nom    <- "Jueves"
dias_nombres   <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
dia_inicio_idx <- match(dia_inicio_nom, dias_nombres)
dia_fin_idx    <- match(dia_fin_nom, dias_nombres)
dias_previos_nom <- dias_nombres[1:(dia_inicio_idx - 1)]
dias_ajustables_nom <- dias_nombres[dia_inicio_idx:dia_fin_idx]

limpiar_variedad <- function(x) {
  if (is.null(x) || length(x) == 0) return(character(0))
  x_str <- toupper(trimws(as.character(x)))
  v <- as.character(chartr("ÁÉÍÓÚÜ", "AEIOUU", x_str))
  as.character(ifelse(grepl("EB\\s*9\\s*[-_.]\\s*2", v, ignore.case = TRUE), "MAGICA", v))
}

norm_codigo_valvula <- function(x) {
  sub("^M0*([0-9]+)[-_]T0*([0-9]+)[-_]V0*([0-9]+)$", "M\\1-T\\2-V\\3", toupper(trimws(as.character(x))))
}

desplazar_dia_frecuencia <- function(dia_origen, frec_dias, dias_nombres, dia_inicio_idx, dia_inicio_nom, dia_fin_idx, dia_fin_nom) {
  idx_orig <- match(dia_origen, dias_nombres)
  idx_orig <- ifelse(is.na(idx_orig), 1L, idx_orig)
  frec_num <- suppressWarnings(as.integer(frec_dias))
  frec_num <- ifelse(is.na(frec_num) | frec_num <= 0, 7L, frec_num)
  idx_teorico <- idx_orig + frec_num
  idx_asig <- ((idx_teorico - 1) %% length(dias_nombres)) + 1
  dias_nombres[idx_asig]
}

# Current implementation of desplazar_dia_2da_pasada from app.R
desplazar_dia_2da_pasada_actual <- function(dia_base_vec, frec_vec, dias_nombres, dia_inicio_idx, dia_inicio_nom, dia_fin_idx = 6L, dia_fin_nom = "Sábado") {
  idx_base <- match(dia_base_vec, dias_nombres)
  frec_int <- as.integer(frec_vec)
  desp <- ifelse(idx_base == 1L, ifelse(!is.na(frec_int) & frec_int <= 3L, 3L, 4L), 3L)
  idx_target <- idx_base + desp
  es_valido <- !is.na(idx_base) & idx_base %in% c(1L, 2L) &
               !is.na(idx_target) & idx_target >= dia_inicio_idx & idx_target <= dia_fin_idx
  ifelse(es_valido, dias_nombres[idx_target], NA_character_)
}

# Proposed fix: Rocío (and short cycle) always rests 3 days for 2nd pasada:
# Lunes -> Jueves (1 + 3 = 4)
# Martes -> Viernes (2 + 3 = 5)
desplazar_dia_2da_pasada_corregida <- function(dia_base_vec, frec_vec, dias_nombres, dia_inicio_idx, dia_inicio_nom, dia_fin_idx = 6L, dia_fin_nom = "Sábado") {
  idx_base <- match(dia_base_vec, dias_nombres)
  # Regla Operativa ACP: Segunda pasada siempre descansa 3 días:
  # Lunes -> Jueves (idx 4), Martes -> Viernes (idx 5)
  desp <- 3L
  idx_target <- idx_base + desp
  es_valido <- !is.na(idx_base) & idx_base %in% c(1L, 2L) &
               !is.na(idx_target) & idx_target >= dia_inicio_idx & idx_target <= dia_fin_idx
  ifelse(es_valido, dias_nombres[idx_target], NA_character_)
}

# Let's run with both and compare!
cat("Testing M10 Turnos 1, 3, 5, 7 with CURRENT function:\n")
cat("Lunes with Frec 4:", desplazar_dia_2da_pasada_actual("Lunes", 4L, dias_nombres, 3L, "Miércoles", 4L, "Jueves"), "\n")

cat("Testing M10 Turnos 1, 3, 5, 7 with CORREGIDA function:\n")
cat("Lunes with Frec 4:", desplazar_dia_2da_pasada_corregida("Lunes", 4L, dias_nombres, 3L, "Miércoles", 4L, "Jueves"), "\n")
