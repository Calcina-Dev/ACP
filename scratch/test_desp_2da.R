library(dplyr)

desplazar_dia_2da_pasada <- function(dia_base_vec, frec_vec, dias_nombres, dia_inicio_idx, dia_inicio_nom, dia_fin_idx = 6L, dia_fin_nom = "Sábado") {
  idx_base <- match(dia_base_vec, dias_nombres)
  frec_int <- as.integer(frec_vec)
  desp <- ifelse(idx_base == 1L, ifelse(!is.na(frec_int) & frec_int <= 3L, 3L, 4L), 3L)
  idx_target <- pmin(dia_fin_idx, pmax(dia_inicio_idx, idx_base + desp))
  dia_target <- dias_nombres[idx_target]
  dia_res <- ifelse(is.na(dia_target) | idx_target < dia_inicio_idx, dia_inicio_nom, dia_target)
  idx_res <- match(dia_res, dias_nombres)
  ifelse(!is.na(idx_res) & idx_res > dia_fin_idx, dia_fin_nom, dia_res)
}

dias_nombres <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")

cat("Miércoles Hasta Sábado:", desplazar_dia_2da_pasada("Miércoles", 6, dias_nombres, 2, "Martes", 6, "Sábado"), "\n")
cat("Miércoles Hasta Viernes:", desplazar_dia_2da_pasada("Miércoles", 6, dias_nombres, 2, "Martes", 5, "Viernes"), "\n")
