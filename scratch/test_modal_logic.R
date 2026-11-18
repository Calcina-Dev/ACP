load("Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
suppressPackageStartupMessages(library(dplyr))

# Simular abrir modal para M2 Turno 3, Jueves en MODO AJUSTE (DESDE: Miércoles)
info_sem <- 46
info_mod <- "2"
info_tur <- "3"
info_var <- "SEKOYA POP"
col_name <- "Jueves"
modo_det_act <- "ajustado"
d_desde_sel  <- "Miércoles"
area_turno   <- 9.32
valor_actual <- 2413.04

dias_semana_todos <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
dias_reales_previos <- character(0)
if (modo_det_act == "ajustado") {
  idx_desde <- match(d_desde_sel, dias_semana_todos)
  if (is.na(idx_desde)) idx_desde <- 1
  if (idx_desde > 1) {
    dias_reales_previos <- dias_semana_todos[1:(idx_desde - 1)]
  }
} else {
  idx_col <- match(col_name, dias_semana_todos)
  if (is.na(idx_col)) idx_col <- 1
  if (idx_col > 1) {
    dias_reales_previos <- dias_semana_todos[1:(idx_col - 1)]
  }
}
cat("Dias reales previos:", paste(dias_reales_previos, collapse = ", "), "\n")

# Calcular Ha real cosechada en días previos consolidados para este turno
ha_real_prev_1ra <- 0
ha_real_prev_2da <- 0
if (length(dias_reales_previos) > 0) {
  if (exists("df_hist_oficial_wide") && !is.null(df_hist_oficial_wide) && nrow(df_hist_oficial_wide) > 0) {
    sub_sap_prev <- df_hist_oficial_wide %>%
      filter(
        as.numeric(Semana) == as.numeric(info_sem),
        as.character(Módulo_Norm) == as.character(info_mod),
        as.character(Turno_Norm)  == as.character(info_tur),
        toupper(trimws(Variedad)) == toupper(trimws(info_var))
      )
    if (nrow(sub_sap_prev) > 0) {
      cols_ha_prev <- intersect(paste0("Ha_", dias_reales_previos), names(sub_sap_prev))
      if (length(cols_ha_prev) > 0) {
        ha_tot_prev <- sum(rowSums(sub_sap_prev %>% select(all_of(cols_ha_prev)), na.rm = TRUE), na.rm = TRUE)
        ha_real_prev_1ra <- round(min(area_turno, ha_tot_prev), 2)
      }
    }
  }
}

cat("ha_real_prev_1ra:", ha_real_prev_1ra, "\n")

# Simular valvs_celda (de ajuste automático)
valvs_celda <- tibble(
  codigo = c("M2-T3-V9", "M2-T3-V10", "M2-T3-V11", "M2-T3-V12"),
  Ha_Asignada = c(2.33, 2.33, 2.33, 2.33),
  Kg_Asignado = c(603.3, 603.3, 603.3, 603.3),
  Num_Pasada = 1L
)

# Simular otros días
dias_otros <- setdiff(dias_semana_todos, c(dias_reales_previos, col_name))
cat("Dias otros:", paste(dias_otros, collapse = ", "), "\n")
ha_man_otros_1ra <- 0
ha_auto_otros_1ra <- 0
ha_consumida_otros_1ra <- round(ha_real_prev_1ra + ha_man_otros_1ra + ha_auto_otros_1ra, 2)
ha_disponible_1ra <- max(0, round(area_turno - ha_consumida_otros_1ra, 2))
cat("ha_disponible_1ra:", ha_disponible_1ra, "\n")

ha_preset <- if (nrow(valvs_celda) > 0 && sum(valvs_celda$Ha_Asignada, na.rm = TRUE) > 0) {
  round(pmin(ha_disponible_1ra, sum(valvs_celda$Ha_Asignada, na.rm = TRUE)), 2)
} else {
  ha_disponible_1ra
}
cat("ha_preset:", ha_preset, "\n")

kg_preset <- round(valor_actual, 2)
kgha_preset <- if (ha_preset > 0 && kg_preset > 0) {
  round(kg_preset / ha_preset, 2)
} else 100
cat("kgha_preset:", kgha_preset, "kg/ha\n")

stopifnot(ha_disponible_1ra == 9.32)
stopifnot(ha_preset == 9.32)
stopifnot(abs(kgha_preset - 258.91) < 0.1)
cat("\n>>> TEST PASSED PERFECTLY! <<<\n")
