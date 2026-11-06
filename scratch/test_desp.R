suppressMessages({
  library(dplyr)
})

# Let's check how Dia_Habitual_Calc evaluates for V11 and V12
# When dia_inicio = "Viernes", dia_fin = "Viernes"
# vs dia_inicio = "Jueves", dia_fin = "Viernes"

dias_nombres <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")

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

# If in week 44, V11 and V12 were on Miércoles (idx 3), or what was Dia_7d?
# In week 44, Dia_7d was "Miércoles" (idx 3) or Dia_Habitual was "Miércoles".
# Wait, why was V11 and V12 on Jueves in the route?
# In rc_map, V11 and V12 had Ha_Jueves = 2.33!
cat("Test desplazar_dia_frecuencia with Dia_Base = 'Jueves', Frec = 7:\n")
cat("Range [Viernes, Viernes]:\n")
print(desplazar_dia_frecuencia("Jueves", 7, dias_nombres, 5, "Viernes", 5, "Viernes"))

cat("Range [Miércoles, Sábado] with Dia_Base = 'Martes':\n")
print(desplazar_dia_frecuencia("Martes", 7, dias_nombres, 3, "Miércoles", 6, "Sábado"))
