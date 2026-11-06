library(dplyr)

cat("=== EDICIONES MANUALES ===\n")
if (file.exists("Actualizacion Semanal/ediciones_manuales_cosecha.rds")) {
  ed <- readRDS("Actualizacion Semanal/ediciones_manuales_cosecha.rds")
  print(glimpse(ed))
  print(tail(ed, 20))
}

cat("\n=== AJUSTE VALVULAS ===\n")
if (file.exists("Actualizacion Semanal/ajuste_valvulas_semana.rds")) {
  valvs <- readRDS("Actualizacion Semanal/ajuste_valvulas_semana.rds")
  print(glimpse(valvs))
  print(tail(valvs, 20))
}
