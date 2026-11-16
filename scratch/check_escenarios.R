library(dplyr)
if (file.exists("Actualizacion Semanal/escenarios_ajuste_semanal.rds")) {
  esc <- readRDS("Actualizacion Semanal/escenarios_ajuste_semanal.rds")
  cat("escenarios_ajuste_semanal.rds:\n")
  print(names(esc))
  if (is.data.frame(esc)) {
    print(head(esc))
    print(unique(esc$Nombre_Escenario))
  }
}
