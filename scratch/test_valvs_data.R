suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

# Cargar archivos reales
ed <- readRDS("Actualizacion Semanal/ediciones_manuales_cosecha.rds")
valvs <- readRDS("Actualizacion Semanal/ajuste_valvulas_semana.rds")

cat("Ediciones manuales cols:", paste(names(ed), collapse=", "), "\n")
cat("Valvulas cols:", paste(names(valvs), collapse=", "), "\n")

# Ver semana 45
cat("\nValvulas S45:\n")
print(head(valvs %>% filter(Semana == 45) %>% select(Sector, Módulo, Turno, Variedad, Dia_Asignado, Num_Pasada, Ha_Asignada, Kg_Asignado)))
