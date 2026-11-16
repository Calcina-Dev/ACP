suppressPackageStartupMessages({
  library(dplyr)
})

aj <- readRDS("D:/ACP/Actualizacion Semanal/ajuste_valvulas_semana.rds")
cat("Dimensions:", dim(aj), "\n")
cat("Columns:", paste(names(aj), collapse = ", "), "\n")

# Check rows where Motivo_Ajuste contains "Sin avance"
sin_av <- aj %>% filter(grepl("Sin avance", coalesce(Motivo_Ajuste, ""), ignore.case = TRUE))
cat("Total valves with 'Sin avance':", nrow(sin_av), "\n")

if (nrow(sin_av) > 0) {
  cat("\nSample turns with Sin avance:\n")
  print(head(sin_av %>% select(Sector, Módulo, Turno, Variedad, codigo, Dia_Asignado, Ha_Asignada, Kg_Asignado, Motivo_Ajuste), 8))
}
