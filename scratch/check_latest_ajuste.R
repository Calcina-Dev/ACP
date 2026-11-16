suppressPackageStartupMessages({
  library(dplyr)
})

aj <- readRDS("D:/ACP/Actualizacion Semanal/ajuste_valvulas_semana.rds")
cat("Semanas in ajuste_valvulas_semana.rds:", paste(unique(aj$Semana), collapse = ", "), "\n")
cat("Sample rows for latest semana:\n")
latest_sem <- max(aj$Semana, na.rm = TRUE)
print(head(aj %>% filter(Semana == latest_sem) %>% select(Sector, Módulo, Turno, Variedad, codigo, Dia_Asignado, Ha_Asignada, Kg_Asignado, Motivo_Ajuste), 6))
