library(dplyr)
if (file.exists("Actualizacion Semanal/ajuste_valvulas_semana.rds")) {
  aj <- readRDS("Actualizacion Semanal/ajuste_valvulas_semana.rds")
  cat("ajuste_valvulas_semana.rds:\n")
  print(head(aj))
  aj12 <- aj %>% filter(as.character(Módulo) == "12")
  print(aj12 %>% select(Módulo, Turno, Variedad, Dia_Asignado, Kg_Asignado, Ha_Asignada, Num_Pasada, Motivo_Ajuste))
}
