suppressPackageStartupMessages({
  library(dplyr)
})
valvs <- readRDS("Actualizacion Semanal/ajuste_valvulas_semana.rds")
s45 <- valvs %>% filter(as.character(Módulo) == "5", as.character(Turno) %in% c("11", "12"), Semana == 45)
print(s45 %>% select(Turno, codigo, Dia_Asignado, Ha_Asignada, Kg_Asignado, Motivo_Ajuste))
