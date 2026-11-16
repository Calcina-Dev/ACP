library(dplyr)
esc <- readRDS("Actualizacion Semanal/escenarios_ajuste_semanal.rds")
e4 <- esc[4, ]
vdata <- e4$Valvulas_Data[[1]]
vdata_t5 <- vdata %>% filter(as.character(Módulo) == "12", as.character(Turno) == "5")
print(vdata_t5 %>% select(Módulo, Turno, Variedad, Dia_Asignado, Kg_Asignado, Ha_Asignada, Num_Pasada, Motivo_Ajuste))
