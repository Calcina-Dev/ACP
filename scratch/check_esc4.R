library(dplyr)
esc <- readRDS("Actualizacion Semanal/escenarios_ajuste_semanal.rds")
e4 <- esc[4, ]
cat("Escenario 4:", e4$Nombre, "Semana:", e4$Semana, "Desde:", e4$Dia_Desde, "Hasta:", e4$Dia_Hasta, "\n")
vdata <- e4$Valvulas_Data[[1]]
print(names(vdata))
vdata12 <- vdata %>% filter(as.character(Módulo) == "12")
print(vdata12 %>% select(Módulo, Turno, Variedad, Dia_Asignado, Kg_Asignado, Ha_Asignada, Num_Pasada, Motivo_Ajuste))
