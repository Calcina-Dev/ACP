library(dplyr)
load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")

cs1 <- df_cosecha %>% filter(as.character(Campaña) == "2026", as.numeric(Semana) == 45, as.character(Módulo) == "1")
print(cs1 %>% group_by(Fecha, Módulo, Turno) %>% summarise(Kg = sum(`Kg Total`), .groups="drop"))
