load("Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
library(dplyr)

esc <- readRDS("Actualizacion Semanal/escenarios_ajuste_semanal.rds")
e4 <- esc[4, ]
vdata <- e4$Valvulas_Data[[1]]

reg_regular <- vdata %>% filter(Num_Pasada == 1)
cat("reg_regular columns:\n")
print(names(reg_regular))
print(head(reg_regular %>% filter(as.character(Módulo) == "12") %>% select(id, codigo, Sector, Módulo, Turno, Dia_Asignado, Ha_Asignada, Kg_Asignado)))
