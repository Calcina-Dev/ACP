suppressPackageStartupMessages(library(dplyr))
aj_valv <- readRDS("D:/ACP/Actualizacion Semanal/ajuste_valvulas_semana.rds")
print(as.data.frame(aj_valv %>% filter(Semana == 46, Módulo == "2", Turno == "4")))
