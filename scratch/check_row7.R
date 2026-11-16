suppressPackageStartupMessages(library(dplyr))
ed <- readRDS("D:/ACP/Actualizacion Semanal/ediciones_manuales_cosecha.rds")
row7 <- ed %>% filter(Semana == 46, Módulo == "2", Turno == "4")
print(as.list(row7))
