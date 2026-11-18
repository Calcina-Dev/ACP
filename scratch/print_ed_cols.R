ed <- readRDS("Actualizacion Semanal/ediciones_manuales_cosecha.rds")
suppressPackageStartupMessages(library(dplyr))
sub_ed <- ed %>% filter(as.numeric(Semana) == 47, as.character(Módulo) == "13", as.character(Turno) == "1")
print(as.data.frame(sub_ed))
