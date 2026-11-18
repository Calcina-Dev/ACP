suppressPackageStartupMessages(library(dplyr))

ed_all <- readRDS("Actualizacion Semanal/ediciones_manuales_cosecha.rds")
m3_t2_ed <- ed_all %>% filter(as.numeric(Semana) == 47, as.character(Módulo) == "3", as.character(Turno) == "2")
print(glimpse(m3_t2_ed))
