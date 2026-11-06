suppressPackageStartupMessages({
  library(dplyr)
})
valvs <- readRDS("Actualizacion Semanal/ajuste_valvulas_semana.rds")
print(valvs %>% filter(Sector == "8", Módulo == "5", Turno %in% c("11", "12"), Semana == 45))
