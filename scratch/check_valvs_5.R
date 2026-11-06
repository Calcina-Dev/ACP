suppressPackageStartupMessages({
  library(dplyr)
})
valvs <- readRDS("Actualizacion Semanal/ajuste_valvulas_semana.rds")
print(valvs %>% filter(as.character(Módulo) == "5", as.character(Turno) %in% c("11", "12")))
