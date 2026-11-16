suppressPackageStartupMessages(library(dplyr))
ed <- readRDS("D:/ACP/Actualizacion Semanal/ediciones_manuales_cosecha.rds")
cat("ed for M2 T4:\n")
print(ed %>% filter(grepl("2", as.character(Módulo)), as.character(Turno) == "4"))

aj_valv <- readRDS("D:/ACP/Actualizacion Semanal/ajuste_valvulas_semana.rds")
cat("\naj_valv for M2 T4:\n")
print(aj_valv %>% filter(grepl("2", as.character(Módulo)), as.character(Turno) == "4"))
