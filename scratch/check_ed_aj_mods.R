suppressPackageStartupMessages(library(dplyr))

ed <- readRDS("D:/ACP/Actualizacion Semanal/ediciones_manuales_cosecha.rds")
cat("ed Módulo values:\n")
print(table(as.character(ed$Módulo)))

aj <- readRDS("D:/ACP/Actualizacion Semanal/ajuste_valvulas_semana.rds")
cat("aj Módulo values:\n")
print(table(as.character(aj$Módulo)))
