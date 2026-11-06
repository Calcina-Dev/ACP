suppressPackageStartupMessages({
  library(dplyr)
})
ed <- readRDS("Actualizacion Semanal/ediciones_manuales_cosecha.rds")
print(glimpse(ed %>% filter(Sector == "8", Módulo == "4", Turno == "5")))
