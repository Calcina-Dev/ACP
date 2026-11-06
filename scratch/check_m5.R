library(dplyr)
if (file.exists("Actualizacion Semanal/ediciones_manuales_cosecha.rds")) {
  ed <- readRDS("Actualizacion Semanal/ediciones_manuales_cosecha.rds")
  print(ed %>% filter(Módulo == "5"))
}
