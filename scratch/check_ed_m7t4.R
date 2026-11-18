suppressPackageStartupMessages(library(dplyr))
if (file.exists("D:/ACP/Actualizacion Semanal/ediciones_manuales_cosecha.rds")) {
  ed <- readRDS("D:/ACP/Actualizacion Semanal/ediciones_manuales_cosecha.rds")
  cat("ediciones_manuales rows:", nrow(ed), "\n")
  ed_m7t4 <- ed %>% filter(trimws(as.character(Módulo)) == "7", trimws(as.character(Turno)) == "4")
  print(as.data.frame(ed_m7t4))
}
