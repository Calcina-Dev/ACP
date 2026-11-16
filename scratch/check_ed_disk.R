suppressPackageStartupMessages({
  library(dplyr)
})
load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")

# Check if there is an rds for ediciones_manuales
file_ed <- "D:/ACP/Actualizacion Semanal/ediciones_manuales.rds"
if (file.exists(file_ed)) {
  ed <- readRDS(file_ed)
  cat("Ediciones manuales found on disk. Total rows:", nrow(ed), "\n")
  ed_45 <- ed %>% filter(as.numeric(Semana) == 45)
  cat("Semana 45 rows in ed:", nrow(ed_45), "\n")
  print(head(ed_45))
} else {
  cat("No ediciones_manuales.rds found on disk\n")
}
