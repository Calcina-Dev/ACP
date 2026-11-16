suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
ediciones_file <- "D:/ACP/Actualizacion Semanal/ediciones_manuales_cosecha.rds"
ediciones <- if (file.exists(ediciones_file)) readRDS(ediciones_file) else data.frame()

cat("Ediciones rows:", nrow(ediciones), "\n")
if (nrow(ediciones) > 0) {
  ed_45 <- ediciones %>% filter(Campana == 2026, Semana == 45)
  cat("Ediciones 2026 sem 45 rows:", nrow(ed_45), "\n")
  print(head(ed_45, 3))
  print(table(ed_45$Campo))
}
