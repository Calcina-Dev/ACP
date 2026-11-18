suppressPackageStartupMessages(library(dplyr))

valvs_file <- "ajuste_valvulas_semana.rds"
if (file.exists(valvs_file)) {
  valvs_all <- readRDS(valvs_file)
  cat("Rows in valvs_all:", nrow(valvs_all), "\n")
  print(table(valvs_all$Semana))
}
