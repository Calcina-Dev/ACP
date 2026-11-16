suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
})

rutas_file <- "D:/ACP/Rutas de Cosecha/rutas_cosecha_mapa.rds"
rutas <- readRDS(rutas_file)
r45 <- rutas %>% filter(Campaña == 2026, Semana == 45)
cat("Names of rutas:\n")
print(names(r45))
print(head(r45, 2))
