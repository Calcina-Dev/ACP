suppressPackageStartupMessages(library(dplyr))
rc <- readRDS("D:/ACP/Rutas de Cosecha/rutas_cosecha_mapa.rds")
cat("Filas en rutas_cosecha_mapa para sem 44 M4:\n")
rc %>% filter(Semana == 44, Módulo == "4") %>% as.data.frame() %>% print()
