library(dplyr)
v <- readRDS("D:/ACP/Rutas de Cosecha/valvulas_normalizadas.rds")
cat("--- Sectors in valvulas_normalizadas.rds by Módulo ---\n")
print(v %>% group_by(Módulo, Sector) %>% summarise(n = n(), .groups = "drop"))
