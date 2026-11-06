v <- readRDS("D:/ACP/Rutas de Cosecha/catalogo_poligonos_valvulas.rds")
library(dplyr)
print(v %>% group_by(Módulo, Sector) %>% summarise(n = n(), .groups = "drop"))
