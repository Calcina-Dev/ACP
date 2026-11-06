rc <- readRDS("D:/ACP/Rutas de Cosecha/rutas_cosecha_mapa.rds")
print(table(as.character(rc$Sector), as.character(rc$Módulo)))
