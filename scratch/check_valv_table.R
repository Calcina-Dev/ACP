v <- readRDS("D:/ACP/Rutas de Cosecha/valvulas_normalizadas.rds")
print(table(as.character(v$Sector), as.character(v$Módulo)))
