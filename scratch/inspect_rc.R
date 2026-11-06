rc <- readRDS("D:/ACP/Rutas de Cosecha/rutas_cosecha_mapa.rds")
cat("Dimensions:", dim(rc), "\n")
cat("Colnames:", names(rc), "\n")
print(table(as.character(rc$Módulo), as.character(rc$Sector)))
