rc_map <- readRDS("D:/ACP/Rutas de Cosecha/rutas_cosecha_mapa.rds")
print(colnames(rc_map))
s45 <- rc_map[rc_map$Semana == 45, ]
print(head(s45[, c("codigo", "Ha_Lunes", "Kg_Lunes", "Ha_Jueves", "Kg_Jueves")]))
