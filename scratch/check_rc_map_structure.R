rc_map <- readRDS("D:/ACP/Rutas de Cosecha/rutas_cosecha_mapa.rds")
print(colnames(rc_map))
print(head(as.data.frame(rc_map %>% select(Semana, codigo, starts_with("Ha_"), starts_with("Kg_")))))
