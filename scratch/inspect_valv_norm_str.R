v <- readRDS("D:/ACP/Rutas de Cosecha/valvulas_normalizadas.rds")
print(str(v))
print(head(v %>% filter(as.character(Módulo) == "12")))
