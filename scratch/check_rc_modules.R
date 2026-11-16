library(dplyr)
rc <- readRDS("D:/ACP/Rutas de Cosecha/rutas_cosecha_mapa.rds")
sem46 <- rc %>% filter(as.numeric(Semana) == 46)
print(table(sem46$Módulo))
