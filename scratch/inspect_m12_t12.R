library(dplyr)
ed <- readRDS("D:/ACP/Actualizacion Semanal/ediciones_manuales_cosecha.rds")
m12_t12 <- ed %>% filter(as.character(Módulo) == "12", as.character(Turno) == "12")
print(as.data.frame(m12_t12))
