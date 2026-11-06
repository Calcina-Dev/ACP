v <- readRDS("D:/ACP/Rutas de Cosecha/particiones_multivariedad.rds")
library(dplyr)
print(head(v))
if ("Sector" %in% names(v) && "Módulo" %in% names(v)) {
  print(v %>% filter(as.character(Módulo) == "12"))
}
