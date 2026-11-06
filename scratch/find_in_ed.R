suppressPackageStartupMessages({
  library(dplyr)
})
ed <- readRDS("Actualizacion Semanal/ediciones_manuales_cosecha.rds")
print(ed %>% filter(abs(Valor - 3006.97) < 5 | abs(Valor - 2997.66) < 5))
