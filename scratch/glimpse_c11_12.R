suppressPackageStartupMessages({
  library(dplyr)
})
load("Rutas de Cosecha/datos_maestros_cosecha_2026.RData")
print(glimpse(df_cosecha %>% filter(Semana %in% c(44, 45), as.character(Módulo) == "5", as.character(Turno) %in% c("11", "12"))))
