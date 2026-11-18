suppressPackageStartupMessages(library(dplyr))

load("Actualizacion Semanal/datos_maestros_cosecha_2026.RData")

m14_t17 <- df_cosecha %>%
  filter(as.character(Campaña) == "2026", Semana == 47,
         as.character(Módulo) %in% c("14", "14.1", "14.2"),
         as.character(Turno) == "17")

print(glimpse(m14_t17))
