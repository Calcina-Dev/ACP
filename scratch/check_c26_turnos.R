suppressPackageStartupMessages(library(dplyr))
load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")

c26 <- df_cosecha %>% filter(as.character(Campaña) == "2026", as.character(Módulo) %in% c("9", "11", "9.1", "9.2", "11.1", "11.2"))
print(c26 %>% count(Sector, Módulo, Turno))
