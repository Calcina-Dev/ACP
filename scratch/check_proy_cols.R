suppressPackageStartupMessages(library(dplyr))
load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
print(df_proyecciones %>% count(Sector, Sector_join, Módulo, Módulo_join))
