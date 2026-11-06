suppressPackageStartupMessages(library(dplyr))
load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")

tratt <- df_tratamientos %>%
  filter(grepl("26|2026", Campania))
print(tratt %>% filter(grepl("9|11|13|14", as.character(Modulo))))
