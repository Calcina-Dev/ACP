suppressPackageStartupMessages(library(dplyr))
load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")

tratt <- df_tratamientos %>%
  mutate(
    Campaña = trimws(as.character(Campania)),
    Módulo  = trimws(as.character(Modulo)),
    Tipo    = case_when(
      grepl("org", as.character(Tratamiento), ignore.case = TRUE) ~ "Orgánico",
      grepl("conv", as.character(Tratamiento), ignore.case = TRUE) ~ "Convencional",
      TRUE ~ Tratamiento
    )
  )

print(tratt %>% filter(grepl("26|2026", Campaña)))
