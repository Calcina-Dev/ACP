suppressPackageStartupMessages(library(dplyr))
load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")

combos <- df_cosecha %>%
  filter(as.character(Campaña) == "2026", !coalesce(Es_Testblock, FALSE)) %>%
  distinct(Módulo, Variedad) %>%
  mutate(Módulo = trimws(as.character(Módulo)), Variedad = toupper(trimws(as.character(Variedad))))

cat("Combos count:", nrow(combos), "\n")
