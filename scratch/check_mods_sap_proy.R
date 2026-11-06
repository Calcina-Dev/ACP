suppressPackageStartupMessages(library(dplyr))

load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")

cat("=== df_proyecciones: MODULOS PRESENTES ===\n")
print(df_proyecciones %>% count(Sector, Módulo))

cat("\n=== df_valvulas: MODULOS PRESENTES ===\n")
print(df_valvulas %>% count(Sector, Módulo))

cat("\n=== df_cosecha (2026): MODULOS PRESENTES ===\n")
print(df_cosecha %>% filter(as.character(Campaña) == "2026") %>% count(Sector, Módulo))
