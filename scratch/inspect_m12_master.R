load('D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData')
library(dplyr)
cat("--- Módulo 12 in df_cosecha by Campaña and Sector ---\n")
print(df_cosecha %>% filter(as.character(Módulo) == "12") %>% group_by(Campaña, Sector) %>% summarise(kg = sum(`Kg Total`, na.rm = TRUE), n = n(), .groups = "drop"))

cat("\n--- Módulo 12 in df_proyecciones by Sector ---\n")
print(df_proyecciones %>% filter(as.character(Módulo) == "12") %>% group_by(Sector) %>% summarise(proy = sum(Proyección, na.rm = TRUE), n = n(), .groups = "drop"))
