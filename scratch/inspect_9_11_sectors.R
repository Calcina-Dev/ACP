load('D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData')
library(dplyr)

cat("=== HISTORIAL DE SECTORES PARA MODULO 9 ===\n")
print(df_cosecha %>% filter(grepl("^9", as.character(Módulo))) %>%
        group_by(Campaña, Sector, Módulo) %>% summarise(kg = sum(`Kg Total`), .groups = "drop"))

cat("\n=== HISTORIAL DE SECTORES PARA MODULO 11 ===\n")
print(df_cosecha %>% filter(grepl("^11", as.character(Módulo))) %>%
        group_by(Campaña, Sector, Módulo) %>% summarise(kg = sum(`Kg Total`), .groups = "drop"))

cat("\n=== SECTOR EN PROYECCIONES PARA MODULO 9 Y 11 ===\n")
print(df_proyecciones %>% filter(as.character(Módulo) %in% c("9", "9.1", "9.2", "11", "11.1", "11.2")) %>%
        group_by(Sector, Módulo) %>% summarise(proy = sum(Proyección), n = n(), .groups = "drop"))
