library(dplyr)
ed <- readRDS("D:/ACP/Actualizacion Semanal/ediciones_manuales_cosecha.rds")
cat("=== Ediciones Manuales: Modulos present ===\n")
print(table(ed$Módulo))

cat("\n=== Ediciones Manuales: Módulo 9 and 11 in Campaña 2026 ===\n")
print(ed %>% filter(Campaña == "2026", grepl("9|11", Módulo)) %>% distinct(Semana, Sector, Módulo))

av <- readRDS("D:/ACP/Actualizacion Semanal/ajuste_valvulas_semana.rds")
cat("\n=== Ajuste Valvulas: Modulos present ===\n")
print(table(av$Módulo))
cat("\n=== Ajuste Valvulas: Sectores present ===\n")
print(table(av$Sector))
