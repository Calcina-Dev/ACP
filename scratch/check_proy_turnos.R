suppressPackageStartupMessages(library(dplyr))
load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")

cat("=== df_proyecciones M9 Turnos ===\n")
p9 <- df_proyecciones %>% filter(as.character(Módulo) %in% c("9", "9.1", "9.2"))
print(p9 %>% count(Sector, Módulo, Turno))

cat("\n=== df_proyecciones M11 Turnos ===\n")
p11 <- df_proyecciones %>% filter(as.character(Módulo) %in% c("11", "11.1", "11.2"))
print(p11 %>% count(Sector, Módulo, Turno))
