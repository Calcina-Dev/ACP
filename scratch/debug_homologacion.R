library(dplyr)
e <- new.env()
load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData", envir = e)

# What is in u_base for Módulo 12?
# Let's inspect rv_cosecha and df_proyecciones
cosecha_m12 <- e$df_cosecha %>% filter(as.character(Campaña) == "2026", Semana == 45, grepl("^12$", trimws(as.character(Módulo))))
cat("cosecha_m12 rows:", nrow(cosecha_m12), "\n")
print(cosecha_m12 %>% select(Sector, Módulo, Turno, Variedad) %>% distinct())

proy_m12 <- e$df_proyecciones %>% filter(as.character(Campaña) == "2026", Semana == 45, grepl("^12$", trimws(as.character(Módulo))))
cat("\nproy_m12 rows:", nrow(proy_m12), "\n")
print(proy_m12 %>% select(Sector, Módulo, Turno, Variedad) %>% distinct())

valv_m12 <- e$df_valvulas %>% filter(grepl("^12$", trimws(as.character(Módulo))))
cat("\nvalv_m12 rows:", nrow(valv_m12), "\n")
print(valv_m12 %>% select(Sector, Módulo, Turno, Variedad) %>% distinct())
