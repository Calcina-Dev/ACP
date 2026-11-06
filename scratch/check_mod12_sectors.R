library(dplyr)
e <- new.env()
load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData", envir = e)

cat("=== DF_VALVULAS: Módulo 12 - Sectores por Turno ===\n")
print(e$df_valvulas %>% filter(grepl("^12$", trimws(as.character(Módulo)))) %>% distinct(Sector, Módulo, Turno, Variedad) %>% as.data.frame())

cat("\n=== DF_PROYECCIONES: Módulo 12 - Sectores por Turno (Semana 45) ===\n")
print(e$df_proyecciones %>% filter(grepl("^12$", trimws(as.character(Módulo))), Semana == 45) %>% distinct(Sector, Módulo, Turno, Variedad) %>% as.data.frame())

cat("\n=== AREAS_BASE (if exists) or all Módulos in df_valvulas and their Sectors ===\n")
print(e$df_valvulas %>% group_by(Módulo) %>% summarise(Sectores = paste(unique(Sector), collapse = ", ")) %>% as.data.frame())
