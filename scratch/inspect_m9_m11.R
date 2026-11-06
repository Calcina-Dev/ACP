load('D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData')
library(dplyr)

cat("=== MODULO 9 EN LOS DIFERENTES DATASETS ===\n")
cat("\n1. df_cosecha Campaña 2026 M9:\n")
print(df_cosecha %>% filter(Campaña == 2026, grepl("^9", as.character(Módulo))) %>%
        group_by(Sector, Módulo) %>% summarise(kg = sum(`Kg Total`), n_turnos = n_distinct(Turno), turnos = paste(sort(unique(Turno))[1:min(5, n_distinct(Turno))], collapse=","), .groups = "drop"))

cat("\n2. df_proyecciones Campaña 2026 M9:\n")
print(df_proyecciones %>% filter(Campaña == 2026, grepl("^9", as.character(Módulo))) %>%
        group_by(Sector, Módulo) %>% summarise(kg = sum(Proyección), n_turnos = n_distinct(Turno), turnos = paste(sort(unique(Turno))[1:min(5, n_distinct(Turno))], collapse=","), .groups = "drop"))

cat("\n3. df_valvulas M9:\n")
print(df_valvulas %>% filter(grepl("^9", as.character(Módulo))) %>%
        group_by(Sector, Módulo) %>% summarise(n = n(), n_turnos = n_distinct(Turno), turnos = paste(sort(unique(as.numeric(Turno)))[1:min(8, n_distinct(Turno))], collapse=","), .groups = "drop"))

cat("\n=== MODULO 11 EN LOS DIFERENTES DATASETS ===\n")
cat("\n1. df_cosecha Campaña 2026 M11:\n")
print(df_cosecha %>% filter(Campaña == 2026, grepl("^11", as.character(Módulo))) %>%
        group_by(Sector, Módulo) %>% summarise(kg = sum(`Kg Total`), n_turnos = n_distinct(Turno), turnos = paste(sort(unique(Turno))[1:min(5, n_distinct(Turno))], collapse=","), .groups = "drop"))

cat("\n2. df_proyecciones Campaña 2026 M11:\n")
print(df_proyecciones %>% filter(Campaña == 2026, grepl("^11", as.character(Módulo))) %>%
        group_by(Sector, Módulo) %>% summarise(kg = sum(Proyección), n_turnos = n_distinct(Turno), turnos = paste(sort(unique(Turno))[1:min(5, n_distinct(Turno))], collapse=","), .groups = "drop"))

cat("\n3. df_valvulas M11:\n")
print(df_valvulas %>% filter(grepl("^11", as.character(Módulo))) %>%
        group_by(Sector, Módulo) %>% summarise(n = n(), n_turnos = n_distinct(Turno), turnos = paste(sort(unique(as.numeric(Turno)))[1:min(8, n_distinct(Turno))], collapse=","), .groups = "drop"))
