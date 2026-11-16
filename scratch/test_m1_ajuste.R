library(dplyr)
library(tidyr)
load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")

sem_sel <- 45
camp_sel <- "2026"
dia_inicio_nom <- "Martes"
dia_fin_nom <- "Viernes"
dias_nombres <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
dias_cols_kg <- c("Kg_Lunes", "Kg_Martes", "Kg_Miércoles", "Kg_Jueves", "Kg_Viernes", "Kg_Sábado")
dia_inicio_idx <- which(dias_nombres == dia_inicio_nom)[1]
dia_fin_idx <- which(dias_nombres == dia_fin_nom)[1]
dias_ajustables_nom <- dias_nombres[dia_inicio_idx:dia_fin_idx]

cat("dias_ajustables_nom:", paste(dias_ajustables_nom, collapse=", "), "\n")

# Check proyecciones for Modulo 1
pr1 <- df_proyecciones %>% filter(Semana == sem_sel, Módulo == "1")
cat("Proyecciones M1:\n")
print(pr1 %>% select(Módulo, Turno, Variedad, Proyección))

# Check cosecha for Modulo 1
cs1 <- df_cosecha %>% filter(Semana == sem_sel, Módulo == "1")
cat("Cosecha M1 fechas y kilos:\n")
print(cs1 %>% group_by(Fecha, Módulo, Turno) %>% summarise(Kg = sum(`Kg Total`), .groups="drop"))

# Check rutas for Modulo 1
if (exists("df_rutas_cosecha")) {
  rc1 <- df_rutas_cosecha %>% filter(Semana == sem_sel, Módulo == "1")
  cat("Rutas M1 rows:", nrow(rc1), "\n")
  if (nrow(rc1) > 0) print(head(rc1))
}
