library(dplyr)
library(tidyr)
load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")

sem_sel  <- 45
camp_sel <- "2026"
dia_inicio_nom <- "Martes"
dia_fin_nom    <- "Viernes"
dias_nombres   <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
dias_cols_kg   <- c("Kg_Lunes", "Kg_Martes", "Kg_Miércoles", "Kg_Jueves", "Kg_Viernes", "Kg_Sábado")
dia_inicio_idx <- which(dias_nombres == dia_inicio_nom)[1]
dia_fin_idx    <- which(dias_nombres == dia_fin_nom)[1]
dias_ajustables_nom <- dias_nombres[dia_inicio_idx:dia_fin_idx]

# Check df_valvulas for Módulo 1
valvs_m1 <- df_valvulas %>% filter(as.character(Módulo) == "1")
cat("Valvulas M1 total:", nrow(valvs_m1), "\n")
print(valvs_m1 %>% group_by(Turno) %>% summarise(N_valvs = n(), Area_tot = sum(Área), .groups="drop"))

# Check rutas for M1
rc <- if (exists("df_rutas_cosecha")) df_rutas_cosecha %>% filter(Semana == sem_sel, as.character(Módulo) == "1") else tibble()
cat("Rutas M1 rows in week 45:", nrow(rc), "\n")
if (nrow(rc) > 0) {
  print(rc %>% group_by(Turno) %>% summarise(across(starts_with("Ha_"), ~ sum(.x, na.rm=TRUE)), .groups="drop"))
}

# Check SAP reception in week 45 for M1
sap_m1 <- df_cosecha %>% filter(as.character(Campaña) == camp_sel, as.numeric(Semana) == sem_sel, as.character(Módulo) == "1")
cat("SAP M1 reception rows:", nrow(sap_m1), "\n")
print(sap_m1 %>% group_by(Turno, Válvula) %>% summarise(Kg = sum(`Kg Total`), Ha = max(coalesce(Área, 0)), .groups="drop"))
