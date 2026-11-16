library(dplyr)
load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")

sem_sel <- 45
camp_sel <- "2026"
dia_inicio_nom <- "Martes"
dia_fin_nom <- "Viernes"
dias_nombres <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
dia_inicio_idx <- 2
dia_fin_idx <- 5
dias_ajustables_nom <- dias_nombres[dia_inicio_idx:dia_fin_idx]

# Check Turnos 3 and 4 in df_valvulas
v34 <- df_valvulas %>%
  filter(as.character(Módulo) == "1", as.character(Turno) %in% c("3", "4"))
cat("Valvulas T3, T4 count:", nrow(v34), "\n")
print(v34 %>% select(Módulo, Turno, codigo, Área))

# Check proyecciones
p34 <- df_proyecciones %>%
  filter(as.character(Campaña) == camp_sel, as.numeric(Semana) == sem_sel, as.character(Módulo) == "1", as.character(Turno) %in% c("3", "4"))
cat("Proyecciones T3, T4:\n")
print(p34 %>% select(Módulo, Turno, Variedad, Proyección))

# In rend_turno:
# What was Rend_Ant_Turno for T3, T4?
# In week 44, df_cosecha:
cs_w44 <- df_cosecha %>%
  filter(as.character(Campaña) == camp_sel, as.numeric(Semana) == 44, as.character(Módulo) == "1", as.character(Turno) %in% c("3", "4"))
print(cs_w44 %>% group_by(Turno) %>% summarise(Kg = sum(`Kg Total`), .groups="drop"))
