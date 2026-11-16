library(dplyr)
load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")

valvs_m1 <- df_valvulas %>% filter(as.character(Módulo) == "1")
print(valvs_m1 %>% select(Módulo, Turno, codigo, Área))

# Where was M1 in week 44?
cs_w44 <- df_cosecha %>% filter(as.character(Campaña) == "2026", as.numeric(Semana) == 44, as.character(Módulo) == "1")
cat("Week 44 M1 rows:", nrow(cs_w44), "\n")
print(cs_w44 %>% select(Fecha, Turno, `Kg Total`))
