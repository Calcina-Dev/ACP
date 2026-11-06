library(dplyr)
rc_map <- readRDS("D:/ACP/Rutas de Cosecha/rutas_cosecha_mapa.rds")
hist_wide <- readRDS("D:/ACP/Rutas de Cosecha/historico_oficial_wide_2026.rds")

cat("=== RUTAS COSECHA MAPA DETALLE ===\n")
rc_m6t3 <- rc_map %>% filter(as.numeric(Semana) == 45, as.character(Módulo) == "6", as.character(Turno) == "3")
print(as.data.frame(rc_m6t3 %>% select(codigo, Ha_Lunes, Ha_Martes, Ha_Miércoles, Ha_Jueves, Ha_Viernes, Ha_Sábado, Kg_Lunes, Kg_Martes, Kg_Miércoles, Kg_Jueves, Kg_Viernes, Kg_Sábado)))

cat("\n=== HISTÓRICO OFICIAL WIDE DETALLE ===\n")
s45_m6t3 <- hist_wide %>% filter(as.numeric(Semana) == 45, as.character(Módulo_Norm) == "6", as.character(Turno_Norm) == "3")
print(as.data.frame(s45_m6t3 %>% select(Codigo_Valv, Ha_Lunes, Ha_Martes, Ha_Miércoles, Ha_Jueves, Ha_Viernes, Ha_Sábado, Kg_Lunes, Kg_Martes, Kg_Miércoles, Kg_Jueves, Kg_Viernes, Kg_Sábado)))
