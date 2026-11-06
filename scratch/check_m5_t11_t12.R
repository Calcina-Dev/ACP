suppressMessages({
  library(dplyr)
})

hist_wide <- readRDS("D:/ACP/Rutas de Cosecha/historico_oficial_wide_2026.rds")
rc_map <- readRDS("D:/ACP/Rutas de Cosecha/rutas_cosecha_mapa.rds")
valvs <- readRDS("D:/ACP/Rutas de Cosecha/valvulas_normalizadas.rds")

# Look at Módulo 5, Turnos 11 and 12 in week 45
t11_12_hw <- hist_wide %>% filter(as.numeric(Semana) == 45, Módulo_Norm == "5", Turno_Norm %in% c("11", "12"))
cat("=== Turnos 11 and 12 in hist_wide (Semana 45) ===\n")
print(t11_12_hw %>% select(Turno_Norm, Codigo_Valv, Ha_Total_Sem, Kg_Total_Sem, any_of(c("Ha_Lunes", "Kg_Lunes", "Ha_Martes", "Kg_Martes", "Ha_Miércoles", "Kg_Miércoles"))))

# Week 44
t11_12_w44 <- hist_wide %>% filter(as.numeric(Semana) == 44, Módulo_Norm == "5", Turno_Norm %in% c("11", "12"))
cat("\n=== Turnos 11 and 12 in hist_wide (Semana 44) ===\n")
print(t11_12_w44 %>% select(Turno_Norm, Codigo_Valv, Ha_Total_Sem, Kg_Total_Sem, any_of(c("Ha_Viernes", "Kg_Viernes", "Ha_Sábado", "Kg_Sábado"))))
