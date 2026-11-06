suppressMessages({
  library(dplyr)
  library(readr)
  library(stringr)
})

rc_map <- readRDS("D:/ACP/Rutas de Cosecha/rutas_cosecha_mapa.rds")
hist_wide <- readRDS("D:/ACP/Rutas de Cosecha/historico_oficial_wide_2026.rds")
df_valvulas <- readRDS("D:/ACP/Rutas de Cosecha/valvulas_normalizadas.rds")

t3_rc <- rc_map %>% filter(Semana == 45, Módulo == "2", Turno == "3")
cat("=== rc_map for Semana 45 Modulo 2 Turno 3 ===\n")
print(t3_rc %>% select(Semana, Módulo, Turno, codigo, any_of(c("Ha_Jueves", "Kg_Jueves", "Ha_Viernes", "Kg_Viernes", "Ha_Lunes", "Ha_Martes", "Ha_Miercoles", "Ha_Miércoles"))))

valvs_t3 <- df_valvulas %>% filter(Módulo == "2", Turno == "3")
cat("=== df_valvulas for Modulo 2 Turno 3 ===\n")
print(valvs_t3 %>% select(Módulo, Turno, codigo, Área))

t3_hw <- hist_wide %>% filter(as.numeric(Semana) == 45, Codigo_Valv %in% valvs_t3$codigo)
cat("=== hist_wide for Modulo 2 Turno 3 Sem 45 ===\n")
print(t3_hw %>% select(Semana, Codigo_Valv, any_of(c("Ha_Lunes", "Kg_Lunes", "Ha_Martes", "Kg_Martes", "Ha_Miercoles", "Kg_Miercoles", "Ha_Miércoles", "Kg_Miércoles", "Ha_Jueves", "Kg_Jueves", "Ha_Viernes", "Kg_Viernes"))))
