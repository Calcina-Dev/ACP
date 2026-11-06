library(dplyr)

df_valvulas <- readRDS("D:/ACP/Rutas de Cosecha/valvulas_normalizadas.rds")
hist_wide <- readRDS("D:/ACP/Rutas de Cosecha/historico_oficial_wide_2026.rds")
rc_map <- readRDS("D:/ACP/Rutas de Cosecha/rutas_cosecha_mapa.rds")

cat("=== VALVULAS EN MÓDULO 6 TURNO 4 ===\n")
m6t4_valvs <- df_valvulas %>% filter(as.character(Módulo) == "6", as.character(Turno) == "4")
print(m6t4_valvs %>% select(codigo, Sector, Módulo, Turno, Variedad, Área))

cat("\n=== RC MAP (Semana 45 - Turno 4) ===\n")
rc_m6t4 <- rc_map %>% filter(as.numeric(Semana) == 45, as.character(Módulo) == "6", as.character(Turno) == "4")
print(as.data.frame(rc_m6t4 %>% select(codigo, starts_with("Ha_"), starts_with("Kg_"))))

cat("\n=== SAP WIDE (Semana 45 - Turno 4) ===\n")
s45_m6t4 <- hist_wide %>% filter(as.numeric(Semana) == 45, as.character(Módulo_Norm) == "6", as.character(Turno_Norm) == "4")
print(as.data.frame(s45_m6t4 %>% select(Codigo_Valv, starts_with("Ha_"), starts_with("Kg_"))))
