library(dplyr)

df_valvulas <- readRDS("D:/ACP/Rutas de Cosecha/valvulas_normalizadas.rds")
hist_wide <- readRDS("D:/ACP/Rutas de Cosecha/historico_oficial_wide_2026.rds")
rc_map <- readRDS("D:/ACP/Rutas de Cosecha/rutas_cosecha_mapa.rds")

norm_codigo_valvula <- function(x) toupper(gsub("[^A-Za-z0-9]", "", as.character(x)))

m6t3_valvs <- df_valvulas %>% 
  filter(as.character(Módulo) == "6", as.character(Turno) == "3")

cat("=== VALVULAS EN MÓDULO 6 TURNO 3 ===\n")
print(m6t3_valvs %>% select(codigo, Sector, Módulo, Turno, Variedad, Área, any_of(c("Tipo", "Tipo_Valvula"))))
cat("Total area catálogo:", sum(as.numeric(m6t3_valvs$Área), na.rm = TRUE), "\n\n")

cat("=== HISTÓRICO OFICIAL WIDE (Semana 45) ===\n")
s45_m6t3 <- hist_wide %>%
  filter(as.numeric(Semana) == 45, as.character(Módulo_Norm) == "6", as.character(Turno_Norm) == "3")
print(s45_m6t3 %>% select(Codigo_Valv, Variedad, starts_with("Ha_"), starts_with("Kg_")))

cat("\n=== RUTAS DE COSECHA MAPA (Semana 45) ===\n")
rc_m6t3 <- rc_map %>%
  filter(as.numeric(Semana) == 45, as.character(Módulo) == "6", as.character(Turno) == "3")
print(rc_m6t3 %>% select(codigo, Variedad, starts_with("Ha_"), starts_with("Kg_")))

cat("\n=== HISTÓRICO OFICIAL WIDE (Semana 44 - S-1) ===\n")
s44_m6t3 <- hist_wide %>%
  filter(as.numeric(Semana) == 44, as.character(Módulo_Norm) == "6", as.character(Turno_Norm) == "3")
print(s44_m6t3 %>% select(Codigo_Valv, Variedad, starts_with("Ha_"), starts_with("Kg_")))
