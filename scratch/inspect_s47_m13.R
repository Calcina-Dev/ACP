# Inspecting evaluar_escenario and how Pasada is handled
load("Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
suppressPackageStartupMessages(library(dplyr))

# Let's inspect Semana 47 Módulo 13 Turno 1
s47_m13 <- df_hist_oficial_wide %>%
  filter(as.numeric(Semana) == 47, as.character(Módulo_Norm) == "13", as.character(Turno_Norm) == "1")

cat("=== S47 M13 T1 in df_hist_oficial_wide ===\n")
print(s47_m13 %>% select(Codigo_Valv, Variedad, Pasada, Area_Ha, starts_with("Kg_"), starts_with("Ha_")))

if (file.exists("Actualizacion Semanal/ediciones_manuales_cosecha.rds")) {
  ed <- readRDS("Actualizacion Semanal/ediciones_manuales_cosecha.rds")
  cat("\n=== S47 M13 T1 in ediciones_manuales ===\n")
  print(ed %>% filter(as.numeric(Semana) == 47, as.character(Módulo) == "13", as.character(Turno) == "1"))
}

if (file.exists("Actualizacion Semanal/ajuste_valvulas_semana.rds")) {
  valvs <- readRDS("Actualizacion Semanal/ajuste_valvulas_semana.rds")
  cat("\n=== S47 M13 T1 in ajuste_valvulas ===\n")
  print(valvs %>% filter(as.numeric(Semana) == 47, as.character(Módulo) == "13", as.character(Turno) == "1"))
}
