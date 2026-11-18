load("Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
suppressPackageStartupMessages(library(dplyr))

cat("=== M2 T3 in df_hist_oficial_wide S46 ===\n")
s46_m2_t3 <- df_hist_oficial_wide %>%
  filter(as.numeric(Semana) == 46, as.character(Módulo_Norm) == "2", as.character(Turno_Norm) == "3")
print(s46_m2_t3 %>% select(Codigo_Valv, Variedad, Area_Ha, Ha_Lunes, Ha_Martes, Ha_Miércoles, Ha_Jueves, Ha_Viernes, Ha_Sábado, Kg_Jueves))

cat("\n=== Rutas de Cosecha S46 M2 T3 ===\n")
if (file.exists("Rutas de Cosecha/rutas_cosecha_mapa.rds")) {
  rc <- readRDS("Rutas de Cosecha/rutas_cosecha_mapa.rds")
  rc_m2_t3 <- rc %>% filter(as.numeric(Semana) == 46, as.character(Módulo) == "2", as.character(Turno) == "3")
  print(rc_m2_t3 %>% select(any_of(c("codigo", "Codigo_Valvula", "Variedad", "Área", "Ha_Lunes", "Ha_Martes", "Ha_Miércoles", "Ha_Jueves", "Ha_Viernes", "Ha_Sábado", "Num_Pasada"))))
}
