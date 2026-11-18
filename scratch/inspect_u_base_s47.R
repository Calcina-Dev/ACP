load("Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
suppressPackageStartupMessages(library(dplyr))

# Inspect S47 M13 T1 columns in df_proyecciones and df_hist_oficial_wide
sub_proy <- df_proyecciones %>%
  filter(as.numeric(Semana) == 47, as.character(Módulo) == "13", as.character(Turno) == "1")
cat("=== df_proyecciones S47 M13 T1 ===\n")
print(sub_proy)

sub_sap <- df_hist_oficial_wide %>%
  filter(as.numeric(Semana) == 47, as.character(Módulo_Norm) == "13", as.character(Turno_Norm) == "1")
cat("\n=== df_hist_oficial_wide S47 M13 T1 ===\n")
print(sub_sap %>% select(Codigo_Valv, Variedad, Pasada, Area_Ha, starts_with("Kg_"), starts_with("Ha_")))
