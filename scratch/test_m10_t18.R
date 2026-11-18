load("Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
suppressPackageStartupMessages(library(dplyr))

sem_sel <- 46
mod_sel <- "10"
tur_sel <- "18"
var_sel <- "MALIBU"

sub_sap <- df_hist_oficial_wide %>%
  filter(
    as.numeric(Semana) == sem_sel,
    as.character(Módulo_Norm) == mod_sel,
    as.character(Turno_Norm) == tur_sel
  )
print(sub_sap %>% select(Codigo_Valv, Variedad, Area_Ha, starts_with("Ha_"), starts_with("Kg_")))
