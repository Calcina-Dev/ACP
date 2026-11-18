load("Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
suppressPackageStartupMessages(library(dplyr))

sem_sel <- 46
mod_sel <- "2"
tur_sel <- "3"
var_sel <- "SEKOYA POP"
dias_previos <- c("Lunes", "Martes")

# Test df_hist_oficial_wide
sub_sap <- df_hist_oficial_wide %>%
  filter(
    as.numeric(Semana) == sem_sel,
    as.character(Módulo_Norm) == mod_sel,
    as.character(Turno_Norm) == tur_sel,
    toupper(trimws(as.character(Variedad))) == toupper(trimws(var_sel))
  )

cols_ha_prev <- paste0("Ha_", dias_previos)
cols_ha_exist <- intersect(cols_ha_prev, names(sub_sap))

ha_prev_sap <- if (length(cols_ha_exist) > 0 && nrow(sub_sap) > 0) {
  sum(rowSums(sub_sap %>% select(all_of(cols_ha_exist)), na.rm = TRUE), na.rm = TRUE)
} else 0

cat("Real ha prior to Wednesday (SAP):", ha_prev_sap, "\n")
