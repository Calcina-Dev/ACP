suppressPackageStartupMessages(library(dplyr))
if (file.exists("Actualizacion Semanal/ediciones_manuales_cosecha.rds")) {
  ed <- readRDS("Actualizacion Semanal/ediciones_manuales_cosecha.rds")
  ed_clean <- ed %>% filter(!(as.numeric(Semana) == 45 & coalesce(Tipo_Edicion, "") %in% c("auto", "ruta_sin_kg")))
  saveRDS(ed_clean, "Actualizacion Semanal/ediciones_manuales_cosecha.rds")
  cat("✔ Actualizacion Semanal ediciones_manuales_cosecha.rds limpiado.\n")
}
if (file.exists("Actualizacion Semanal/ajuste_valvulas_semana.rds")) {
  aj <- readRDS("Actualizacion Semanal/ajuste_valvulas_semana.rds")
  aj_clean <- aj %>% filter(as.numeric(Semana) != 45)
  saveRDS(aj_clean, "Actualizacion Semanal/ajuste_valvulas_semana.rds")
  cat("✔ Actualizacion Semanal ajuste_valvulas_semana.rds limpiado.\n")
}
