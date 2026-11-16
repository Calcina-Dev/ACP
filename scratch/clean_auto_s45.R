suppressPackageStartupMessages(library(dplyr))
ed <- readRDS("Actualizacion Semanal RNA/ediciones_manuales_cosecha.rds")
cat("Tipo de edicion en ediciones_manuales_cosecha.rds:\n")
print(table(as.character(ed[["Semana"]]), as.character(ed[["Tipo_Edicion"]]), useNA = "always"))

# Let's clean out the auto records for semana 45 that had the false 2da pasada
ed_clean <- ed %>%
  filter(!(as.numeric(Semana) == 45 & coalesce(Tipo_Edicion, "") %in% c("auto", "ruta_sin_kg")))

saveRDS(ed_clean, "Actualizacion Semanal RNA/ediciones_manuales_cosecha.rds")
cat("✔ Registros automáticos de S45 limpiados en Actualizacion Semanal RNA. Total registros restantes:", nrow(ed_clean), "\n")
