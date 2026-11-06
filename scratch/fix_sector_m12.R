library(dplyr)

ed_paths <- c(
  "D:/ACP/Actualizacion Semanal/ediciones_manuales_cosecha.rds",
  "D:/ACP/Actualizacion data maestra/ediciones_manuales_cosecha.rds",
  "D:/ACP/ediciones_manuales_cosecha.rds"
)
for (ep in ed_paths) {
  if (file.exists(ep)) {
    ed <- readRDS(ep)
    if (nrow(ed) > 0 && "Módulo" %in% names(ed)) {
      # Mapear Sector 8 a 9 para Módulo 12
      ed$Sector[as.character(ed$Módulo) == "12"] <- "9"
      
      # Si tiene Tipo_Edicion y Fecha_Edicion, ordenar prioritariamente
      has_te <- "Tipo_Edicion" %in% names(ed)
      has_fe <- "Fecha_Edicion" %in% names(ed)
      
      if (has_te && has_fe) {
        ed <- ed %>% arrange(desc(Tipo_Edicion == "manual"), desc(Fecha_Edicion))
      } else if (has_fe) {
        ed <- ed %>% arrange(desc(Fecha_Edicion))
      }
      
      cols_dist <- intersect(c("Campaña", "Semana", "Sector", "Módulo", "Turno", "Variedad", "Campo"), names(ed))
      ed <- ed %>% distinct(across(all_of(cols_dist)), .keep_all = TRUE)
      
      saveRDS(ed, ep)
      cat("✔ ediciones_manuales_cosecha.rds deduplicado y actualizado:", ep, "| Filas finales:", nrow(ed), "\n")
    }
  }
}
cat("\n=== COMPLETADO CON ÉXITO ===\n")
