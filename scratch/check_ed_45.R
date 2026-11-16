suppressPackageStartupMessages({
  library(dplyr)
})

ediciones_file <- "D:/ACP/Actualizacion Semanal/ediciones_manuales_cosecha.rds"
ediciones <- readRDS(ediciones_file)
ed_45 <- ediciones %>% filter(`Campaña` == "2026", Semana == 45)
cat("Rows in 2026 sem 45:", nrow(ed_45), "\n")
print(table(ed_45$Campo))
print(table(ed_45$Tipo_Edicion))
cat("Total Valor (Kg ajustados) in ed_45:", sum(ed_45$Valor, na.rm = TRUE), "\n")
cat("Ha_Asignada total:", sum(ed_45$Ha_Asignada, na.rm = TRUE), "\n")
cat("Ha 1ra:", sum(ed_45$Ha_Asignada_1ra, na.rm = TRUE), "Kg 1ra:", sum(ed_45$Kg_Asignado_1ra, na.rm = TRUE), "\n")
cat("Ha 2da:", sum(ed_45$Ha_Asignada_2da, na.rm = TRUE), "Kg 2da:", sum(ed_45$Kg_Asignado_2da, na.rm = TRUE), "\n")
