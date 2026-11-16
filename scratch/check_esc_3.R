suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

escenarios_file <- "D:/ACP/Actualizacion Semanal/escenarios_ajuste_semanal.rds"
df_esc <- readRDS(escenarios_file)
esc_3 <- df_esc[3, ]
cat("Nombre:", esc_3$Nombre, "\n")
cat("Dia_Desde:", esc_3$Dia_Desde, "Dia_Hasta:", esc_3$Dia_Hasta, "\n")
cat("Dias_Ajustados:", esc_3$Dias_Ajustados, "\n")
cat("Total_Kg_Ajustados:", esc_3$Total_Kg_Ajustados, "\n")

ed_3 <- esc_3$Ediciones_Data[[1]]
cat("Ediciones en esc_3 rows:", nrow(ed_3), "\n")
print(table(ed_3$Campo))
print(table(ed_3$Módulo))
