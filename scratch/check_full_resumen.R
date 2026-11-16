suppressMessages({
  library(readxl)
  library(dplyr)
})

base_dir <- "G:/Unidades compartidas/Compartido Proyecciones/3.- Arándano/Campaña 26-27/Proyección Semanal"
f_form_v1 <- file.path(base_dir, "Proyección Semanal Formalizada", "26.- Proyección Cosecha Arándano ACP _ 2026 _ Sem 46 al 51.xlsx")
f_form_v2 <- file.path(base_dir, "Proyección Semanal Formalizada", "26.- Proyección Cosecha Arándano ACP _ 2026 _ Sem 46 al 51_V2.xlsx")

cat("=== FULL RESUMEN V1 ===\n")
r_v1 <- read_excel(f_form_v1, sheet = "Resumen")
print(as.data.frame(r_v1), row.names = FALSE)

cat("\n=== FULL RESUMEN V2 ===\n")
r_v2 <- read_excel(f_form_v2, sheet = "Resumen")
print(as.data.frame(r_v2), row.names = FALSE)
