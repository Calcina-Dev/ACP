suppressMessages({
  library(readxl)
  library(dplyr)
})

base_dir <- "G:/Unidades compartidas/Compartido Proyecciones/3.- Arándano/Campaña 26-27/Proyección Semanal"

f_det_v1 <- file.path(base_dir, "Proyección Semanal Detallada", "26. Proyecciones Semana 46 + 5 semanas ACP (Conteo S44).xlsx")
f_det_v2 <- file.path(base_dir, "Proyección Semanal Detallada", "26. Proyecciones Semana 46 + 5 semanas ACP (Conteo S44)_V2.xlsx")
f_form_v1 <- file.path(base_dir, "Proyección Semanal Formalizada", "26.- Proyección Cosecha Arándano ACP _ 2026 _ Sem 46 al 51.xlsx")
f_form_v2 <- file.path(base_dir, "Proyección Semanal Formalizada", "26.- Proyección Cosecha Arándano ACP _ 2026 _ Sem 46 al 51_V2.xlsx")

cat("--- Resumen in Formalizada V1 ---\n")
if (file.exists(f_form_v1)) {
  r_v1 <- read_excel(f_form_v1, sheet = "Resumen")
  print(head(r_v1, 20))
}

cat("\n--- Resumen in Formalizada V2 ---\n")
if (file.exists(f_form_v2)) {
  r_v2 <- read_excel(f_form_v2, sheet = "Resumen")
  print(head(r_v2, 20))
}

cat("\n--- SEM 46 in Formalizada V1 ---\n")
if (file.exists(f_form_v1)) {
  s46_v1 <- read_excel(f_form_v1, sheet = "SEM 46")
  print(head(s46_v1, 15))
}

cat("\n--- SEM 46 in Formalizada V2 ---\n")
if (file.exists(f_form_v2)) {
  s46_v2 <- read_excel(f_form_v2, sheet = "SEM 46")
  print(head(s46_v2, 15))
}
