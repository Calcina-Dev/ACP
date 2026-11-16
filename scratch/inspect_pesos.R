suppressMessages({
  library(readxl)
  library(dplyr)
})

base_dir <- "G:/Unidades compartidas/Compartido Proyecciones/3.- Arándano/Campaña 26-27/Proyección Semanal"
f_det_v2  <- file.path(base_dir, "Proyección Semanal Detallada", "26. Proyecciones Semana 46 + 5 semanas ACP (Conteo S44)_V2.xlsx")

pesos <- read_excel(f_det_v2, sheet = "Pesos")
cat("Sheet Pesos columns:\n")
print(names(pesos))
print(head(pesos, 15))

for (col in names(pesos)) {
  val <- suppressWarnings(as.numeric(pesos[[col]]))
  if (sum(!is.na(val)) > 0) {
    cat("  Col", col, "sum:", sum(val, na.rm = TRUE), "\n")
  }
}
