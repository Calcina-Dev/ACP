suppressMessages({
  library(readxl)
  library(dplyr)
})

base_dir <- "G:/Unidades compartidas/Compartido Proyecciones/3.- Arándano/Campaña 26-27/Proyección Semanal"
f_det_v2  <- file.path(base_dir, "Proyección Semanal Detallada", "26. Proyecciones Semana 46 + 5 semanas ACP (Conteo S44)_V2.xlsx")

res <- read_excel(f_det_v2, sheet = "Resumen Proyección")
# Print rows 70 to 83
for (r in 70:min(83, nrow(res))) {
  cat("\nRow", r, ":\n")
  vals <- as.character(res[r, ])
  # non-na
  idx <- which(!is.na(vals) & vals != "")
  for (i in idx) {
    cat("  Col", i, "(", names(res)[i], ") =", vals[i], "\n")
  }
}
