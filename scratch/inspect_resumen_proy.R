suppressMessages({
  library(readxl)
  library(dplyr)
})

base_dir <- "G:/Unidades compartidas/Compartido Proyecciones/3.- Arándano/Campaña 26-27/Proyección Semanal"
f_det_v2  <- file.path(base_dir, "Proyección Semanal Detallada", "26. Proyecciones Semana 46 + 5 semanas ACP (Conteo S44)_V2.xlsx")

res <- read_excel(f_det_v2, sheet = "Resumen Proyección")
cat("Dimensions of Resumen Proyección:", dim(res), "\n")

# find rows where any cell has "Total" or similar, or find numeric columns
for (i in 1:nrow(res)) {
  txt <- paste(res[i, 1:6], collapse = " ")
  if (grepl("Total|Acum|405|383", txt, ignore.case = TRUE)) {
    cat("Row", i, ":", txt, "\n")
  }
}

# Look at bottom rows
cat("\nBottom 15 rows of Resumen Proyección:\n")
print(tail(res[, 1:12], 15))
