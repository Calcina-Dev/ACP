suppressMessages({
  library(readxl)
  library(dplyr)
})

base_dir <- "G:/Unidades compartidas/Compartido Proyecciones/3.- Arándano/Campaña 26-27/Proyección Semanal"
f_det_v2  <- file.path(base_dir, "Proyección Semanal Detallada", "26. Proyecciones Semana 46 + 5 semanas ACP (Conteo S44)_V2.xlsx")

# Check if tidyxl or openxlsx is available
cat("tidyxl available:", requireNamespace("tidyxl", quietly = TRUE), "\n")
cat("openxlsx available:", requireNamespace("openxlsx", quietly = TRUE), "\n")

if (requireNamespace("tidyxl", quietly = TRUE)) {
  library(tidyxl)
  cells <- xlsx_cells(f_det_v2, sheets = "Resumen Proyección")
  # Filter cells in col 32 (AF)
  sem2_cells <- cells %>% filter(col == 32, row %in% c(5, 6, 7, 9, 10, 21, 23, 31, 76))
  print(sem2_cells %>% select(row, col, address, formula, numeric))
} else if (requireNamespace("openxlsx", quietly = TRUE)) {
  wb <- openxlsx::loadWorkbook(f_det_v2)
  # check formula
} else {
  cat("Neither tidyxl nor openxlsx installed.\n")
}
