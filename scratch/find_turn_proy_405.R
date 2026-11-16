suppressMessages({
  library(readxl)
  library(dplyr)
})

base_dir <- "G:/Unidades compartidas/Compartido Proyecciones/3.- Arándano/Campaña 26-27/Proyección Semanal"
f_det_v2  <- file.path(base_dir, "Proyección Semanal Detallada", "26. Proyecciones Semana 46 + 5 semanas ACP (Conteo S44)_V2.xlsx")
f_form_v2 <- file.path(base_dir, "Proyección Semanal Formalizada", "26.- Proyección Cosecha Arándano ACP _ 2026 _ Sem 46 al 51_V2.xlsx")

# 1. In sheet "Ajuste Semana" of Detallada V2:
# What are all the columns, and what are the totals of each column?
aj <- read_excel(f_det_v2, sheet = "Ajuste Semana")
cat("Ajuste Semana rows:", nrow(aj), "\n")

# Check all numeric columns sums in Ajuste Semana
for (col in names(aj)) {
  val <- suppressWarnings(as.numeric(aj[[col]]))
  if (sum(!is.na(val)) > 10) {
    s <- sum(val, na.rm = TRUE)
    cat(sprintf("%-25s: sum = %12.2f | non-na = %d\n", col, s, sum(!is.na(val))))
  }
}

# 2. Are there other sheets in Detallada V2?
cat("\nAll sheets in Detallada V2:\n")
print(excel_sheets(f_det_v2))

# 3. What sheets in Formalizada V2?
cat("\nAll sheets in Formalizada V2:\n")
print(excel_sheets(f_form_v2))

# Let's inspect each sheet of Formalizada V2
for (sh in excel_sheets(f_form_v2)) {
  df <- read_excel(f_form_v2, sheet = sh, n_max = 10)
  cat("Sheet", sh, "in Formalizada V2 - cols:\n")
  print(names(df))
}
