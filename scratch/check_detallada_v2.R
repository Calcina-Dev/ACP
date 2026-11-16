suppressMessages({
  library(readxl)
  library(dplyr)
})

base_dir <- "G:/Unidades compartidas/Compartido Proyecciones/3.- Arándano/Campaña 26-27/Proyección Semanal"
f_det_v2  <- file.path(base_dir, "Proyección Semanal Detallada", "26. Proyecciones Semana 46 + 5 semanas ACP (Conteo S44)_V2.xlsx")

calc_det <- read_excel(f_det_v2, sheet = "Calculo", skip = 3)
cat("Names of sheet Calculo:\n")
print(names(calc_det)[1:25])

cat("\nFilter Sem == 46 if column Sem exists:\n")
if ("Sem" %in% names(calc_det)) {
  cat("Unique values in Sem:\n")
  print(unique(calc_det$Sem))
}

col_p <- "Proyección (kg)"
if (col_p %in% names(calc_det)) {
  vals <- suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(calc_det[[col_p]]))))
  cat("Total sum of Proyección (kg) in entire sheet Calculo:", sum(vals, na.rm = TRUE), "\n")
  
  # Group by Sem
  if ("Sem" %in% names(calc_det)) {
    print(calc_det %>% group_by(Sem) %>% summarise(Sum_Kg = sum(suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(.data[[col_p]])))), na.rm = TRUE)))
  }
}

# Let's inspect other sheets in f_det_v2!
cat("\nOther sheets in f_det_v2:\n")
for (sh in c("Ajuste Semana", "Resumen Proyección")) {
  cat("--- Sheet:", sh, "---\n")
  df_s <- read_excel(f_det_v2, sheet = sh)
  print(head(df_s, 10))
}
