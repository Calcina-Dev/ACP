suppressMessages({
  library(readxl)
  library(dplyr)
})

base_dir <- "G:/Unidades compartidas/Compartido Proyecciones/3.- Arándano/Campaña 26-27/Proyección Semanal"
f_det_v2  <- file.path(base_dir, "Proyección Semanal Detallada", "26. Proyecciones Semana 46 + 5 semanas ACP (Conteo S44)_V2.xlsx")
f_integ   <- file.path(base_dir, "Proyección Semanal Detallada", "Integradas", "26. Proyecciones Semana 46 + 5 semanas ACP (Conteo S44)_V2_Integrada.xlsx")

cat("Sheets in Detallada V2:\n")
print(excel_sheets(f_det_v2))

cat("\nSheets in Detallada V2 Integrada:\n")
if (file.exists(f_integ)) {
  print(excel_sheets(f_integ))
  for (sh in excel_sheets(f_integ)) {
    df <- tryCatch(read_excel(f_integ, sheet = sh, n_max = 20), error = function(e) NULL)
    cat("Sheet", sh, "cols:", paste(names(df)[1:min(10, ncol(df))], collapse = ", "), "\n")
  }
}

# Let's inspect Detallada V2 sheet "Resumen Proyección"
cat("\n--- Resumen Proyección in Detallada V2 ---\n")
res_det <- read_excel(f_det_v2, sheet = "Resumen Proyección", skip = 2)
print(head(res_det, 25))
