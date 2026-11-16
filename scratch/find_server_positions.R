lines <- readLines("D:/ACP/Actualizacion Semanal/app.R", encoding = "UTF-8", warn = FALSE)
cat("Line of output$tabla_conciliacion_campo_sap:", grep("output\\$tabla_conciliacion_campo_sap", lines), "\n")
cat("Line of output$btn_descarga_conciliacion_excel:", grep("output\\$btn_descarga_conciliacion_excel", lines), "\n")
cat("End of file line:", length(lines), "\n")
