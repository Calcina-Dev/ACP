lines <- readLines("Actualizacion data maestra/actualizar_data_maestra.R", encoding = "UTF-8", warn = FALSE)
idx <- grep("Reconciliaci.n oficial", lines)
cat("Reconciliation inserted at line:", idx, "\n")
print(lines[(idx - 10):(idx + 30)])
