lines <- readLines("D:/ACP/Actualizacion Semanal/app.R", warn = FALSE, encoding = "UTF-8")
idx <- grep("dia_destino <- if \\(!is.null\\(input\\$modal_input_dia\\)", lines)
cat("idx:", idx, "\n")
for (i in idx) {
  cat(sprintf("\n--- Occurrence at line %d ---\n", i))
  cat(paste(lines[(i-10):(i+40)], collapse = "\n"), "\n")
}
