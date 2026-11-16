lines <- readLines("D:/ACP/Actualizacion Semanal/app.R", warn = FALSE, encoding = "UTF-8")
idx <- grep("modal_input_pasada", lines)
cat("Lines with modal_input_pasada:", idx, "\n")
for (i in idx) {
  cat(sprintf("[%5d] %s\n", i, trimws(lines[i])))
}
