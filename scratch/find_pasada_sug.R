lines <- readLines("D:/ACP/Actualizacion Semanal/app.R", warn = FALSE, encoding = "UTF-8")
idx <- grep("pasada_sug", lines)
cat("Lines with pasada_sug:", idx, "\n")
for (i in idx) {
  cat(sprintf("[%5d] %s\n", i, trimws(lines[i])))
}
