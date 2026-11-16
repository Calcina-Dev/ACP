lines <- readLines("D:/ACP/Actualizacion Semanal/app.R", warn = FALSE, encoding = "UTF-8")
idx <- grep("grafico_comparativo\\b", lines)
cat("Occurrences of 'grafico_comparativo':", length(idx), "\n")
for (i in idx) {
  cat(sprintf("Line %d: %s\n", i, trimws(lines[i])))
}
