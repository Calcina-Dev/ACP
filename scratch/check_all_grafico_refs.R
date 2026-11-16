lines <- readLines("D:/ACP/Actualizacion Semanal/app.R", warn = FALSE, encoding = "UTF-8")
cat("Search 'grafico_comparativo' anywhere in file:\n")
for (i in grep("grafico_comparativo", lines)) {
  cat(sprintf("Line %d: %s\n", i, trimws(lines[i])))
}
