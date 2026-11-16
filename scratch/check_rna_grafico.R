lines_rna <- readLines("D:/ACP/Actualizacion Semanal RNA/app.R", warn = FALSE, encoding = "UTF-8")
cat("In RNA app, searching for grafico_comparativo:\n")
for (i in grep("grafico_comparativo", lines_rna)) {
  cat(sprintf("Line %d: %s\n", i, trimws(lines_rna[i])))
}
