lines <- readLines("D:/ACP/Actualizacion Semanal/app.R", warn = FALSE, encoding = "UTF-8")
idx <- grep("grafico_comparativo_turnos", lines)
cat("Occurrences of 'grafico_comparativo_turnos':", length(idx), "\n")
for (i in idx) {
  cat(sprintf("Line %d: %s\n", i, trimws(lines[i])))
}
