lines <- readLines("D:/ACP/Actualizacion Semanal/app.R", warn = FALSE, encoding = "UTF-8")
cat("Searching for ui_comparativo_chart_badge:\n")
for (i in grep("ui_comparativo_chart_badge", lines)) {
  cat(sprintf("Line %d: %s\n", i, trimws(lines[i])))
}
