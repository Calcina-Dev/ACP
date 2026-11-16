lines <- readLines("D:/ACP/Actualizacion Semanal/app.R", warn = FALSE, encoding = "UTF-8")
idx <- grep("GR[AÁ]FICO COMPARATIVO|grafico_comp|renderPlotly", lines, ignore.case = TRUE)
cat("Hits:", length(idx), "\n")
for (i in idx) {
  cat(sprintf("%d: %s\n", i, trimws(lines[i])))
}
