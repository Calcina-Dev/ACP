lines <- readLines("D:/ACP/Actualizacion Semanal/app.R", encoding = "UTF-8", warn = FALSE)
idx_tc <- grep('tabPanel\\("Comparativo"', lines)[1]
for (i in idx_tc:(idx_tc + 55)) {
  cat(sprintf("%4d: %s\n", i, lines[i]))
}
