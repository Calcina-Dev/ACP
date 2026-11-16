lines <- readLines("D:/ACP/Actualizacion Semanal/app.R", encoding = "UTF-8", warn = FALSE)
for (i in 6625:6735) {
  cat(sprintf("%4d: %s\n", i, lines[i]))
}
