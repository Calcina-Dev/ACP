lines <- readLines("D:/ACP/Actualizacion Semanal/app.R", encoding = "UTF-8", warn = FALSE)
for (i in 9815:9860) {
  cat(sprintf("%4d: %s\n", i, lines[i]))
}
