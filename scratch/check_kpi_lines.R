lines <- readLines("D:/ACP/Actualizacion Semanal/app.R", encoding = "UTF-8", warn = FALSE)
for (i in 15143:15160) {
  cat(sprintf("%5d: %s\n", i, lines[i]))
}
