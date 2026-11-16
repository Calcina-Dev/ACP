lines <- readLines("D:/ACP/Actualizacion Semanal/app.R", encoding = "UTF-8", warn = FALSE)
for (i in (length(lines) - 25):length(lines)) {
  cat(sprintf("%4d: %s\n", i, lines[i]))
}
