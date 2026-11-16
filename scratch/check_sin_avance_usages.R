lines <- readLines("D:/ACP/Actualizacion Semanal/app.R", warn = FALSE, encoding = "UTF-8")

idx <- grep("Sin avance previo", lines)
for (i in idx) {
  cat(sprintf("%d: %s\n", i, trimws(lines[i])))
}
