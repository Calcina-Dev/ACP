lines <- readLines("D:/ACP/Actualizacion Semanal/app.R", warn = FALSE, encoding = "UTF-8")
idx <- grep("desplazar_dia_2da_pasada", lines)
for (i in idx) {
  cat(sprintf("[%5d] %s\n", i, trimws(lines[i])))
}
