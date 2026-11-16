lines <- readLines("D:/ACP/Actualizacion Semanal/app.R", warn = FALSE, encoding = "UTF-8")

for (i in grep("tabla_detalle", lines)) {
  cat(sprintf("%d: %s\n", i, trimws(lines[i])))
}
