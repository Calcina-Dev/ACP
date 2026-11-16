lines <- readLines("D:/ACP/Actualizacion Semanal/app.R", warn = FALSE, encoding = "UTF-8")
idx <- grep("tabla_auditoria|tabla_comparativo_detalle|output\\$tabla_auditoria", lines)
for (i in idx) {
  cat(sprintf("Line %d: %s\n", i, trimws(lines[i])))
}
