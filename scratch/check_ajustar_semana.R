lines <- readLines("D:/ACP/Actualizacion Semanal/app.R", warn = FALSE, encoding = "UTF-8")
idx <- grep("observeEvent.*btn_ajustar|btn_ejecutar_ajuste|ajustar_semana", lines, ignore.case = TRUE)
for (i in idx) {
  cat(sprintf("Line %d: %s\n", i, trimws(lines[i])))
}
