lines <- readLines("D:/ACP/Actualizacion Semanal/app.R", warn = FALSE, encoding = "UTF-8")
idx <- grep("kpi_proy|renderUI.*kpi|kpi_proyeccion|Proyección Total|Cosecha Ajustada", lines)
for (i in head(idx, 25)) {
  cat(sprintf("Line %d: %s\n", i, trimws(lines[i])))
}
