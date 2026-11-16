lines <- readLines("D:/ACP/Actualizacion Semanal/app.R", warn = FALSE, encoding = "UTF-8")

for (i in grep("datos_reactivos <- reactive", lines)) {
  cat(sprintf("%d: %s\n", i, trimws(lines[i])))
}
