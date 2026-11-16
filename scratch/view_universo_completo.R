lines <- readLines("D:/ACP/Actualizacion Semanal/app.R", encoding = "UTF-8", warn = FALSE)
idx_uc <- grep("universo_completo <- reactive", lines)[1]
for (i in idx_uc:(idx_uc + 110)) {
  cat(sprintf("%4d: %s\n", i, lines[i]))
}
