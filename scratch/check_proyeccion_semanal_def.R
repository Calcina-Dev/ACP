lines <- readLines("D:/ACP/Actualizacion Semanal/app.R", warn = FALSE, encoding = "UTF-8")
idx <- grep("proyeccion_semanal <- reactive", lines)
cat(sprintf("proyeccion_semanal at line %d\n", idx))
cat(paste(lines[idx:(idx+60)], collapse = "\n"))
