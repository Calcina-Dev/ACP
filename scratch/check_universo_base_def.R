lines <- readLines("D:/ACP/Actualizacion Semanal/app.R", warn = FALSE, encoding = "UTF-8")
idx <- grep("universo_base <- reactive", lines)
cat(sprintf("universo_base at line %d\n", idx))
cat(paste(lines[idx:(idx+65)], collapse = "\n"))
