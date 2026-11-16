lines <- readLines("Actualizacion Semanal/app.R", encoding = "UTF-8", warn = FALSE)
cat(paste(lines[5475:5515], collapse = "\n"), "\n")
