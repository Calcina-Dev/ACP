lines <- readLines("Actualizacion Semanal/app.R", encoding = "UTF-8")
cat(paste(lines[6400:6450], collapse = "\n"))
