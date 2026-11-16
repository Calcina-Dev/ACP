app_path <- "D:/ACP/Actualizacion Semanal/app.R"
lines <- readLines(app_path, warn = FALSE, encoding = "UTF-8")
cat(paste(lines[195:240], collapse = "\n"), "\n")
