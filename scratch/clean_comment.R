# clean_comment.R
app_path <- "D:/ACP/Actualizacion Semanal/app.R"
lines <- readLines(app_path, encoding = "UTF-8", warn = FALSE)
lines <- lines[!grepl("# Selector de D.*en Escenario", lines)]
writeLines(lines, app_path, useBytes = TRUE)
cat("Comentario eliminado limpiamente.\n")
