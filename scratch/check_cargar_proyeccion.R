lines <- readLines("D:/ACP/Actualizacion Semanal/app.R", warn = FALSE, encoding = "UTF-8")
idx <- grep("^cargar_proyeccion <- function", lines)
cat(sprintf("cargar_proyeccion at line %d\n", idx))
cat(paste(lines[idx:(idx+60)], collapse = "\n"))
