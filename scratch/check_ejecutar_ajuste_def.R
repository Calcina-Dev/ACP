lines <- readLines("D:/ACP/Actualizacion Semanal/app.R", warn = FALSE, encoding = "UTF-8")
idx <- grep("ejecutar_ajuste_semana <- function", lines)
cat(sprintf("ejecutar_ajuste_semana at line %d\n", idx))
cat(paste(lines[idx:(idx+60)], collapse = "\n"))
