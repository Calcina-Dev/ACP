lines_adm <- readLines("D:/ACP/Actualizacion data maestra/actualizar_data_maestra.R", warn = FALSE, encoding = "UTF-8")
cat("Lines in actualizar_data_maestra.R:", length(lines_adm), "\n")
cat(paste(head(lines_adm, 40), collapse = "\n"))
