lines_rna <- readLines("D:/ACP/Actualizacion Semanal RNA/app.R", warn = FALSE, encoding = "UTF-8")

cat(paste(lines_rna[10980:11025], collapse = "\n"))
