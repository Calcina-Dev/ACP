lines_std <- readLines("D:/ACP/Actualizacion Semanal/app.R", warn = FALSE, encoding = "UTF-8")
lines_rna <- readLines("D:/ACP/Actualizacion Semanal RNA/app.R", warn = FALSE, encoding = "UTF-8")

cat("Actualizacion Semanal lines:", length(lines_std), "\n")
cat("Actualizacion Semanal RNA lines:", length(lines_rna), "\n")

cat("Searching for 'modelo_rna' in Actualizacion Semanal:\n")
cat("Hits:", length(grep("modelo_rna", lines_std)), "\n")

cat("Searching for 'modelo_rna' in Actualizacion Semanal RNA:\n")
cat("Hits:", length(grep("modelo_rna", lines_rna)), "\n")
