lines <- readLines("Actualizacion Semanal/app.R", encoding = "UTF-8")
idx_start <- grep('tabPanel\\("Histórico Conteo"', lines)
cat("Found tabPanel Histórico Conteo at line:", idx_start, "\n")
idx_next <- grep('tabPanel\\("Ruta de cosecha"', lines)
cat("Found tabPanel Ruta de cosecha at line:", idx_next, "\n")

# Target to remove is from idx_start to idx_next - 1
idx_end <- idx_next - 1
cat("Line end to remove:", lines[idx_end], "\n")

lines <- lines[-c(idx_start:idx_end)]
writeLines(lines, "Actualizacion Semanal/app.R", useBytes = TRUE)
cat("Successfully removed tabPanel Histórico Conteo!\n")
