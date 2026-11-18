lines <- readLines("Actualizacion Semanal/app.R", encoding = "UTF-8")
lines[10667] <- '      txt_frec_aj <- if (grepl("Frecuencia ([0-9.]+d \\\\([+-]?[0-9.]+%\\\\))", motivo_str)) {'
lines[10668] <- '        sub(".*Frecuencia ([0-9.]+d \\\\([+-]?[0-9.]+%\\\\)).*", "\\\\1", motivo_str)'
writeLines(lines, "Actualizacion Semanal/app.R", useBytes = TRUE)
cat("Escapes fixed!\n")
