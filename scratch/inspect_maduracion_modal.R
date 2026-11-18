lines <- readLines("Actualizacion Semanal/app.R", encoding = "UTF-8")
cat("=== Días de Maduración occurrences ===\n")
print(grep("Días de Maduración|Dias de Maduracion", lines, ignore.case = TRUE))
for (g in grep("Días de Maduración|Dias de Maduracion", lines, ignore.case = TRUE)) {
  cat(sprintf("\nLine %d:\n", g))
  cat(paste(lines[max(1, g - 4):min(length(lines), g + 10)], collapse = "\n"))
}
