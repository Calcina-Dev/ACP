lines <- readLines("Actualizacion Semanal/app.R", encoding = "UTF-8")
cat("=== Frecuencias Modal occurrences ===\n")
print(grep("modal_frecuencia", lines, ignore.case = TRUE))
for (g in grep("modal_frecuencia", lines, ignore.case = TRUE)) {
  cat(sprintf("\nLine %d:\n", g))
  cat(paste(lines[max(1, g - 4):min(length(lines), g + 10)], collapse = "\n"))
}
