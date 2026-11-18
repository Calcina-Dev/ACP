lines <- readLines("Actualizacion Semanal/app.R", encoding = "UTF-8")
cat("=== modal_celda_activa occurrences ===\n")
grep_res <- grep("modal_celda_activa\\(", lines)
print(grep_res)
for (g in grep_res) {
  cat(sprintf("\nLine %d:\n", g))
  cat(paste(lines[max(1, g - 5):min(length(lines), g + 15)], collapse = "\n"))
}
