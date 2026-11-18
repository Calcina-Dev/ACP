lines <- readLines("Actualizacion Semanal/app.R", encoding = "UTF-8")
cat("Occurrences of 'Castigo floral':\n")
grep_cf <- grep("Castigo floral", lines, ignore.case = TRUE)
print(grep_cf)
for (idx in grep_cf) {
  cat(sprintf("\n--- Line %d ---\n", idx))
  cat(paste(lines[max(1, idx-3):min(length(lines), idx+4)], collapse = "\n"))
}

cat("\n\nOccurrences of '40.0%':\n")
print(grep("40\\.0%", lines))
for (idx in grep("40\\.0%", lines)) {
  cat(sprintf("\n--- Line %d ---\n", idx))
  cat(paste(lines[max(1, idx-3):min(length(lines), idx+4)], collapse = "\n"))
}
