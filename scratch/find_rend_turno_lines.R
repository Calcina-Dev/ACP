lines <- readLines("Actualizacion Semanal/app.R", encoding = "UTF-8")
cat("Search rend_turno:\n")
grep_rt <- grep("rend_turno", lines)
grep_rt <- grep_rt[grep_rt > 14240 & grep_rt < 14400]
print(grep_rt)
for (idx in grep_rt[1:10]) {
  cat(sprintf("\nLine %d:\n", idx))
  cat(paste(lines[max(1, idx-1):min(length(lines), idx+3)], collapse = "\n"))
}
