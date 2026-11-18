lines <- readLines("Actualizacion Semanal/app.R", encoding = "UTF-8")
cat("Search for Factor Floral S46 or card rendering:\n")
idx_card <- grep("Factor Floral S|Pendiente flor", lines)
print(idx_card)
for (idx in idx_card) {
  cat(sprintf("\nLine %d:\n", idx))
  cat(paste(lines[max(1, idx-5):min(length(lines), idx+20)], collapse = "\n"))
}
