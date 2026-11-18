lines <- readLines("Actualizacion Semanal/app.R", encoding = "UTF-8")
cat("Search for Ajuste_Floral_Pct:\n")
print(grep("Ajuste_Floral_Pct", lines))
for (idx in grep("Ajuste_Floral_Pct", lines)) {
  cat(sprintf("\nLine %d:\n", idx))
  cat(paste(lines[max(1, idx-2):min(length(lines), idx+3)], collapse = "\n"))
}

cat("\nSearch for Delta_Floral_Pct:\n")
print(grep("Delta_Floral_Pct", lines))
for (idx in grep("Delta_Floral_Pct", lines)) {
  cat(sprintf("\nLine %d:\n", idx))
  cat(paste(lines[max(1, idx-2):min(length(lines), idx+3)], collapse = "\n"))
}
