lines <- readLines("Actualizacion Semanal/app.R", encoding = "UTF-8")
cat("Search for dias_previos_u:\n")
print(grep("dias_previos_u", lines))
for (idx in grep("dias_previos_u", lines)) {
  cat(sprintf("\nLine %d:\n", idx))
  cat(paste(lines[max(1, idx-2):min(length(lines), idx+5)], collapse = "\n"))
}
