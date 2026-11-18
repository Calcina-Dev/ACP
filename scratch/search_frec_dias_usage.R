lines <- readLines("Actualizacion Semanal/app.R", encoding = "UTF-8")
cat("Search for Frecuencia_Dias:\n")
print(grep("Frecuencia_Dias", lines))
for (idx in grep("Frecuencia_Dias", lines)[1:10]) {
  cat(sprintf("\nLine %d:\n", idx))
  cat(paste(lines[max(1, idx-2):min(length(lines), idx+5)], collapse = "\n"))
}
