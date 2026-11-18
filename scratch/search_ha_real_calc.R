lines <- readLines("Actualizacion Semanal/app.R", encoding = "UTF-8")
cat("Search for 'Ha Real 1' or 'Kg/Ha Real':\n")
print(grep("Ha Real 1ª Pas|Kg/Ha Real 1ª Pas|Ha_Real_1ra|Kg_Ha_Real", lines))
for (idx in grep("Ha Real 1ª Pas|Kg/Ha Real 1ª Pas", lines)) {
  cat(sprintf("\nLine %d:\n", idx))
  cat(paste(lines[max(1, idx-5):min(length(lines), idx+10)], collapse = "\n"))
}
