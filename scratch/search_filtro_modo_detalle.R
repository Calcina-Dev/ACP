lines <- readLines("Actualizacion Semanal/app.R", encoding = "UTF-8")
cat("Search for filtro_modo_detalle:\n")
print(grep("filtro_modo_detalle", lines))
for (idx in grep("filtro_modo_detalle", lines)) {
  cat(sprintf("\nLine %d:\n", idx))
  cat(paste(lines[max(1, idx-2):min(length(lines), idx+15)], collapse = "\n"))
}
