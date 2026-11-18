lines <- readLines("Actualizacion Semanal/app.R", encoding = "UTF-8")
cat("Search for datos_conteo_reactivos:\n")
print(grep("datos_conteo_reactivos", lines))
for (idx in grep("datos_conteo_reactivos", lines)[1:5]) {
  cat(sprintf("\nLine %d:\n", idx))
  cat(paste(lines[max(1, idx-2):min(length(lines), idx+10)], collapse = "\n"))
}

cat("\nSearch for df_conteo_historico:\n")
print(grep("df_conteo_historico", lines))
for (idx in grep("df_conteo_historico", lines)[1:5]) {
  cat(sprintf("\nLine %d:\n", idx))
  cat(paste(lines[max(1, idx-2):min(length(lines), idx+10)], collapse = "\n"))
}
