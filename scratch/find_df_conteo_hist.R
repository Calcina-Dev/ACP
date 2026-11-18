lines <- readLines("Actualizacion Semanal/app.R", encoding = "UTF-8")
print(grep("df_conteo_historico", lines[1:1500]))
for (idx in grep("df_conteo_historico", lines[1:1500])) {
  cat(sprintf("\nLine %d:\n", idx))
  cat(paste(lines[max(1, idx-2):min(length(lines), idx+10)], collapse = "\n"))
}
