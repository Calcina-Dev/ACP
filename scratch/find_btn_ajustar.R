lines <- readLines("Actualizacion Semanal/app.R", encoding = "UTF-8")
print(grep("btn_ajustar_semana", lines))
for (idx in grep("btn_ajustar_semana", lines)) {
  cat(sprintf("\nLine %d:\n", idx))
  cat(paste(lines[max(1, idx-2):min(length(lines), idx+15)], collapse = "\n"))
}
