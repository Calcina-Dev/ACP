lines <- readLines("Actualizacion Semanal/app.R", encoding = "UTF-8")
print(grep("Diagnóstico y Desglose del Turno", lines))
for (idx in grep("Diagnóstico y Desglose del Turno", lines)) {
  cat(sprintf("\nLine %d:\n", idx))
  cat(paste(lines[max(1, idx-2):min(length(lines), idx+15)], collapse = "\n"))
}
