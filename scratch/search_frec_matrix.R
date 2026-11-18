lines <- readLines("Actualizacion Semanal/app.R", encoding = "UTF-8")
cat("Search for Turnos por semana / Matriz de dias:\n")
print(grep("Turnos por semana|Matriz de días transcurridos", lines, ignore.case = TRUE))
for (idx in grep("Turnos por semana|Matriz de días transcurridos", lines, ignore.case = TRUE)) {
  cat(sprintf("\nLine %d:\n", idx))
  cat(paste(lines[max(1, idx-2):min(length(lines), idx+15)], collapse = "\n"))
}
