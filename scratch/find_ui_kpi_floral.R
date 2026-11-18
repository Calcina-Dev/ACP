lines <- readLines("Actualizacion Semanal/app.R", encoding = "UTF-8")
print(grep("ui_kpi_factor_floral", lines))
for (idx in grep("ui_kpi_factor_floral", lines)) {
  cat(sprintf("\nLine %d:\n", idx))
  cat(paste(lines[max(1, idx-5):min(length(lines), idx+10)], collapse = "\n"))
}
