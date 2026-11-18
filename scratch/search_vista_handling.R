lines <- readLines("Actualizacion Semanal/app.R", encoding = "UTF-8")
cat("Search for vista_tabla or VISTA:\n")
print(grep("vista_tabla|VISTA|btn_vista", lines, ignore.case = TRUE))
for (idx in grep("vista_tabla", lines, ignore.case = TRUE)) {
  cat(sprintf("\nLine %d:\n", idx))
  cat(paste(lines[max(1, idx-2):min(length(lines), idx+10)], collapse = "\n"))
}
