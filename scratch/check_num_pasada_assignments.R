lines <- readLines("D:/ACP/Actualizacion Semanal/app.R", warn = FALSE, encoding = "UTF-8")
idx <- grep("Num_Pasada\\s*=\\s*2L", lines)
for (i in idx) {
  cat(sprintf("Line %d: %s\n", i, trimws(lines[i])))
}
