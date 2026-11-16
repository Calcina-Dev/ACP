lines <- readLines("D:/ACP/Actualizacion Semanal/app.R", warn = FALSE, encoding = "UTF-8")

idx <- grep("Motivo_Ajuste", lines)
cat("Lines with Motivo_Ajuste:", length(idx), "\n")
for (i in head(idx, 20)) {
  cat(sprintf("%d: %s\n", i, trimws(lines[i])))
}
