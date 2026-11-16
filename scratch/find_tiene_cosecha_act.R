lines <- readLines("D:/ACP/Actualizacion Semanal/app.R", warn = FALSE, encoding = "UTF-8")

idx <- grep("Tiene_Cosecha_Act", lines)
cat("Lines with Tiene_Cosecha_Act:\n")
for (i in idx) {
  cat(sprintf("%d: %s\n", i, trimws(lines[i])))
}
