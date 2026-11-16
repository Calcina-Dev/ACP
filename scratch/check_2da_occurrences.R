lines <- readLines("D:/ACP/Actualizacion Semanal/app.R", warn = FALSE, encoding = "UTF-8")
pas_idx <- grep("Num_Pasada.*2", lines)
cat("Lines with Num_Pasada == 2:", length(pas_idx), "\n")
for (i in head(pas_idx, 20)) {
  cat(sprintf("[%5d] %s\n", i, trimws(lines[i])))
}
