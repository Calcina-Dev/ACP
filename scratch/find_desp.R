lines <- readLines("D:/ACP/Actualizacion Semanal/app.R", warn = FALSE, encoding = "UTF-8")
idx <- grep("desplazar_dia_2da_pasada <- function", lines)
cat("Lines found at:", idx, "\n")
for (i in idx) {
  cat("--- Line", i, "---\n")
  cat(paste(lines[i:(i+20)], collapse = "\n"), "\n")
}
