lines <- readLines("D:/ACP/Actualizacion Semanal/app.R", warn = FALSE, encoding = "UTF-8")
idx1 <- grep("Selector de Día de cosecha programado", lines)
idx2 <- grep("valvs_a_mover", lines)
cat("idx1:", idx1, "\n")
cat("idx2:", idx2, "\n")
if (length(idx1) > 0) {
  cat("--- Context 1 ---\n")
  cat(paste(lines[(idx1-5):(idx1+25)], collapse = "\n"), "\n")
}
if (length(idx2) > 0) {
  cat("--- Context 2 ---\n")
  cat(paste(lines[(idx2-10):(idx2+30)], collapse = "\n"), "\n")
}
