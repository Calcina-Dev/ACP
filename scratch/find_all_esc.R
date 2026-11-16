lines <- readLines("Actualizacion Semanal/app.R", encoding = "UTF-8", warn = FALSE)
all_esc <- which(grepl("^\\s*df_esc\\s*<-\\s*rv_escenarios\\(\\)", lines))
cat("Occurrences of df_esc <- rv_escenarios():", paste(all_esc, collapse = ", "), "\n")
for (idx in all_esc) {
  cat("At", idx, ":\n")
  cat(paste(lines[max(1, idx-2):min(length(lines), idx+3)], collapse = "\n"), "\n---\n")
}
