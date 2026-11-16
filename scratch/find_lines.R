lines <- readLines("Actualizacion Semanal/app.R", encoding = "UTF-8", warn = FALSE)

start_idx <- which(grepl("^\\s*df_esc\\s*<-\\s*rv_escenarios\\(\\)", lines))[1]
end_idx <- which(grepl("^\\s*# PERSPECTIVA 1: COMPARAR 2 ESCENARIOS VS REAL", lines))[1]

cat("start_idx:", start_idx, "end_idx:", end_idx, "\n")
cat("Line at start_idx:", lines[start_idx], "\n")
cat("Line at end_idx:", lines[end_idx], "\n")
