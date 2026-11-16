files <- c(
  "D:/ACP/Rutas de Cosecha/app_mapa.R",
  "D:/ACP/Actualizacion Semanal/app.R",
  "D:/ACP/Actualizacion Semanal RNA/app.R",
  "D:/ACP/Actualizacion data maestra/actualizar_data_maestra.R"
)

cat("Scanning for URLs, Webhooks, tokens, passwords:\n")
for (f in files) {
  lines <- readLines(f, warn = FALSE, encoding = "UTF-8")
  hits <- grep("https://script\\.google\\.com|token|api_key|password|secret|key\\s*=", lines, ignore.case = TRUE)
  if (length(hits) > 0) {
    cat(sprintf("\nFile: %s (Lines: %s)\n", basename(f), paste(hits, collapse = ", ")))
    for (h in head(hits, 5)) {
      cat("  Line", h, ":", substr(trimws(lines[h]), 1, 100), "\n")
    }
  }
}
