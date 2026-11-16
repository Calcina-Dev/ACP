suppressPackageStartupMessages({
  library(dplyr)
})

# 1. Search for hardcoded drive paths (C:, D:, G:, etc.)
scan_paths <- function(file_path) {
  lines <- readLines(file_path, warn = FALSE, encoding = "UTF-8")
  hardcoded <- grep("[A-Z]:[/\\\\]", lines, value = TRUE)
  return(hardcoded)
}

files_to_check <- c(
  "D:/ACP/Actualizacion Semanal/app.R",
  "D:/ACP/Actualizacion Semanal RNA/app.R",
  "D:/ACP/Rutas de Cosecha/app_mapa.R",
  "D:/ACP/Actualizacion data maestra/actualizar_data_maestra.R"
)

cat("==================================================\n")
cat("1. HARDCODED PATHS DETECTED\n")
cat("==================================================\n")
for (f in files_to_check) {
  if (file.exists(f)) {
    hc <- scan_paths(f)
    cat(sprintf("\nFile: %s (Total hardcoded path references: %d)\n", basename(f), length(hc)))
    for (h in head(unique(trimws(hc)), 8)) {
      cat("  ", substr(h, 1, 100), "\n")
    }
  }
}
