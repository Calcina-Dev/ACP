suppressPackageStartupMessages({
  library(dplyr)
})

find_modulo_logic <- function(file_path) {
  lines <- readLines(file_path, warn = FALSE, encoding = "UTF-8")
  idx <- grep("between\\(Turno|Módulo == \"9\"|Módulo == \"11\"|Módulo_Norm == \"9\"|11\\.1|11\\.2|13\\.1|14\\.1", lines)
  return(lines[idx])
}

files <- c(
  "D:/ACP/Actualizacion data maestra/actualizar_data_maestra.R",
  "D:/ACP/Actualizacion Semanal/app.R",
  "D:/ACP/Actualizacion Semanal RNA/app.R",
  "D:/ACP/Rutas de Cosecha/app_mapa.R"
)

for (f in files) {
  cat("\n=== File:", basename(f), "===\n")
  hits <- find_modulo_logic(f)
  cat(paste(head(trimws(hits), 15), collapse = "\n"), "\n")
}
