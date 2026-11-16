lines <- readLines("Actualizacion Semanal/app.R")
start <- grep("ejecutar_ajuste_semana <- function", lines)[1]
end <- grep("# BOTÓN AJUSTAR SEMANA", lines)[1]

for (i in start:end) {
  if (grepl("bind_rows", lines[i])) {
    cat(sprintf("Line %d: %s\n", i, trimws(lines[i])))
    for (j in max(start, i-2):min(end, i+8)) {
      cat(sprintf("   %d: %s\n", j, lines[j]))
    }
    cat("----------------------------------------\n")
  }
}
