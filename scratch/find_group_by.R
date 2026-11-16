lines <- readLines("Actualizacion Semanal/app.R")
start <- grep("ejecutar_ajuste_semana <- function", lines)[1]
end <- grep("# BOTÓN AJUSTAR SEMANA", lines)[1]
cat(sprintf("ejecutar_ajuste_semana from %d to %d\n", start, end))

for (i in start:end) {
  if (grepl("group_by", lines[i])) {
    cat(sprintf("Line %d: %s\n", i, trimws(lines[i])))
    # also print the 2 lines before it
    cat(sprintf("   Context: %s\n", trimws(lines[max(1, i-1)])))
  }
}
