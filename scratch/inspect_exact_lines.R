lines <- readLines("Actualizacion Semanal RNA/app.R", warn = FALSE)
cat("=== 1. rectif_martes_ids (around 10528) ===\n")
cat(lines[10525:10535], sep = "\n")

cat("\n=== 2. turno_avance_ha (around 10753) ===\n")
cat(lines[10750:10780], sep = "\n")

cat("\n=== 3. valvulas_calc join rend_turno (around 11007) ===\n")
cat(lines[11005:11025], sep = "\n")
