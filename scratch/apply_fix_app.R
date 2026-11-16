app_path <- "D:/ACP/Actualizacion Semanal/app.R"
lines <- readLines(app_path, warn = FALSE, encoding = "UTF-8")

# Find the function definition
idx_func <- which(grepl("^desplazar_dia_2da_pasada <- function", lines))[1]
cat("Found at line:", idx_func, "\n")

# Check lines around it
cat("Current lines:\n")
cat(paste(lines[idx_func:(idx_func + 18)], collapse = "\n"), "\n")

target_text <- paste(lines[idx_func:(idx_func + 18)], collapse = "\n")

new_func_lines <- c(
  "desplazar_dia_2da_pasada <- function(dia_base_vec, frec_vec, dias_nombres, dia_inicio_idx, dia_inicio_nom, dia_fin_idx = 6L, dia_fin_nom = \"Sábado\") {",
  "  idx_base <- match(dia_base_vec, dias_nombres)",
  "  # Regla Agronómica y Operativa ACP:",
  "  # Para variedades de ciclo corto (ej. Rocío), la 2ª pasada intra-semanal descansa 3 días:",
  "  # - Cosecha de Lunes -> Jueves (idx 1 + 3 = 4)",
  "  # - Cosecha de Martes -> Viernes (idx 2 + 3 = 5)",
  "  # Cosechas de Miércoles en adelante NO tienen 2ª pasada intra-semanal.",
  "  desp <- 3L",
  "  idx_target <- idx_base + desp",
  "  ",
  "  # REGLA OPERATIVA ACP:",
  "  # Para la 2ª pasada NUNCA se patean kilos ni se adelanta la cosecha sin el descanso requerido.",
  "  # Si el día resultante cae fuera del rango de ajuste [dia_inicio_idx, dia_fin_idx] (ej. Viernes cuando ajuste termina en Jueves),",
  "  # la 2ª pasada NO se realiza en esa semana (retorna NA_character_).",
  "  es_valido <- !is.na(idx_base) & idx_base %in% c(1L, 2L) &",
  "               !is.na(idx_target) & idx_target >= dia_inicio_idx & idx_target <= dia_fin_idx",
  "  ",
  "  ifelse(es_valido, dias_nombres[idx_target], NA_character_)",
  "}"
)

# Replace lines idx_func to idx_func+18 with new_func_lines
lines_updated <- c(
  lines[1:(idx_func - 1)],
  new_func_lines,
  lines[(idx_func + 19):length(lines)]
)

writeLines(lines_updated, app_path, sep = "\r\n", useBytes = TRUE)
cat("Updated app.R successfully!\n")

# Verify parse
p <- parse(app_path)
cat("Parsed successfully! Total expressions:", length(p), "\n")
