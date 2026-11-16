app_path <- "D:/ACP/Actualizacion Semanal/app.R"
lines <- readLines(app_path, warn = FALSE, encoding = "UTF-8")

# 1. Update cargar_ediciones_disco
idx_ced <- which(grepl("^cargar_ediciones_disco <- function", lines))[1]
cat("cargar_ediciones_disco at:", idx_ced, "\n")
idx_ced_mut <- idx_ced + which(grepl("Ha_Asignada_1ra = if", lines[idx_ced:(idx_ced + 30)]))[1] - 1
cat("Ha_Asignada_1ra line:", idx_ced_mut, "\n")
cat("Lines around it:\n")
cat(paste(lines[(idx_ced_mut - 2):(idx_ced_mut + 6)], collapse = "\n"), "\n")

# 2. Update pasada_sugerida
idx_psug <- which(grepl("^\\s*pasada_sugerida <- if \\(nrow\\(valvs_celda\\)", lines))[1]
cat("pasada_sugerida at:", idx_psug, "\n")
cat("Lines around it:\n")
cat(paste(lines[idx_psug:(idx_psug + 10)], collapse = "\n"), "\n")

# 3. Update ui_modal_calculo_vivo pasada_activa
idx_pact1 <- which(grepl("^\\s*pasada_activa <- if \\(!is\\.null\\(input\\$modal_input_pasada\\)", lines))
cat("pasada_activa lines:", idx_pact1, "\n")
for (ip in idx_pact1) {
  cat("--- Line", ip, "---\n")
  cat(paste(lines[(ip - 2):(ip + 4)], collapse = "\n"), "\n")
}

# 4. Update nueva_edicion num_pasada_sel
idx_npas <- which(grepl("^\\s*num_pasada_sel <- pasada_activa", lines))[1]
cat("num_pasada_sel at:", idx_npas, "\n")

# 5. Update ha_ajustadas_ed in tabla_detalle_turnos
idx_haed <- which(grepl("^\\s*ha_ajustadas_ed <- ed_filtradas %>%", lines))[1]
cat("ha_ajustadas_ed at:", idx_haed, "\n")
cat("Lines around it:\n")
cat(paste(lines[idx_haed:(idx_haed + 30)], collapse = "\n"), "\n")
