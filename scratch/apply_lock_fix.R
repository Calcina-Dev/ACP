file_path <- "D:/ACP/Actualizacion Semanal/app.R"
lines <- readLines(file_path, encoding = "UTF-8", warn = FALSE)

idx <- grep("for \\(dia_col in dias_nombres\\) \\{", lines)[1]
cat("Found for (dia_col in dias_nombres) at line:", idx, "\n")

target_replacement <- c(
  "        # REGLA SIMULACIÓN Y AJUSTE DE ESCENARIOS:",
  "        # Solo se bloquean por SAP los días PREVIOS al inicio del rango (ej. Lunes si inicia Martes).",
  "        # Los días dentro del rango [dia_inicio_idx, dia_fin_idx] son los que se están simulando/ajustando,",
  "        # por lo que NO deben ser bloqueados por la recepción histórica de SAP.",
  "        dias_previos_bloq <- if (dia_inicio_idx > 1) dias_nombres[1:(dia_inicio_idx - 1)] else character(0)",
  "        for (dia_col in dias_previos_bloq) {"
)

lines <- c(lines[1:(idx - 1)], target_replacement, lines[(idx + 1):length(lines)])

# Also add updateRadioButtons(session, "filtro_modo_detalle", selected = "ajustado") right after showNotification
idx_notif <- grep('Se completaron autom.ticamente %d turnos proyectados', lines)[1]
cat("Found showNotification at line:", idx_notif, "\n")
idx_end_notif <- idx_notif
while (!grepl("\\)$", trimws(lines[idx_end_notif]))) {
  idx_end_notif <- idx_end_notif + 1
}
cat("showNotification ends at line:", idx_end_notif, "\n")

lines <- c(
  lines[1:idx_end_notif],
  '      updateRadioButtons(session, "filtro_modo_detalle", selected = "ajustado")',
  lines[(idx_end_notif + 1):length(lines)]
)

writeLines(lines, file_path, useBytes = TRUE)
cat("Successfully updated app.R. New line count:", length(lines), "\n")
