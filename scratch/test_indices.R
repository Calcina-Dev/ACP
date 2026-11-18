lines <- readLines("Actualizacion Semanal/app.R", encoding = "UTF-8")

# 1. Verificar banner_cambio_dia
idx_start_banner <- grep('# Banner de retroalimentación de cambio de día', lines)
idx_end_banner   <- grep('cuerpo_calculo <- if \\(!is.null\\(modo\\) && modo == "kgha"\\)', lines) - 1

cat(sprintf("Banner range: %d to %d\n", idx_start_banner, idx_end_banner))
cat("First line of banner:\n", lines[idx_start_banner], "\n")
cat("Last line of banner:\n", lines[idx_end_banner], "\n")

# 2. Verificar Rejilla
idx_cards <- grep('# Rejilla de tarjetas por Módulo y Variedad', lines)
cat(sprintf("Rejilla cards found at line: %d\n", idx_cards))

# 3. Verificar renderUI ui_simulacion insertion point
idx_obs_elast <- grep('guardar_elasticidad_frecuencia_disco\\(val_clamped\\)', lines)
cat(sprintf("Elasticidad observer save at line: %d\n", idx_obs_elast))
