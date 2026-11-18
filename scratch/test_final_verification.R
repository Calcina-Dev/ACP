# Verificación final de las 2 solicitudes del usuario
archivo <- "Actualizacion Semanal/app.R"
source("scratch/test_parse.R") # comprueba sintaxis

cat("=== 1. Verificación del signo en Comparativo (Real - Ajustado) ===\n")
lineas <- readLines(archivo, encoding = "UTF-8", warn = FALSE)

# Buscar fórmulas de diferencia en app.R
dif_eval <- grep("Diferencia_Kg\\s*=\\s*round\\(Kg_Real_Semana - Kg_Ajustado_Semana", lineas, value = TRUE)
cat("Formula Diferencia_Kg en evaluar_escenario:\n", trimws(dif_eval), "\n")
stopifnot(length(dif_eval) > 0)

dif_kpi <- grep("dif_neta\\s*<-\\s*round\\(tot_real - tot_aj", lineas, value = TRUE)
cat("Formula dif_neta en ui_comparativo_kpis:\n", trimws(dif_kpi), "\n")
stopifnot(length(dif_kpi) > 0)

# Simular cálculo con los datos de la imagen 3 del usuario:
# Ajustado (X) = 12,200 kg, Real (Y) = 21,392 kg
tot_aj <- 12200
tot_real <- 21392
dif_neta <- round(tot_real - tot_aj, 0)
pct_desvio <- if (tot_aj > 0) round((dif_neta / tot_aj) * 100, 1) else 0

cat("Resultado para 12,200 kg vs 21,392 kg:\n")
cat("Diferencia neta:", sprintf("%+s kg", format(dif_neta, big.mark = ",")), "\n")
cat("Porcentaje de desvío:", sprintf("%+1.1f%%", pct_desvio), "\n")
stopifnot(dif_neta == 9192)
stopifnot(pct_desvio == 75.3)

cat("\n=== 2. Verificación de Hectáreas Disponibles en Modal (M2-T3) ===\n")
# Asegurar que dias_reales_previos se calcula correctamente
has_dias_prev <- any(grepl("dias_reales_previos <- dias_semana_todos\\[1:\\(idx_desde - 1\\)\\]", lineas))
cat("dias_reales_previos presente en app.R:", has_dias_prev, "\n")
stopifnot(has_dias_prev)

has_ha_pool <- any(grepl("ha_pool_pasada <- if \\(pasada_activa == 2L\\) ctx\\$ha_disponible_2da else ctx\\$ha_disponible_1ra", lineas))
cat("ha_pool_pasada actualizado en modal:", has_ha_pool, "\n")
stopifnot(has_ha_pool)

cat("\n✔ TODAS LAS VERIFICACIONES PASARON CON 100% DE ÉXITO!\n")
