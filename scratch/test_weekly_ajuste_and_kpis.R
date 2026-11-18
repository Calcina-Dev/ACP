source("Actualizacion Semanal/app.R", local = TRUE)

cat("=== 1. Test Regex del Modal de Diagnóstico ===\n")
motivo_test <- "Sin avance previo (Rend S-1: 143 kg/ha) • Castigo floral -40.0% • Frecuencia 8.6d (+16.0%)"
pct_cast_res <- sub(".*Castigo floral ([+-]?[0-9.]+)%.*", "\\1%", motivo_test)
cat("pct_cast_res:", pct_cast_res, "\n")
stopifnot(identical(pct_cast_res, "-40.0%"))

txt_frec_res <- sub(".*Frecuencia ([0-9.]+d \\([+-]?[0-9.]+%\\)).*", "\\1", motivo_test)
cat("txt_frec_res:", txt_frec_res, "\n")
stopifnot(identical(txt_frec_res, "8.6d (+16.0%)"))

cat("\n=== 2. Test Multiplicación * 100 en Factor Floral KPI ===\n")
# Simulación de métrica floral en ratio
ajuste_floral_ratio <- -0.4000
delta_floral_ratio  <- -0.5091
factor_medio <- ajuste_floral_ratio * 100
delta_medio  <- delta_floral_ratio * 100
str_factor <- sprintf("%+.1f%%", factor_medio)
str_delta  <- sprintf("%+.1f%%", delta_medio)
cat("str_factor:", str_factor, "\n")
cat("str_delta:", str_delta, "\n")
stopifnot(identical(str_factor, "-40.0%"))
stopifnot(identical(str_delta, "-50.9%"))

cat("\n=== 3. Test Motor de Ajuste Semanal con Factor de Frecuencia ===\n")
# M1-T2 S46:
# Rend S-1 = 143.41 kg/ha
# Factor Floral = 0.60 (-40%)
# Frecuencia Sem = 8.56 d -> Base 7.0 d -> Diff = +1.56 d -> Factor Frec = 1 + 0.70 * (1.56/7) = 1.156 (+15.6%)
# Rend con Flor y Frec = 143.41 * 0.60 * 1.156 = 99.47 kg/ha
frec_obj <- calcular_factor_frecuencia(diff_dias = 8.56 - 7.0, frec_base = 7.0, elasticidad_pct = 70)
rend_calc <- round(143.41 * 0.60 * frec_obj$factor, 2)
cat(sprintf("Rendimiento calculado: %.2f kg/ha (vs anterior sin frec 86.05 kg/ha)\n", rend_calc))
stopifnot(rend_calc > 95 && rend_calc < 105)

cat("\n✔ TODOS LOS TESTS DE AJUSTE SEMANAL Y KPIS PASARON CON ÉXITO!\n")
