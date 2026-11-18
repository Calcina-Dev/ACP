source("Actualizacion Semanal/app.R", local = TRUE)

cat("--- Probando calcular_factor_frecuencia ---\n")
# Caso 1: Mover Miércoles a Viernes (+2 días, base 7d, elast 70%)
res1 <- calcular_factor_frecuencia(diff_dias = 2, frec_base = 7, elasticidad_pct = 70)
cat(sprintf("Caso 1 (+2d): Factor = %.4f (Delta: %+.1f%%)\n", res1$factor, res1$delta_pct))
stopifnot(abs(res1$factor - 1.20) < 1e-4)
stopifnot(abs(res1$delta_pct - 20.0) < 1e-4)

# Caso 2: Mover Viernes a Miércoles (-2 días, base 7d, elast 70%)
res2 <- calcular_factor_frecuencia(diff_dias = -2, frec_base = 7, elasticidad_pct = 70)
cat(sprintf("Caso 2 (-2d): Factor = %.4f (Delta: %+.1f%%)\n", res2$factor, res2$delta_pct))
stopifnot(abs(res2$factor - 0.80) < 1e-4)
stopifnot(abs(res2$delta_pct - (-20.0)) < 1e-4)

# Caso 3: Mismo día (0 días)
res3 <- calcular_factor_frecuencia(diff_dias = 0, frec_base = 7, elasticidad_pct = 70)
cat(sprintf("Caso 3 (0d): Factor = %.4f (Delta: %+.1f%%)\n", res3$factor, res3$delta_pct))
stopifnot(res3$factor == 1.0)
stopifnot(res3$delta_pct == 0.0)

# Caso 4: Variedad Rocío (base 4d, +1 día, elast 70%)
res4 <- calcular_factor_frecuencia(diff_dias = 1, frec_base = 4, elasticidad_pct = 70)
cat(sprintf("Caso 4 (Rocío +1d): Factor = %.4f (Delta: %+.1f%%)\n", res4$factor, res4$delta_pct))
stopifnot(abs(res4$factor - 1.175) < 1e-4)

# Caso 5: Clamping extremo (+10 días)
res5 <- calcular_factor_frecuencia(diff_dias = 10, frec_base = 7, elasticidad_pct = 100)
cat(sprintf("Caso 5 (Clamping max): Factor = %.4f\n", res5$factor))
stopifnot(res5$factor <= 1.60)

# Caso 6: Clamping extremo (-10 días)
res6 <- calcular_factor_frecuencia(diff_dias = -10, frec_base = 7, elasticidad_pct = 100)
cat(sprintf("Caso 6 (Clamping min): Factor = %.4f\n", res6$factor))
stopifnot(res6$factor >= 0.40)

cat("\n✔ TODOS LOS TESTS DE FACTOR DE FRECUENCIA PASARON CON ÉXITO!\n")
