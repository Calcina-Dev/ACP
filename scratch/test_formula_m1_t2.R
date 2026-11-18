source("Actualizacion Semanal/app.R", local = TRUE)

rend_base <- 143.41
factor_flor <- 0.60
diff_dias <- 8.6 - 7.0
info_frec <- calcular_factor_frecuencia(diff_dias, 7, 70)
rend_ajustado <- round(rend_base * factor_flor * info_frec$factor, 2)

cat(sprintf("Rend S-1: %.2f\n", rend_base))
cat(sprintf("Factor Flor: %.2f (Delta: %+.1f%%)\n", factor_flor, (factor_flor - 1) * 100))
cat(sprintf("Factor Frec: %.4f (Delta: %+.1f%%)\n", info_frec$factor, info_frec$delta_pct))
cat(sprintf("Rend Ajustado: %.2f kg/ha\n", rend_ajustado))
cat(sprintf("Total Kg para 9.32 ha: %.1f kg\n", rend_ajustado * 9.32))
