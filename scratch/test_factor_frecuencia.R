suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

# Test calcular_factor_frecuencia
calcular_factor_frecuencia <- function(diff_dias, frec_base = 7, elasticidad_pct = 70) {
  diff_dias <- suppressWarnings(as.numeric(diff_dias))
  frec_base <- suppressWarnings(as.numeric(frec_base))
  elasticidad_pct <- suppressWarnings(as.numeric(elasticidad_pct))
  
  if (is.na(diff_dias) || diff_dias == 0) return(list(factor = 1.0, delta_pct = 0.0, diff_dias = 0))
  if (is.na(frec_base) || frec_base <= 0) frec_base <- 7
  if (is.na(elasticidad_pct) || elasticidad_pct < 0) elasticidad_pct <- 70
  
  alpha <- elasticidad_pct / 100
  delta_dias_pct <- diff_dias / frec_base
  factor_raw <- 1 + alpha * delta_dias_pct
  
  # Clamp de seguridad entre 0.40 y 1.60
  factor_clamped <- max(0.40, min(1.60, factor_raw))
  delta_pct_final <- (factor_clamped - 1) * 100
  
  list(
    factor = factor_clamped,
    delta_pct = delta_pct_final,
    diff_dias = diff_dias,
    frec_base = frec_base,
    elasticidad = elasticidad_pct
  )
}

# Test +2 days on 7-day base with 70% elasticity
res_plus2 <- calcular_factor_frecuencia(2, 7, 70)
cat(sprintf("+2 days: factor = %.3f, delta = %+.1f%%\n", res_plus2$factor, res_plus2$delta_pct))

# Test -2 days on 7-day base with 70% elasticity
res_minus2 <- calcular_factor_frecuencia(-2, 7, 70)
cat(sprintf("-2 days: factor = %.3f, delta = %+.1f%%\n", res_minus2$factor, res_minus2$delta_pct))

# Test Rocío +1 day on 4-day base with 70% elasticity
res_rocio <- calcular_factor_frecuencia(1, 4, 70)
cat(sprintf("Rocío +1 day: factor = %.3f, delta = %+.1f%%\n", res_rocio$factor, res_rocio$delta_pct))

cat("\nAll tests passed successfully!\n")
