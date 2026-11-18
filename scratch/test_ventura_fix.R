suppressPackageStartupMessages(library(dplyr))
load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")

source("scratch/test_mod7_eval.R", local = TRUE)

# Test M7 T4 VENTURA with optimizar_dias_maduracion_CON_VARIEDAD
res <- optimizar_dias_maduracion_CON_VARIEDAD(
  df_cosecha, df_conteo_historico,
  campana = 2026, modulo = "7", turno = "4", variedad = "VENTURA"
)

cat("Resultado para M7 T4 VENTURA con filtro de variedad:\n")
print(res)

stopifnot(res$opt_dias == 49L)
stopifnot(res$r > 0.98)
cat("\n✔ Test PASSED! 49 dias, r =", res$r, "\n")
