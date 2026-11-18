source("Actualizacion Semanal/app.R", local = TRUE)

df_c_hist <- if (file.exists("historico_conteo.rds")) readRDS("historico_conteo.rds") else tibble()
df_mad_actual <- cargar_maduracion_disco()
df_mad_t_actual <- cargar_maduracion_turno_disco()
sens_floral_pct <- cargar_sensibilidad_floral()

df_factores_flor <- calcular_factor_tendencia_floral(
  df_conteo        = df_c_hist,
  campana_sel      = 2026,
  sem_cosecha      = 46,
  df_maduracion    = df_mad_actual,
  sensibilidad_pct = sens_floral_pct,
  df_mad_turno     = df_mad_t_actual
)

cat("Columns in df_factores_flor:\n")
print(colnames(df_factores_flor))
cat("Head of df_factores_flor:\n")
print(head(df_factores_flor))
