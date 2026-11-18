source("Actualizacion Semanal/app.R", local = TRUE)

df_c_hist <- readRDS("D:/ACP/Actualizacion Semanal/historico_conteo.rds")
df_mad_actual <- cargar_maduracion_disco()
df_mad_t_actual <- cargar_maduracion_turno_disco()
sens_floral_pct <- cargar_sensibilidad_floral()

cat("sens_floral_pct:", sens_floral_pct, "\n")
cat("df_mad_actual for M1:\n")
print(df_mad_actual %>% filter(as.character(Módulo) == "1"))
cat("df_mad_t_actual for M1-T2:\n")
print(df_mad_t_actual %>% filter(as.character(Módulo) == "1", as.character(Turno) == "2"))

df_factores_flor <- calcular_factor_tendencia_floral(
  df_conteo        = df_c_hist,
  campana_sel      = 2026,
  sem_cosecha      = 46,
  df_maduracion    = df_mad_actual,
  sensibilidad_pct = sens_floral_pct,
  df_mad_turno     = df_mad_t_actual
)

cat("\ndf_factores_flor for M1-T2 in S46:\n")
print(df_factores_flor %>% filter(as.character(Módulo) == "1", as.character(Turno) == "2"))

cat("\ndf_factores_flor for ALL M1 in S46:\n")
print(df_factores_flor %>% filter(as.character(Módulo) == "1"))
