source("Actualizacion Semanal/app.R", local = TRUE)

df_c_hist <- readRDS("D:/ACP/Actualizacion Semanal/historico_conteo.rds")
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

cat("M1-T2 in S46 details:\n")
glimpse(df_factores_flor %>% filter(as.character(Módulo) == "1", as.character(Turno) == "2"))
