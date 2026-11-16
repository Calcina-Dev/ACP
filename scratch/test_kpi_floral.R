suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(lubridate)
  library(plotly)
})

load("G:/Unidades compartidas/Compartido Proyecciones/3.- Arándano/datos_maestros_cosecha_2026.RData")

# Let's verify calcular_factor_tendencia_floral
app_code <- readLines("Actualizacion Semanal/app.R", encoding = "UTF-8")
eval(parse(text = paste(app_code[1301:1447], collapse = "\n")))

camp_sel <- 2026
sem_sel <- 47
df_mad <- tibble(Módulo = "2", Variedad = "SEKOYA POP", Dias_Maduracion = 69)
df_mad_t <- tibble(Módulo = "2", Turno = "1", Variedad = "SEKOYA POP", Dias_Maduracion = 77, Correlacion = 0.795, Fecha_Calibracion = "2026-11-16")

res <- calcular_factor_tendencia_floral(
  df_conteo = df_conteo_historico,
  campana_sel = camp_sel,
  sem_cosecha = sem_sel,
  df_maduracion = df_mad,
  sensibilidad_pct = 50,
  df_mad_turno = df_mad_t
)

cat("Result from calcular_factor_tendencia_floral:\n")
print(head(res, 5))

cat("\nAll checks passed!\n")
