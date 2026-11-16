library(dplyr)
load("Actualizacion Semanal/datos_maestros_cosecha_2026.RData")

# Extract expressions before shinyApp
lines <- readLines("Actualizacion Semanal/app.R", encoding = "UTF-8", warn = FALSE)
idx_app <- grep("shinyApp\\(", lines)
cat("shinyApp is at line:", idx_app, "\n")

# Eval top-level helper functions up to server
eval(parse(text = paste(lines[1:(grep("^server <- function", lines)[1] - 1)], collapse = "\n")))

cat("1. Testing cargar_maduracion_turno_disco...\n")
df_t <- cargar_maduracion_turno_disco()
cat("Rows in df_t:", nrow(df_t), "\n")

cat("2. Testing optimizar_dias_maduracion for Modulo 2 Turno 1...\n")
opt <- optimizar_dias_maduracion(df_cosecha, df_conteo_historico, campana = 2026, modulo = "2", turno = "1", variedad = "SEKOYA POP")
print(opt)
stopifnot(!is.na(opt$r), opt$opt_dias == 77)

cat("3. Testing guardar_maduracion_turno_disco with custom override...\n")
df_custom <- tibble(
  Módulo = "2", Turno = "1", Variedad = "SEKOYA POP", Dias_Maduracion = 77L, Correlacion = opt$r, Fecha_Calibracion = as.character(Sys.Date())
)
guardar_maduracion_turno_disco(df_custom)
df_reloaded <- cargar_maduracion_turno_disco()
cat("Reloaded rows:", nrow(df_reloaded), "Dias M2-T1:", df_reloaded$Dias_Maduracion[df_reloaded$Módulo == "2" & df_reloaded$Turno == "1"], "\n")
stopifnot(df_reloaded$Dias_Maduracion[df_reloaded$Módulo == "2" & df_reloaded$Turno == "1"] == 77)

cat("4. Testing calcular_factor_tendencia_floral with turn override vs module fallback...\n")
df_mod_general <- tibble(Módulo = "2", Variedad = "SEKOYA POP", Dias_Maduracion = 69L)
factores <- calcular_factor_tendencia_floral(
  df_conteo = df_conteo_historico,
  campana_sel = 2026,
  sem_cosecha = 46,
  df_maduracion = df_mod_general,
  sensibilidad_pct = 50,
  df_mad_turno = df_reloaded
)

print(factores %>% filter(Módulo == "2") %>% select(Módulo, Turno, Variedad, Dias_Maduracion, Es_Personalizado, Sem_Flor, Factor_Floral))

m2_t1 <- factores %>% filter(Módulo == "2", Turno == "1")
m2_t2 <- factores %>% filter(Módulo == "2", Turno == "2")

cat("Turno 1 Dias:", m2_t1$Dias_Maduracion[1], "Es_Personalizado:", m2_t1$Es_Personalizado[1], "\n")
cat("Turno 2 Dias:", m2_t2$Dias_Maduracion[1], "Es_Personalizado:", m2_t2$Es_Personalizado[1], "\n")

stopifnot(m2_t1$Dias_Maduracion[1] == 77)
stopifnot(m2_t1$Es_Personalizado[1] == TRUE)
stopifnot(m2_t2$Dias_Maduracion[1] == 69)
stopifnot(m2_t2$Es_Personalizado[1] == FALSE)

cat("\nALL E2E ASSERTIONS PASSED WITH FLYING COLORS!\n")
