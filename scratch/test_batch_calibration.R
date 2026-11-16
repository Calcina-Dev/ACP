suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(lubridate)
})

load("G:/Unidades compartidas/Compartido Proyecciones/3.- Arándano/datos_maestros_cosecha_2026.RData")

app_code <- readLines("Actualizacion Semanal/app.R", encoding = "UTF-8")
eval(parse(text = paste(app_code[1084:1191], collapse = "\n")))

camp_sel <- 2026
cat("Testing batch turn optimization with pre-filtering...\n")

t0 <- Sys.time()

# Pre-filter for campaign 2026
df_cos_camp <- df_cosecha %>%
  filter(as.character(Campaña) == as.character(camp_sel) | Campaña == suppressWarnings(as.numeric(camp_sel))) %>%
  mutate(
    Módulo_clean   = trimws(as.character(Módulo)),
    Turno_clean    = trimws(as.character(Turno)),
    Variedad_clean = toupper(trimws(as.character(Variedad)))
  )

df_con_camp <- if (exists("df_conteo_historico") && is.data.frame(df_conteo_historico)) {
  df_conteo_historico %>%
    filter(as.character(Campaña) == as.character(camp_sel) | Campaña == suppressWarnings(as.numeric(camp_sel))) %>%
    mutate(
      Módulo_clean   = trimws(as.character(Módulo)),
      Turno_clean    = trimws(as.character(Turno)),
      Variedad_clean = toupper(trimws(as.character(Variedad)))
    )
} else tibble()

combos <- df_cos_camp %>%
  filter(!is.na(Módulo_clean), nzchar(Módulo_clean),
         !is.na(Variedad_clean), nzchar(Variedad_clean)) %>%
  distinct(Módulo = Módulo_clean, Variedad = Variedad_clean)

cat(sprintf("Evaluating %d active combinations with pre-filtered data...\n", nrow(combos)))

calib_rows <- list()
for (i in seq_len(nrow(combos))) {
  m <- combos$Módulo[i]
  v <- combos$Variedad[i]
  
  sub_cos <- df_cos_camp %>% filter(Módulo_clean == m, Variedad_clean == v)
  sub_con <- if (nrow(df_con_camp) > 0) df_con_camp %>% filter(Módulo_clean == m, Variedad_clean == v) else tibble()
  
  turnos <- sort(unique(c(sub_cos$Turno_clean, sub_con$Turno_clean)))
  turnos <- turnos[!is.na(turnos) & nzchar(turnos)]
  num_o <- suppressWarnings(as.numeric(turnos))
  if (all(!is.na(num_o))) turnos <- turnos[order(num_o)]
  
  for (t in turnos) {
    opt <- optimizar_dias_maduracion(sub_cos, sub_con, campana = camp_sel, modulo = m, turno = t, variedad = v)
    if (!is.na(opt$r)) {
      calib_rows[[length(calib_rows) + 1]] <- tibble(
        Módulo = m,
        Turno = t,
        Variedad = v,
        Dias_Maduracion = opt$opt_dias,
        Correlacion = round(opt$r, 3),
        Fecha_Calibracion = as.character(Sys.Date())
      )
    }
  }
}

t1 <- Sys.time()
df_calib_total <- bind_rows(calib_rows)
cat(sprintf("\nDone in %.2f seconds!\nTotal turns calibrated: %d\n", as.numeric(difftime(t1, t0, units = "secs")), nrow(df_calib_total)))
print(as.data.frame(head(df_calib_total, 25)))
cat(sprintf("\nMean correlation: %+.3f\n", mean(df_calib_total$Correlacion, na.rm = TRUE)))
