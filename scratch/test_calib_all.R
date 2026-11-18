suppressPackageStartupMessages(library(dplyr))
load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")

camp_sel <- "2026"
df_cos    <- df_cosecha
df_conteo <- df_conteo_historico

# We recreate the exact code of ejecutar_calibracion_todos_turnos
combos <- df_cosecha %>%
  filter(as.character(Campaña) == camp_sel, !coalesce(Es_Testblock, FALSE)) %>%
  distinct(Módulo, Variedad) %>%
  mutate(Módulo = trimws(as.character(Módulo)), Variedad = toupper(trimws(as.character(Variedad))))

df_cos_camp <- df_cos %>%
  filter(as.character(Campaña) == as.character(camp_sel) | Campaña == suppressWarnings(as.numeric(camp_sel))) %>%
  mutate(
    Módulo_clean   = trimws(as.character(Módulo)),
    Turno_clean    = trimws(as.character(Turno)),
    Variedad_clean = toupper(trimws(as.character(Variedad)))
  )

df_con_camp <- if (nrow(df_conteo) > 0) {
  df_conteo %>%
    filter(as.character(Campaña) == as.character(camp_sel) | Campaña == suppressWarnings(as.numeric(camp_sel))) %>%
    mutate(
      Módulo_clean   = trimws(as.character(Módulo)),
      Turno_clean    = trimws(as.character(Turno)),
      Variedad_clean = toupper(trimws(as.character(Variedad)))
    )
} else tibble()

# Load optimizar_dias_maduracion from app.R
source("scratch/test_mod7_eval.R", local = TRUE)

calib_rows <- list()
for (i in seq_len(nrow(combos))) {
  m <- combos$Módulo[i]
  v <- combos$Variedad[i]
  
  sub_cos <- df_cos_camp %>% filter(Módulo_clean == m, Variedad_clean == toupper(v))
  sub_con <- if (nrow(df_con_camp) > 0) df_con_camp %>% filter(Módulo_clean == m, Variedad_clean == toupper(v)) else tibble()
  
  turnos <- sort(unique(c(sub_cos$Turno_clean, sub_con$Turno_clean)))
  turnos <- turnos[!is.na(turnos) & nzchar(turnos)]
  num_o <- suppressWarnings(as.numeric(turnos))
  if (all(!is.na(num_o))) turnos <- turnos[order(num_o)]
  
  for (t in turnos) {
    opt <- optimizar_dias_maduracion_ORIG(sub_cos, sub_con, campana = camp_sel, modulo = m, turno = t, variedad = v)
    if (!is.na(opt$r)) {
      calib_rows[[length(calib_rows) + 1]] <- tibble(
        Módulo            = m,
        Turno             = t,
        Variedad          = v,
        Dias_Maduracion   = opt$opt_dias,
        Correlacion       = round(opt$r, 3),
        Fecha_Calibracion = as.character(Sys.Date())
      )
    }
  }
}

df_calib_total <- bind_rows(calib_rows)
cat("df_calib_total nrow:", nrow(df_calib_total), "\n")
print(head(as.data.frame(df_calib_total), 15))
