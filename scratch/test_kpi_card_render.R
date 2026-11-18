source("Actualizacion Semanal/app.R", local = TRUE)

sem_sel <- 46
camp_sel <- "2026"
mod_sel <- "1"
tur_sel <- "2"

# Test data retrieval for the card
df_w <- df_hist_oficial_wide %>% filter(!is.na(Codigo_Valv), nzchar(as.character(Codigo_Valv)))
dias_cols <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
dias_map  <- setNames(1:6, dias_cols)

# S-1 Yield
sub_s1 <- df_w %>%
  filter(
    as.character(Módulo_Norm) == mod_sel,
    as.character(Turno_Norm) == tur_sel,
    as.numeric(Semana) == (sem_sel - 1)
  )
kg_s1 <- sum(sub_s1 %>% select(any_of(paste0("Kg_", dias_cols))) %>% as.matrix(), na.rm = TRUE)
area_t <- sum(sub_s1$Área, na.rm = TRUE)
rend_s1 <- if (area_t > 0 && kg_s1 > 0) round(kg_s1 / area_t, 1) else 0

cat(sprintf("S-1 Yield: %.1f kg/ha (Kg: %.1f, Area: %.2f ha)\n", rend_s1, kg_s1, area_t))

# Floral Factor
df_c_hist <- readRDS("D:/ACP/Actualizacion Semanal/historico_conteo.rds")
df_mad_actual <- cargar_maduracion_disco()
df_mad_t_actual <- cargar_maduracion_turno_disco()
sens_floral_pct <- cargar_sensibilidad_floral()

df_fac <- calcular_factor_tendencia_floral(
  df_conteo        = df_c_hist,
  campana_sel      = camp_sel,
  sem_cosecha      = sem_sel,
  df_maduracion    = df_mad_actual,
  sensibilidad_pct = sens_floral_pct,
  df_mad_turno     = df_mad_t_actual
) %>% filter(as.character(Módulo) == mod_sel, as.character(Turno) == tur_sel)

fac_flor <- if (nrow(df_fac) > 0) df_fac$Factor_Floral[1] else 1.0
delta_flor_pct <- if (nrow(df_fac) > 0) df_fac$Ajuste_Floral_Pct[1] * 100 else 0

cat(sprintf("Floral Factor: %.4f (Delta: %+.1f%%)\n", fac_flor, delta_flor_pct))

# Frequency Factor
events_list <- list()
for (d in dias_cols) {
  kg_col <- paste0("Kg_", d)
  if (kg_col %in% names(df_w)) {
    sub_d <- df_w %>%
      filter(!is.na(.data[[kg_col]]), .data[[kg_col]] > 0) %>%
      select(Semana, Módulo = Módulo_Norm, Turno = Turno_Norm, Variedad, Codigo_Valv, Kg = all_of(kg_col)) %>%
      mutate(Dia_Num = dias_map[d], Dia_Abs = (as.numeric(Semana) - 1) * 7 + dias_map[d])
    events_list[[d]] <- sub_d
  }
}
all_ev <- bind_rows(events_list) %>% arrange(Codigo_Valv, Dia_Abs)
all_ev <- all_ev %>%
  group_by(Codigo_Valv) %>%
  mutate(diff_days = Dia_Abs - lag(Dia_Abs)) %>%
  ungroup() %>%
  filter(!is.na(diff_days), diff_days >= 1, diff_days <= 35)

frec_t <- all_ev %>%
  filter(as.character(Módulo) == mod_sel, as.character(Turno) == tur_sel, as.numeric(Semana) == sem_sel) %>%
  summarise(
    Frecuencia = round(weighted.mean(diff_days, w = Kg), 1),
    Total_Kg   = sum(Kg, na.rm = TRUE)
  )

frec_val <- if (nrow(frec_t) > 0 && !is.na(frec_t$Frecuencia[1])) frec_t$Frecuencia[1] else 7.0
info_frec <- calcular_factor_frecuencia(frec_val - 7.0, 7.0, 70)

cat(sprintf("Frequency: %.1f d -> Factor: %.4f (Delta: %+.1f%%)\n", frec_val, info_frec$factor, info_frec$delta_pct))

rend_ajustado <- round(rend_s1 * fac_flor * info_frec$factor, 1)
delta_tot_pct <- round((rend_ajustado / rend_s1 - 1) * 100, 1)
cat(sprintf("Final Applied Yield: %.1f kg/ha (Total delta: %+.1f%%)\n", rend_ajustado, delta_tot_pct))
