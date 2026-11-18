source("Actualizacion Semanal/app.R", local = TRUE)

sem_sel <- 46
camp_sel <- "2026"
mod_sel <- "1"
tur_sel <- "2"

# 1. Base S-1
s1_rend <- df_cosecha %>%
  filter(
    as.character(Campaña) == camp_sel,
    as.numeric(Semana) == (sem_sel - 1),
    as.character(Módulo) == mod_sel,
    as.character(Turno) == tur_sel,
    !isTRUE(Es_Testblock)
  ) %>%
  summarise(
    Total_Kg = sum(`Kg Total`, na.rm = TRUE),
    Area     = mean(as.numeric(Área), na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  mutate(Rend_S1 = ifelse(Area > 0, Total_Kg / Area, 0))

cat("S-1 for M1-T2 in S45:\n")
print(s1_rend)

# 2. Frecuencia in S46
dias_cols <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
dias_map  <- setNames(1:6, dias_cols)
df_w <- df_hist_oficial_wide %>% filter(!is.na(Codigo_Valv), nzchar(as.character(Codigo_Valv)))
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

cat("Frecuencia in S46 for M1-T2:\n")
print(frec_t)
