library(dplyr)
load("Actualizacion Semanal/datos_maestros_cosecha_2026.RData")

m2_c <- df_conteo_historico %>%
  filter(as.character(Campaña) == "2026", Módulo %in% c("2", "02"), Turno %in% c("1", "01")) %>%
  mutate(Semana = as.numeric(Semana)) %>%
  group_by(Semana) %>%
  summarise(
    Flor = sum(Boton + Flor, na.rm = TRUE) / pmax(1, sum(MuestraF, na.rm = TRUE)),
    .groups = "drop"
  )
cat("m2_c rows:", nrow(m2_c), "Semanas:", paste(range(m2_c$Semana), collapse = "-"), "\n")

m2_y <- df_cosecha %>%
  filter(Campaña == 2026, Módulo %in% c("2", "02"), Turno %in% c("1", "01"), !coalesce(Es_Testblock, FALSE)) %>%
  mutate(Semana = as.numeric(Semana)) %>%
  group_by(Semana) %>%
  summarise(
    Rend = sum(`Kg Total`, na.rm = TRUE) / pmax(0.1, first(na.omit(Área))),
    .groups = "drop"
  )
cat("m2_y rows:", nrow(m2_y), "Semanas:", paste(range(m2_y$Semana), collapse = "-"), "\n")

# Test shift 11 weeks (77 days)
shift_weeks <- 11
df_merged <- m2_y %>%
  mutate(Semana_Flor = Semana - shift_weeks) %>%
  inner_join(m2_c, by = c("Semana_Flor" = "Semana"))
cat("Merged rows with shift 11:", nrow(df_merged), "\n")
print(df_merged)
val_df <- df_merged %>% filter(!is.na(Rend), !is.na(Flor))
cat("Correlation with shift 11 weeks (77 days):", cor(val_df$Rend, val_df$Flor), "\n")
