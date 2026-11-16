library(dplyr)
library(plotly)
load("Actualizacion Semanal/datos_maestros_cosecha_2026.RData")

m2_c <- df_conteo_historico %>%
  filter(as.character(Campaña) == "2026", Módulo == "2", Turno == "1") %>%
  mutate(Semana = as.numeric(Semana)) %>%
  group_by(Semana) %>%
  summarise(
    Flor = sum(Boton + Flor, na.rm = TRUE) / pmax(1, sum(MuestraF, na.rm = TRUE)),
    .groups = "drop"
  )

m2_y <- df_cosecha %>%
  filter(Campaña == 2026, Módulo == "2", Turno == "1", !coalesce(Es_Testblock, FALSE)) %>%
  mutate(Semana = as.numeric(Semana)) %>%
  group_by(Semana) %>%
  summarise(
    Rend = sum(`Kg Total`, na.rm = TRUE) / pmax(0.1, first(na.omit(Área))),
    .groups = "drop"
  )

dias_slider <- 77
shift_weeks <- dias_slider / 7

# Normalizado
max_r <- max(m2_y$Rend, na.rm = TRUE)
max_f <- max(m2_c$Flor, na.rm = TRUE)

df_y_norm <- m2_y %>% mutate(Val_Norm = Rend / max_r * 100)
df_c_norm <- m2_c %>% mutate(Semana_Proyectada = Semana + shift_weeks, Val_Norm = Flor / max_f * 100)

p <- plot_ly() %>%
  add_lines(
    data = df_y_norm, x = ~Semana, y = ~Val_Norm,
    name = "Cosecha Real (Kg/ha)",
    line = list(color = "#007AFF", width = 3),
    marker = list(size = 6, color = "#007AFF")
  ) %>%
  add_lines(
    data = df_c_norm, x = ~Semana_Proyectada, y = ~Val_Norm,
    name = sprintf("Conteo Floral (+%.1f sem / %d d)", shift_weeks, dias_slider),
    line = list(color = "#AF52DE", width = 2.5, dash = "dash"),
    marker = list(size = 6, color = "#AF52DE")
  )

cat("Plot built successfully!\n")
