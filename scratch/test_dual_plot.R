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

df_c_s <- m2_c %>% mutate(Semana_Proy = Semana + shift_weeks)

p_dual <- plot_ly() %>%
  add_lines(
    data = m2_y, x = ~Semana, y = ~Rend,
    name = "Cosecha Real (Kg/ha)",
    line = list(color = "#007AFF", width = 3),
    marker = list(size = 6, color = "#007AFF")
  ) %>%
  add_lines(
    data = df_c_s, x = ~Semana_Proy, y = ~Flor,
    name = sprintf("Conteo Floral (+%.1f sem)", shift_weeks),
    yaxis = "y2",
    line = list(color = "#AF52DE", width = 2.5, dash = "dash"),
    marker = list(size = 6, color = "#AF52DE")
  ) %>%
  layout(
    xaxis = list(title = "Semana"),
    yaxis = list(title = "Kg/ha", titlefont = list(color = "#007AFF"), tickfont = list(color = "#007AFF")),
    yaxis2 = list(
      title = "Órganos / Planta",
      titlefont = list(color = "#AF52DE"),
      tickfont = list(color = "#AF52DE"),
      overlaying = "y",
      side = "right"
    )
  )

cat("Dual axis plot created successfully!\n")
