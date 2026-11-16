suppressPackageStartupMessages({
  library(dplyr)
  library(plotly)
})

# Let's test the plot code with mock data or actual data
df_test <- tibble(
  Sector = c("8", "8", "8"),
  Módulo = c("6", "6", "6"),
  Turno = c("7", "6", "5"),
  Variedad = c("MAGICA", "MAGICA", "MAGICA"),
  Pasada = c("1ª Pasada", "1ª Pasada", "1ª Pasada"),
  Pct_Termino = c(100, 100, 100),
  Ha_Ajustada = c(0, 0, 0),
  Kg_Ajustado = c(0, 0, 0),
  Kg_Real_SAP = c(10501.3, 8538.9, 8299.5),
  Diferencia_Kg = c(-10501.3, -8538.9, -8299.5),
  Pct_Desvio = c(-100, -100, -100),
  Turno_Label = c("M6-T7 (MAGICA)", "M6-T6 (MAGICA)", "M6-T5 (MAGICA)")
)

df_plot <- head(df_test, 30)
p <- plot_ly(df_plot)

metrica <- "kilos"
enfoque <- "rango_escenario"

p <- p %>%
  add_bars(x = ~Turno_Label, y = ~Kg_Ajustado, name = "Kg Ajustado (Rango)", marker = list(color = "#4F46E5", opacity = 0.88),
           hovertemplate = "<b>%{x}</b><br>Kg Ajustado: %{y:,.1f} kg<extra></extra>") %>%
  add_bars(x = ~Turno_Label, y = ~Kg_Real_SAP, name = "Kg Real SAP (Mismo Rango)", marker = list(color = "#10B981", opacity = 0.88),
           hovertemplate = "<b>%{x}</b><br>Kg Real SAP: %{y:,.1f} kg<br>Diferencia: %{customdata[0]:+,.1f} kg (%{customdata[1]:+1.1f}%)<extra></extra>",
           customdata = ~mapply(list, Diferencia_Kg, Pct_Desvio, SIMPLIFY = FALSE)) %>%
  layout(barmode = "group")

p <- p %>% layout(
  font = list(family = "-apple-system, BlinkMacSystemFont, Segoe UI, Roboto, sans-serif", size = 11, color = "#1C1C1E"),
  xaxis = list(title = "", tickangle = -38, showgrid = FALSE),
  yaxis = list(title = if (metrica == "kilos") "Kilogramos" else if (metrica == "dif_kg") "Diferencia Kilos" else "%", gridcolor = "rgba(0,0,0,0.05)", zeroline = TRUE),
  margin = list(t = 20, b = 80, l = 50, r = 20),
  legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.12)
)

cat("Plot created successfully! Class:", class(p), "\n")
