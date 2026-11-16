suppressPackageStartupMessages({
  library(dplyr)
  library(plotly)
})

# Let's test creating a sample plotly bar chart
df_test <- tibble(
  Turno_Label = c("M6-T1", "M6-T4", "M3-T3", "M10-T14"),
  Kg_Ajustado = c(1895.7, 5988.0, 4728.0, 1259.0),
  Kg_Real_SAP = c(0, 4412.0, 984.0, 0),
  Diferencia_Kg = c(1895.7, 1576.0, 3744.0, 1259.0),
  Pct_Desvio = c(NA, 35.7, 380.5, NA),
  Rend_Ajustado = c(581.5, 965.8, 274.9, 740.6),
  Rend_Real = c(NA, 711.6, 57.2, NA)
)

p <- plot_ly(df_test) %>%
  add_bars(
    x = ~Turno_Label, y = ~Kg_Ajustado, name = "Ajustado",
    marker = list(color = "#4F46E5", opacity = 0.88, line = list(color = "#4338CA", width = 1))
  ) %>%
  add_bars(
    x = ~Turno_Label, y = ~Kg_Real_SAP, name = "Real SAP",
    marker = list(color = "#10B981", opacity = 0.88, line = list(color = "#059669", width = 1))
  ) %>%
  layout(
    barmode = "group",
    font = list(family = "-apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif"),
    xaxis = list(title = "", tickangle = -35),
    yaxis = list(title = "Kilogramos"),
    margin = list(t = 20, b = 60, l = 50, r = 20),
    legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.1)
  )

cat("Plotly test successful!\n")
