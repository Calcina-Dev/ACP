suppressPackageStartupMessages({
  library(plotly)
  library(dplyr)
  library(tibble)
})

p <- plot_ly() %>%
  add_trace(x = 1:5, y = c(100, 200, 150, 300, 250), name = "Rendimiento", type = "scatter", mode = "lines+markers", line = list(color = "#007AFF")) %>%
  add_trace(x = 1:5, y = c(20, 45, 60, 40, 15), name = "Conteo Floral", yaxis = "y2", type = "scatter", mode = "lines+markers", line = list(color = "#5856D6", dash = "dash")) %>%
  add_trace(x = 1:5, y = c(7, 6, 8, 7, 5), name = "Frecuencia", yaxis = "y3", type = "scatter", mode = "lines+markers", line = list(color = "#D97706", dash = "dot")) %>%
  layout(
    xaxis = list(title = "Semana"),
    yaxis = list(title = "Rendimiento (kg/ha)"),
    yaxis2 = list(title = "Carga Floral", overlaying = "y", side = "right", showgrid = FALSE),
    yaxis3 = list(title = "Frecuencia (días)", overlaying = "y", side = "right", position = 0.95, anchor = "free", showgrid = FALSE),
    margin = list(r = 75)
  )

cat("Plotly 3-axis test successful!\n")
