suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(shiny)
})

# Let's inspect how the KPI card would look
sem_sel <- 47
factor_val <- -0.151
delta_raw <- -0.302
dias_mad <- 69
sem_flor <- 37
es_calib <- TRUE
damping <- 50

# Factor formatted
factor_pct <- round(factor_val * 100, 1)
factor_str <- sprintf("%+.1f%%", factor_pct)
delta_str <- sprintf("%+.1f%%", round(delta_raw * 100, 1))

tipo_txt <- if (factor_pct > 0) "Premio Floral" else if (factor_pct < 0) "Castigo Floral" else "Sin Ajuste Floral"
color_badge <- if (factor_pct > 0) "#15803D" else if (factor_pct < 0) "#B91C1C" else "#6B7280"
bg_badge <- if (factor_pct > 0) "rgba(22, 163, 74, 0.08)" else if (factor_pct < 0) "rgba(220, 38, 38, 0.08)" else "rgba(107, 114, 128, 0.08)"
border_badge <- if (factor_pct > 0) "rgba(22, 163, 74, 0.20)" else if (factor_pct < 0) "rgba(220, 38, 38, 0.20)" else "rgba(107, 114, 128, 0.20)"

card <- tags$div(
  style = "display: flex; flex-direction: column; gap: 8px;",
  tags$div(
    style = "display: flex; justify-content: space-between; align-items: center;",
    tags$div(
      style = "display: flex; align-items: center; gap: 6px;",
      tags$span(style = "width: 7px; height: 7px; border-radius: 50%; background: #5856D6; display: inline-block;"),
      tags$span(style = "font-size: 12px; font-weight: 600; color: #1C1C1E; letter-spacing: -0.01em;", "Factor Floral S47")
    ),
    tags$span(
      style = sprintf("background: %s; color: %s; border: 1px solid %s; font-size: 10px; font-weight: 600; padding: 1.5px 7px; border-radius: 5px; font-variant-numeric: tabular-nums;", bg_badge, color_badge, border_badge),
      tipo_txt
    )
  ),
  tags$div(
    style = "display: flex; align-items: baseline; justify-content: space-between; margin-top: 2px;",
    tags$div(
      tags$span(style = "font-size: 24px; font-weight: 700; color: #1C1C1E; letter-spacing: -0.02em; font-variant-numeric: tabular-nums;", factor_str),
      tags$span(style = "font-size: 11px; color: #8E8E93; margin-left: 5px;", "impacto en cosecha")
    ),
    tags$span(
      style = "font-size: 11px; font-weight: 500; color: #636366; font-variant-numeric: tabular-nums;",
      sprintf("Pendiente: %s", delta_str)
    )
  ),
  tags$div(
    style = "background: #F8F9FA; border-radius: 8px; padding: 7px 10px; border: 1px solid rgba(0,0,0,0.04); display: flex; justify-content: space-between; align-items: center; font-size: 11px; color: #636366;",
    tags$div(
      tags$span(style = "color: #8E8E93;", "Origen: "),
      tags$strong(style = "color: #1C1C1E; font-variant-numeric: tabular-nums;", sprintf("Semana %d", sem_flor)),
      tags$span(style = "color: #8E8E93; margin-left: 4px;", sprintf("(hace %d días)", dias_mad))
    ),
    if (es_calib) tags$span(style = "font-size: 9.5px; font-weight: 600; color: #5856D6; background: rgba(88,86,214,0.08); padding: 1px 5px; border-radius: 4px;", "Calibrado") else NULL
  )
)

cat("Card HTML generated successfully.\n")
