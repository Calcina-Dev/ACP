suppressPackageStartupMessages({
  library(shiny)
  library(htmltools)
})

# Probar generación de UI de la fila de día
d <- "Jueves"
pct_val <- 50.0
d_orig <- 2565.57
d_nuevo <- 2565.57
d_diff <- 0.0

row_ui <- div(
  class = "modal-kgha-day-row",
  `data-day` = d,
  `data-orig-kg` = sprintf("%.2f", d_orig),
  `data-orig-ha` = "3.20",
  style = "display: flex; align-items: center; justify-content: space-between; padding: 6px 10px; background: #ffffff; border: 1px solid #e2e8f0; border-radius: 6px;",
  div(
    style = "display: flex; align-items: center; gap: 8px;",
    tags$span(style = "font-size: 12px; font-weight: 600; color: #1e293b; min-width: 65px;", d),
    div(
      style = "display: inline-flex; align-items: center; background: #f4f4f5; border: 1px solid #e4e4e7; border-radius: 6px; padding: 1px 6px;",
      tags$input(
        type = "number",
        class = "modal-kgha-pct-input",
        `data-day` = d,
        min = "0",
        max = "100",
        step = "0.5",
        value = sprintf("%.1f", pct_val),
        style = "width: 48px; border: none; background: transparent; font-size: 11.5px; font-weight: 600; color: #09090b; text-align: right; outline: none; padding: 2px 0; font-variant-numeric: tabular-nums;"
      ),
      tags$span(style = "font-size: 10.5px; font-weight: 600; color: #71717a; margin-left: 2px;", "%")
    )
  ),
  div(
    style = "display: flex; align-items: center; gap: 10px; font-variant-numeric: tabular-nums;",
    tags$span(style = "font-size: 11.5px; color: #94a3b8; text-decoration: line-through;", sprintf("%s kg", format(round(d_orig, 1), big.mark = ","))),
    tags$span(class = "modal-kgha-day-kilos", `data-day` = d, style = "font-size: 12.5px; font-weight: 600; color: #0f172a;", sprintf("%s kg", format(d_nuevo, big.mark = ","))),
    tags$span(
      class = "modal-kgha-day-diff",
      `data-day` = d,
      style = sprintf("font-size: 11px; font-weight: 500; min-width: 55px; text-align: right; color: %s;", if (d_diff >= 0) "#16a34a" else "#dc2626"),
      sprintf("%+0.1f kg", d_diff)
    )
  )
)

cat("Rendered HTML:\n")
cat(as.character(row_ui), "\n")
