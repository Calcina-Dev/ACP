suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(tibble)
})

# Simular datos
ha_pasada <- 6.40
kgha_actual <- 801.74
kg_pasada <- 5131.14
kgha_proy <- 662.70
dias <- c("Jueves", "Viernes")
kg_orig <- c(2565.57, 2565.57)
tot_orig <- sum(kg_orig)

props_pct <- if (tot_orig > 0) round((kg_orig / tot_orig) * 100, 1) else rep(round(100 / length(dias), 1), length(dias))
props_pct[length(dias)] <- round(100 - sum(props_pct[seq_len(length(dias)-1)]), 1)

cat("Props pct iniciales:", paste(props_pct, collapse=", "), "\n")

# Construir UI completa de prueba
cuerpo_ui <- tagList(
  tags$input(type = "hidden", id = "modal_kgha_hidden_ha", value = sprintf("%.4f", ha_pasada)),
  tags$input(type = "hidden", id = "modal_kgha_hidden_kg_orig", value = sprintf("%.4f", kg_pasada)),
  
  # Métricas
  div(
    style = "display: grid; grid-template-columns: repeat(3, 1fr); gap: 10px; margin-bottom: 18px;",
    div(tags$div("Área asignada"), tags$div(sprintf("%.2f ha", ha_pasada))),
    div(tags$div("Rendimiento actual"), tags$div(sprintf("%.2f kg/ha", kgha_actual))),
    div(tags$div("Rendimiento proyección"), tags$div(sprintf("%.2f kg/ha", kgha_proy)))
  ),
  
  # Input kgha
  numericInput("modal_kgha_input_valor", label = "Nuevo rendimiento objetivo (kg/ha)", value = round(kgha_actual, 2)),
  
  # Card de distribución editable
  div(
    style = "background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 9px; padding: 12px 14px; margin-top: 6px;",
    div(
      style = "display: flex; align-items: baseline; justify-content: space-between; margin-bottom: 10px;",
      div(
        style = "display: flex; align-items: center; gap: 8px;",
        tags$span(style = "font-size: 12px; font-weight: 600; color: #334155;", "Distribución por día (editable en %)"),
        tags$span(id = "modal_kgha_suma_pct_badge", style = "font-size: 11px; font-weight: 600; padding: 1px 7px; border-radius: 6px; background: #ecfdf5; color: #059669; border: 1px solid #a7f3d0;", "100.0%")
      ),
      tags$div(
        style = "font-size: 13.5px; font-weight: 700; color: #0f172a; font-variant-numeric: tabular-nums;",
        "Total: ", tags$span(id = "modal_kgha_tot_kg", sprintf("%s kg", format(round(kg_pasada, 2), big.mark = ","))),
        " (", tags$span(id = "modal_kgha_tot_diff", "+0.0 kg"), ")"
      )
    ),
    div(
      style = "display: flex; flex-direction: column; gap: 6px;",
      lapply(seq_along(dias), function(i) {
        d <- dias[i]
        d_orig <- kg_orig[i]
        p_ini <- props_pct[i]
        
        div(
          class = "modal-kgha-day-row",
          `data-day` = d,
          `data-orig-kg` = sprintf("%.4f", d_orig),
          style = "display: flex; align-items: center; justify-content: space-between; padding: 6px 10px; background: #ffffff; border: 1px solid #e2e8f0; border-radius: 6px;",
          div(
            style = "display: flex; align-items: center; gap: 8px;",
            tags$span(style = "font-size: 12px; font-weight: 600; color: #1e293b; min-width: 65px;", d),
            div(
              class = "modal-kgha-pct-container",
              style = "display: inline-flex; align-items: center; background: #ffffff; border: 1px solid #d4d4d8; border-radius: 6px; padding: 2px 6px; box-shadow: 0 1px 2px rgba(0,0,0,0.03);",
              tags$input(
                type = "number",
                class = "modal-kgha-pct-input",
                `data-day` = d,
                min = "0",
                max = "100",
                step = "0.5",
                value = sprintf("%.1f", p_ini),
                style = "width: 48px; border: none; background: transparent; font-size: 11.5px; font-weight: 600; color: #09090b; text-align: right; outline: none; padding: 0; font-variant-numeric: tabular-nums;"
              ),
              tags$span(style = "font-size: 10.5px; font-weight: 600; color: #71717a; margin-left: 2px;", "%")
            )
          ),
          div(
            style = "display: flex; align-items: center; gap: 10px; font-variant-numeric: tabular-nums;",
            tags$span(style = "font-size: 11.5px; color: #94a3b8; text-decoration: line-through;", sprintf("%s kg", format(round(d_orig, 1), big.mark = ","))),
            tags$span(class = "modal-kgha-day-kilos", `data-day` = d, style = "font-size: 12.5px; font-weight: 600; color: #0f172a;", sprintf("%s kg", format(round(d_orig, 2), big.mark = ","))),
            tags$span(class = "modal-kgha-day-diff", `data-day` = d, style = "font-size: 11px; font-weight: 500; min-width: 55px; text-align: right; color: #16a34a;", "+0.0 kg")
          )
        )
      })
    )
  )
)

cat("UI construida exitosamente. Expresiones válidas!\n")
