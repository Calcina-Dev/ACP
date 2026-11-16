library(shiny)
library(plotly)
library(dplyr)

test_fun <- function(input, output, session) {
  # Verify syntax of all expressions
  observeEvent(input$abrir_calibrador_mad, {
    showModal(modalDialog(
      title = div(
        style = "display: flex; align-items: center; justify-content: space-between; width: 100%; gap: 10px;",
        div(
          style = "display: flex; align-items: center; gap: 9px;",
          div(
            style = "width: 32px; height: 32px; border-radius: 8px; background: #F2F2F7; color: #1C1C1E; display: flex; align-items: center; justify-content: center; font-size: 14px;",
            icon("chart-line")
          ),
          div(
            tags$div(style = "font-size: 14.5px; font-weight: 600; color: #1C1C1E; letter-spacing: -0.015em;", "Calibración de Maduración"),
            tags$div(style = "font-size: 11px; color: #8E8E93;", "Alineación de curva de cosecha real vs conteo floral por turno")
          )
        ),
        actionButton(
          "btn_volver_a_modal_maduracion",
          label = tagList(icon("arrow-left"), "Volver"),
          style = "font-size: 11.5px; font-weight: 500; padding: 4px 11px; border-radius: 7px; background: #FFFFFF; color: #1C1C1E; border: 1px solid #D1D1D6; box-shadow: 0 1px 2px rgba(0,0,0,0.03); cursor: pointer;"
        )
      ),
      size = "l",
      easyClose = TRUE,
      footer = div(
        actionButton("btn_guardar_calibracion_turno", "Guardar Turno", style = "background: #1C1C1E; color: #FFFFFF;")
      )
    ))
  })
}

cat("Scratch syntax verified!\n")
