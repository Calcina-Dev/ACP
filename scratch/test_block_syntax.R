# Test syntax of the new block
library(shiny)
library(dplyr)
library(plotly)

test_expr <- quote({
  rv_maduracion <- reactiveVal(cargar_maduracion_disco())
  rv_maduracion_turno <- reactiveVal(cargar_maduracion_turno_disco())
  rv_sensibilidad_floral <- reactiveVal(cargar_sensibilidad_floral())
  
  calibrador_state <- reactiveValues(
    modulo      = NULL,
    variedad    = NULL,
    turno       = NULL,
    dias        = 45L,
    dias_mod    = 45L,
    turnos_disp = character()
  )
  
  combinaciones_maduracion_activas <- reactive({
    u <- universo_base()
    combos <- if (!is.null(u) && nrow(u) > 0 && all(c("Módulo", "Variedad") %in% names(u))) {
      u %>%
        filter(!is.na(Módulo), nzchar(trimws(as.character(Módulo))),
               !is.na(Variedad), nzchar(trimws(as.character(Variedad)))) %>%
        distinct(Módulo = trimws(as.character(Módulo)), Variedad = trimws(as.character(Variedad)))
    } else {
      tibble(Módulo = character(), Variedad = character())
    }
    combos %>% arrange(suppressWarnings(as.numeric(Módulo)), Módulo, Variedad)
  })
})

cat("Syntax test successful!\n")
