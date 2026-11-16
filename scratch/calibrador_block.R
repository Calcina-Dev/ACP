# Test calibrador block
library(shiny)
library(plotly)
library(dplyr)

test_fun <- function(input, output, session) {
  # MODAL DE CALIBRACIÓN VISUAL DE MADURACIÓN POR TURNO
  observeEvent(input$abrir_calibrador_mad, {
    req(input$abrir_calibrador_mad$modulo, input$abrir_calibrador_mad$variedad)
    m <- as.character(input$abrir_calibrador_mad$modulo)
    v <- as.character(input$abrir_calibrador_mad$variedad)
    
    turnos_disp <- c()
    if (exists("df_cosecha") && is.data.frame(df_cosecha) && nrow(df_cosecha) > 0) {
      t_c <- df_cosecha %>%
        filter(trimws(as.character(Módulo)) == trimws(as.character(m)),
               toupper(trimws(as.character(Variedad))) == toupper(trimws(as.character(v)))) %>%
        pull(Turno) %>% as.character() %>% trimws() %>% unique()
      turnos_disp <- c(turnos_disp, t_c)
    }
    if (exists("df_conteo_historico") && is.data.frame(df_conteo_historico) && nrow(df_conteo_historico) > 0) {
      t_h <- df_conteo_historico %>%
        filter(trimws(as.character(Módulo)) == trimws(as.character(m)),
               toupper(trimws(as.character(Variedad))) == toupper(trimws(as.character(v)))) %>%
        pull(Turno) %>% as.character() %>% trimws() %>% unique()
      turnos_disp <- c(turnos_disp, t_h)
    }
    turnos_disp <- sort(unique(turnos_disp[!is.na(turnos_disp) & nzchar(turnos_disp)]))
    num_order <- suppressWarnings(as.numeric(turnos_disp))
    if (all(!is.na(num_order))) turnos_disp <- turnos_disp[order(num_order)]
    if (length(turnos_disp) == 0) turnos_disp <- c("1")
    
    t_sel <- turnos_disp[1]
    
    df_mad_mod <- rv_maduracion()
    idx_m <- which(df_mad_mod$Módulo == m & df_mad_mod$Variedad == v)
    dias_mod <- if (length(idx_m) > 0 && !is.na(df_mad_mod$Dias_Maduracion[idx_m[1]])) as.integer(df_mad_mod$Dias_Maduracion[idx_m[1]]) else 45L
    
    df_mad_t <- rv_maduracion_turno()
    idx_t <- which(df_mad_t$Módulo == m & df_mad_t$Turno == t_sel & df_mad_t$Variedad == v)
    dias_turno <- if (length(idx_t) > 0 && !is.na(df_mad_t$Dias_Maduracion[idx_t[1]])) as.integer(df_mad_t$Dias_Maduracion[idx_t[1]]) else dias_mod
    
    calibrador_state$modulo      <- m
    calibrador_state$variedad    <- v
    calibrador_state$turno       <- t_sel
    calibrador_state$dias        <- dias_turno
    calibrador_state$dias_mod    <- dias_mod
    calibrador_state$turnos_disp <- turnos_disp
    
    showModal(
      modalDialog(
        title = div(
          style = "display: flex; align-items: center; justify-content: space-between; width: 100%; flex-wrap: wrap; gap: 8px;",
          div(
            style = "display: flex; align-items: center; gap: 10px;",
            div(
              style = "width: 36px; height: 36px; border-radius: 10px; background: rgba(175, 82, 222, 0.12); color: #AF52DE; display: flex; align-items: center; justify-content: center; font-size: 16px;",
              icon("sliders-h")
            ),
            div(
              tags$div(style = "font-size: 15px; font-weight: 700; color: #1C1C1E; line-height: 1.2;", sprintf("Calibración de Maduración — Módulo %s (%s)", m, v)),
              tags$div(style = "font-size: 11.5px; font-weight: 400; color: #8E8E93; margin-top: 2px;", "Alinea la curva de rendimiento real con el conteo floral desplazado")
            )
          ),
          actionButton(
            "btn_volver_a_modal_maduracion",
            label = tagList(icon("arrow-left"), "Volver a Módulos"),
            style = "font-size: 11.5px; font-weight: 600; padding: 5px 12px; border-radius: 8px; background: #F2F2F7; color: #3A3A3C; border: 1px solid rgba(0,0,0,0.08); cursor: pointer;"
          )
        ),
        size = "l",
        easyClose = TRUE,
        div(
          style = "padding: 4px 2px;",
          div(
            style = "background: #F9F9FB; border: 1px solid rgba(0,0,0,0.06); border-radius: 12px; padding: 12px 16px; margin-bottom: 12px; display: flex; flex-direction: column; gap: 10px;",
            div(
              style = "display: flex; align-items: center; justify-content: space-between; flex-wrap: wrap; gap: 12px;",
              div(
                style = "display: flex; align-items: center; gap: 10px; flex-wrap: wrap;",
                tags$label(style = "font-size: 12px; font-weight: 700; color: #1C1C1E; margin: 0;", "Turno:"),
                selectInput(
                  "calibrar_mad_turno_sel", label = NULL,
                  choices = turnos_disp, selected = t_sel, width = "100px"
                ),
                uiOutput("ui_calibrar_badge_status", inline = TRUE)
              ),
              div(
                style = "display: flex; align-items: center; gap: 10px; flex-wrap: wrap;",
                tags$label(style = "font-size: 11.5px; font-weight: 600; color: #636366; margin: 0;", "Escala:"),
                radioButtons(
                  "calibrar_escala_modo", label = NULL,
                  choices = c("Normalizado (0-100%)" = "norm", "Doble Eje (Kg/ha vs Carga)" = "dual"),
                  selected = "norm", inline = TRUE
                ),
                actionButton(
                  "btn_auto_ajustar_calce",
                  label = tagList(icon("wand-magic-sparkles"), "Ajustar Automáticamente"),
                  title = "Calcula la máxima correlación de Pearson y sincroniza los picos",
                  style = "font-size: 11.5px; font-weight: 600; padding: 5px 12px; border-radius: 8px; background: rgba(16, 185, 129, 0.12); color: #059669; border: 1px solid rgba(16, 185, 129, 0.32); cursor: pointer;"
                )
              )
            ),
            div(
              style = "background: #FFFFFF; border: 1px solid rgba(0,0,0,0.06); border-radius: 8px; padding: 10px 14px;",
              sliderInput(
                "calibrar_slider_dias",
                label = div(
                  style = "display: flex; justify-content: space-between; align-items: center; width: 100%; font-size: 12px; font-weight: 700; color: #1C1C1E;",
                  span(icon("clock"), " Días de Maduración (Desfase Flor → Cosecha):"),
                  uiOutput("ui_calibrar_desfase_semanas", inline = TRUE)
                ),
                min = 30, max = 130, value = dias_turno, step = 1, width = "100%"
              )
            )
          ),
          div(
            style = "background: #FFFFFF; border: 1px solid rgba(0,0,0,0.06); border-radius: 12px; padding: 10px; margin-bottom: 12px;",
            plotlyOutput("grafico_calce_maduracion", height = "360px")
          ),
          uiOutput("ui_calibrar_kpis_calce")
        ),
        footer = div(
          style = "display: flex; align-items: center; justify-content: space-between; width: 100%; flex-wrap: wrap; gap: 8px;",
          div(
            style = "display: flex; align-items: center; gap: 8px; flex-wrap: wrap;",
            actionButton(
              "btn_reset_turno_a_modulo",
              label = tagList(icon("rotate-left"), "Restablecer al Módulo"),
              title = "Elimina la personalización del turno y hereda los días del módulo",
              style = "font-size: 11.5px; font-weight: 600; padding: 6px 12px; border-radius: 8px; background: #F2F2F7; color: #48484A; border: 1px solid rgba(0,0,0,0.08); cursor: pointer;"
            ),
            actionButton(
              "btn_aplicar_calce_a_todo_modulo",
              label = tagList(icon("check-double"), "Aplicar a todo el Módulo"),
              title = "Aplica este valor a la tarjeta general del módulo",
              style = "font-size: 11.5px; font-weight: 600; padding: 6px 12px; border-radius: 8px; background: rgba(0, 122, 255, 0.10); color: #007AFF; border: 1px solid rgba(0, 122, 255, 0.28); cursor: pointer;"
            )
          ),
          div(
            style = "display: flex; align-items: center; gap: 8px;",
            modalButton("Cerrar"),
            actionButton(
              "btn_guardar_calibracion_turno",
              label = tagList(icon("check"), "Guardar para este Turno"),
              style = "font-size: 12px; font-weight: 600; padding: 6px 16px; border-radius: 8px; background: #AF52DE; color: #FFFFFF; border: none; box-shadow: 0 2px 6px rgba(175, 82, 222, 0.28); cursor: pointer;"
            )
          )
        )
      )
    )
  })
  
  # Estado de personalización del turno
  output$ui_calibrar_badge_status <- renderUI({
    m <- calibrador_state$modulo
    v <- calibrador_state$variedad
    t <- input$calibrar_mad_turno_sel
    req(m, v, t)
    
    df_mad_t <- rv_maduracion_turno()
    idx_t <- which(df_mad_t$Módulo == m & df_mad_t$Turno == t & df_mad_t$Variedad == v)
    
    if (length(idx_t) > 0 && !is.na(df_mad_t$Dias_Maduracion[idx_t[1]])) {
      d_t <- as.integer(df_mad_t$Dias_Maduracion[idx_t[1]])
      tags$span(
        style = "font-size: 11px; font-weight: 700; color: #7B1FA2; background: rgba(175, 82, 222, 0.12); padding: 3px 9px; border-radius: 6px; display: inline-flex; align-items: center; gap: 4px;",
        icon("sliders-h", style = "font-size: 10px;"),
        sprintf("Personalizado: %d días", d_t)
      )
    } else {
      df_mad_mod <- rv_maduracion()
      idx_m <- which(df_mad_mod$Módulo == m & df_mad_mod$Variedad == v)
      d_m <- if (length(idx_m) > 0 && !is.na(df_mad_mod$Dias_Maduracion[idx_m[1]])) as.integer(df_mad_mod$Dias_Maduracion[idx_m[1]]) else 45L
      tags$span(
        style = "font-size: 11px; font-weight: 600; color: #636366; background: #E5E5EA; padding: 3px 9px; border-radius: 6px; display: inline-flex; align-items: center; gap: 4px;",
        icon("info-circle", style = "font-size: 10px;"),
        sprintf("Heredado del Módulo: %d días", d_m)
      )
    }
  })
  
  # Desfase en semanas
  output$ui_calibrar_desfase_semanas <- renderUI({
    d <- input$calibrar_slider_dias
    if (is.null(d)) d <- 45
    sw <- d / 7
    tags$span(
      style = "font-size: 11.5px; font-weight: 600; color: #AF52DE;",
      sprintf("Desfase: %.1f sem (~%d semanas)", sw, round(sw))
    )
  })
  
  # Gráfico Plotly de calce de curvas
  output$grafico_calce_maduracion <- renderPlotly({
    m <- calibrador_state$modulo
    v <- calibrador_state$variedad
    t <- input$calibrar_mad_turno_sel
    d <- input$calibrar_slider_dias
    modo_escala <- input$calibrar_escala_modo
    if (is.null(modo_escala)) modo_escala <- "norm"
    if (is.null(d) || is.na(d)) d <- 45
    
    req(m, t)
    
    camp_sel <- if (!is.null(input$filtro_campana)) input$filtro_campana else 2026
    camp_chr <- as.character(camp_sel)
    camp_num <- suppressWarnings(as.numeric(camp_chr))
    
    # 1. Cosecha real
    df_cos <- if (exists("df_cosecha") && is.data.frame(df_cosecha) && nrow(df_cosecha) > 0) df_cosecha else tibble()
    df_y <- df_cos %>%
      filter(
        (as.character(Campaña) == camp_chr | (!is.na(camp_num) & Campaña == camp_num)),
        trimws(as.character(Módulo)) == trimws(as.character(m)),
        trimws(as.character(Turno)) == trimws(as.character(t)),
        !coalesce(Es_Testblock, FALSE),
        `Kg Total` > 0
      ) %>%
      mutate(Semana_Num = suppressWarnings(as.numeric(Semana))) %>%
      filter(!is.na(Semana_Num)) %>%
      group_by(Semana_Num) %>%
      summarise(
        Kg_Total = sum(`Kg Total`, na.rm = TRUE),
        Area     = first(na.omit(Área)),
        Rend     = ifelse(!is.na(Area) && Area > 0, round(Kg_Total / Area, 1), round(Kg_Total, 1)),
        .groups  = "drop"
      ) %>%
      arrange(Semana_Num)
    
    if (nrow(df_y) == 0) {
      df_y <- df_cos %>%
        filter(
          (as.character(Campaña) == camp_chr | (!is.na(camp_num) & Campaña == camp_num)),
          trimws(as.character(Módulo)) == trimws(as.character(m)),
          !coalesce(Es_Testblock, FALSE),
          `Kg Total` > 0
        ) %>%
        mutate(Semana_Num = suppressWarnings(as.numeric(Semana))) %>%
        filter(!is.na(Semana_Num)) %>%
        group_by(Semana_Num) %>%
        summarise(
          Kg_Total = sum(`Kg Total`, na.rm = TRUE),
          Area     = sum(first(na.omit(Área)), na.rm = TRUE),
          Rend     = ifelse(!is.na(Area) && Area > 0, round(Kg_Total / Area, 1), round(Kg_Total, 1)),
          .groups  = "drop"
        ) %>%
        arrange(Semana_Num)
    }
    
    # 2. Conteo floral
    df_c_hist <- if (exists("df_conteo_historico") && is.data.frame(df_conteo_historico) && nrow(df_conteo_historico) > 0) df_conteo_historico else tibble()
    df_c <- df_c_hist %>%
      filter(
        (as.character(Campaña) == camp_chr | (!is.na(camp_num) & Campaña == camp_num)),
        trimws(as.character(Módulo)) == trimws(as.character(m)),
        trimws(as.character(Turno)) == trimws(as.character(t))
      ) %>%
      mutate(Semana_Num = suppressWarnings(as.numeric(Semana))) %>%
      filter(!is.na(Semana_Num)) %>%
      group_by(Semana_Num) %>%
      summarise(
        Organos = sum(Boton + Flor, na.rm = TRUE),
        Muestra = sum(MuestraF, na.rm = TRUE),
        Carga   = ifelse(Muestra > 0, round(Organos / Muestra, 1), round(mean(Total_Floral_Planta, na.rm = TRUE), 1)),
        .groups = "drop"
      ) %>%
      arrange(Semana_Num)
    
    if (nrow(df_c) == 0) {
      df_c <- df_c_hist %>%
        filter(
          (as.character(Campaña) == camp_chr | (!is.na(camp_num) & Campaña == camp_num)),
          trimws(as.character(Módulo)) == trimws(as.character(m))
        ) %>%
        mutate(Semana_Num = suppressWarnings(as.numeric(Semana))) %>%
        filter(!is.na(Semana_Num)) %>%
        group_by(Semana_Num) %>%
        summarise(
          Organos = sum(Boton + Flor, na.rm = TRUE),
          Muestra = sum(MuestraF, na.rm = TRUE),
          Carga   = ifelse(Muestra > 0, round(Organos / Muestra, 1), round(mean(Total_Floral_Planta, na.rm = TRUE), 1)),
          .groups = "drop"
        ) %>%
        arrange(Semana_Num)
    }
    
    if (nrow(df_y) == 0 && nrow(df_c) == 0) {
      return(
        plot_ly() %>%
          layout(
            title = list(text = "Sin datos de cosecha ni conteo disponibles para este turno.", font = list(size = 13, color = "#8E8E93")),
            xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)
          )
      )
    }
    
    shift_sem <- d / 7
    df_c_shifted <- df_c %>%
      mutate(
        Semana_Proy = Semana_Num + shift_sem,
        Semana_Origen = Semana_Num
      )
    
    if (modo_escala == "norm") {
      max_y <- if (nrow(df_y) > 0) max(df_y$Rend, na.rm = TRUE) else 1
      max_c <- if (nrow(df_c) > 0) max(df_c$Carga, na.rm = TRUE) else 1
      if (is.na(max_y) || max_y <= 0) max_y <- 1
      if (is.na(max_c) || max_c <= 0) max_c <- 1
      
      df_y_p <- df_y %>% mutate(Y_Plot = Rend / max_y * 100)
      df_c_p <- df_c_shifted %>% mutate(Y_Plot = Carga / max_c * 100)
      
      p <- plot_ly()
      if (nrow(df_y_p) > 0) {
        p <- p %>% add_lines(
          data = df_y_p, x = ~Semana_Num, y = ~Y_Plot,
          name = "Cosecha Real (Kg/ha)",
          line = list(color = "#007AFF", width = 3.2),
          marker = list(size = 7, color = "#007AFF"),
          hovertemplate = "<b>Cosecha Real: Sem %{x}</b><br>Rendimiento: %{customdata:.1f} kg/ha (%{y:.0f}%% del pico)<extra></extra>",
          customdata = ~Rend
        )
      }
      if (nrow(df_c_p) > 0) {
        p <- p %>% add_lines(
          data = df_c_p, x = ~Semana_Proy, y = ~Y_Plot,
          name = sprintf("Conteo Floral (+%.1f sem / %d d)", shift_sem, d),
          line = list(color = "#AF52DE", width = 2.8, dash = "dash"),
          marker = list(size = 7, color = "#AF52DE", symbol = "diamond"),
          hovertemplate = "<b>Floración: Sem %{customdata[0]} → Proy S%{x:.1f}</b><br>Carga: %{customdata[1]:.1f} órg/planta (%{y:.0f}%% del pico)<extra></extra>",
          customdata = ~mapply(list, Semana_Origen, Carga, SIMPLIFY = FALSE)
        )
      }
      
      p <- p %>% layout(
        font = list(family = "-apple-system, BlinkMacSystemFont, 'Inter', sans-serif"),
        xaxis = list(
          title = list(text = "Semana de Cosecha (Eje Temporal Sincronizado)", font = list(size = 11, color = "#1C1C1E")),
          tickfont = list(size = 10, color = "#636366"),
          gridcolor = "rgba(0,0,0,0.06)",
          dtick = 1
        ),
        yaxis = list(
          title = list(text = "Intensidad Normalizada (% respecto al pico)", font = list(size = 11, color = "#1C1C1E")),
          tickfont = list(size = 10, color = "#636366"),
          gridcolor = "rgba(0,0,0,0.06)",
          range = c(0, 110)
        ),
        legend = list(orientation = "h", x = 0.5, y = 1.12, xanchor = "center"),
        margin = list(t = 30, b = 40, l = 50, r = 20),
        plot_bgcolor = "#FFFFFF",
        paper_bgcolor = "#FFFFFF",
        hovermode = "x unified"
      )
      return(p)
    } else {
      # Doble eje Y
      p <- plot_ly()
      if (nrow(df_y) > 0) {
        p <- p %>% add_lines(
          data = df_y, x = ~Semana_Num, y = ~Rend,
          name = "Cosecha Real (Kg/ha)",
          line = list(color = "#007AFF", width = 3.2),
          marker = list(size = 7, color = "#007AFF"),
          hovertemplate = "<b>Cosecha Real: Sem %{x}</b><br>Rendimiento: %{y:.1f} kg/ha<extra></extra>"
        )
      }
      if (nrow(df_c_shifted) > 0) {
        p <- p %>% add_lines(
          data = df_c_shifted, x = ~Semana_Proy, y = ~Carga,
          name = sprintf("Conteo Floral (+%.1f sem / %d d)", shift_sem, d),
          yaxis = "y2",
          line = list(color = "#AF52DE", width = 2.8, dash = "dash"),
          marker = list(size = 7, color = "#AF52DE", symbol = "diamond"),
          hovertemplate = "<b>Floración: Sem %{customdata} → Proy S%{x:.1f}</b><br>Carga: %{y:.1f} órg/planta<extra></extra>",
          customdata = ~Semana_Origen
        )
      }
      
      p <- p %>% layout(
        font = list(family = "-apple-system, BlinkMacSystemFont, 'Inter', sans-serif"),
        xaxis = list(
          title = list(text = "Semana de Cosecha", font = list(size = 11, color = "#1C1C1E")),
          tickfont = list(size = 10, color = "#636366"),
          gridcolor = "rgba(0,0,0,0.06)",
          dtick = 1
        ),
        yaxis = list(
          title = list(text = "Rendimiento Cosecha (Kg/ha)", font = list(size = 11, color = "#007AFF")),
          tickfont = list(size = 10, color = "#007AFF"),
          gridcolor = "rgba(0,0,0,0.06)"
        ),
        yaxis2 = list(
          title = list(text = "Carga Floral (Órganos/Planta)", font = list(size = 11, color = "#AF52DE")),
          tickfont = list(size = 10, color = "#AF52DE"),
          overlaying = "y",
          side = "right",
          showgrid = FALSE
        ),
        legend = list(orientation = "h", x = 0.5, y = 1.12, xanchor = "center"),
        margin = list(t = 30, b = 40, l = 50, r = 50),
        plot_bgcolor = "#FFFFFF",
        paper_bgcolor = "#FFFFFF",
        hovermode = "x unified"
      )
      return(p)
    }
  })
  
  # Fila de KPIs de Calibración
  output$ui_calibrar_kpis_calce <- renderUI({
    m <- calibrador_state$modulo
    v <- calibrador_state$variedad
    t <- input$calibrar_mad_turno_sel
    d <- input$calibrar_slider_dias
    if (is.null(d) || is.na(d)) d <- 45
    req(m, t)
    
    camp_sel <- if (!is.null(input$filtro_campana)) input$filtro_campana else 2026
    df_cos    <- if (exists("df_cosecha") && is.data.frame(df_cosecha) && nrow(df_cosecha) > 0) df_cosecha else tibble()
    df_c_hist <- if (exists("df_conteo_historico") && is.data.frame(df_conteo_historico) && nrow(df_conteo_historico) > 0) df_conteo_historico else tibble()
    
    camp_chr <- as.character(camp_sel)
    camp_num <- suppressWarnings(as.numeric(camp_chr))
    
    df_y <- df_cos %>%
      filter(
        (as.character(Campaña) == camp_chr | (!is.na(camp_num) & Campaña == camp_num)),
        trimws(as.character(Módulo)) == trimws(as.character(m)),
        trimws(as.character(Turno)) == trimws(as.character(t)),
        !coalesce(Es_Testblock, FALSE),
        `Kg Total` > 0
      ) %>%
      mutate(Semana_Num = suppressWarnings(as.numeric(Semana))) %>%
      filter(!is.na(Semana_Num)) %>%
      group_by(Semana_Num) %>%
      summarise(
        Kg_Total = sum(`Kg Total`, na.rm = TRUE),
        Area     = first(na.omit(Área)),
        Rend     = ifelse(!is.na(Area) && Area > 0, Kg_Total / Area, Kg_Total),
        .groups  = "drop"
      )
    
    df_c <- df_c_hist %>%
      filter(
        (as.character(Campaña) == camp_chr | (!is.na(camp_num) & Campaña == camp_num)),
        trimws(as.character(Módulo)) == trimws(as.character(m)),
        trimws(as.character(Turno)) == trimws(as.character(t))
      ) %>%
      mutate(Semana_Num = suppressWarnings(as.numeric(Semana))) %>%
      filter(!is.na(Semana_Num)) %>%
      group_by(Semana_Num) %>%
      summarise(
        Organos = sum(Boton + Flor, na.rm = TRUE),
        Muestra = sum(MuestraF, na.rm = TRUE),
        Carga   = ifelse(Muestra > 0, Organos / Muestra, mean(Total_Floral_Planta, na.rm = TRUE)),
        .groups = "drop"
      )
    
    sw_int <- round(d / 7)
    df_join <- df_y %>%
      mutate(Sem_Flor = Semana_Num - sw_int) %>%
      inner_join(df_c, by = c("Sem_Flor" = "Semana_Num")) %>%
      filter(!is.na(Rend), !is.na(Carga))
    
    r_val <- if (nrow(df_join) >= 3) suppressWarnings(cor(df_join$Rend, df_join$Carga)) else NA_real_
    
    pico_y <- if (nrow(df_y) > 0) df_y$Semana_Num[which.max(df_y$Rend)] else NA_real_
    pico_c <- if (nrow(df_c) > 0) df_c$Semana_Num[which.max(df_c$Carga)] else NA_real_
    pico_c_proy <- if (!is.na(pico_c)) pico_c + round(d / 7) else NA_real_
    dif_picos <- if (!is.na(pico_y) && !is.na(pico_c_proy)) abs(pico_y - pico_c_proy) else NA_real_
    
    badge_cor_color <- if (!is.na(r_val) && r_val >= 0.65) {
      "#16A34A"
    } else if (!is.na(r_val) && r_val >= 0.35) {
      "#D97706"
    } else {
      "#6B7280"
    }
    
    badge_cor_bg <- if (!is.na(r_val) && r_val >= 0.65) {
      "rgba(22, 163, 74, 0.12)"
    } else if (!is.na(r_val) && r_val >= 0.35) {
      "rgba(217, 119, 6, 0.12)"
    } else {
      "#F2F2F7"
    }
    
    badge_cor_text <- if (!is.na(r_val) && r_val >= 0.65) {
      "Sincronía Alta"
    } else if (!is.na(r_val) && r_val >= 0.35) {
      "Sincronía Moderada"
    } else if (!is.na(r_val)) {
      "Baja Sincronía"
    } else {
      "Datos insuficientes"
    }
    
    div(
      style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 10px;",
      div(
        style = "background: #F9F9FB; border: 1px solid rgba(0,0,0,0.06); border-radius: 10px; padding: 10px 14px; display: flex; flex-direction: column; gap: 2px;",
        div(style = "font-size: 10.5px; font-weight: 600; color: #8E8E93; text-transform: uppercase; letter-spacing: 0.3px;", "Correlación de Calce (r)"),
        div(
          style = "display: flex; align-items: baseline; gap: 8px;",
          tags$span(style = sprintf("font-size: 20px; font-weight: 800; color: %s;", badge_cor_color),
                    if (!is.na(r_val)) sprintf("%+.3f", r_val) else "—"),
          tags$span(style = sprintf("font-size: 10.5px; font-weight: 700; color: %s; background: %s; padding: 2px 7px; border-radius: 6px;", badge_cor_color, badge_cor_bg), badge_cor_text)
        ),
        tags$span(style = "font-size: 10px; color: #8E8E93;", sprintf("%d semanas analizadas en conjunto", nrow(df_join)))
      ),
      div(
        style = "background: #F9F9FB; border: 1px solid rgba(0,0,0,0.06); border-radius: 10px; padding: 10px 14px; display: flex; flex-direction: column; gap: 2px;",
        div(style = "font-size: 10.5px; font-weight: 600; color: #8E8E93; text-transform: uppercase; letter-spacing: 0.3px;", "Pico Conteo → Proyectado"),
        div(
          style = "display: flex; align-items: baseline; gap: 6px;",
          tags$span(style = "font-size: 20px; font-weight: 800; color: #AF52DE;",
                    if (!is.na(pico_c_proy)) sprintf("Sem %d", as.integer(pico_c_proy)) else "—"),
          tags$span(style = "font-size: 11px; font-weight: 600; color: #8E8E93;",
                    if (!is.na(pico_c)) sprintf("(Origen: Sem %d)", as.integer(pico_c)) else "")
        ),
        tags$span(style = "font-size: 10px; color: #8E8E93;", sprintf("Desfase aplicado: +%d días", d))
      ),
      div(
        style = "background: #F9F9FB; border: 1px solid rgba(0,0,0,0.06); border-radius: 10px; padding: 10px 14px; display: flex; flex-direction: column; gap: 2px;",
        div(style = "font-size: 10.5px; font-weight: 600; color: #8E8E93; text-transform: uppercase; letter-spacing: 0.3px;", "Pico Real vs Diferencia"),
        div(
          style = "display: flex; align-items: baseline; gap: 6px;",
          tags$span(style = "font-size: 20px; font-weight: 800; color: #007AFF;",
                    if (!is.na(pico_y)) sprintf("Sem %d", as.integer(pico_y)) else "—"),
          tags$span(style = sprintf("font-size: 10.5px; font-weight: 700; color: %s; background: %s; padding: 2px 7px; border-radius: 6px;",
                                    if (!is.na(dif_picos) && dif_picos == 0) "#16A34A" else "#D97706",
                                    if (!is.na(dif_picos) && dif_picos == 0) "rgba(22, 163, 74, 0.12)" else "rgba(217, 119, 6, 0.12)"),
                    if (!is.na(dif_picos)) sprintf("Δ %d sem", as.integer(dif_picos)) else "—")
        ),
        tags$span(style = "font-size: 10px; color: #8E8E93;",
                  if (!is.na(dif_picos) && dif_picos == 0) "✔ Picos perfectamente alineados" else "Desfase entre picos de curvas")
      )
    )
  })
  
  # Observer: Cambio de turno en calibrador
  observeEvent(input$calibrar_mad_turno_sel, {
    tur_sel <- input$calibrar_mad_turno_sel
    req(tur_sel)
    calibrador_state$turno <- tur_sel
    m <- calibrador_state$modulo
    v <- calibrador_state$variedad
    
    df_mad_mod <- rv_maduracion()
    idx_m <- which(df_mad_mod$Módulo == m & df_mad_mod$Variedad == v)
    d_mod <- if (length(idx_m) > 0 && !is.na(df_mad_mod$Dias_Maduracion[idx_m[1]])) as.integer(df_mad_mod$Dias_Maduracion[idx_m[1]]) else 45L
    
    df_mad_t <- rv_maduracion_turno()
    idx_t <- which(df_mad_t$Módulo == m & df_mad_t$Turno == tur_sel & df_mad_t$Variedad == v)
    d_val <- if (length(idx_t) > 0 && !is.na(df_mad_t$Dias_Maduracion[idx_t[1]])) as.integer(df_mad_t$Dias_Maduracion[idx_t[1]]) else d_mod
    
    calibrador_state$dias <- d_val
    updateSliderInput(session, "calibrar_slider_dias", value = d_val)
  }, ignoreInit = TRUE)
  
  # Observer: Ajustar Automáticamente
  observeEvent(input$btn_auto_ajustar_calce, {
    m <- calibrador_state$modulo
    v <- calibrador_state$variedad
    t <- input$calibrar_mad_turno_sel
    req(m, t)
    
    camp_sel <- if (!is.null(input$filtro_campana)) input$filtro_campana else 2026
    df_c_hist <- if (exists("df_conteo_historico") && is.data.frame(df_conteo_historico) && nrow(df_conteo_historico) > 0) df_conteo_historico else tibble()
    df_cos    <- if (exists("df_cosecha") && is.data.frame(df_cosecha) && nrow(df_cosecha) > 0) df_cosecha else tibble()
    
    opt <- optimizar_dias_maduracion(df_cos, df_c_hist, campana = camp_sel, modulo = m, turno = t, variedad = v)
    
    if (!is.na(opt$r)) {
      updateSliderInput(session, "calibrar_slider_dias", value = opt$opt_dias)
      showNotification(
        sprintf("✔ Calce óptimo para Turno %s: %d días (%d semanas, r = %+.3f).", t, opt$opt_dias, opt$sem_shift, opt$r),
        type = "message", duration = 4
      )
    } else {
      showNotification("No se pudo calcular una correlación suficiente para este turno.", type = "warning", duration = 4)
    }
  })
  
  # Observer: Guardar para este Turno
  observeEvent(input$btn_guardar_calibracion_turno, {
    m <- calibrador_state$modulo
    v <- calibrador_state$variedad
    t <- input$calibrar_mad_turno_sel
    d <- suppressWarnings(as.integer(input$calibrar_slider_dias))
    req(m, v, t, d)
    
    df_mad_t <- rv_maduracion_turno()
    if (is.null(df_mad_t) || !is.data.frame(df_mad_t)) df_mad_t <- cargar_maduracion_turno_disco()
    
    camp_sel <- if (!is.null(input$filtro_campana)) input$filtro_campana else 2026
    df_c_hist <- if (exists("df_conteo_historico") && is.data.frame(df_conteo_historico) && nrow(df_conteo_historico) > 0) df_conteo_historico else tibble()
    df_cos    <- if (exists("df_cosecha") && is.data.frame(df_cosecha) && nrow(df_cosecha) > 0) df_cosecha else tibble()
    opt <- optimizar_dias_maduracion(df_cos, df_c_hist, campana = camp_sel, modulo = m, turno = t, variedad = v, min_dias = d, max_dias = d)
    r_val <- if (!is.na(opt$r)) opt$r else NA_real_
    
    df_mad_t <- df_mad_t %>%
      filter(!(Módulo == m & Turno == t & Variedad == v))
    
    nueva_fila <- tibble(
      Módulo             = m,
      Turno              = t,
      Variedad           = v,
      Dias_Maduracion    = d,
      Correlacion        = r_val,
      Fecha_Calibracion  = as.character(Sys.Date())
    )
    df_mad_t <- bind_rows(df_mad_t, nueva_fila)
    
    rv_maduracion_turno(df_mad_t)
    guardar_maduracion_turno_disco(df_mad_t)
    
    showNotification(
      sprintf("✔ Turno %s (Módulo %s) guardado con %d días de maduración.", t, m, d),
      type = "message", duration = 4
    )
  })
  
  # Observer: Restablecer Turno al Módulo
  observeEvent(input$btn_reset_turno_a_modulo, {
    m <- calibrador_state$modulo
    v <- calibrador_state$variedad
    t <- input$calibrar_mad_turno_sel
    req(m, v, t)
    
    df_mad_t <- rv_maduracion_turno()
    if (!is.null(df_mad_t) && is.data.frame(df_mad_t) && nrow(df_mad_t) > 0) {
      df_mad_t <- df_mad_t %>%
        filter(!(Módulo == m & Turno == t & Variedad == v))
      rv_maduracion_turno(df_mad_t)
      guardar_maduracion_turno_disco(df_mad_t)
    }
    
    df_mad_mod <- rv_maduracion()
    idx_m <- which(df_mad_mod$Módulo == m & df_mad_mod$Variedad == v)
    d_mod <- if (length(idx_m) > 0 && !is.na(df_mad_mod$Dias_Maduracion[idx_m[1]])) as.integer(df_mad_mod$Dias_Maduracion[idx_m[1]]) else 45L
    
    updateSliderInput(session, "calibrar_slider_dias", value = d_mod)
    showNotification(
      sprintf("Turno %s restablecido a %d días (heredado del Módulo %s).", t, d_mod, m),
      type = "message", duration = 3
    )
  })
  
  # Observer: Aplicar a todo el Módulo
  observeEvent(input$btn_aplicar_calce_a_todo_modulo, {
    m <- calibrador_state$modulo
    v <- calibrador_state$variedad
    d <- suppressWarnings(as.integer(input$calibrar_slider_dias))
    req(m, v, d)
    
    updateNumericInput(session, id_input_maduracion(m, v), value = d)
    
    df_mad_mod <- rv_maduracion()
    if (!is.null(df_mad_mod) && nrow(df_mad_mod) > 0) {
      df_mad_mod <- df_mad_mod %>%
        mutate(Dias_Maduracion = ifelse(Módulo == m & Variedad == v, d, Dias_Maduracion))
      rv_maduracion(df_mad_mod)
      guardar_maduracion_disco(df_mad_mod)
    }
    
    df_mad_t <- rv_maduracion_turno()
    if (!is.null(df_mad_t) && nrow(df_mad_t) > 0) {
      df_mad_t <- df_mad_t %>% filter(!(Módulo == m & Variedad == v))
      rv_maduracion_turno(df_mad_t)
      guardar_maduracion_turno_disco(df_mad_t)
    }
    
    showNotification(
      sprintf("✔ Aplicados %d días a todos los turnos del Módulo %s (%s).", d, m, v),
      type = "message", duration = 4
    )
  })
}
cat("Block parsed successfully!\n")
