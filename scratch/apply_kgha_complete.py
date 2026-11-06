# -*- coding: utf-8 -*-
import sys

with open('Actualizacion Semanal/app.R', 'r', encoding='utf-8') as f:
    text = f.read()

# Determine CRLF
is_crlf = '\r\n' in text
nl = '\r\n' if is_crlf else '\n'

# 1. Target 1: colDias and colDiasMap
t1_old = """        // 1. Mostrar cursor pointer sobre celdas interactivas de días y proyección
        table.on('mouseenter', 'tbody td', function() {
          var cell = table.cell(this);
          if (!cell.index()) return;
          var colIdx = cell.index().column;
          var colDias = [5, 16, 17, 18, 19, 20, 21];
          if (colDias.indexOf(colIdx) !== -1) {
            $(this).css('cursor', 'pointer');
          }
        });

        // 2. Clic en celda para abrir modal interactivo de detalle / cálculo / edición
        table.on('click', 'tbody td', function(e) {
          var cell = table.cell(this);
          if (!cell.index()) return;
          var colIdx = cell.index().column;
          var rowIdx = cell.index().row;
          var rowData = table.row(rowIdx).data();
          if (!rowData) return;
          
          var colDiasMap = { 5: 'Proyección', 16: 'Lunes', 17: 'Martes', 18: 'Miércoles', 19: 'Jueves', 20: 'Viernes', 21: 'Sábado' };
          if (colDiasMap.hasOwnProperty(colIdx)) {
            var lockMap = { 5: 32, 16: 33, 17: 34, 18: 35, 19: 36, 20: 37, 21: 38 };
            var isLocked = rowData[lockMap[colIdx]];
            if (isLocked === true || isLocked === 1 || isLocked === 'TRUE') {
              Shiny.setInputValue('alerta_celda_bloqueada', { time: new Date().getTime() }, { priority: 'event' });
              return;
            }"""

t1_new = """        // 1. Mostrar cursor pointer sobre celdas interactivas de días, rendimiento ajustado y proyección
        table.on('mouseenter', 'tbody td', function() {
          var cell = table.cell(this);
          if (!cell.index()) return;
          var colIdx = cell.index().column;
          var colDias = [5, 14, 15, 16, 17, 18, 19, 20, 21];
          if (colDias.indexOf(colIdx) !== -1) {
            $(this).css('cursor', 'pointer');
          }
        });

        // 2. Clic en celda para abrir modal interactivo de detalle / cálculo / edición
        table.on('click', 'tbody td', function(e) {
          var cell = table.cell(this);
          if (!cell.index()) return;
          var colIdx = cell.index().column;
          var rowIdx = cell.index().row;
          var rowData = table.row(rowIdx).data();
          if (!rowData) return;
          
          var colDiasMap = { 5: 'Proyección', 14: 'Kg/Ha Aj. 1ª Pas', 15: 'Kg/Ha Aj. 2ª Pas', 16: 'Lunes', 17: 'Martes', 18: 'Miércoles', 19: 'Jueves', 20: 'Viernes', 21: 'Sábado' };
          if (colDiasMap.hasOwnProperty(colIdx)) {
            var lockMap = { 5: 32, 16: 33, 17: 34, 18: 35, 19: 36, 20: 37, 21: 38 };
            if (lockMap.hasOwnProperty(colIdx)) {
              var isLocked = rowData[lockMap[colIdx]];
              if (isLocked === true || isLocked === 1 || isLocked === 'TRUE') {
                Shiny.setInputValue('alerta_celda_bloqueada', { time: new Date().getTime() }, { priority: 'event' });
                return;
              }
            }"""

# 2. Target 2: modal_celda_activa
t2_old = "modal_celda_activa <- reactiveVal(NULL)"
t2_new = "modal_celda_activa <- reactiveVal(NULL)\n  modal_kgha_activo  <- reactiveVal(NULL)"

# 3. Target 3: Intercept click in observeEvent(input$abrir_modal_editar_celda)
t3_old = """    col_name <- as.character(info$colName)
    lock_col <- if (col_name == "Proyección") "Proy_bloqueado" else paste0(col_name, "_bloqueado")"""

t3_new = """    col_name <- as.character(info$colName)
    
    # Intercepción para edición directa de rendimiento ajustado (1ª o 2ª Pasada)
    if (col_name %in% c("Kg/Ha Aj. 1ª Pas", "Kg/Ha Aj. 2ª Pas")) {
      abrir_modal_kgha_ajustado(fila, col_name)
      return()
    }
    
    lock_col <- if (col_name == "Proyección") "Proy_bloqueado" else paste0(col_name, "_bloqueado")"""

# 4. Target 4: Insert abrir_modal_kgha_ajustado and handlers before TAB RESUMEN
t4_old = """    showNotification(sprintf("Se restableció la celda [%s] del Turno %s a su estado automático.", ctx$campo, ctx$fila$Turno), type = "message", duration = 3)
  })
  
  # ==============================================================================
  # SERVER: TAB RESUMEN (REAL VS AJUSTADO EN 3 FORMAS)"""

t4_new = """    showNotification(sprintf("Se restableció la celda [%s] del Turno %s a su estado automático.", ctx$campo, ctx$fila$Turno), type = "message", duration = 3)
  })
  
  # ==============================================================================
  # MODAL INTERACTIVO: AJUSTE DIRECTO DE RENDIMIENTO (KG/HA) Y REPARTO PROPORCIONAL
  # ==============================================================================
  abrir_modal_kgha_ajustado <- function(fila, col_name) {
    pasada_sel <- if (col_name == "Kg/Ha Aj. 2ª Pas") 2L else 1L
    col_ha <- if (pasada_sel == 2L) "Ha Aj. 2ª Pas" else "Ha Aj. 1ª Pas"
    col_kgha <- if (pasada_sel == 2L) "Kg/Ha Aj. 2ª Pas" else "Kg/Ha Aj. 1ª Pas"
    col_kg <- if (pasada_sel == 2L) "Kg_Aj_2da" else "Kg_Aj_1ra"
    
    ha_pasada <- coalesce(as.numeric(fila[[col_ha]]), 0)
    kgha_actual <- coalesce(as.numeric(fila[[col_kgha]]), 0)
    kg_pasada <- coalesce(as.numeric(fila[[col_kg]]), 0)
    kgha_proy <- coalesce(as.numeric(fila[['Kg/Ha Proy']]), 0)
    area_turno <- coalesce(as.numeric(fila$Área), 0)
    
    if (ha_pasada <= 0) {
      showNotification(
        sprintf("El Turno %s no tiene hectáreas asignadas en la %s para ajustar.", 
                fila$Turno, if (pasada_sel == 2L) "2ª Pasada" else "1ª Pasada"),
        type = "warning",
        duration = 4
      )
      return()
    }
    
    # Identificar días ajustados de este turno para esta pasada
    dias_semana <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
    ed_act <- ediciones_manuales()
    valvs_act <- rv_ajuste_valvulas()
    
    # Días con asignación específica de pasada en ediciones previas o válvulas
    dias_pasada_valvs <- if (!is.null(valvs_act) && nrow(valvs_act) > 0) {
      valvs_act %>%
        filter(
          as.character(Campaña) == as.character(input$filtro_campana),
          as.numeric(Semana)    == as.numeric(input$filtro_semana),
          as.character(Módulo)   == as.character(fila$Módulo),
          as.character(Turno)    == as.character(fila$Turno),
          limpiar_variedad(Variedad) == limpiar_variedad(fila$Variedad),
          coalesce(Num_Pasada, 1L) == pasada_sel
        ) %>%
        pull(Dia_Asignado) %>% unique()
    } else character(0)
    
    dias_pasada_ed <- if (!is.null(ed_act) && nrow(ed_act) > 0) {
      ed_act %>%
        filter(
          as.character(Campaña) == as.character(input$filtro_campana),
          as.numeric(Semana)    == as.numeric(input$filtro_semana),
          as.character(Módulo)   == as.character(fila$Módulo),
          as.character(Turno)    == as.character(fila$Turno),
          limpiar_variedad(Variedad) == limpiar_variedad(fila$Variedad),
          if (pasada_sel == 2L) coalesce(Ha_Asignada_2da, 0) > 0 | coalesce(Kg_Asignado_2da, 0) > 0
          else coalesce(Ha_Asignada_1ra, 0) > 0 | coalesce(Kg_Asignado_1ra, 0) > 0
        ) %>%
        pull(Campo) %>% unique()
    } else character(0)
    
    # Días con datos de ajuste en la fila
    dias_con_ajuste_fila <- dias_semana[sapply(dias_semana, function(d) {
      val <- coalesce(as.numeric(fila[[d]]), 0)
      tip <- coalesce(as.character(fila[[paste0(d, "_tipo")]]), "none")
      bloq <- isTRUE(fila[[paste0(d, "_bloqueado")]])
      val > 0 && tip %in% c("auto", "manual") && !bloq
    })]
    
    dias_ajustados <- if (pasada_sel == 2L) {
      cands <- unique(c(dias_pasada_valvs, dias_pasada_ed))
      intersect(dias_con_ajuste_fila, cands)
    } else {
      # 1ra pasada
      if (limpiar_variedad(fila$Variedad) == "ROCIO" && coalesce(as.numeric(fila[['Ha Aj. 2ª Pas']]), 0) > 0) {
        dias_2da_cands <- if (!is.null(valvs_act) && nrow(valvs_act) > 0) {
          valvs_act %>%
            filter(
              as.character(Campaña) == as.character(input$filtro_campana),
              as.numeric(Semana)    == as.numeric(input$filtro_semana),
              as.character(Módulo)   == as.character(fila$Módulo),
              as.character(Turno)    == as.character(fila$Turno),
              limpiar_variedad(Variedad) == limpiar_variedad(fila$Variedad),
              coalesce(Num_Pasada, 1L) == 2L
            ) %>% pull(Dia_Asignado) %>% unique()
        } else character(0)
        setdiff(dias_con_ajuste_fila, dias_2da_cands)
      } else {
        dias_con_ajuste_fila
      }
    }
    
    if (length(dias_ajustados) == 0) {
      showNotification(
        sprintf("No se encontraron días con ajuste activo para la %s del Turno %s.", 
                if (pasada_sel == 2L) "2ª Pasada" else "1ª Pasada", fila$Turno),
        type = "warning",
        duration = 4
      )
      return()
    }
    
    # Kilos actuales de cada día ajustado
    kg_dias_actuales <- sapply(dias_ajustados, function(d) coalesce(as.numeric(fila[[d]]), 0))
    
    # Si algún día tiene edición manual previa
    tiene_manuales <- any(sapply(dias_ajustados, function(d) {
      coalesce(as.character(fila[[paste0(d, "_tipo")]]), "") == "manual"
    }))
    
    ctx_kgha <- list(
      fila             = fila,
      col_name         = col_name,
      pasada_sel       = pasada_sel,
      ha_pasada        = ha_pasada,
      kgha_actual      = kgha_actual,
      kg_pasada        = kg_pasada,
      kgha_proy        = kgha_proy,
      area_turno       = area_turno,
      dias_ajustados   = dias_ajustados,
      kg_dias_actuales = kg_dias_actuales,
      tiene_manuales   = tiene_manuales
    )
    modal_kgha_activo(ctx_kgha)
    
    showModal(modalDialog(
      title = tagList(
        div(
          style = "display: flex; align-items: flex-start; justify-content: space-between; width: 100%;",
          div(
            tags$div(style = "font-size: 15.5px; font-weight: 600; color: #09090b; letter-spacing: -0.01em; line-height: 1.3;", 
                     sprintf("Ajustar Rendimiento • %s", if (pasada_sel == 2L) "2ª Pasada" else "1ª Pasada")),
            tags$div(style = "font-size: 12px; font-weight: 400; color: #71717a; margin-top: 3px;",
                     sprintf("Sector %s • Módulo %s • Turno %s • %s (%s) • %.2f ha",
                             fila$Sector, fila$Módulo, fila$Turno, fila$Variedad, fila$Tipo, area_turno))
          ),
          div(
            if (tiene_manuales) {
              tags$span(style = "display: inline-flex; align-items: center; padding: 2px 8px; border-radius: 9999px; font-size: 11px; font-weight: 500; background: #ede9fe; color: #5b21b6; border: 1px solid #ddd6fe;", "Edición manual")
            } else {
              tags$span(style = "display: inline-flex; align-items: center; padding: 2px 8px; border-radius: 9999px; font-size: 11px; font-weight: 500; background: #fef3c7; color: #92400e; border: 1px solid #fde68a;", "Ajuste automático")
            }
          )
        )
      ),
      size = "m",
      easyClose = TRUE,
      uiOutput("ui_modal_kgha_cuerpo"),
      footer = tagList(
        if (tiene_manuales) {
          actionButton("modal_kgha_btn_restablecer", "Restablecer a automático", class = "ek-btn-danger", style = "margin-right: auto;")
        },
        modalButton("Cancelar"),
        actionButton("modal_kgha_btn_guardar", "Aplicar y Guardar como Manual", class = "ek-btn-primary")
      )
    ))
  }
  
  output$ui_modal_kgha_cuerpo <- renderUI({
    ctx <- modal_kgha_activo()
    req(ctx)
    
    fila <- ctx$fila
    ha_pasada <- ctx$ha_pasada
    kgha_actual <- ctx$kgha_actual
    kg_pasada <- ctx$kg_pasada
    kgha_proy <- ctx$kgha_proy
    
    tagList(
      div(
        style = "display: grid; grid-template-columns: repeat(3, 1fr); gap: 10px; margin-bottom: 18px;",
        div(
          style = "background: #fbfbfa; border: 1px solid #e7e5e4; border-radius: 9px; padding: 10px 12px;",
          tags$div(style = "font-size: 11px; font-weight: 500; color: #78716c;", "Área asignada"),
          tags$div(style = "font-size: 15px; font-weight: 700; color: #1c1917; margin-top: 3px; font-variant-numeric: tabular-nums;", 
                   sprintf("%.2f ha", ha_pasada))
        ),
        div(
          style = "background: #fbfbfa; border: 1px solid #e7e5e4; border-radius: 9px; padding: 10px 12px;",
          tags$div(style = "font-size: 11px; font-weight: 500; color: #78716c;", "Rendimiento actual"),
          tags$div(style = "font-size: 15px; font-weight: 700; color: #0284c7; margin-top: 3px; font-variant-numeric: tabular-nums;", 
                   sprintf("%.2f kg/ha", kgha_actual)),
          tags$div(style = "font-size: 10.5px; color: #a8a29e; margin-top: 1px;", 
                   sprintf("Total: %s kg", format(round(kg_pasada, 1), big.mark = ",")))
        ),
        div(
          style = "background: #fbfbfa; border: 1px solid #e7e5e4; border-radius: 9px; padding: 10px 12px;",
          tags$div(style = "font-size: 11px; font-weight: 500; color: #78716c;", "Rendimiento proyección"),
          tags$div(style = "font-size: 15px; font-weight: 700; color: #16a34a; margin-top: 3px; font-variant-numeric: tabular-nums;", 
                   if (kgha_proy > 0) sprintf("%.2f kg/ha", kgha_proy) else "-"),
          tags$div(style = "font-size: 10.5px; color: #a8a29e; margin-top: 1px;", 
                   if (kgha_proy > 0) sprintf("Total: %s kg", format(round(ha_pasada * kgha_proy, 1), big.mark = ",")) else "")
        )
      ),
      
      div(
        style = "margin-bottom: 14px;",
        div(
          style = "display: flex; align-items: center; justify-content: space-between; margin-bottom: 6px;",
          tags$label(style = "font-size: 12.5px; font-weight: 600; color: #18181b; margin: 0;", "Nuevo rendimiento objetivo (kg/ha)"),
          if (kgha_proy > 0) {
            actionLink(
              "modal_kgha_btn_copiar_proy",
              label = tagList(
                tags$span(style = "font-size: 11px; color: #2563eb; font-weight: 500;", 
                          sprintf("Usar proyección (%.2f)", kgha_proy))
              )
            )
          }
        ),
        numericInput(
          "modal_kgha_input_valor",
          label = NULL,
          value = round(kgha_actual, 2),
          min = 1,
          step = 10,
          width = "100%"
        )
      ),
      
      uiOutput("ui_modal_kgha_preview_distribucion")
    )
  })
  
  observeEvent(input$modal_kgha_btn_copiar_proy, {
    ctx <- modal_kgha_activo()
    req(ctx)
    if (ctx$kgha_proy > 0) {
      updateNumericInput(session, "modal_kgha_input_valor", value = round(ctx$kgha_proy, 2))
    }
  })
  
  output$ui_modal_kgha_preview_distribucion <- renderUI({
    ctx <- modal_kgha_activo()
    req(ctx)
    
    nuevo_kgha <- suppressWarnings(as.numeric(input$modal_kgha_input_valor))
    nuevo_kgha <- if (length(nuevo_kgha) == 0 || is.na(nuevo_kgha) || nuevo_kgha < 0) 0 else nuevo_kgha
    
    ha_pasada <- ctx$ha_pasada
    dias <- ctx$dias_ajustados
    kg_orig <- ctx$kg_dias_actuales
    tot_orig <- sum(kg_orig)
    
    kg_tot_nuevo <- round(ha_pasada * nuevo_kgha, 2)
    diff_tot <- round(kg_tot_nuevo - ctx$kg_pasada, 2)
    
    props <- if (tot_orig > 0) kg_orig / tot_orig else rep(1 / length(dias), length(dias))
    
    kg_dist <- numeric(length(dias))
    kg_acum <- 0
    for (i in seq_along(dias)) {
      if (i == length(dias)) {
        kg_dist[i] <- max(0, round(kg_tot_nuevo - kg_acum, 2))
      } else {
        kg_dist[i] <- round(kg_tot_nuevo * props[i], 2)
        kg_acum <- kg_acum + kg_dist[i]
      }
    }
    
    div(
      style = "background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 9px; padding: 12px 14px; margin-top: 6px;",
      div(
        style = "display: flex; align-items: baseline; justify-content: space-between; margin-bottom: 10px;",
        tags$div(style = "font-size: 12px; font-weight: 600; color: #334155;", "Distribución proporcional estimada"),
        tags$div(
          style = "font-size: 13.5px; font-weight: 700; color: #0f172a; font-variant-numeric: tabular-nums;",
          sprintf("Total: %s kg (%+0.1f kg)", format(kg_tot_nuevo, big.mark = ","), diff_tot)
        )
      ),
      div(
        style = "display: flex; flex-direction: column; gap: 6px;",
        lapply(seq_along(dias), function(i) {
          d <- dias[i]
          d_orig <- kg_orig[i]
          d_nuevo <- kg_dist[i]
          d_diff <- round(d_nuevo - d_orig, 1)
          pct <- round(props[i] * 100, 1)
          
          div(
            style = "display: flex; align-items: center; justify-content: space-between; padding: 6px 10px; background: #ffffff; border: 1px solid #e2e8f0; border-radius: 6px;",
            div(
              style = "display: flex; align-items: center; gap: 8px;",
              tags$span(style = "font-size: 12px; font-weight: 600; color: #1e293b; min-width: 65px;", d),
              tags$span(style = "font-size: 11px; color: #64748b;", sprintf("%0.1f%%", pct))
            ),
            div(
              style = "display: flex; align-items: center; gap: 10px; font-variant-numeric: tabular-nums;",
              tags$span(style = "font-size: 11.5px; color: #94a3b8; text-decoration: line-through;", sprintf("%s kg", format(round(d_orig, 1), big.mark = ","))),
              tags$span(style = "font-size: 12.5px; font-weight: 600; color: #0f172a;", sprintf("%s kg", format(d_nuevo, big.mark = ","))),
              tags$span(
                style = sprintf("font-size: 11px; font-weight: 500; min-width: 55px; text-align: right; color: %s;", if (d_diff >= 0) "#16a34a" else "#dc2626"),
                sprintf("%+0.1f kg", d_diff)
              )
            )
          )
        })
      ),
      div(
        style = "font-size: 11px; color: #64748b; margin-top: 10px; line-height: 1.35;",
        "Nota: Al guardar, los valores de estos días se convertirán en ",
        tags$b(style = "color: #7c3aed;", "ediciones manuales"),
        " y el rendimiento ajustado del turno se actualizará a este nuevo valor."
      )
    )
  })
  
  observeEvent(input$modal_kgha_btn_guardar, {
    ctx <- modal_kgha_activo()
    req(ctx)
    
    nuevo_kgha <- suppressWarnings(as.numeric(input$modal_kgha_input_valor))
    if (length(nuevo_kgha) == 0 || is.na(nuevo_kgha) || nuevo_kgha <= 0) {
      showNotification("Por favor ingresa un rendimiento válido (mayor a 0 kg/ha).", type = "warning", duration = 4)
      return()
    }
    
    nuevo_kgha <- round(nuevo_kgha, 2)
    fila <- ctx$fila
    pasada_sel <- ctx$pasada_sel
    ha_pasada <- ctx$ha_pasada
    dias <- ctx$dias_ajustados
    kg_orig <- ctx$kg_dias_actuales
    tot_orig <- sum(kg_orig)
    
    kg_tot_nuevo <- round(ha_pasada * nuevo_kgha, 2)
    
    ed_actuales <- ediciones_manuales()
    if (!is.null(ed_actuales) && nrow(ed_actuales) > 0) {
      ed_actuales <- ed_actuales %>% mutate(Campaña = as.character(Campaña), Semana = as.numeric(Semana))
    } else {
      ed_actuales <- tibble()
    }
    
    valvs_aj <- rv_ajuste_valvulas()
    
    props <- if (tot_orig > 0) kg_orig / tot_orig else rep(1 / length(dias), length(dias))
    
    kg_dist <- numeric(length(dias))
    ha_dist <- numeric(length(dias))
    kg_acum <- 0
    ha_acum <- 0
    
    for (i in seq_along(dias)) {
      d <- dias[i]
      if (i == length(dias)) {
        kg_dist[i] <- max(0, round(kg_tot_nuevo - kg_acum, 2))
        ha_dist[i] <- max(0, round(ha_pasada - ha_acum, 2))
      } else {
        kg_dist[i] <- round(kg_tot_nuevo * props[i], 2)
        kg_acum <- kg_acum + kg_dist[i]
        
        ed_d <- if (nrow(ed_actuales) > 0) {
          ed_actuales %>% filter(
            as.character(Campaña) == as.character(input$filtro_campana),
            as.numeric(Semana)    == as.numeric(input$filtro_semana),
            as.character(Sector)  == as.character(fila$Sector),
            as.character(Módulo)  == as.character(fila$Módulo),
            as.character(Turno)   == as.character(fila$Turno),
            limpiar_variedad(Variedad) == limpiar_variedad(fila$Variedad),
            as.character(Campo)   == as.character(d)
          )
        } else tibble()
        
        ha_d_existente <- if (nrow(ed_d) > 0 && coalesce(ed_d$Ha_Asignada[1], 0) > 0) {
          ed_d$Ha_Asignada[1]
        } else if (!is.null(valvs_aj) && nrow(valvs_aj) > 0) {
          v_d <- valvs_aj %>% filter(
            as.character(Campaña) == as.character(input$filtro_campana),
            as.numeric(Semana)    == as.numeric(input$filtro_semana),
            as.character(Sector)  == as.character(fila$Sector),
            as.character(Módulo)  == as.character(fila$Módulo),
            as.character(Turno)   == as.character(fila$Turno),
            limpiar_variedad(Variedad) == limpiar_variedad(fila$Variedad),
            as.character(Dia_Asignado) == as.character(d),
            coalesce(Num_Pasada, 1L) == pasada_sel
          )
          if (nrow(v_d) > 0) round(sum(v_d$Ha_Asignada, na.rm = TRUE), 2) else 0
        } else 0
        
        ha_dist[i] <- if (ha_d_existente > 0) ha_d_existente else round(ha_pasada * props[i], 2)
        ha_acum <- ha_acum + ha_dist[i]
      }
    }
    
    if (sum(ha_dist) != ha_pasada) {
      ha_dist[length(dias)] <- max(0, round(ha_pasada - sum(ha_dist[seq_len(length(dias)-1)]), 2))
    }
    
    nuevas_filas <- lapply(seq_along(dias), function(i) {
      d <- dias[i]
      k_val <- kg_dist[i]
      h_val <- ha_dist[i]
      tibble(
        Campaña         = as.character(input$filtro_campana),
        Semana          = as.numeric(input$filtro_semana),
        Sector          = as.character(fila$Sector),
        Módulo          = as.character(fila$Módulo),
        Turno           = as.character(fila$Turno),
        Variedad        = limpiar_variedad(fila$Variedad),
        Campo           = as.character(d),
        Valor           = k_val,
        Ha_Asignada     = h_val,
        Ha_Asignada_1ra = if (pasada_sel == 1L) h_val else 0,
        Kg_Asignado_1ra = if (pasada_sel == 1L) k_val else 0,
        Ha_Asignada_2da = if (pasada_sel == 2L) h_val else 0,
        Kg_Asignado_2da = if (pasada_sel == 2L) k_val else 0,
        Tipo_Edicion    = "manual",
        Modo_Ingreso    = "kgha",
        Motivo_Ajuste   = sprintf("Manual (%s kg/ha en %.2f ha)", format(round(nuevo_kgha, 2)), h_val),
        Fecha_Edicion   = Sys.time()
      )
    }) %>% bind_rows()
    
    ed_actualizadas <- ed_actuales %>%
      filter(!(
        as.character(Campaña) == as.character(input$filtro_campana) &
        as.numeric(Semana)    == as.numeric(input$filtro_semana) &
        as.character(Sector)  == as.character(fila$Sector) &
        as.character(Módulo)  == as.character(fila$Módulo) &
        as.character(Turno)   == as.character(fila$Turno) &
        limpiar_variedad(Variedad) == limpiar_variedad(fila$Variedad) &
        as.character(Campo)   %in% as.character(dias)
      )) %>%
      bind_rows(nuevas_filas)
    
    ediciones_manuales(ed_actualizadas)
    ok_disco <- guardar_ediciones_disco(ed_actualizadas)
    removeModal()
    
    if (ok_disco) {
      showNotification(
        sprintf("Rendimiento ajustado a %.2f kg/ha para Turno %s. Los días %s se guardaron como manuales.",
                nuevo_kgha, fila$Turno, paste(dias, collapse = ", ")),
        type = "message",
        duration = 4
      )
    } else {
      showNotification(
        sprintf("Rendimiento actualizado en sesión a %.2f kg/ha para Turno %s (%s).",
                nuevo_kgha, fila$Turno, paste(dias, collapse = ", ")),
        type = "warning",
        duration = 4
      )
    }
  })
  
  observeEvent(input$modal_kgha_btn_restablecer, {
    ctx <- modal_kgha_activo()
    req(ctx)
    
    fila <- ctx$fila
    dias <- ctx$dias_ajustados
    pasada_sel <- ctx$pasada_sel
    
    valvs_aj <- rv_ajuste_valvulas()
    valvs_t <- if (!is.null(valvs_aj) && nrow(valvs_aj) > 0) {
      valvs_aj %>%
        filter(
          as.character(Campaña) == as.character(input$filtro_campana),
          as.numeric(Semana)    == as.numeric(input$filtro_semana),
          as.character(Módulo)   == as.character(fila$Módulo),
          as.character(Turno)    == as.character(fila$Turno),
          limpiar_variedad(Variedad) == limpiar_variedad(fila$Variedad),
          as.character(Dia_Asignado) %in% as.character(dias)
        )
    } else tibble()
    
    ed_actuales <- ediciones_manuales()
    if (!is.null(ed_actuales) && nrow(ed_actuales) > 0) {
      ed_filtradas <- ed_actuales %>%
        mutate(Campaña = as.character(Campaña), Semana = as.numeric(Semana)) %>%
        filter(!(
          as.character(Campaña) == as.character(input$filtro_campana) &
          as.numeric(Semana)    == as.numeric(input$filtro_semana) &
          as.character(Sector)  == as.character(fila$Sector) &
          as.character(Módulo)  == as.character(fila$Módulo) &
          as.character(Turno)   == as.character(fila$Turno) &
          limpiar_variedad(Variedad) == limpiar_variedad(fila$Variedad) &
          as.character(Campo)   %in% as.character(dias)
        ))
    } else {
      ed_filtradas <- tibble()
    }
    
    if (nrow(valvs_t) > 0) {
      ed_auto_restauradas <- valvs_t %>%
        group_by(Campaña, Semana, Sector, Módulo, Turno, Variedad, Dia_Asignado) %>%
        summarise(
          Valor           = round(sum(Kg_Asignado, na.rm = TRUE), 2),
          Ha_Asignada     = round(sum(Ha_Asignada, na.rm = TRUE), 2),
          Ha_Asignada_1ra = round(sum(ifelse(coalesce(Num_Pasada, 1L) == 1L, Ha_Asignada, 0), na.rm = TRUE), 2),
          Kg_Asignado_1ra = round(sum(ifelse(coalesce(Num_Pasada, 1L) == 1L, Kg_Asignado, 0), na.rm = TRUE), 2),
          Ha_Asignada_2da = round(sum(ifelse(coalesce(Num_Pasada, 1L) == 2L, Ha_Asignada, 0), na.rm = TRUE), 2),
          Kg_Asignado_2da = round(sum(ifelse(coalesce(Num_Pasada, 1L) == 2L, Kg_Asignado, 0), na.rm = TRUE), 2),
          Motivo_Ajuste   = first(na.omit(Motivo_Ajuste)),
          .groups         = "drop"
        ) %>%
        rename(Campo = Dia_Asignado) %>%
        mutate(
          Fecha_Edicion = Sys.time(),
          Tipo_Edicion  = "auto",
          Modo_Ingreso  = NA_character_
        )
      ed_filtradas <- bind_rows(ed_filtradas, ed_auto_restauradas)
    }
    
    ediciones_manuales(ed_filtradas)
    guardar_ediciones_disco(ed_filtradas)
    removeModal()
    
    showNotification(
      sprintf("Se restableció el cálculo automático para los días %s del Turno %s.", paste(dias, collapse = ", "), fila$Turno),
      type = "message",
      duration = 4
    )
  })
  
  # ==============================================================================
  # SERVER: TAB RESUMEN (REAL VS AJUSTADO EN 3 FORMAS)"""

# Perform replacements using normalized newlines
text_norm = text.replace('\r\n', '\n')

def replace_block(content, old_b, new_b, name):
    old_norm = old_b.replace('\r\n', '\n')
    new_norm = new_b.replace('\r\n', '\n')
    if old_norm not in content:
        print(f"Error: {name} not found in content!")
        sys.exit(1)
    print(f"Replacing {name} (length {len(old_norm)})...")
    return content.replace(old_norm, new_norm, 1)

text_norm = replace_block(text_norm, t1_old, t1_new, "Target 1 (DT callback)")
text_norm = replace_block(text_norm, t2_old, t2_new, "Target 2 (reactiveVal)")
text_norm = replace_block(text_norm, t3_old, t3_new, "Target 3 (abrir_modal intercept)")
text_norm = replace_block(text_norm, t4_old, t4_new, "Target 4 (abrir_modal_kgha_ajustado and observers)")

if is_crlf:
    text_final = text_norm.replace('\n', '\r\n')
else:
    text_final = text_norm

with open('Actualizacion Semanal/app.R', 'w', encoding='utf-8', newline='') as f:
    f.write(text_final)

print("Successfully written to Actualizacion Semanal/app.R")
