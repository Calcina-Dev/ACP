import os

file_path = r"D:\ACP\Actualizacion Semanal\app.R"
with open(file_path, "r", encoding="utf-8") as f:
    code = f.read()

# 1. Update pasada_sugerida check
target_1 = """    pasada_sugerida <- if (nrow(valvs_celda) > 0 && any(valvs_celda$Num_Pasada == 2)) {
      2L
    } else if (nrow(ed_celda) > 0 && coalesce(ed_celda$Ha_Asignada_2da[1], 0) > 0) {
      2L
    } else if (limpiar_variedad(fila$Variedad) == "ROCIO" && ha_disponible_1ra <= 0.05 && ha_disponible_2da > 0.05) {
      2L
    } else {
      1L
    }"""

replacement_1 = """    pasada_sugerida <- if (nrow(valvs_celda) > 0 && any(valvs_celda$Num_Pasada == 2)) {
      2L
    } else if (nrow(ed_celda) > 0 && coalesce(ed_celda$Ha_Asignada_2da[1], 0) > 0) {
      2L
    } else if (limpiar_variedad(fila$Variedad) == "ROCIO" && ha_real_1ra >= (area_turno - 0.05)) {
      2L
    } else {
      1L
    }"""

assert target_1 in code, "target_1 not found"
code = code.replace(target_1, replacement_1, 1)

# 2. Update modal subtitle in modal_header
target_2 = """          tags$div(style = "font-size: 12px; font-weight: 400; color: #71717a; margin-top: 3px;",
                   sprintf("Sector %s • Módulo %s • Turno %s • %s (%s) • Área: %.2f ha (Disponibles: %.2f ha)",
                           fila$Sector, fila$Módulo, fila$Turno, fila$Variedad, fila$Tipo, area_turno, ha_disponible_sug))"""

replacement_2 = """          tags$div(style = "font-size: 12px; font-weight: 400; color: #71717a; margin-top: 3px;",
                   sprintf("Sector %s • Módulo %s • Turno %s • %s (%s) • %.2f ha",
                           fila$Sector, fila$Módulo, fila$Turno, fila$Variedad, fila$Tipo, area_turno))"""

assert target_2 in code, "target_2 not found"
code = code.replace(target_2, replacement_2, 1)

# 3. Update ui_modal_calculo_vivo and observeEvent(input$modal_btn_guardar) and observeEvent(input$modal_btn_restablecer)
target_3 = """  output$ui_modal_calculo_vivo <- renderUI({
    modo <- input$modal_modo_ingreso
    ctx <- modal_celda_activa()
    req(ctx)
    
    pasada_activa <- if (!is.null(input$modal_input_pasada)) as.integer(input$modal_input_pasada) else ctx$pasada_sug
    ha_disp <- if (pasada_activa == 2L) ctx$ha_disponible_2da else ctx$ha_disponible_1ra
    
    if (!is.null(modo) && modo == "kgha") {
      kgha <- suppressWarnings(as.numeric(input$modal_input_kgha))
      ha   <- suppressWarnings(as.numeric(input$modal_input_ha))
      kgha <- if (length(kgha) == 0 || is.na(kgha) || kgha < 0) 0 else kgha
      ha   <- if (length(ha) == 0 || is.na(ha) || ha < 0) 0 else ha
      kilos_calc <- round(kgha * ha, 2)
      
      div(
        style = "background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 8px; padding: 12px 14px; display: flex; align-items: baseline; justify-content: space-between; margin-bottom: 14px;",
        div(
          tags$div(style = "font-size: 12px; font-weight: 500; color: #475569;", "Total estimado"),
          tags$div(style = "font-size: 11.5px; color: #94a3b8; margin-top: 2px;", sprintf("%.2f kg/ha × %.2f ha", kgha, ha))
        ),
        tags$div(
          style = "font-size: 20px; font-weight: 600; color: #0f172a; font-variant-numeric: tabular-nums;",
          sprintf("%s kg", format(kilos_calc, big.mark = ","))
        )
      )
    } else {
      kg <- suppressWarnings(as.numeric(input$modal_input_kilos))
      kg <- if (length(kg) == 0 || is.na(kg) || kg < 0) 0 else kg
      kgha_res <- if (ha_disp > 0) round(kg / ha_disp, 2) else 0
      
      div(
        style = "background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 8px; padding: 10px 14px; display: flex; align-items: baseline; justify-content: space-between; margin-bottom: 14px;",
        div(
          tags$div(style = "font-size: 12px; font-weight: 500; color: #475569;", sprintf("Rendimiento implícito (%.2f ha disponibles)", ha_disp)),
          tags$div(style = "font-size: 11px; color: #94a3b8; margin-top: 2px;", "Se asume el consumo total de las hectáreas faltantes")
        ),
        tags$div(
          style = "font-size: 14.5px; font-weight: 600; color: #0f172a; font-variant-numeric: tabular-nums;",
          sprintf("%s kg/ha", format(kgha_res, big.mark = ","))
        )
      )
    }
  })
  
  observeEvent(input$modal_btn_guardar, {
    ctx <- modal_celda_activa()
    req(ctx)
    
    modo <- input$modal_modo_ingreso
    pasada_activa <- if (!is.null(input$modal_input_pasada)) as.integer(input$modal_input_pasada) else ctx$pasada_sug
    ha_disp <- if (pasada_activa == 2L) ctx$ha_disponible_2da else ctx$ha_disponible_1ra
    
    val_final <- if (!is.null(modo) && modo == "kgha") {
      kgha <- suppressWarnings(as.numeric(input$modal_input_kgha))
      ha   <- suppressWarnings(as.numeric(input$modal_input_ha))
      kgha <- if (length(kgha) == 0 || is.na(kgha)) 0 else kgha
      ha   <- if (length(ha) == 0 || is.na(ha)) 0 else ha
      round(kgha * ha, 2)
    } else {
      kg <- suppressWarnings(as.numeric(input$modal_input_kilos))
      kg <- if (length(kg) == 0 || is.na(kg)) 0 else kg
      round(kg, 2)
    }
    
    ha_final <- if (!is.null(modo) && modo == "kgha") {
      ha <- suppressWarnings(as.numeric(input$modal_input_ha))
      ha <- if (length(ha) == 0 || is.na(ha)) 0 else ha
      round(ha, 2)
    } else {
      # Al ingresar solo kilos directos, se asume que se consumieron todas las ha faltantes disponibles
      round(ha_disp, 2)
    }
    
    if (val_final < 0) {
      showNotification("El valor debe ser un número positivo (>= 0).", type = "warning", duration = 4)
      return()
    }
    
    num_pasada_sel <- pasada_activa
    
    motivo_txt <- if (!is.null(input$modal_input_motivo)) trimws(as.character(input$modal_input_motivo)) else ""
    motivo_final <- if (length(motivo_txt) > 0 && nzchar(motivo_txt[1])) {
      motivo_txt[1]
    } else if (!is.null(modo) && modo == "kgha") {
      sprintf("Manual (%s kg/ha en %.2f ha)", format(round(coalesce(as.numeric(input$modal_input_kgha), 0), 1)), ha_final)
    } else {
      sprintf("Manual (%s kg en %.2f ha faltantes)", format(round(val_final, 1), big.mark = ","), ha_final)
    }
    motivo_final <- if (length(motivo_txt) > 0 && nzchar(motivo_txt[1])) {
      motivo_txt[1]
    } else if (!is.null(modo) && modo == "kgha") {
      sprintf("Manual (%s kg/ha en %.2f ha)", format(round(coalesce(as.numeric(input$modal_input_kgha), 0), 1)), ha_final)
    } else {
      "Edición manual"
    }
    
    # Si el valor ingresado es 0, restablecer celda
    if (val_final == 0) {
      ed_actuales <- ediciones_manuales()
      ed_filtradas <- ed_actuales %>%
        filter(!(
          as.character(Campaña) == as.character(input$filtro_campana) &
            as.numeric(Semana)    == as.numeric(input$filtro_semana) &
            as.character(Sector)   == as.character(ctx$fila$Sector) &
            as.character(Módulo)   == as.character(ctx$fila$Módulo) &
            as.character(Turno)    == as.character(ctx$fila$Turno) &
            limpiar_variedad(Variedad) == limpiar_variedad(ctx$fila$Variedad) &
            as.character(Campo)    == as.character(ctx$campo)
        ))
      ediciones_manuales(ed_filtradas)
      guardar_ediciones_disco(ed_filtradas)
      removeModal()
      showNotification(sprintf("Se restableció la celda [%s] del Turno %s.", ctx$campo, ctx$fila$Turno), type = "message", duration = 3)
      return()
    }
    
    nueva_edicion <- tibble(
      Campaña       = as.character(input$filtro_campana),
      Semana        = as.numeric(input$filtro_semana),
      Sector        = as.character(ctx$fila$Sector),
      Módulo        = as.character(ctx$fila$Módulo),
      Turno         = as.character(ctx$fila$Turno),
      Variedad      = limpiar_variedad(ctx$fila$Variedad),
      Campo         = as.character(ctx$campo),
      Valor         = val_final,
      Ha_Asignada   = ha_final,
      Ha_Asignada_1ra = ifelse(num_pasada_sel == 1L, ha_final, 0),
      Kg_Asignado_1ra = ifelse(num_pasada_sel == 1L, val_final, 0),
      Ha_Asignada_2da = ifelse(num_pasada_sel == 2L, ha_final, 0),
      Kg_Asignado_2da = ifelse(num_pasada_sel == 2L, val_final, 0),
      Tipo_Edicion  = "manual",
      Motivo_Ajuste = motivo_final,
      Fecha_Edicion = Sys.time()
    )
    
    ed_actuales <- ediciones_manuales()
    ed_actuales <- ed_actuales %>%
      mutate(Campaña = as.character(Campaña), Semana = as.numeric(Semana)) %>%
      filter(!(
        as.character(Campaña) == as.character(input$filtro_campana) &
          as.numeric(Semana)    == as.numeric(input$filtro_semana) &
          as.character(Sector)   == as.character(ctx$fila$Sector) &
          as.character(Módulo)   == as.character(ctx$fila$Módulo) &
          as.character(Turno)    == as.character(ctx$fila$Turno) &
          limpiar_variedad(Variedad) == limpiar_variedad(ctx$fila$Variedad) &
          as.character(Campo)    == as.character(ctx$campo)
      )) %>%
      bind_rows(nueva_edicion)
    
    ediciones_manuales(ed_actuales)
    ok_disco <- guardar_ediciones_disco(ed_actuales)
    removeModal()
    
    if (ok_disco) {
      showNotification(
        sprintf("Dato guardado como manual: [%s] = %s Kg (Turno %s)", ctx$campo, format(val_final, big.mark = ","), ctx$fila$Turno),
        type = "message",
        duration = 3
      )
    } else {
      showNotification(
        sprintf("Dato registrado en sesión: [%s] = %s Kg (Turno %s)", ctx$campo, format(val_final, big.mark = ","), ctx$fila$Turno),
        type = "warning",
        duration = 4
      )
    }
  })
  
  observeEvent(input$modal_btn_restablecer, {
    ctx <- modal_celda_activa()
    req(ctx)
    
    # Si tenía asignación automática en rv_ajuste_valvulas, restaurarla
    valvs_aj <- rv_ajuste_valvulas()
    valvs_celda <- if (!is.null(valvs_aj) && nrow(valvs_aj) > 0) {
      valvs_aj %>%
        filter(
          as.character(Campaña) == as.character(input$filtro_campana),
          as.numeric(Semana)    == as.numeric(input$filtro_semana),
          as.character(Módulo)   == as.character(ctx$fila$Módulo),
          as.character(Turno)    == as.character(ctx$fila$Turno),
          limpiar_variedad(Variedad) == limpiar_variedad(ctx$fila$Variedad),
          as.character(Dia_Asignado) == as.character(ctx$campo)
        )
    } else tibble()
    
    ed_actuales <- ediciones_manuales()
    ed_filtradas <- ed_actuales %>%
      mutate(Campaña = as.character(Campaña), Semana = as.numeric(Semana)) %>%
      filter(!(
        as.character(Campaña) == as.character(input$filtro_campana) &
          as.numeric(Semana)    == as.numeric(input$filtro_semana) &
          as.character(Sector)   == as.character(ctx$fila$Sector) &
          as.character(Módulo)   == as.character(ctx$fila$Módulo) &
          as.character(Turno)    == as.character(ctx$fila$Turno) &
          limpiar_variedad(Variedad) == limpiar_variedad(ctx$fila$Variedad) &
          as.character(Campo)    == as.character(ctx$campo)
      ))
    
    if (nrow(valvs_celda) > 0) {
      reg_restaurado <- tibble(
        Campaña         = as.character(input$filtro_campana),
        Semana          = as.numeric(input$filtro_semana),
        Sector          = as.character(ctx$fila$Sector),
        Módulo          = as.character(ctx$fila$Módulo),
        Turno           = as.character(ctx$fila$Turno),
        Variedad        = limpiar_variedad(ctx$fila$Variedad),
        Campo           = as.character(ctx$campo),
        Valor           = round(sum(valvs_celda$Kg_Asignado, na.rm = TRUE), 2),
        Ha_Asignada     = round(sum(valvs_celda$Ha_Asignada, na.rm = TRUE), 2),
        Ha_Asignada_1ra = round(sum(ifelse(coalesce(valvs_celda$Num_Pasada, 1L) == 1L, valvs_celda$Ha_Asignada, 0), na.rm = TRUE), 2),
        Kg_Asignado_1ra = round(sum(ifelse(coalesce(valvs_celda$Num_Pasada, 1L) == 1L, valvs_celda$Kg_Asignado, 0), na.rm = TRUE), 2),
        Ha_Asignada_2da = round(sum(ifelse(coalesce(valvs_celda$Num_Pasada, 1L) == 2L, valvs_celda$Ha_Asignada, 0), na.rm = TRUE), 2),
        Kg_Asignado_2da = round(sum(ifelse(coalesce(valvs_celda$Num_Pasada, 1L) == 2L, valvs_celda$Kg_Asignado, 0), na.rm = TRUE), 2),
        Tipo_Edicion    = "auto",
        Motivo_Ajuste   = first(na.omit(valvs_celda$Motivo_Ajuste)),
        Fecha_Edicion   = Sys.time()
      )
      ed_filtradas <- bind_rows(ed_filtradas, reg_restaurado)
    }
    
    ediciones_manuales(ed_filtradas)
    guardar_ediciones_disco(ed_filtradas)
    removeModal()
    showNotification(sprintf("Se restableció la celda [%s] del Turno %s a su estado automático.", ctx$campo, ctx$fila$Turno), type = "message", duration = 3)
  })"""

replacement_3 = """  output$ui_modal_calculo_vivo <- renderUI({
    modo <- input$modal_modo_ingreso
    ctx <- modal_celda_activa()
    req(ctx)
    
    pasada_activa <- if (!is.null(input$modal_input_pasada)) as.integer(input$modal_input_pasada) else ctx$pasada_sug
    ha_real_pasada <- if (pasada_activa == 2L) ctx$ha_real_2da else ctx$ha_real_1ra
    ha_pool_pasada <- max(0, round(ctx$area_turno - ha_real_pasada, 2))
    
    ed_act <- ediciones_manuales()
    if (!is.null(ed_act) && nrow(ed_act) > 0) {
      if (!("Modo_Ingreso" %in% names(ed_act))) {
        ed_act$Modo_Ingreso <- ifelse(grepl("kg/ha en", coalesce(ed_act$Motivo_Ajuste, "")), "kgha", "kilos")
      }
    }
    
    ed_otros_pasada <- if (!is.null(ed_act) && nrow(ed_act) > 0) {
      ed_act %>%
        filter(
          as.character(Campaña) == as.character(input$filtro_campana),
          as.numeric(Semana)    == as.numeric(input$filtro_semana),
          as.character(Módulo)   == as.character(ctx$fila$Módulo),
          as.character(Turno)    == as.character(ctx$fila$Turno),
          limpiar_variedad(Variedad) == limpiar_variedad(ctx$fila$Variedad),
          as.character(Campo)    != as.character(ctx$campo),
          coalesce(Tipo_Edicion, "") == "manual",
          if (pasada_activa == 2L) coalesce(Ha_Asignada_2da, 0) > 0 | coalesce(Kg_Asignado_2da, 0) > 0
          else coalesce(Ha_Asignada_1ra, 0) > 0 | coalesce(Kg_Asignado_1ra, 0) > 0
        )
    } else tibble()
    
    ed_fijas <- ed_otros_pasada %>%
      filter(coalesce(Modo_Ingreso, "") == "kgha" | grepl("kg/ha en", coalesce(Motivo_Ajuste, "")))
    ha_fijas <- sum(if (pasada_activa == 2L) coalesce(ed_fijas$Ha_Asignada_2da, 0) else coalesce(ed_fijas$Ha_Asignada_1ra, 0), na.rm = TRUE)
    ha_pool_prorr <- max(0, round(ha_pool_pasada - ha_fijas, 2))
    
    ed_prorr <- ed_otros_pasada %>%
      filter(!(coalesce(Modo_Ingreso, "") == "kgha" | grepl("kg/ha en", coalesce(Motivo_Ajuste, ""))))
    
    if (!is.null(modo) && modo == "kgha") {
      kgha <- suppressWarnings(as.numeric(input$modal_input_kgha))
      ha   <- suppressWarnings(as.numeric(input$modal_input_ha))
      kgha <- if (length(kgha) == 0 || is.na(kgha) || kgha < 0) 0 else kgha
      ha   <- if (length(ha) == 0 || is.na(ha) || ha < 0) 0 else ha
      kilos_calc <- round(kgha * ha, 2)
      ha_restante <- max(0, round(ha_pool_prorr - ha, 2))
      
      div(
        style = "background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 8px; padding: 12px 14px; display: flex; align-items: baseline; justify-content: space-between; margin-bottom: 14px;",
        div(
          tags$div(style = "font-size: 12px; font-weight: 500; color: #475569;", "Total estimado"),
          tags$div(
            style = "font-size: 11.5px; color: #94a3b8; margin-top: 2px;",
            sprintf("%.2f kg/ha × %.2f ha%s", kgha, ha,
                    if (nrow(ed_prorr) > 0) sprintf(" • Restan %.2f ha para otros días", ha_restante) else "")
          )
        ),
        tags$div(
          style = "font-size: 20px; font-weight: 600; color: #0f172a; font-variant-numeric: tabular-nums;",
          sprintf("%s kg", format(kilos_calc, big.mark = ","))
        )
      )
    } else {
      kg_hoy <- suppressWarnings(as.numeric(input$modal_input_kilos))
      kg_hoy <- if (length(kg_hoy) == 0 || is.na(kg_hoy) || kg_hoy < 0) 0 else kg_hoy
      
      if (nrow(ed_prorr) > 0) {
        kg_otros_sum <- sum(coalesce(as.numeric(ed_prorr$Valor), 0), na.rm = TRUE)
        dias_otros_str <- paste(unique(ed_prorr$Campo), collapse = ", ")
        kg_total <- kg_otros_sum + kg_hoy
        
        ha_hoy_prorr <- if (kg_total > 0) round(ha_pool_prorr * (kg_hoy / kg_total), 2) else 0
        ha_otros_prorr <- max(0, round(ha_pool_prorr - ha_hoy_prorr, 2))
        kgha_conjunto <- if (ha_pool_prorr > 0 && kg_total > 0) round(kg_total / ha_pool_prorr, 1) else 0
        
        div(
          style = "background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 8px; padding: 11px 14px; margin-bottom: 14px;",
          div(
            style = "display: flex; align-items: baseline; justify-content: space-between;",
            div(
              tags$div(
                style = "font-size: 12px; font-weight: 600; color: #334155;",
                sprintf("Prorrateo proporcional con %s (%s kg)", dias_otros_str, format(round(kg_otros_sum, 1), big.mark = ","))
              ),
              tags$div(
                style = "font-size: 11.5px; color: #64748b; margin-top: 3px;",
                sprintf("Asignación: %s %.2f ha • %s %.2f ha (Total: %.2f ha)",
                        ctx$campo, ha_hoy_prorr, dias_otros_str, ha_otros_prorr, ha_pool_prorr)
              )
            ),
            tags$div(
              style = "text-align: right;",
              tags$div(
                style = "font-size: 14.5px; font-weight: 600; color: #0f172a; font-variant-numeric: tabular-nums;",
                sprintf("%s kg/ha", format(kgha_conjunto, big.mark = ","))
              ),
              tags$div(style = "font-size: 10.5px; color: #94a3b8;", "rendimiento uniforme")
            )
          )
        )
      } else {
        kgha_res <- if (ha_pool_prorr > 0 && kg_hoy > 0) round(kg_hoy / ha_pool_prorr, 1) else 0
        
        div(
          style = "background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 8px; padding: 10px 14px; display: flex; align-items: baseline; justify-content: space-between; margin-bottom: 14px;",
          div(
            tags$div(style = "font-size: 12px; font-weight: 500; color: #475569;", sprintf("Rendimiento implícito (%.2f ha disponibles)", ha_pool_prorr)),
            tags$div(style = "font-size: 11px; color: #94a3b8; margin-top: 2px;", "Se asume el consumo total de las hectáreas faltantes")
          ),
          tags$div(
            style = "font-size: 14.5px; font-weight: 600; color: #0f172a; font-variant-numeric: tabular-nums;",
            sprintf("%s kg/ha", format(kgha_res, big.mark = ","))
          )
        )
      }
    }
  })
  
  observeEvent(input$modal_btn_guardar, {
    ctx <- modal_celda_activa()
    req(ctx)
    
    modo <- input$modal_modo_ingreso
    pasada_activa <- if (!is.null(input$modal_input_pasada)) as.integer(input$modal_input_pasada) else ctx$pasada_sug
    ha_real_pasada <- if (pasada_activa == 2L) ctx$ha_real_2da else ctx$ha_real_1ra
    ha_pool_pasada <- max(0, round(ctx$area_turno - ha_real_pasada, 2))
    
    val_final <- if (!is.null(modo) && modo == "kgha") {
      kgha <- suppressWarnings(as.numeric(input$modal_input_kgha))
      ha   <- suppressWarnings(as.numeric(input$modal_input_ha))
      kgha <- if (length(kgha) == 0 || is.na(kgha)) 0 else kgha
      ha   <- if (length(ha) == 0 || is.na(ha)) 0 else ha
      round(kgha * ha, 2)
    } else {
      kg <- suppressWarnings(as.numeric(input$modal_input_kilos))
      kg <- if (length(kg) == 0 || is.na(kg)) 0 else kg
      round(kg, 2)
    }
    
    if (val_final < 0) {
      showNotification("El valor debe ser un número positivo (>= 0).", type = "warning", duration = 4)
      return()
    }
    
    ed_actuales <- ediciones_manuales()
    if (!is.null(ed_actuales) && nrow(ed_actuales) > 0) {
      ed_actuales <- ed_actuales %>% mutate(Campaña = as.character(Campaña), Semana = as.numeric(Semana))
      if (!("Modo_Ingreso" %in% names(ed_actuales))) {
        ed_actuales$Modo_Ingreso <- ifelse(grepl("kg/ha en", coalesce(ed_actuales$Motivo_Ajuste, "")), "kgha", "kilos")
      }
    }
    
    ed_otros_pasada <- if (!is.null(ed_actuales) && nrow(ed_actuales) > 0) {
      ed_actuales %>%
        filter(
          as.character(Campaña) == as.character(input$filtro_campana),
          as.numeric(Semana)    == as.numeric(input$filtro_semana),
          as.character(Módulo)   == as.character(ctx$fila$Módulo),
          as.character(Turno)    == as.character(ctx$fila$Turno),
          limpiar_variedad(Variedad) == limpiar_variedad(ctx$fila$Variedad),
          as.character(Campo)    != as.character(ctx$campo),
          coalesce(Tipo_Edicion, "") == "manual",
          if (pasada_activa == 2L) coalesce(Ha_Asignada_2da, 0) > 0 | coalesce(Kg_Asignado_2da, 0) > 0
          else coalesce(Ha_Asignada_1ra, 0) > 0 | coalesce(Kg_Asignado_1ra, 0) > 0
        )
    } else tibble()
    
    ed_fijas <- ed_otros_pasada %>%
      filter(coalesce(Modo_Ingreso, "") == "kgha" | grepl("kg/ha en", coalesce(Motivo_Ajuste, "")))
    ha_fijas <- sum(if (pasada_activa == 2L) coalesce(ed_fijas$Ha_Asignada_2da, 0) else coalesce(ed_fijas$Ha_Asignada_1ra, 0), na.rm = TRUE)
    ha_pool_prorr <- max(0, round(ha_pool_pasada - ha_fijas, 2))
    
    ed_prorr <- ed_otros_pasada %>%
      filter(!(coalesce(Modo_Ingreso, "") == "kgha" | grepl("kg/ha en", coalesce(Motivo_Ajuste, ""))))
    
    # Si el valor ingresado es 0, restablecer celda y recalcular días prorrateables restantes
    if (val_final == 0) {
      ed_filtradas <- ed_actuales %>%
        filter(!(
          as.character(Campaña) == as.character(input$filtro_campana) &
            as.numeric(Semana)    == as.numeric(input$filtro_semana) &
            as.character(Sector)   == as.character(ctx$fila$Sector) &
            as.character(Módulo)   == as.character(ctx$fila$Módulo) &
            as.character(Turno)    == as.character(ctx$fila$Turno) &
            limpiar_variedad(Variedad) == limpiar_variedad(ctx$fila$Variedad) &
            as.character(Campo)    == as.character(ctx$campo)
        ))
      
      if (nrow(ed_prorr) > 0) {
        kg_tot_rest <- sum(coalesce(as.numeric(ed_prorr$Valor), 0), na.rm = TRUE)
        ha_acum <- 0
        for (i in seq_len(nrow(ed_prorr))) {
          d_campo <- ed_prorr$Campo[i]
          ha_nueva_i <- if (i == nrow(ed_prorr)) {
            max(0, round(ha_pool_prorr - ha_acum, 2))
          } else {
            if (kg_tot_rest > 0) round(ha_pool_prorr * (as.numeric(ed_prorr$Valor[i]) / kg_tot_rest), 2) else 0
          }
          ha_acum <- ha_acum + ha_nueva_i
          
          idx <- which(
            as.character(ed_filtradas$Campaña) == as.character(input$filtro_campana) &
            as.numeric(ed_filtradas$Semana)    == as.numeric(input$filtro_semana) &
            as.character(ed_filtradas$Módulo)   == as.character(ctx$fila$Módulo) &
            as.character(ed_filtradas$Turno)    == as.character(ctx$fila$Turno) &
            limpiar_variedad(ed_filtradas$Variedad) == limpiar_variedad(ctx$fila$Variedad) &
            as.character(ed_filtradas$Campo)    == as.character(d_campo)
          )
          if (length(idx) > 0) {
            ed_filtradas$Ha_Asignada[idx] <- ha_nueva_i
            if (pasada_activa == 2L) {
              ed_filtradas$Ha_Asignada_2da[idx] <- ha_nueva_i
            } else {
              ed_filtradas$Ha_Asignada_1ra[idx] <- ha_nueva_i
            }
            ed_filtradas$Motivo_Ajuste[idx] <- if (nrow(ed_prorr) > 1) {
              sprintf("Manual prorrateado (%s kg en %.2f ha)", format(round(as.numeric(ed_prorr$Valor[i]), 1)), ha_nueva_i)
            } else {
              sprintf("Manual (%s kg en %.2f ha faltantes)", format(round(as.numeric(ed_prorr$Valor[i]), 1)), ha_nueva_i)
            }
          }
        }
      }
      
      ediciones_manuales(ed_filtradas)
      guardar_ediciones_disco(ed_filtradas)
      removeModal()
      showNotification(sprintf("Se restableció la celda [%s] del Turno %s.", ctx$campo, ctx$fila$Turno), type = "message", duration = 3)
      return()
    }
    
    # Calcular hectáreas finales según modo
    ha_final <- 0
    if (!is.null(modo) && modo == "kgha") {
      ha_in <- suppressWarnings(as.numeric(input$modal_input_ha))
      ha_final <- if (length(ha_in) == 0 || is.na(ha_in)) 0 else round(ha_in, 2)
      
      if (nrow(ed_prorr) > 0) {
        ha_restante <- max(0, round(ha_pool_prorr - ha_final, 2))
        kg_tot_otros <- sum(coalesce(as.numeric(ed_prorr$Valor), 0), na.rm = TRUE)
        ha_acum <- 0
        for (i in seq_len(nrow(ed_prorr))) {
          d_campo <- ed_prorr$Campo[i]
          ha_nueva_i <- if (i == nrow(ed_prorr)) {
            max(0, round(ha_restante - ha_acum, 2))
          } else {
            if (kg_tot_otros > 0) round(ha_restante * (as.numeric(ed_prorr$Valor[i]) / kg_tot_otros), 2) else 0
          }
          ha_acum <- ha_acum + ha_nueva_i
          
          idx <- which(
            as.character(ed_actuales$Campaña) == as.character(input$filtro_campana) &
            as.numeric(ed_actuales$Semana)    == as.numeric(input$filtro_semana) &
            as.character(ed_actuales$Módulo)   == as.character(ctx$fila$Módulo) &
            as.character(ed_actuales$Turno)    == as.character(ctx$fila$Turno) &
            limpiar_variedad(ed_actuales$Variedad) == limpiar_variedad(ctx$fila$Variedad) &
            as.character(ed_actuales$Campo)    == as.character(d_campo)
          )
          if (length(idx) > 0) {
            ed_actuales$Ha_Asignada[idx] <- ha_nueva_i
            if (pasada_activa == 2L) {
              ed_actuales$Ha_Asignada_2da[idx] <- ha_nueva_i
            } else {
              ed_actuales$Ha_Asignada_1ra[idx] <- ha_nueva_i
            }
          }
        }
      }
    } else {
      # Modo kilos directos con prorrateo proporcional
      if (nrow(ed_prorr) > 0) {
        kg_tot_otros <- sum(coalesce(as.numeric(ed_prorr$Valor), 0), na.rm = TRUE)
        kg_total_conjunto <- kg_tot_otros + val_final
        ha_acum <- 0
        for (i in seq_len(nrow(ed_prorr))) {
          d_campo <- ed_prorr$Campo[i]
          ha_nueva_i <- if (kg_total_conjunto > 0) round(ha_pool_prorr * (as.numeric(ed_prorr$Valor[i]) / kg_total_conjunto), 2) else 0
          ha_acum <- ha_acum + ha_nueva_i
          
          idx <- which(
            as.character(ed_actuales$Campaña) == as.character(input$filtro_campana) &
            as.numeric(ed_actuales$Semana)    == as.numeric(input$filtro_semana) &
            as.character(ed_actuales$Módulo)   == as.character(ctx$fila$Módulo) &
            as.character(ed_actuales$Turno)    == as.character(ctx$fila$Turno) &
            limpiar_variedad(ed_actuales$Variedad) == limpiar_variedad(ctx$fila$Variedad) &
            as.character(ed_actuales$Campo)    == as.character(d_campo)
          )
          if (length(idx) > 0) {
            ed_actuales$Ha_Asignada[idx] <- ha_nueva_i
            if (pasada_activa == 2L) {
              ed_actuales$Ha_Asignada_2da[idx] <- ha_nueva_i
            } else {
              ed_actuales$Ha_Asignada_1ra[idx] <- ha_nueva_i
            }
            ed_actuales$Motivo_Ajuste[idx] <- sprintf("Manual prorrateado (%s kg en %.2f ha)", format(round(as.numeric(ed_prorr$Valor[i]), 1)), ha_nueva_i)
          }
        }
        ha_final <- max(0, round(ha_pool_prorr - ha_acum, 2))
      } else {
        ha_final <- round(ha_pool_prorr, 2)
      }
    }
    
    num_pasada_sel <- pasada_activa
    motivo_txt <- if (!is.null(input$modal_input_motivo)) trimws(as.character(input$modal_input_motivo)) else ""
    motivo_final <- if (length(motivo_txt) > 0 && nzchar(motivo_txt[1])) {
      motivo_txt[1]
    } else if (!is.null(modo) && modo == "kgha") {
      sprintf("Manual (%s kg/ha en %.2f ha)", format(round(coalesce(as.numeric(input$modal_input_kgha), 0), 1)), ha_final)
    } else if (nrow(ed_prorr) > 0) {
      sprintf("Manual prorrateado (%s kg en %.2f ha)", format(round(val_final, 1)), ha_final)
    } else {
      sprintf("Manual (%s kg en %.2f ha faltantes)", format(round(val_final, 1), big.mark = ","), ha_final)
    }
    
    nueva_edicion <- tibble(
      Campaña         = as.character(input$filtro_campana),
      Semana          = as.numeric(input$filtro_semana),
      Sector          = as.character(ctx$fila$Sector),
      Módulo          = as.character(ctx$fila$Módulo),
      Turno           = as.character(ctx$fila$Turno),
      Variedad        = limpiar_variedad(ctx$fila$Variedad),
      Campo           = as.character(ctx$campo),
      Valor           = val_final,
      Ha_Asignada     = ha_final,
      Ha_Asignada_1ra = ifelse(num_pasada_sel == 1L, ha_final, 0),
      Kg_Asignado_1ra = ifelse(num_pasada_sel == 1L, val_final, 0),
      Ha_Asignada_2da = ifelse(num_pasada_sel == 2L, ha_final, 0),
      Kg_Asignado_2da = ifelse(num_pasada_sel == 2L, val_final, 0),
      Tipo_Edicion    = "manual",
      Modo_Ingreso    = if (!is.null(modo)) modo else "kilos",
      Motivo_Ajuste   = motivo_final,
      Fecha_Edicion   = Sys.time()
    )
    
    ed_actuales <- ed_actuales %>%
      mutate(Campaña = as.character(Campaña), Semana = as.numeric(Semana)) %>%
      filter(!(
        as.character(Campaña) == as.character(input$filtro_campana) &
          as.numeric(Semana)    == as.numeric(input$filtro_semana) &
          as.character(Sector)   == as.character(ctx$fila$Sector) &
          as.character(Módulo)   == as.character(ctx$fila$Módulo) &
          as.character(Turno)    == as.character(ctx$fila$Turno) &
          limpiar_variedad(Variedad) == limpiar_variedad(ctx$fila$Variedad) &
          as.character(Campo)    == as.character(ctx$campo)
      )) %>%
      bind_rows(nueva_edicion)
    
    ediciones_manuales(ed_actuales)
    ok_disco <- guardar_ediciones_disco(ed_actuales)
    removeModal()
    
    msg_extra <- if (nrow(ed_prorr) > 0) sprintf(" (Prorrateado con %s)", paste(unique(ed_prorr$Campo), collapse = ", ")) else ""
    if (ok_disco) {
      showNotification(
        sprintf("Dato guardado como manual: [%s] = %s Kg, %.2f ha (Turno %s)%s", ctx$campo, format(val_final, big.mark = ","), ha_final, ctx$fila$Turno, msg_extra),
        type = "message",
        duration = 3
      )
    } else {
      showNotification(
        sprintf("Dato registrado en sesión: [%s] = %s Kg, %.2f ha (Turno %s)%s", ctx$campo, format(val_final, big.mark = ","), ha_final, ctx$fila$Turno, msg_extra),
        type = "warning",
        duration = 4
      )
    }
  })
  
  observeEvent(input$modal_btn_restablecer, {
    ctx <- modal_celda_activa()
    req(ctx)
    
    pasada_activa <- if (nrow(ctx$ed_celda) > 0 && coalesce(ctx$ed_celda$Ha_Asignada_2da[1], 0) > 0) 2L else ctx$pasada_sug
    ha_real_pasada <- if (pasada_activa == 2L) ctx$ha_real_2da else ctx$ha_real_1ra
    ha_pool_pasada <- max(0, round(ctx$area_turno - ha_real_pasada, 2))
    
    valvs_aj <- rv_ajuste_valvulas()
    valvs_celda <- if (!is.null(valvs_aj) && nrow(valvs_aj) > 0) {
      valvs_aj %>%
        filter(
          as.character(Campaña) == as.character(input$filtro_campana),
          as.numeric(Semana)    == as.numeric(input$filtro_semana),
          as.character(Módulo)   == as.character(ctx$fila$Módulo),
          as.character(Turno)    == as.character(ctx$fila$Turno),
          limpiar_variedad(Variedad) == limpiar_variedad(ctx$fila$Variedad),
          as.character(Dia_Asignado) == as.character(ctx$campo)
        )
    } else tibble()
    
    ed_actuales <- ediciones_manuales()
    ed_filtradas <- ed_actuales %>%
      mutate(Campaña = as.character(Campaña), Semana = as.numeric(Semana)) %>%
      filter(!(
        as.character(Campaña) == as.character(input$filtro_campana) &
          as.numeric(Semana)    == as.numeric(input$filtro_semana) &
          as.character(Sector)   == as.character(ctx$fila$Sector) &
          as.character(Módulo)   == as.character(ctx$fila$Módulo) &
          as.character(Turno)    == as.character(ctx$fila$Turno) &
          limpiar_variedad(Variedad) == limpiar_variedad(ctx$fila$Variedad) &
          as.character(Campo)    == as.character(ctx$campo)
      ))
    
    ed_otros_pasada <- ed_filtradas %>%
      filter(
        as.character(Campaña) == as.character(input$filtro_campana),
        as.numeric(Semana)    == as.numeric(input$filtro_semana),
        as.character(Módulo)   == as.character(ctx$fila$Módulo),
        as.character(Turno)    == as.character(ctx$fila$Turno),
        limpiar_variedad(Variedad) == limpiar_variedad(ctx$fila$Variedad),
        coalesce(Tipo_Edicion, "") == "manual",
        if (pasada_activa == 2L) coalesce(Ha_Asignada_2da, 0) > 0 | coalesce(Kg_Asignado_2da, 0) > 0
        else coalesce(Ha_Asignada_1ra, 0) > 0 | coalesce(Kg_Asignado_1ra, 0) > 0
      )
    
    ed_fijas <- ed_otros_pasada %>%
      filter(coalesce(Modo_Ingreso, "") == "kgha" | grepl("kg/ha en", coalesce(Motivo_Ajuste, "")))
    ha_fijas <- sum(if (pasada_activa == 2L) coalesce(ed_fijas$Ha_Asignada_2da, 0) else coalesce(ed_fijas$Ha_Asignada_1ra, 0), na.rm = TRUE)
    ha_pool_prorr <- max(0, round(ha_pool_pasada - ha_fijas, 2))
    
    ed_prorr <- ed_otros_pasada %>%
      filter(!(coalesce(Modo_Ingreso, "") == "kgha" | grepl("kg/ha en", coalesce(Motivo_Ajuste, ""))))
    
    if (nrow(ed_prorr) > 0) {
      kg_tot_rest <- sum(coalesce(as.numeric(ed_prorr$Valor), 0), na.rm = TRUE)
      ha_acum <- 0
      for (i in seq_len(nrow(ed_prorr))) {
        d_campo <- ed_prorr$Campo[i]
        ha_nueva_i <- if (i == nrow(ed_prorr)) {
          max(0, round(ha_pool_prorr - ha_acum, 2))
        } else {
          if (kg_tot_rest > 0) round(ha_pool_prorr * (as.numeric(ed_prorr$Valor[i]) / kg_tot_rest), 2) else 0
        }
        ha_acum <- ha_acum + ha_nueva_i
        
        idx <- which(
          as.character(ed_filtradas$Campaña) == as.character(input$filtro_campana) &
          as.numeric(ed_filtradas$Semana)    == as.numeric(input$filtro_semana) &
          as.character(ed_filtradas$Módulo)   == as.character(ctx$fila$Módulo) &
          as.character(ed_filtradas$Turno)    == as.character(ctx$fila$Turno) &
          limpiar_variedad(ed_filtradas$Variedad) == limpiar_variedad(ctx$fila$Variedad) &
          as.character(ed_filtradas$Campo)    == as.character(d_campo)
        )
        if (length(idx) > 0) {
          ed_filtradas$Ha_Asignada[idx] <- ha_nueva_i
          if (pasada_activa == 2L) {
            ed_filtradas$Ha_Asignada_2da[idx] <- ha_nueva_i
          } else {
            ed_filtradas$Ha_Asignada_1ra[idx] <- ha_nueva_i
          }
          ed_filtradas$Motivo_Ajuste[idx] <- if (nrow(ed_prorr) > 1) {
            sprintf("Manual prorrateado (%s kg en %.2f ha)", format(round(as.numeric(ed_prorr$Valor[i]), 1)), ha_nueva_i)
          } else {
            sprintf("Manual (%s kg en %.2f ha faltantes)", format(round(as.numeric(ed_prorr$Valor[i]), 1)), ha_nueva_i)
          }
        }
      }
    }
    
    if (nrow(valvs_celda) > 0) {
      reg_restaurado <- tibble(
        Campaña         = as.character(input$filtro_campana),
        Semana          = as.numeric(input$filtro_semana),
        Sector          = as.character(ctx$fila$Sector),
        Módulo          = as.character(ctx$fila$Módulo),
        Turno           = as.character(ctx$fila$Turno),
        Variedad        = limpiar_variedad(ctx$fila$Variedad),
        Campo           = as.character(ctx$campo),
        Valor           = round(sum(valvs_celda$Kg_Asignado, na.rm = TRUE), 2),
        Ha_Asignada     = round(sum(valvs_celda$Ha_Asignada, na.rm = TRUE), 2),
        Ha_Asignada_1ra = round(sum(ifelse(coalesce(valvs_celda$Num_Pasada, 1L) == 1L, valvs_celda$Ha_Asignada, 0), na.rm = TRUE), 2),
        Kg_Asignado_1ra = round(sum(ifelse(coalesce(valvs_celda$Num_Pasada, 1L) == 1L, valvs_celda$Kg_Asignado, 0), na.rm = TRUE), 2),
        Ha_Asignada_2da = round(sum(ifelse(coalesce(valvs_celda$Num_Pasada, 1L) == 2L, valvs_celda$Ha_Asignada, 0), na.rm = TRUE), 2),
        Kg_Asignado_2da = round(sum(ifelse(coalesce(valvs_celda$Num_Pasada, 1L) == 2L, valvs_celda$Kg_Asignado, 0), na.rm = TRUE), 2),
        Tipo_Edicion    = "auto",
        Motivo_Ajuste   = first(na.omit(valvs_celda$Motivo_Ajuste)),
        Fecha_Edicion   = Sys.time()
      )
      ed_filtradas <- bind_rows(ed_filtradas, reg_restaurado)
    }
    
    ediciones_manuales(ed_filtradas)
    guardar_ediciones_disco(ed_filtradas)
    removeModal()
    showNotification(sprintf("Se restableció la celda [%s] del Turno %s a su estado automático.", ctx$campo, ctx$fila$Turno), type = "message", duration = 3)
  })"""

assert target_3 in code, "target_3 not found"
code = code.replace(target_3, replacement_3, 1)

with open(file_path, "w", encoding="utf-8") as f:
    f.write(code)

print("SUCCESS: Code updated successfully!")
