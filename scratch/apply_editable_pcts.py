# -*- coding: utf-8 -*-
import sys

with open('Actualizacion Semanal/app.R', 'r', encoding='utf-8') as f:
    text = f.read()

start_marker = "output$ui_modal_kgha_cuerpo <- renderUI({"
end_marker = "observeEvent(input$modal_kgha_btn_restablecer, {"

pos_start = text.find(start_marker)
pos_end = text.find(end_marker)

if pos_start == -1 or pos_end == -1:
    print("Error: Markers not found!")
    sys.exit(1)

new_block = """output$ui_modal_kgha_cuerpo <- renderUI({
    ctx <- modal_kgha_activo()
    req(ctx)
    
    fila <- ctx$fila
    ha_pasada <- ctx$ha_pasada
    kgha_actual <- ctx$kgha_actual
    kg_pasada <- ctx$kg_pasada
    kgha_proy <- ctx$kgha_proy
    dias <- ctx$dias_ajustados
    kg_orig <- ctx$kg_dias_actuales
    tot_orig <- sum(kg_orig)
    
    # Proporciones porcentuales iniciales
    props_pct <- if (tot_orig > 0) round((kg_orig / tot_orig) * 100, 1) else rep(round(100 / length(dias), 1), length(dias))
    props_pct[length(dias)] <- round(100 - sum(props_pct[seq_len(length(dias)-1)]), 1)
    
    tagList(
      # Datos ocultos para cálculo JS
      tags$input(type = "hidden", id = "modal_kgha_hidden_ha", value = sprintf("%.4f", ha_pasada)),
      tags$input(type = "hidden", id = "modal_kgha_hidden_kg_orig", value = sprintf("%.4f", kg_pasada)),
      
      # 3 Tarjetas de métricas superiores
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
      
      # Input para nuevo rendimiento
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
      
      # Card de distribución editable (porcentajes interactivos por día)
      div(
        style = "background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 9px; padding: 12px 14px; margin-top: 6px;",
        div(
          style = "display: flex; align-items: baseline; justify-content: space-between; margin-bottom: 10px;",
          div(
            style = "display: flex; align-items: center; gap: 8px;",
            tags$div(style = "font-size: 12px; font-weight: 600; color: #334155;", "Distribución proporcional estimada"),
            tags$span(id = "modal_kgha_suma_pct_badge", style = "font-size: 10.5px; font-weight: 600; padding: 1px 7px; border-radius: 6px; background: #ecfdf5; color: #059669; border: 1px solid #a7f3d0;", "100.0%")
          ),
          tags$div(
            style = "font-size: 13.5px; font-weight: 700; color: #0f172a; font-variant-numeric: tabular-nums;",
            "Total: ", tags$span(id = "modal_kgha_tot_kg", sprintf("%s kg", format(round(kg_pasada, 2), big.mark = ","))),
            " (", tags$span(id = "modal_kgha_tot_diff", style = "color: #64748b;", "+0.0 kg"), ")"
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
        ),
        div(
          style = "font-size: 11px; color: #64748b; margin-top: 10px; line-height: 1.35;",
          "Nota: Los ", tags$b("porcentajes son editables"), ". Al guardar, los valores de estos días se convertirán en ",
          tags$b(style = "color: #7c3aed;", "ediciones manuales"),
          " con la proporción indicada y el rendimiento ajustado del turno se actualizará al nuevo objetivo."
        )
      ),
      
      # Script interactivo para recálculo instantáneo en cliente sin pérdida de foco
      tags$script(HTML("
        (function() {
          function recalcularKghaModal(changedDay) {
            var areaPasada = parseFloat($('#modal_kgha_hidden_ha').val()) || 0;
            var kgOrigTotal = parseFloat($('#modal_kgha_hidden_kg_orig').val()) || 0;
            var targetKgha = parseFloat($('#modal_kgha_input_valor').val()) || 0;
            var totalKgNuevo = Math.round(areaPasada * targetKgha * 100) / 100;
            
            var rows = $('.modal-kgha-day-row');
            var nDays = rows.length;
            if (nDays === 0) return;
            
            // Auto-balance automático si son exactamente 2 días
            if (nDays === 2 && changedDay) {
              var otherRow = rows.filter(function() { return $(this).attr('data-day') !== changedDay; });
              var changedRow = rows.filter(function() { return $(this).attr('data-day') === changedDay; });
              var changedInput = changedRow.find('.modal-kgha-pct-input');
              var changedVal = parseFloat(changedInput.val());
              if (isNaN(changedVal)) changedVal = 0;
              if (changedVal < 0) changedVal = 0;
              if (changedVal > 100) changedVal = 100;
              var otherVal = Math.round((100 - changedVal) * 10) / 10;
              otherRow.find('.modal-kgha-pct-input').val(otherVal);
            }
            
            var sumPct = 0;
            rows.each(function() {
              var p = parseFloat($(this).find('.modal-kgha-pct-input').val()) || 0;
              sumPct += p;
            });
            sumPct = Math.round(sumPct * 10) / 10;
            
            var badge = $('#modal_kgha_suma_pct_badge');
            if (Math.abs(sumPct - 100) < 0.1) {
              badge.text('100.0%').css({ background: '#ecfdf5', color: '#059669', borderColor: '#a7f3d0' });
            } else {
              badge.text('Suma: ' + sumPct + '%').css({ background: '#fef2f2', color: '#dc2626', borderColor: '#fecaca' });
            }
            
            var kgAcum = 0;
            var haAcum = 0;
            var diasData = [];
            
            rows.each(function(idx) {
              var row = $(this);
              var day = row.attr('data-day');
              var origKg = parseFloat(row.attr('data-orig-kg')) || 0;
              var pct = parseFloat(row.find('.modal-kgha-pct-input').val()) || 0;
              var prop = sumPct > 0 ? (pct / sumPct) : (1 / nDays);
              
              var dayKg = 0;
              var dayHa = 0;
              if (idx === nDays - 1) {
                dayKg = Math.max(0, Math.round((totalKgNuevo - kgAcum) * 100) / 100);
                dayHa = Math.max(0, Math.round((areaPasada - haAcum) * 100) / 100);
              } else {
                dayKg = Math.round(totalKgNuevo * prop * 100) / 100;
                dayHa = Math.round(areaPasada * prop * 100) / 100;
                kgAcum += dayKg;
                haAcum += dayHa;
              }
              
              var dayDiff = Math.round((dayKg - origKg) * 10) / 10;
              
              row.find('.modal-kgha-day-kilos').text(dayKg.toLocaleString('en-US', { minimumFractionDigits: 2, maximumFractionDigits: 2 }) + ' kg');
              var diffSpan = row.find('.modal-kgha-day-diff');
              diffSpan.text((dayDiff >= 0 ? '+' : '') + dayDiff.toFixed(1) + ' kg');
              diffSpan.css('color', dayDiff >= 0 ? '#16a34a' : '#dc2626');
              
              diasData.push({
                dia: day,
                pct: pct,
                kg: dayKg,
                ha: dayHa
              });
            });
            
            var totDiff = Math.round((totalKgNuevo - kgOrigTotal) * 10) / 10;
            $('#modal_kgha_tot_kg').text(totalKgNuevo.toLocaleString('en-US', { minimumFractionDigits: 2, maximumFractionDigits: 2 }) + ' kg');
            var totDiffSpan = $('#modal_kgha_tot_diff');
            totDiffSpan.text((totDiff >= 0 ? '+' : '') + totDiff.toFixed(1) + ' kg');
            totDiffSpan.css('color', totDiff >= 0 ? '#16a34a' : '#dc2626');
            
            Shiny.setInputValue('modal_kgha_distribucion_actualizada', {
              nuevo_kgha: targetKgha,
              total_kg: totalKgNuevo,
              area_pasada: areaPasada,
              dias: diasData
            }, { priority: 'event' });
          }
          
          $(document).off('input change', '.modal-kgha-pct-input').on('input change', '.modal-kgha-pct-input', function() {
            var day = $(this).attr('data-day');
            recalcularKghaModal(day);
          });
          
          $(document).off('input change', '#modal_kgha_input_valor').on('input change', '#modal_kgha_input_valor', function() {
            recalcularKghaModal(null);
          });
          
          setTimeout(function() { recalcularKghaModal(null); }, 60);
        })();
      "))
    )
  })
  
  observeEvent(input$modal_kgha_btn_copiar_proy, {
    ctx <- modal_kgha_activo()
    req(ctx)
    if (ctx$kgha_proy > 0) {
      updateNumericInput(session, "modal_kgha_input_valor", value = round(ctx$kgha_proy, 2))
    }
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
    
    # Obtener distribución enviada desde el cliente (con porcentajes editados)
    dist_info <- input$modal_kgha_distribucion_actualizada
    
    nuevas_filas <- if (!is.null(dist_info) && length(dist_info$dias) > 0) {
      df_dias_cli <- bind_rows(lapply(dist_info$dias, as_tibble))
      lapply(seq_len(nrow(df_dias_cli)), function(i) {
        d_nom <- as.character(df_dias_cli$dia[i])
        k_val <- round(as.numeric(df_dias_cli$kg[i]), 2)
        h_val <- round(as.numeric(df_dias_cli$ha[i]), 2)
        p_val <- round(as.numeric(df_dias_cli$pct[i]), 1)
        tibble(
          Campaña         = as.character(input$filtro_campana),
          Semana          = as.numeric(input$filtro_semana),
          Sector          = as.character(fila$Sector),
          Módulo          = as.character(fila$Módulo),
          Turno           = as.character(fila$Turno),
          Variedad        = limpiar_variedad(fila$Variedad),
          Campo           = d_nom,
          Valor           = k_val,
          Ha_Asignada     = h_val,
          Ha_Asignada_1ra = if (pasada_sel == 1L) h_val else 0,
          Kg_Asignado_1ra = if (pasada_sel == 1L) k_val else 0,
          Ha_Asignada_2da = if (pasada_sel == 2L) h_val else 0,
          Kg_Asignado_2da = if (pasada_sel == 2L) k_val else 0,
          Tipo_Edicion    = "manual",
          Modo_Ingreso    = "kgha",
          Motivo_Ajuste   = sprintf("Manual (%s kg/ha en %.2f ha, %.1f%%)", format(round(nuevo_kgha, 2)), h_val, p_val),
          Fecha_Edicion   = Sys.time()
        )
      }) %>% bind_rows()
    } else {
      # Fallback proporcional por servidor
      kg_orig <- ctx$kg_dias_actuales
      tot_orig <- sum(kg_orig)
      kg_tot_nuevo <- round(ha_pasada * nuevo_kgha, 2)
      props <- if (tot_orig > 0) kg_orig / tot_orig else rep(1 / length(dias), length(dias))
      kg_dist <- numeric(length(dias))
      ha_dist <- numeric(length(dias))
      kg_acum <- 0
      ha_acum <- 0
      for (i in seq_along(dias)) {
        if (i == length(dias)) {
          kg_dist[i] <- max(0, round(kg_tot_nuevo - kg_acum, 2))
          ha_dist[i] <- max(0, round(ha_pasada - ha_acum, 2))
        } else {
          kg_dist[i] <- round(kg_tot_nuevo * props[i], 2)
          ha_dist[i] <- round(ha_pasada * props[i], 2)
          kg_acum <- kg_acum + kg_dist[i]
          ha_acum <- ha_acum + ha_dist[i]
        }
      }
      lapply(seq_along(dias), function(i) {
        d_nom <- dias[i]
        k_val <- kg_dist[i]
        h_val <- ha_dist[i]
        tibble(
          Campaña         = as.character(input$filtro_campana),
          Semana          = as.numeric(input$filtro_semana),
          Sector          = as.character(fila$Sector),
          Módulo          = as.character(fila$Módulo),
          Turno           = as.character(fila$Turno),
          Variedad        = limpiar_variedad(fila$Variedad),
          Campo           = d_nom,
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
    }
    
    ed_actuales <- ediciones_manuales()
    if (!is.null(ed_actuales) && nrow(ed_actuales) > 0) {
      ed_actuales <- ed_actuales %>% mutate(Campaña = as.character(Campaña), Semana = as.numeric(Semana))
    } else {
      ed_actuales <- tibble()
    }
    
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
  
  """

text_final = text[:pos_start] + new_block + text[pos_end:]

# Preserve CRLF if original had CRLF
if '\r\n' in text:
    text_final = text_final.replace('\r\n', '\n').replace('\n', '\r\n')

with open('Actualizacion Semanal/app.R', 'w', encoding='utf-8', newline='') as f:
    f.write(text_final)

print("Applied editable percentages patch successfully!")
