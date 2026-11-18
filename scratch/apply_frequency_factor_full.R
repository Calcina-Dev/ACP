lines <- readLines("Actualizacion Semanal/app.R", encoding = "UTF-8")

# 1. Update banner_cambio_dia
idx_start_banner <- grep('# Banner de retroalimentación de cambio de día', lines)
idx_end_banner   <- grep('cuerpo_calculo <- if \\(!is.null\\(modo\\) && modo == "kgha"\\)', lines) - 1

cat(sprintf("Banner range: %d to %d\n", idx_start_banner, idx_end_banner))

new_banner <- c(
  "    # Banner de retroalimentación de cambio de día con Factor de Frecuencia",
  "    banner_cambio_dia <- if (dia_destino != dia_origen) {",
  "      if (isTRUE(ctx$fila[[paste0(dia_destino, \"_bloqueado\")]])) {",
  "        div(",
  "          style = \"background: #fff1f2; border: 1px solid #fecdd3; border-radius: 8px; padding: 10px 14px; margin-bottom: 14px;\",",
  "          tags$div(style = \"font-size: 12px; font-weight: 600; color: #9f1239; display: flex; align-items: center; gap: 6px;\",",
  "                   tags$span(style = \"display: inline-block; width: 6px; height: 6px; border-radius: 50%; background: #e11d48;\"),",
  "                   sprintf(\"%s ya tiene recepción oficial en báscula SAP\", dia_destino)),",
  "          tags$div(style = \"font-size: 11px; color: #be123c; margin-top: 2px;\", \"Este día está bloqueado por recepción oficial. Por favor selecciona otro día.\")",
  "        )",
  "      } else {",
  "        dias_map <- setNames(1:6, c(\"Lunes\", \"Martes\", \"Miércoles\", \"Jueves\", \"Viernes\", \"Sábado\"))",
  "        diff_dias <- as.integer(dias_map[dia_destino] - dias_map[dia_origen])",
  "        ",
  "        # Frecuencia base del turno/variedad",
  "        frec_base <- 7",
  "        df_frec_sub <- tryCatch(rv_frecuencias(), error = function(e) NULL)",
  "        if (is.null(df_frec_sub) || nrow(df_frec_sub) == 0) df_frec_sub <- cargar_frecuencias_disco()",
  "        if (!is.null(df_frec_sub) && nrow(df_frec_sub) > 0) {",
  "          f_match <- df_frec_sub %>% filter(as.character(Módulo) == as.character(ctx$fila$Módulo),",
  "                                            limpiar_variedad(Variedad) == limpiar_variedad(ctx$fila$Variedad))",
  "          if (nrow(f_match) > 0 && !is.na(f_match$Frecuencia_Dias[1])) frec_base <- as.numeric(f_match$Frecuencia_Dias[1])",
  "        } else if (limpiar_variedad(ctx$fila$Variedad) == \"ROCIO\") {",
  "          frec_base <- 4",
  "        }",
  "        ",
  "        elast_pct <- tryCatch(rv_elasticidad_frecuencia(), error = function(e) 70)",
  "        info_frec <- calcular_factor_frecuencia(diff_dias, frec_base, elast_pct)",
  "        ",
  "        es_kgha_modo <- (!is.null(modo) && modo == \"kgha\")",
  "        if (es_kgha_modo) {",
  "          val_orig <- coalesce(as.numeric(ctx$kgha_preset), 0)",
  "          val_sug  <- round(val_orig * info_frec$factor, 1)",
  "          delta_val <- round(val_sug - val_orig, 1)",
  "          unidad_str <- \"kg/ha\"",
  "        } else {",
  "          val_orig <- coalesce(as.numeric(ctx$valor_actual), 0)",
  "          val_sug  <- round(val_orig * info_frec$factor, 0)",
  "          delta_val <- round(val_sug - val_orig, 0)",
  "          unidad_str <- \"kg\"",
  "        }",
  "        ",
  "        badge_bg  <- if (info_frec$delta_pct > 0) \"rgba(88, 86, 214, 0.10)\" else if (info_frec$delta_pct < 0) \"rgba(255, 59, 48, 0.10)\" else \"#F4F4F5\"",
  "        badge_col <- if (info_frec$delta_pct > 0) \"#5856D6\" else if (info_frec$delta_pct < 0) \"#D32F2F\" else \"#636366\"",
  "        badge_bdr <- if (info_frec$delta_pct > 0) \"rgba(88, 86, 214, 0.25)\" else if (info_frec$delta_pct < 0) \"rgba(255, 59, 48, 0.25)\" else \"#E5E5EA\"",
  "        ",
  "        div(",
  "          style = \"background: #F9F9FB; border: 1.5px solid rgba(88, 86, 214, 0.25); border-radius: 10px; padding: 12px 14px; margin-bottom: 14px;\",",
  "          div(",
  "            style = \"display: flex; justify-content: space-between; align-items: center; margin-bottom: 6px; flex-wrap: wrap; gap: 6px;\",",
  "            div(",
  "              style = \"display: flex; align-items: center; gap: 6px;\",",
  "              tags$span(style = \"width: 7px; height: 7px; border-radius: 50%; background: #5856D6; display: inline-block;\"),",
  "              tags$span(style = \"font-size: 12.5px; font-weight: 700; color: #1C1C1E;\", sprintf(\"Moviendo: %s ➔ %s (%+d día%s)\", dia_origen, dia_destino, diff_dias, if (abs(diff_dias) == 1) \"\" else \"s\"))",
  "            ),",
  "            tags$span(",
  "              style = sprintf(\"background: %s; color: %s; border: 1px solid %s; font-size: 10.5px; font-weight: 700; padding: 2px 8px; border-radius: 6px; font-variant-numeric: tabular-nums;\", badge_bg, badge_col, badge_bdr),",
  "              sprintf(\"Factor Frecuencia: %+.1f%%\", info_frec$delta_pct)",
  "            )",
  "          ),",
  "          div(",
  "            style = \"font-size: 11.5px; color: #48484A; line-height: 1.45; margin-bottom: 8px;\",",
  "            if (diff_dias > 0) {",
  "              HTML(sprintf(\"Al postergar la cosecha <b>+%d días</b> (intervalo: %d d vs base %d d), las plantas acumulan fruta madura adicional (+%.1f%%). Sugerido con factor: <b style='color:#5856D6;'>%s %s</b> (%+s %s vs original).\",",
  "                           diff_dias, frec_base + diff_dias, frec_base, info_frec$delta_pct, format(val_sug, big.mark = \",\"), unidad_str, format(delta_val, big.mark = \",\"), unidad_str))",
  "            } else {",
  "              HTML(sprintf(\"Al adelantar la cosecha <b>%d días</b> (intervalo: %d d vs base %d d), el periodo de maduración en planta es menor (%.1f%%). Sugerido con factor: <b style='color:#D32F2F;'>%s %s</b> (%+s %s vs original).\",",
  "                           abs(diff_dias), frec_base + diff_dias, frec_base, info_frec$delta_pct, format(val_sug, big.mark = \",\"), unidad_str, format(delta_val, big.mark = \",\"), unidad_str))",
  "            }",
  "          ),",
  "          if (val_orig > 0) {",
  "            div(",
  "              style = \"display: flex; justify-content: flex-end; margin-top: 4px;\",",
  "              actionButton(",
  "                \"btn_aplicar_factor_frecuencia_modal\",",
  "                label = sprintf(\"Aplicar Sugerencia: %s %s (%+s %s)\", format(val_sug, big.mark = \",\"), unidad_str, format(delta_val, big.mark = \",\"), unidad_str),",
  "                style = \"font-size: 11px; font-weight: 600; padding: 6px 14px; border-radius: 7px; background: #1C1C1E; color: #FFFFFF; border: none; box-shadow: 0 1px 2px rgba(0,0,0,0.12); cursor: pointer; transition: all 0.15s ease;\"",
  "              )",
  "            )",
  "          }",
  "        )",
  "      }",
  "    }",
  "    "
)

lines <- c(lines[1:(idx_start_banner - 1)], new_banner, lines[(idx_end_banner + 1):length(lines)])

# 2. Add renderUI for ui_simulacion_elasticidad_frecuencia right after observer of input_elasticidad_frecuencia
idx_obs_save <- grep('guardar_elasticidad_frecuencia_disco\\(val_clamped\\)', lines)
cat(sprintf("Observer save found at line: %d\n", idx_obs_save))
# find the closing brace of observeEvent(input$input_elasticidad_frecuencia
idx_obs_end <- idx_obs_save + 1
while (idx_obs_end <= length(lines) && !grepl("^\\s*\\}\\)", lines[idx_obs_end])) {
  idx_obs_end <- idx_obs_end + 1
}
cat(sprintf("Observer closes at line: %d\n", idx_obs_end))

simulacion_server <- c(
  "",
  "  # Tira de simulación reactiva de respuesta al mover días por elasticidad de frecuencia",
  "  output$ui_simulacion_elasticidad_frecuencia <- renderUI({",
  "    elast <- if (!is.null(input$input_elasticidad_frecuencia) && !is.na(input$input_elasticidad_frecuencia)) {",
  "      as.numeric(input$input_elasticidad_frecuencia)",
  "    } else {",
  "      tryCatch(rv_elasticidad_frecuencia(), error = function(e) 70)",
  "    }",
  "    ",
  "    div(",
  "      style = \"display: grid; grid-template-columns: repeat(7, 1fr); gap: 6px; text-align: center;\",",
  "      lapply(c(-3, -2, -1, 0, 1, 2, 3), function(d) {",
  "        f_obj <- calcular_factor_frecuencia(d, frec_base = 7, elasticidad_pct = elast)",
  "        es_cero <- (d == 0)",
  "        col_bg <- if (es_cero) \"#1C1C1E\" else if (d > 0) \"rgba(88, 86, 214, 0.08)\" else \"rgba(255, 59, 48, 0.08)\"",
  "        col_tx <- if (es_cero) \"#FFFFFF\" else if (d > 0) \"#5856D6\" else \"#D32F2F\"",
  "        bdr    <- if (es_cero) \"#1C1C1E\" else if (d > 0) \"rgba(88, 86, 214, 0.25)\" else \"rgba(255, 59, 48, 0.25)\"",
  "        ",
  "        div(",
  "          style = sprintf(\"background: %s; border: 1px solid %s; border-radius: 8px; padding: 6px 4px; transition: all 0.15s ease;\", col_bg, bdr),",
  "          tags$div(style = sprintf(\"font-size: 10px; font-weight: 600; color: %s;\", if (es_cero) \"rgba(255,255,255,0.7)\" else \"#8E8E93\"), sprintf(\"%+d día%s\", d, if (abs(d) == 1) \"\" else \"s\")),",
  "          tags$div(style = sprintf(\"font-size: 12px; font-weight: 700; color: %s; font-variant-numeric: tabular-nums;\", col_tx), if (es_cero) \"100%\" else sprintf(\"%+.1f%%\", f_obj$delta_pct)),",
  "          tags$div(style = sprintf(\"font-size: 9.5px; color: %s;\", if (es_cero) \"rgba(255,255,255,0.85)\" else \"#636366\"), sprintf(\"%d días\", 7 + d))",
  "        )",
  "      })",
  "    )",
  "  })"
)

lines <- c(lines[1:idx_obs_end], simulacion_server, lines[(idx_obs_end + 1):length(lines)])

# 3. Add graphical calibration section in ui_modal_frecuencias_contenido
idx_cards <- grep('# Rejilla de tarjetas por Módulo y Variedad', lines)
cat(sprintf("Rejilla cards found at line: %d\n", idx_cards))

graphical_section <- c(
  "      # Sección: Sensibilidad de Acumulación por Frecuencia (Factor al Mover Días)",
  "      div(",
  "        style = \"background: #F8F9FA; border: 1px solid #E5E5EA; border-radius: 12px; padding: 13px 16px; margin-bottom: 14px;\",",
  "        div(",
  "          style = \"display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px; flex-wrap: wrap; gap: 8px;\",",
  "          div(",
  "            tags$div(",
  "              style = \"font-size: 13px; font-weight: 700; color: #1C1C1E; display: flex; align-items: center; gap: 6px;\",",
  "              tags$span(style = \"width: 7px; height: 7px; border-radius: 50%; background: #5856D6; display: inline-block;\"),",
  "              \"Sensibilidad de Acumulación: Factor de Frecuencia al Mover Cosecha\"",
  "            ),",
  "            tags$div(",
  "              style = \"font-size: 11px; color: #636366; margin-top: 2px;\",",
  "              \"Modela el % de acumulación o pérdida de fruta madura cuando se adelanta o posterga una cosecha.\"",
  "            )",
  "          ),",
  "          div(",
  "            style = \"display: flex; align-items: center; gap: 8px;\",",
  "            tags$span(style = \"font-size: 10.5px; font-weight: 600; color: #5856D6; background: rgba(88, 86, 214, 0.08); border: 1px solid rgba(88, 86, 214, 0.20); padding: 2px 8px; border-radius: 6px; font-variant-numeric: tabular-nums;\", \"Recomendado: 60% - 80%\"),",
  "            div(",
  "              style = \"display: flex; align-items: center; gap: 4px;\",",
  "              numericInput(",
  "                \"input_elasticidad_frecuencia\",",
  "                label = NULL,",
  "                value = isolate(rv_elasticidad_frecuencia()),",
  "                min = 0,",
  "                max = 100,",
  "                step = 5,",
  "                width = \"75px\"",
  "              ),",
  "              tags$span(style = \"font-size: 12px; font-weight: 700; color: #1C1C1E;\", \"%\")",
  "            )",
  "          )",
  "        ),",
  "        # Simulación de Respuesta Gráfica en vivo para base 7 días",
  "        div(",
  "          style = \"background: #FFFFFF; border: 1px solid rgba(0,0,0,0.06); border-radius: 10px; padding: 10px 12px;\",",
  "          tags$div(",
  "            style = \"display: flex; justify-content: space-between; align-items: center; margin-bottom: 6px; font-size: 11px; font-weight: 600; color: #8E8E93;\",",
  "            tags$span(\"Simulación de Respuesta Dinámica al Mover Días (Base 7 días):\"),",
  "            tags$span(style = \"font-weight: 400;\", \"Kilos esperados por pase\")",
  "          ),",
  "          uiOutput(\"ui_simulacion_elasticidad_frecuencia\")",
  "        )",
  "      ),",
  "      "
)

lines <- c(lines[1:(idx_cards - 1)], graphical_section, lines[idx_cards:length(lines)])

writeLines(lines, "Actualizacion Semanal/app.R", useBytes = TRUE)
cat("Successfully applied Full UI and Banner enhancements!\n")
