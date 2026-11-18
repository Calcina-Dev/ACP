# Script to apply Modal Available Ha & Comparativo Sign inversion to app.R
archivo <- "Actualizacion Semanal/app.R"
lineas <- readLines(archivo, encoding = "UTF-8", warn = FALSE)
cat("Line count before:", length(lineas), "\n")

# 1. Modificar abrir_modal_editar_celda (alrededor de linea 8194)
# Localizar "# 3. Calcular hectáreas consumidas en otros días y hectáreas disponibles para este turno"
idx_calc_ha <- grep("# 3. Calcular hectáreas consumidas en otros días y hectáreas disponibles para este turno", lineas, fixed = TRUE)
if (length(idx_calc_ha) == 0) {
  stop("No se encontró el bloque de cálculo de hectáreas en abrir_modal_editar_celda")
}
idx_calc_ha <- idx_calc_ha[1]
cat("idx_calc_ha:", idx_calc_ha, "\n")

# Localizar el final de ese bloque antes de "pasada_sugerida <- if (limpiar_variedad(fila$Variedad) != \"ROCIO\") {"
idx_pas_sug <- grep("pasada_sugerida <- if (limpiar_variedad(fila$Variedad) != \"ROCIO\") {", lineas, fixed = TRUE)
idx_pas_sug <- idx_pas_sug[idx_pas_sug > idx_calc_ha][1]
cat("idx_pas_sug:", idx_pas_sug, "\n")

nuevo_bloque_ha <- c(
  "    # 3. Calcular hectáreas consumidas en días previos reales y otros días de la semana",
  "    dias_semana_todos <- c(\"Lunes\", \"Martes\", \"Miércoles\", \"Jueves\", \"Viernes\", \"Sábado\")",
  "    modo_det_act <- if (!is.null(input$filtro_modo_detalle)) as.character(input$filtro_modo_detalle) else \"real\"",
  "    ",
  "    dias_reales_previos <- character(0)",
  "    if (modo_det_act == \"ajustado\") {",
  "      d_desde_sel <- if (!is.null(input$ajustar_desde_dia)) as.character(input$ajustar_desde_dia) else \"Lunes\"",
  "      idx_desde <- match(d_desde_sel, dias_semana_todos)",
  "      if (is.na(idx_desde)) idx_desde <- 1",
  "      if (idx_desde > 1) {",
  "        dias_reales_previos <- dias_semana_todos[1:(idx_desde - 1)]",
  "      }",
  "    } else {",
  "      idx_col <- match(col_name, dias_semana_todos)",
  "      if (is.na(idx_col)) idx_col <- 1",
  "      if (idx_col > 1) {",
  "        dias_reales_previos <- dias_semana_todos[1:(idx_col - 1)]",
  "      }",
  "    }",
  "    ",
  "    # Ha real ejecutada estrictamente en días previos consolidados para este turno",
  "    ha_real_prev_1ra <- 0",
  "    ha_real_prev_2da <- 0",
  "    if (length(dias_reales_previos) > 0) {",
  "      if (exists(\"df_hist_oficial_wide\") && !is.null(df_hist_oficial_wide) && nrow(df_hist_oficial_wide) > 0) {",
  "        sub_sap_prev <- df_hist_oficial_wide %>%",
  "          filter(",
  "            as.numeric(Semana) == as.numeric(input$filtro_semana),",
  "            as.character(Módulo_Norm) == as.character(fila$Módulo),",
  "            as.character(Turno_Norm)  == as.character(fila$Turno),",
  "            limpiar_variedad(Variedad) == limpiar_variedad(fila$Variedad)",
  "          )",
  "        if (nrow(sub_sap_prev) > 0) {",
  "          cols_ha_prev <- intersect(paste0(\"Ha_\", dias_reales_previos), names(sub_sap_prev))",
  "          if (length(cols_ha_prev) > 0) {",
  "            ha_tot_prev <- sum(rowSums(sub_sap_prev %>% select(all_of(cols_ha_prev)), na.rm = TRUE), na.rm = TRUE)",
  "            if (limpiar_variedad(fila$Variedad) == \"ROCIO\") {",
  "              ha_p2_prev <- sum(rowSums(sub_sap_prev %>% filter(coalesce(as.character(Pasada), \"1\") == \"2\") %>% select(all_of(cols_ha_prev)), na.rm = TRUE), na.rm = TRUE)",
  "              ha_real_prev_2da <- round(min(area_turno, ha_p2_prev), 2)",
  "              ha_real_prev_1ra <- round(min(area_turno, ha_tot_prev - ha_p2_prev), 2)",
  "            } else {",
  "              ha_real_prev_1ra <- round(min(area_turno, ha_tot_prev), 2)",
  "            }",
  "          }",
  "        }",
  "      }",
  "      if (ha_real_prev_1ra <= 0 && ha_real_prev_2da <= 0) {",
  "        rc_disco_prev <- tryCatch(cargar_rutas_cosecha(), error = function(e) NULL)",
  "        if (!is.null(rc_disco_prev) && nrow(rc_disco_prev) > 0 && \"Semana\" %in% names(rc_disco_prev)) {",
  "          rc_sub_prev <- rc_disco_prev %>%",
  "            filter(",
  "              as.numeric(Semana) == as.numeric(input$filtro_semana),",
  "              as.character(Módulo) == as.character(fila$Módulo),",
  "              as.character(Turno)  == as.character(fila$Turno),",
  "              limpiar_variedad(Variedad) == limpiar_variedad(fila$Variedad)",
  "            )",
  "          if (nrow(rc_sub_prev) > 0) {",
  "            cols_rc_prev <- intersect(paste0(\"Ha_\", dias_reales_previos), names(rc_sub_prev))",
  "            if (length(cols_rc_prev) > 0) {",
  "              p1_rc <- rc_sub_prev %>% filter(coalesce(as.integer(Num_Pasada), 1L) == 1L)",
  "              p2_rc <- rc_sub_prev %>% filter(coalesce(as.integer(Num_Pasada), 1L) == 2L)",
  "              if (nrow(p1_rc) > 0) ha_real_prev_1ra <- round(min(area_turno, sum(rowSums(p1_rc %>% select(all_of(cols_rc_prev)), na.rm = TRUE), na.rm = TRUE)), 2)",
  "              if (nrow(p2_rc) > 0) ha_real_prev_2da <- round(min(area_turno, sum(rowSums(p2_rc %>% select(all_of(cols_rc_prev)), na.rm = TRUE), na.rm = TRUE)), 2)",
  "            }",
  "          }",
  "        }",
  "      }",
  "    }",
  "    ",
  "    # Hectáreas asignadas en otros días de la semana (excluyendo el día actual que se edita)",
  "    dias_otros <- setdiff(dias_semana_todos, c(dias_reales_previos, col_name))",
  "    ha_man_otros_1ra <- 0",
  "    ha_man_otros_2da <- 0",
  "    ha_auto_otros_1ra <- 0",
  "    ha_auto_otros_2da <- 0",
  "    ",
  "    # Manuales en otros días",
  "    if (!is.null(ed_actuales) && nrow(ed_actuales) > 0) {",
  "      ed_otros_dias <- ed_actuales %>%",
  "        filter(",
  "          as.character(Campaña) == as.character(input$filtro_campana),",
  "          as.numeric(Semana)    == as.numeric(input$filtro_semana),",
  "          as.character(Módulo)   == as.character(fila$Módulo),",
  "          as.character(Turno)    == as.character(fila$Turno),",
  "          limpiar_variedad(Variedad) == limpiar_variedad(fila$Variedad),",
  "          as.character(Campo)    %in% dias_otros,",
  "          coalesce(Tipo_Edicion, \"\") == \"manual\"",
  "        )",
  "      if (nrow(ed_otros_dias) > 0) {",
  "        ha_man_otros_1ra <- sum(coalesce(as.numeric(ed_otros_dias$Ha_Asignada_1ra), 0), na.rm = TRUE)",
  "        ha_man_otros_2da <- sum(coalesce(as.numeric(ed_otros_dias$Ha_Asignada_2da), 0), na.rm = TRUE)",
  "      }",
  "    }",
  "    ",
  "    # Válvulas de ajuste automático en otros días (si no fueron sobreescritas manualmente)",
  "    dias_man_otros <- if (!is.null(ed_actuales) && nrow(ed_actuales) > 0) {",
  "      unique(ed_actuales %>%",
  "        filter(",
  "          as.character(Campaña) == as.character(input$filtro_campana),",
  "          as.numeric(Semana)    == as.numeric(input$filtro_semana),",
  "          as.character(Módulo)   == as.character(fila$Módulo),",
  "          as.character(Turno)    == as.character(fila$Turno),",
  "          limpiar_variedad(Variedad) == limpiar_variedad(fila$Variedad),",
  "          as.character(Campo)    %in% dias_otros,",
  "          coalesce(Tipo_Edicion, \"\") == \"manual\"",
  "        ) %>% pull(Campo))",
  "    } else character(0)",
  "    ",
  "    dias_auto_otros <- setdiff(dias_otros, dias_man_otros)",
  "    if (length(dias_auto_otros) > 0 && !is.null(valvs_aj) && nrow(valvs_aj) > 0) {",
  "      valvs_otros <- valvs_aj %>%",
  "        filter(",
  "          as.character(Campaña) == as.character(input$filtro_campana),",
  "          as.numeric(Semana)    == as.numeric(input$filtro_semana),",
  "          as.character(Módulo)   == as.character(fila$Módulo),",
  "          as.character(Turno)    == as.character(fila$Turno),",
  "          limpiar_variedad(Variedad) == limpiar_variedad(fila$Variedad),",
  "          as.character(Dia_Asignado) %in% dias_auto_otros",
  "        )",
  "      if (nrow(valvs_otros) > 0) {",
  "        ha_auto_otros_1ra <- sum(valvs_otros$Ha_Asignada[coalesce(valvs_otros$Num_Pasada, 1L) == 1L], na.rm = TRUE)",
  "        ha_auto_otros_2da <- sum(valvs_otros$Ha_Asignada[coalesce(valvs_otros$Num_Pasada, 1L) == 2L], na.rm = TRUE)",
  "      }",
  "    }",
  "    ",
  "    ha_consumida_otros_1ra <- round(ha_real_prev_1ra + ha_man_otros_1ra + ha_auto_otros_1ra, 2)",
  "    ha_consumida_otros_2da <- round(ha_real_prev_2da + ha_man_otros_2da + ha_auto_otros_2da, 2)",
  "    ",
  "    ha_disponible_1ra <- max(0, round(area_turno - ha_consumida_otros_1ra, 2))",
  "    ",
  "    # 2ª Pasada: Cálculo inteligente por válvula y frecuencia para el día específico",
  "    frec_dias_var <- if (limpiar_variedad(fila$Variedad) == \"ROCIO\") 3L else 7L",
  "    frec_df_disco <- tryCatch(cargar_frecuencias_disco(), error = function(e) NULL)",
  "    if (!is.null(frec_df_disco) && nrow(frec_df_disco) > 0) {",
  "      f_row <- frec_df_disco %>% filter(as.character(Módulo) == as.character(fila$Módulo), limpiar_variedad(Variedad) == limpiar_variedad(fila$Variedad))",
  "      if (nrow(f_row) > 0 && !is.na(f_row$Frecuencia_Dias[1])) frec_dias_var <- as.integer(f_row$Frecuencia_Dias[1])",
  "    }",
  "    ",
  "    ha_valvs_2da_dia <- 0",
  "    if (nrow(valvs_celda) > 0 && any(coalesce(valvs_celda$Num_Pasada, 1L) == 2L)) {",
  "      ha_valvs_2da_dia <- sum(valvs_celda$Ha_Asignada[coalesce(valvs_celda$Num_Pasada, 1L) == 2L], na.rm = TRUE)",
  "    }",
  "    if (ha_valvs_2da_dia <= 0) {",
  "      rc_disco <- tryCatch(cargar_rutas_cosecha(), error = function(e) NULL)",
  "      if (!is.null(rc_disco) && nrow(rc_disco) > 0 && \"Semana\" %in% names(rc_disco)) {",
  "        rc_sub_2da <- rc_disco %>%",
  "          filter(",
  "            as.numeric(Semana) == as.numeric(input$filtro_semana),",
  "            as.character(Módulo) == as.character(fila$Módulo),",
  "            as.character(Turno) == as.character(fila$Turno),",
  "            limpiar_variedad(Variedad) == limpiar_variedad(fila$Variedad),",
  "            coalesce(as.integer(Num_Pasada), 1L) == 1L",
  "          )",
  "        if (nrow(rc_sub_2da) > 0) {",
  "          dias_semana_noms <- c(\"Lunes\", \"Martes\", \"Miércoles\", \"Jueves\", \"Viernes\", \"Sábado\")",
  "          for (iv in seq_len(nrow(rc_sub_2da))) {",
  "            v_area_iv <- coalesce(as.numeric(rc_sub_2da$Área[iv]), 0)",
  "            dias_harv <- c()",
  "            # Regla de Negocio ACP: Solo las cosechas de Lunes y Martes generan 2ª pasada intra-semanal",
  "            if (coalesce(as.numeric(rc_sub_2da$Ha_Lunes[iv]), 0) > 0) dias_harv <- c(dias_harv, \"Lunes\")",
  "            if (coalesce(as.numeric(rc_sub_2da$Ha_Martes[iv]), 0) > 0) dias_harv <- c(dias_harv, \"Martes\")",
  "            ",
  "            for (dh in dias_harv) {",
  "              d_esp <- as.character(desplazar_dia_2da_pasada(dh, frec_dias_var, dias_semana_noms, 1L, \"Lunes\", 6L, \"Sábado\"))",
  "              if (identical(d_esp, col_name)) {",
  "                ha_dia_dh <- coalesce(as.numeric(rc_sub_2da[[paste0(\"Ha_\", dh)]][iv]), v_area_iv)",
  "                ha_valvs_2da_dia <- ha_valvs_2da_dia + ifelse(ha_dia_dh > 0, ha_dia_dh, v_area_iv)",
  "                break",
  "              }",
  "            }",
  "          }",
  "        }",
  "      }",
  "    }",
  "    ",
  "    ha_pool_2da_restante <- max(0, round(area_turno - ha_consumida_otros_2da, 2))",
  "    ha_disponible_2da <- if (ha_valvs_2da_dia > 0) {",
  "      round(min(ha_pool_2da_restante, ha_valvs_2da_dia), 2)",
  "    } else {",
  "      ha_pool_2da_restante",
  "    }"
)

# Reemplazar líneas desde idx_calc_ha hasta (idx_pas_sug - 1)
lineas <- c(lineas[1:(idx_calc_ha - 1)], nuevo_bloque_ha, lineas[idx_pas_sug:length(lineas)])
cat("Line count after block 1:", length(lineas), "\n")

# 2. Modificar cálculo de ha_preset y kgha_preset en abrir_modal_editar_celda
idx_ha_pre <- grep("# Hectáreas y rendimiento sugeridos para precargar", lineas, fixed = TRUE)[1]
idx_ctx <- grep("ctx <- list(", lineas, fixed = TRUE)
idx_ctx <- idx_ctx[idx_ctx > idx_ha_pre][1]

nuevo_ha_pre <- c(
  "    # Hectáreas y rendimiento sugeridos para precargar",
  "    ha_preset <- if (nrow(ed_celda) > 0 && !is.na(ed_celda$Ha_Asignada[1]) && ed_celda$Ha_Asignada[1] > 0) {",
  "      round(pmin(area_turno, ed_celda$Ha_Asignada[1]), 2)",
  "    } else if (nrow(valvs_celda) > 0 && sum(valvs_celda$Ha_Asignada, na.rm = TRUE) > 0) {",
  "      round(pmin(area_turno, sum(valvs_celda$Ha_Asignada, na.rm = TRUE)), 2)",
  "    } else {",
  "      ha_disponible_sug",
  "    }",
  "    ",
  "    kg_preset <- round(valor_actual, 2)",
  "    kgha_preset <- if (ha_preset > 0 && kg_preset > 0) {",
  "      round(kg_preset / ha_preset, 2)",
  "    } else if (!is.na(fila$`Kg/Ha Aj. 1ª Pas`) && as.numeric(fila$`Kg/Ha Aj. 1ª Pas`) > 0) {",
  "      round(as.numeric(fila$`Kg/Ha Aj. 1ª Pas`), 2)",
  "    } else if (!is.na(fila$`Kg/Ha Proy`) && as.numeric(fila$`Kg/Ha Proy`) > 0) {",
  "      round(as.numeric(fila$`Kg/Ha Proy`), 2)",
  "    } else {",
  "      100.0",
  "    }"
)

lineas <- c(lineas[1:(idx_ha_pre - 1)], nuevo_ha_pre, lineas[idx_ctx:length(lineas)])
cat("Line count after block 2:", length(lineas), "\n")

# 3. Actualizar ctx list para pasar ha_real_prev_1ra y ha_real_prev_2da
idx_ctx_list <- grep("ctx <- list(", lineas, fixed = TRUE)[1]
idx_ctx_end  <- grep("modal_celda_activa(ctx)", lineas, fixed = TRUE)[1]

nuevo_ctx_list <- c(
  "    ctx <- list(",
  "      fila               = fila,",
  "      campo              = col_name,",
  "      area_turno         = area_turno,",
  "      valor_actual       = valor_actual,",
  "      tipo_celda         = tipo_celda,",
  "      ed_celda           = ed_celda,",
  "      valvs_celda        = valvs_celda,",
  "      ha_preset          = ha_preset,",
  "      kg_preset          = kg_preset,",
  "      kgha_preset        = kgha_preset,",
  "      pasada_sug         = pasada_sugerida,",
  "      ha_disponible_1ra  = ha_disponible_1ra,",
  "      ha_disponible_2da  = ha_disponible_2da,",
  "      ha_disponible_sug  = ha_disponible_sug,",
  "      ha_real_1ra        = ha_real_prev_1ra,",
  "      ha_real_2da        = ha_real_prev_2da,",
  "      ha_man_otros_1ra   = ha_man_otros_1ra + ha_auto_otros_1ra,",
  "      ha_man_otros_2da   = ha_man_otros_2da + ha_auto_otros_2da",
  "    )"
)

lineas <- c(lineas[1:(idx_ctx_list - 1)], nuevo_ctx_list, lineas[idx_ctx_end:length(lineas)])
cat("Line count after block 3:", length(lineas), "\n")

# 4. Actualizar ha_pool_pasada en ui_modal_calculo_vivo y modal_btn_guardar
idx_vivo <- grep("ha_pool_pasada <- if (pasada_activa == 2L) ctx$ha_disponible_2da else max(0, round(ctx$area_turno - ha_real_pasada, 2))", lineas, fixed = TRUE)
cat("Ocurrencias de ha_pool_pasada:", length(idx_vivo), "\n")
for (iv in idx_vivo) {
  lineas[iv] <- "    ha_pool_pasada <- if (pasada_activa == 2L) ctx$ha_disponible_2da else ctx$ha_disponible_1ra"
}

# 5. Comparativo: evaluar_escenario (Diferencia_Kg = Kg_Real_Semana - Kg_Ajustado_Semana)
idx_eval_dif <- grep("Diferencia_Kg      = round(Kg_Ajustado_Semana - Kg_Real_Semana, 1),", lineas, fixed = TRUE)[1]
if (!is.na(idx_eval_dif)) {
  lineas[idx_eval_dif] <- "          Diferencia_Kg      = round(Kg_Real_Semana - Kg_Ajustado_Semana, 1),"
}

idx_eval_pct <- grep("Pct_Desvio         = ifelse(Kg_Real_Semana > 0, round((Diferencia_Kg / Kg_Real_Semana) * 100, 1), NA_real_),", lineas, fixed = TRUE)[1]
if (!is.na(idx_eval_pct)) {
  lineas[idx_eval_pct] <- "          Pct_Desvio         = ifelse(Kg_Ajustado_Semana > 0, round((Diferencia_Kg / Kg_Ajustado_Semana) * 100, 1), ifelse(Kg_Real_Semana > 0, 100, NA_real_)),"
}

# 6. Comparativo: comp_df en vs_escenarios
idx_comp_esc <- grep("Dif_A_vs_Real  = round(Kg_Esc_A - Kg_Real_SAP, 1),", lineas, fixed = TRUE)[1]
if (!is.na(idx_comp_esc)) {
  lineas[idx_comp_esc]     <- "          Dif_A_vs_Real  = round(Kg_Real_SAP - Kg_Esc_A, 1),"
  lineas[idx_comp_esc + 1] <- "          Pct_A_vs_Real  = ifelse(Kg_Esc_A > 0, round((Dif_A_vs_Real / Kg_Esc_A) * 100, 1), ifelse(Kg_Real_SAP > 0, 100, NA_real_)),"
  lineas[idx_comp_esc + 2] <- "          Dif_B_vs_Real  = round(Kg_Real_SAP - Kg_Esc_B, 1),"
  lineas[idx_comp_esc + 3] <- "          Pct_B_vs_Real  = ifelse(Kg_Esc_B > 0, round((Dif_B_vs_Real / Kg_Esc_B) * 100, 1), ifelse(Kg_Real_SAP > 0, 100, NA_real_)),"
}

# 7. Comparativo: comp_sem en semana_completa
idx_comp_sem <- grep("Dif_Aj_vs_Real = round(Kg_Ajustado_Semana - Kg_Real_Semana, 1),", lineas, fixed = TRUE)[1]
if (!is.na(idx_comp_sem)) {
  lineas[idx_comp_sem]     <- "          Dif_Aj_vs_Real = round(Kg_Real_Semana - Kg_Ajustado_Semana, 1),"
  lineas[idx_comp_sem + 1] <- "          Pct_Aj_vs_Real = ifelse(Kg_Ajustado_Semana > 0, round((Dif_Aj_vs_Real / Kg_Ajustado_Semana) * 100, 1), ifelse(Kg_Real_Semana > 0, 100, NA_real_)),"
}

# 8. Comparativo: ui_comparativo_kpis
idx_kpi_sem <- grep("dif_real <- round(tot_aj - tot_real, 0)", lineas, fixed = TRUE)[1]
if (!is.na(idx_kpi_sem)) {
  lineas[idx_kpi_sem]     <- "      dif_real <- round(tot_real - tot_aj, 0)"
  lineas[idx_kpi_sem + 1] <- "      pct_real <- if (tot_aj > 0) round((dif_real / tot_aj) * 100, 1) else if (tot_real > 0) 100 else 0"
}

idx_kpi_esc_a <- grep("dif_a_real <- round(tot_a - tot_real, 0)", lineas, fixed = TRUE)[1]
if (!is.na(idx_kpi_esc_a)) {
  lineas[idx_kpi_esc_a]     <- "      dif_a_real <- round(tot_real - tot_a, 0)"
  lineas[idx_kpi_esc_a + 1] <- "      pct_a_real <- if (tot_a > 0) round((dif_a_real / tot_a) * 100, 1) else if (tot_real > 0) 100 else 0"
}

idx_kpi_esc_b <- grep("dif_b_real <- round(tot_b - tot_real, 0)", lineas, fixed = TRUE)[1]
if (!is.na(idx_kpi_esc_b)) {
  lineas[idx_kpi_esc_b]     <- "      dif_b_real <- round(tot_real - tot_b, 0)"
  lineas[idx_kpi_esc_b + 1] <- "      pct_b_real <- if (tot_b > 0) round((dif_b_real / tot_b) * 100, 1) else if (tot_real > 0) 100 else 0"
}

idx_kpi_main <- grep("dif_neta <- round(tot_aj - tot_real, 0)", lineas, fixed = TRUE)[1]
if (!is.na(idx_kpi_main)) {
  lineas[idx_kpi_main]     <- "      dif_neta <- round(tot_real - tot_aj, 0)"
  lineas[idx_kpi_main + 1] <- "      pct_desvio <- if (tot_aj > 0) round((dif_neta / tot_aj) * 100, 1) else if (tot_real > 0) 100 else 0"
}

# 9. Modificar etiquetas en ui_comparativo_kpis
idx_lbl_kpi_main <- grep("div(class = \"kpi-label\", \"Desviación Neta (Semana)\"),", lineas, fixed = TRUE)[1]
if (!is.na(idx_lbl_kpi_main)) {
  lineas[idx_lbl_kpi_main] <- "              div(class = \"kpi-label\", \"Desviación Real vs Ajuste (Semana)\"),"
}

idx_badge_kpi_main <- grep("sprintf(\"Desvío: %+1.1f%%\", pct_desvio)", lineas, fixed = TRUE)[1]
if (!is.na(idx_badge_kpi_main)) {
  lineas[idx_badge_kpi_main] <- "              span(class = \"kpi-badge\", style = if (abs(pct_desvio) <= 5) \"background: rgba(52, 199, 89, 0.12); color: #34C759;\" else \"background: rgba(245, 158, 11, 0.12); color: #D97706;\", sprintf(\"Δ Real vs Ajuste: %+1.1f%%\", pct_desvio))"
}

# 10. Modificar nombres de barras en grafico_comparativo
idx_bar_name <- grep("name = \"Δ Kilos (Ajuste - Real)\",", lineas, fixed = TRUE)[1]
if (!is.na(idx_bar_name)) {
  lineas[idx_bar_name] <- "        p <- p %>% add_bars(x = ~Turno_Label, y = ~Diferencia_Kg, name = \"Δ Kilos (Real - Ajuste)\", marker = list(color = colors_dif, opacity = 0.88),"
}

idx_bar_hov <- grep("hovertemplate = \"<b>%{x}</b><br>Δ Ajuste vs Real: %{customdata[0]:+,.1f} kg (%{customdata[1]:+1.1f}%)<extra></extra>\",", lineas, fixed = TRUE)[1]
if (!is.na(idx_bar_hov)) {
  lineas[idx_bar_hov] <- "                 hovertemplate = \"<b>%{x}</b><br>Cosecha Real SAP: %{y:,.1f} kg<br>Δ Real vs Ajuste: %{customdata[0]:+,.1f} kg (%{customdata[1]:+1.1f}%)<extra></extra>\","
}

# 11. Modificar columna en tabla_comparativo_detalle
idx_col_dt <- grep("`Δ vs Real` = sprintf(\"%+s kg\", format(round(Diferencia_Kg, 1), big.mark = \",\")),", lineas, fixed = TRUE)[1]
if (!is.na(idx_col_dt)) {
  lineas[idx_col_dt] <- "          `Δ Real vs Ajuste` = sprintf(\"%+s kg\", format(round(Diferencia_Kg, 1), big.mark = \",\")),"
}

# Escribir archivo actualizado
writeLines(lineas, archivo, useBytes = TRUE)
cat("Archivo app.R actualizado exitosamente. Total líneas:", length(lineas), "\n")
