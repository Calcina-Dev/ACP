app_path <- "D:/ACP/Actualizacion Semanal/app.R"
content <- paste(readLines(app_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

replace_target <- function(content, name, target, replacement, expected_count = 1) {
  matches <- gregexpr(target, content, fixed = TRUE)[[1]]
  actual_count <- if (length(matches) == 1 && matches[1] == -1) 0 else length(matches)
  if (actual_count != expected_count) {
    stop(sprintf("Target count mismatch for %s: expected %d, got %d", name, expected_count, actual_count))
  }
  content_new <- gsub(target, replacement, content, fixed = TRUE)
  cat(sprintf("[OK] Replaced %s (%d occurrences)\n", name, actual_count))
  return(content_new)
}

# 1. Target 1: cargar_ediciones_disco and guardar_ediciones_disco
target_1 <- '      Ha_Asignada_1ra = if ("Ha_Asignada_1ra" %in% names(.)) suppressWarnings(as.numeric(Ha_Asignada_1ra)) else NA_real_,
      Kg_Asignado_1ra = if ("Kg_Asignado_1ra" %in% names(.)) suppressWarnings(as.numeric(Kg_Asignado_1ra)) else NA_real_,
      Ha_Asignada_2da = if ("Ha_Asignada_2da" %in% names(.)) suppressWarnings(as.numeric(Ha_Asignada_2da)) else NA_real_,
      Kg_Asignado_2da = if ("Kg_Asignado_2da" %in% names(.)) suppressWarnings(as.numeric(Kg_Asignado_2da)) else NA_real_,'

replacement_1 <- '      Ha_Asignada_1ra = if ("Ha_Asignada_1ra" %in% names(.)) {
        ifelse(Variedad != "ROCIO", coalesce(suppressWarnings(as.numeric(Ha_Asignada)), suppressWarnings(as.numeric(Ha_Asignada_1ra))), suppressWarnings(as.numeric(Ha_Asignada_1ra)))
      } else if (Variedad != "ROCIO") suppressWarnings(as.numeric(Ha_Asignada)) else NA_real_,
      Kg_Asignado_1ra = if ("Kg_Asignado_1ra" %in% names(.)) {
        ifelse(Variedad != "ROCIO", coalesce(suppressWarnings(as.numeric(Valor)), suppressWarnings(as.numeric(Kg_Asignado_1ra))), suppressWarnings(as.numeric(Kg_Asignado_1ra)))
      } else if (Variedad != "ROCIO") suppressWarnings(as.numeric(Valor)) else NA_real_,
      Ha_Asignada_2da = if ("Ha_Asignada_2da" %in% names(.)) {
        ifelse(Variedad != "ROCIO", 0, suppressWarnings(as.numeric(Ha_Asignada_2da)))
      } else 0,
      Kg_Asignado_2da = if ("Kg_Asignado_2da" %in% names(.)) {
        ifelse(Variedad != "ROCIO", 0, suppressWarnings(as.numeric(Kg_Asignado_2da)))
      } else 0,'

content <- replace_target(content, "Target 1 (cargar & guardar)", target_1, replacement_1, 2)

# 2. Target 2: ha_ajustadas_ed
target_2 <- '            Ha_val   = if ("Ha_Asignada" %in% names(.)) suppressWarnings(as.numeric(Ha_Asignada)) else NA_real_,
            Ha_1ra   = if ("Ha_Asignada_1ra" %in% names(.)) suppressWarnings(as.numeric(Ha_Asignada_1ra)) else NA_real_,
            Kg_1ra   = if ("Kg_Asignado_1ra" %in% names(.)) suppressWarnings(as.numeric(Kg_Asignado_1ra)) else NA_real_,
            Ha_2da   = if ("Ha_Asignada_2da" %in% names(.)) suppressWarnings(as.numeric(Ha_Asignada_2da)) else NA_real_,
            Kg_2da   = if ("Kg_Asignado_2da" %in% names(.)) suppressWarnings(as.numeric(Kg_Asignado_2da)) else NA_real_,
            Mot_val  = if ("Motivo_Ajuste" %in% names(.)) as.character(Motivo_Ajuste) else NA_character_
          ) %>%
          group_by(Módulo_k, Turno_k, Var_k) %>%
          summarise(
            Ha_Ajustada_Total = round(sum(Ha_val, na.rm = TRUE), 2),
            Ha_Aj_1ra_Ed      = round(sum(Ha_1ra, na.rm = TRUE), 2),
            Kg_Aj_1ra_Ed      = round(sum(Kg_1ra, na.rm = TRUE), 2),
            Ha_Aj_2da_Ed      = round(sum(Ha_2da, na.rm = TRUE), 2),
            Kg_Aj_2da_Ed      = round(sum(Kg_2da, na.rm = TRUE), 2),
            Tiene_Ha_Asig     = any(!is.na(Ha_val) & Ha_val > 0),
            Tiene_Detalle_Pas = any(!is.na(Ha_1ra) | !is.na(Ha_2da)),'

replacement_2 <- '            es_rocio = (Var_k == "ROCIO"),
            Ha_val   = if ("Ha_Asignada" %in% names(.)) suppressWarnings(as.numeric(Ha_Asignada)) else NA_real_,
            Ha_1ra   = if ("Ha_Asignada_1ra" %in% names(.)) {
              ifelse(es_rocio, suppressWarnings(as.numeric(Ha_Asignada_1ra)), suppressWarnings(as.numeric(Ha_Asignada)))
            } else if (!es_rocio) suppressWarnings(as.numeric(Ha_Asignada)) else NA_real_,
            Kg_1ra   = if ("Kg_Asignado_1ra" %in% names(.)) {
              ifelse(es_rocio, suppressWarnings(as.numeric(Kg_Asignado_1ra)), suppressWarnings(as.numeric(Valor)))
            } else if (!es_rocio) suppressWarnings(as.numeric(Valor)) else NA_real_,
            Ha_2da   = if ("Ha_Asignada_2da" %in% names(.)) {
              ifelse(es_rocio, suppressWarnings(as.numeric(Ha_Asignada_2da)), 0)
            } else 0,
            Kg_2da   = if ("Kg_Asignado_2da" %in% names(.)) {
              ifelse(es_rocio, suppressWarnings(as.numeric(Kg_Asignado_2da)), 0)
            } else 0,
            Mot_val  = if ("Motivo_Ajuste" %in% names(.)) as.character(Motivo_Ajuste) else NA_character_
          ) %>%
          group_by(Módulo_k, Turno_k, Var_k) %>%
          summarise(
            Ha_Ajustada_Total = round(sum(Ha_val, na.rm = TRUE), 2),
            Ha_Aj_1ra_Ed      = round(sum(Ha_1ra, na.rm = TRUE), 2),
            Kg_Aj_1ra_Ed      = round(sum(Kg_1ra, na.rm = TRUE), 2),
            Ha_Aj_2da_Ed      = round(sum(Ha_2da, na.rm = TRUE), 2),
            Kg_Aj_2da_Ed      = round(sum(Kg_2da, na.rm = TRUE), 2),
            Tiene_Ha_Asig     = any(!is.na(Ha_val) & Ha_val > 0),
            Tiene_Detalle_Pas = first(es_rocio) & any(!is.na(Ha_1ra) | !is.na(Ha_2da)),'

content <- replace_target(content, "Target 2 (ha_ajustadas_ed)", target_2, replacement_2, 1)

# 3. Target 3: Ha Aj. 1ª Pas
target_3 <- '        # 1ª Pasada Ajustada: Faltante para completar el turno físico
        `Ha Aj. 1ª Pas` = case_when(
          Tipo == "Testblock" ~ 0,
          !Tiene_Dias_Ajustados ~ 0,
          coalesce(Tiene_Detalle_Pas, FALSE) & coalesce(Ha_Aj_1ra_Ed, 0) > 0 ~ round(pmin(Área, Ha_Aj_1ra_Ed), 2),'

replacement_3 <- '        # 1ª Pasada Ajustada: Faltante para completar el turno físico
        `Ha Aj. 1ª Pas` = case_when(
          Tipo == "Testblock" ~ 0,
          !Tiene_Dias_Ajustados ~ 0,
          limpiar_variedad(Variedad) != "ROCIO" & coalesce(Tiene_Ha_Asig, FALSE) & coalesce(Ha_Ajustada_Total, 0) > 0 ~ round(pmin(Área, Ha_Ajustada_Total), 2),
          coalesce(Tiene_Detalle_Pas, FALSE) & coalesce(Ha_Aj_1ra_Ed, 0) > 0 ~ round(pmin(Área, Ha_Aj_1ra_Ed), 2),'

content <- replace_target(content, "Target 3 (Ha Aj. 1ra Pas)", target_3, replacement_3, 1)

# 4. Target 4: Kg_Aj_1ra
target_4 <- '        Kg_Aj_1ra = case_when(
          Tipo == "Testblock" ~ 0,
          !Tiene_Dias_Ajustados ~ 0,
          coalesce(Tiene_Detalle_Pas, FALSE) & (coalesce(Kg_Aj_1ra_Ed, 0) + coalesce(Kg_Aj_2da_Ed, 0)) > 0 ~'

replacement_4 <- '        Kg_Aj_1ra = case_when(
          Tipo == "Testblock" ~ 0,
          !Tiene_Dias_Ajustados ~ 0,
          limpiar_variedad(Variedad) != "ROCIO" ~ kg_aj_neto_u,
          coalesce(Tiene_Detalle_Pas, FALSE) & (coalesce(Kg_Aj_1ra_Ed, 0) + coalesce(Kg_Aj_2da_Ed, 0)) > 0 ~'

content <- replace_target(content, "Target 4 (Kg_Aj_1ra)", target_4, replacement_4, 1)

# 5. Target 5: pasada_sugerida
target_5 <- '    pasada_sugerida <- if (nrow(valvs_celda) > 0 && any(valvs_celda$Num_Pasada == 2)) {
      2L
    } else if (nrow(ed_celda) > 0 && coalesce(ed_celda$Ha_Asignada_2da[1], 0) > 0) {
      2L
    } else if (limpiar_variedad(fila$Variedad) == "ROCIO" && ha_real_1ra >= (area_turno - 0.05)) {
      2L
    } else {
      1L
    }'

replacement_5 <- '    pasada_sugerida <- if (limpiar_variedad(fila$Variedad) != "ROCIO") {
      1L
    } else if (nrow(valvs_celda) > 0 && any(valvs_celda$Num_Pasada == 2)) {
      2L
    } else if (nrow(ed_celda) > 0 && coalesce(ed_celda$Ha_Asignada_2da[1], 0) > 0) {
      2L
    } else if (limpiar_variedad(fila$Variedad) == "ROCIO" && ha_real_1ra >= (area_turno - 0.05)) {
      2L
    } else {
      1L
    }'

content <- replace_target(content, "Target 5 (pasada_sugerida)", target_5, replacement_5, 1)

# 6. Target 6: pasada_activa in renderUI
target_6 <- '    dia_origen <- as.character(ctx$campo)
    dia_destino <- if (!is.null(input$modal_input_dia) && nzchar(input$modal_input_dia)) as.character(input$modal_input_dia) else dia_origen
    
    pasada_activa <- if (!is.null(input$modal_input_pasada)) as.integer(input$modal_input_pasada) else ctx$pasada_sug
    ha_real_pasada <- if (pasada_activa == 2L) ctx$ha_real_2da else ctx$ha_real_1ra'

replacement_6 <- '    dia_origen <- as.character(ctx$campo)
    dia_destino <- if (!is.null(input$modal_input_dia) && nzchar(input$modal_input_dia)) as.character(input$modal_input_dia) else dia_origen
    
    pasada_activa <- if (limpiar_variedad(ctx$fila$Variedad) == "ROCIO") {
      if (!is.null(input$modal_input_pasada)) as.integer(input$modal_input_pasada) else ctx$pasada_sug
    } else {
      1L
    }
    ha_real_pasada <- if (pasada_activa == 2L) ctx$ha_real_2da else ctx$ha_real_1ra'

content <- replace_target(content, "Target 6 (pasada_activa renderUI)", target_6, replacement_6, 1)

# 7. Target 7: pasada_activa in modal_btn_guardar
target_7 <- '    modo <- input$modal_modo_ingreso
    pasada_activa <- if (!is.null(input$modal_input_pasada)) as.integer(input$modal_input_pasada) else ctx$pasada_sug
    ha_real_pasada <- if (pasada_activa == 2L) ctx$ha_real_2da else ctx$ha_real_1ra'

replacement_7 <- '    modo <- input$modal_modo_ingreso
    pasada_activa <- if (limpiar_variedad(ctx$fila$Variedad) == "ROCIO") {
      if (!is.null(input$modal_input_pasada)) as.integer(input$modal_input_pasada) else ctx$pasada_sug
    } else {
      1L
    }
    ha_real_pasada <- if (pasada_activa == 2L) ctx$ha_real_2da else ctx$ha_real_1ra'

content <- replace_target(content, "Target 7 (pasada_activa modal_btn_guardar)", target_7, replacement_7, 1)

# 8. Target 8: num_pasada_sel
target_8 <- '    num_pasada_sel <- pasada_activa
    motivo_txt <- if (!is.null(input$modal_input_motivo)) trimws(as.character(input$modal_input_motivo)) else ""'

replacement_8 <- '    num_pasada_sel <- if (limpiar_variedad(ctx$fila$Variedad) == "ROCIO") pasada_activa else 1L
    motivo_txt <- if (!is.null(input$modal_input_motivo)) trimws(as.character(input$modal_input_motivo)) else ""'

content <- replace_target(content, "Target 8 (num_pasada_sel)", target_8, replacement_8, 1)

# 9. Target 9: pasada_activa in modal_btn_restablecer
target_9 <- '  observeEvent(input$modal_btn_restablecer, {
    ctx <- modal_celda_activa()
    req(ctx)
    
    pasada_activa <- if (nrow(ctx$ed_celda) > 0 && coalesce(ctx$ed_celda$Ha_Asignada_2da[1], 0) > 0) 2L else ctx$pasada_sug'

replacement_9 <- '  observeEvent(input$modal_btn_restablecer, {
    ctx <- modal_celda_activa()
    req(ctx)
    
    pasada_activa <- if (limpiar_variedad(ctx$fila$Variedad) == "ROCIO" && nrow(ctx$ed_celda) > 0 && coalesce(ctx$ed_celda$Ha_Asignada_2da[1], 0) > 0) {
      2L
    } else if (limpiar_variedad(ctx$fila$Variedad) == "ROCIO") {
      ctx$pasada_sug
    } else {
      1L
    }'

content <- replace_target(content, "Target 9 (pasada_activa modal_btn_restablecer)", target_9, replacement_9, 1)

# Write to app.R
writeLines(strsplit(content, "\n", fixed = TRUE)[[1]], app_path, sep = "\r\n", useBytes = TRUE)
cat("\nFile written successfully to:", app_path, "\n")

# Verify syntax
p <- parse(app_path)
cat(sprintf("SUCCESS: app.R parsed with 0 syntax errors! (%d top-level expressions)\n", length(p)))
