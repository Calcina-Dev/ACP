app_path <- "D:/ACP/Actualizacion Semanal/app.R"
content <- paste(readLines(app_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

check_target <- function(name, target, expected_count = 1) {
  matches <- gregexpr(target, content, fixed = TRUE)[[1]]
  actual_count <- if (length(matches) == 1 && matches[1] == -1) 0 else length(matches)
  cat(sprintf("[%s] Expected: %d, Found: %d\n", name, expected_count, actual_count))
  if (actual_count != expected_count) {
    cat(sprintf("ERROR: Target mismatch for %s!\n", name))
  }
}

# Target 1: Ha_Asignada_1ra in cargar_ediciones_disco and guardar_ediciones_disco
target_1 <- '      Ha_Asignada_1ra = if ("Ha_Asignada_1ra" %in% names(.)) suppressWarnings(as.numeric(Ha_Asignada_1ra)) else NA_real_,
      Kg_Asignado_1ra = if ("Kg_Asignado_1ra" %in% names(.)) suppressWarnings(as.numeric(Kg_Asignado_1ra)) else NA_real_,
      Ha_Asignada_2da = if ("Ha_Asignada_2da" %in% names(.)) suppressWarnings(as.numeric(Ha_Asignada_2da)) else NA_real_,
      Kg_Asignado_2da = if ("Kg_Asignado_2da" %in% names(.)) suppressWarnings(as.numeric(Kg_Asignado_2da)) else NA_real_,'
check_target("Target 1 (cargar & guardar)", target_1, 2)

# Target 2: ha_ajustadas_ed
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
check_target("Target 2 (ha_ajustadas_ed)", target_2, 1)

# Target 3: Ha Aj. 1ª Pas
target_3 <- '        # 1ª Pasada Ajustada: Faltante para completar el turno físico
        `Ha Aj. 1ª Pas` = case_when(
          Tipo == "Testblock" ~ 0,
          !Tiene_Dias_Ajustados ~ 0,
          coalesce(Tiene_Detalle_Pas, FALSE) & coalesce(Ha_Aj_1ra_Ed, 0) > 0 ~ round(pmin(Área, Ha_Aj_1ra_Ed), 2),'
check_target("Target 3 (Ha Aj. 1ra Pas)", target_3, 1)

# Target 4: Kg_Aj_1ra
target_4 <- '        Kg_Aj_1ra = case_when(
          Tipo == "Testblock" ~ 0,
          !Tiene_Dias_Ajustados ~ 0,
          coalesce(Tiene_Detalle_Pas, FALSE) & (coalesce(Kg_Aj_1ra_Ed, 0) + coalesce(Kg_Aj_2da_Ed, 0)) > 0 ~'
check_target("Target 4 (Kg_Aj_1ra)", target_4, 1)

# Target 5: pasada_sugerida
target_5 <- '    pasada_sugerida <- if (nrow(valvs_celda) > 0 && any(valvs_celda$Num_Pasada == 2)) {
      2L
    } else if (nrow(ed_celda) > 0 && coalesce(ed_celda$Ha_Asignada_2da[1], 0) > 0) {
      2L
    } else if (limpiar_variedad(fila$Variedad) == "ROCIO" && ha_real_1ra >= (area_turno - 0.05)) {
      2L
    } else {
      1L
    }'
check_target("Target 5 (pasada_sugerida)", target_5, 1)

# Target 6: pasada_activa in renderUI
target_6 <- '    dia_origen <- as.character(ctx$campo)
    dia_destino <- if (!is.null(input$modal_input_dia) && nzchar(input$modal_input_dia)) as.character(input$modal_input_dia) else dia_origen
    
    pasada_activa <- if (!is.null(input$modal_input_pasada)) as.integer(input$modal_input_pasada) else ctx$pasada_sug
    ha_real_pasada <- if (pasada_activa == 2L) ctx$ha_real_2da else ctx$ha_real_1ra'
check_target("Target 6 (pasada_activa renderUI)", target_6, 1)

# Target 7: pasada_activa in modal_btn_guardar
target_7 <- '    modo <- input$modal_modo_ingreso
    pasada_activa <- if (!is.null(input$modal_input_pasada)) as.integer(input$modal_input_pasada) else ctx$pasada_sug
    ha_real_pasada <- if (pasada_activa == 2L) ctx$ha_real_2da else ctx$ha_real_1ra'
check_target("Target 7 (pasada_activa modal_btn_guardar)", target_7, 1)

# Target 8: num_pasada_sel
target_8 <- '    num_pasada_sel <- pasada_activa
    motivo_txt <- if (!is.null(input$modal_input_motivo)) trimws(as.character(input$modal_input_motivo)) else ""'
check_target("Target 8 (num_pasada_sel)", target_8, 1)

# Target 9: pasada_activa in modal_btn_restablecer
target_9 <- '  observeEvent(input$modal_btn_restablecer, {
    ctx <- modal_celda_activa()
    req(ctx)
    
    pasada_activa <- if (nrow(ctx$ed_celda) > 0 && coalesce(ctx$ed_celda$Ha_Asignada_2da[1], 0) > 0) 2L else ctx$pasada_sug'
check_target("Target 9 (pasada_activa modal_btn_restablecer)", target_9, 1)
