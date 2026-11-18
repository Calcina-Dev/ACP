lines <- readLines("Actualizacion Semanal/app.R", encoding = "UTF-8")

# 1. Add rv_elasticidad_frecuencia right after rv_frecuencias
idx_rv <- grep('rv_frecuencias     <- reactiveVal\\(cargar_frecuencias_disco\\(\\)\\)', lines)
cat("Found rv_frecuencias at line:", idx_rv, "\n")

block_rv <- "  rv_elasticidad_frecuencia <- reactiveVal(cargar_elasticidad_frecuencia_disco())"

# 2. Add listener for input_elasticidad_frecuencia and btn_aplicar_factor_frecuencia_modal
block_listeners <- c(
  "  # Guardar elasticidad de frecuencia cuando el usuario la edita",
  "  observeEvent(input$input_elasticidad_frecuencia, {",
  "    val <- input$input_elasticidad_frecuencia",
  "    req(!is.null(val), !is.na(val))",
  "    val_clamped <- max(0, min(100, as.numeric(val)))",
  "    rv_elasticidad_frecuencia(val_clamped)",
  "    guardar_elasticidad_frecuencia_disco(val_clamped)",
  "  })",
  "  ",
  "  # Aplicar sugerencia de Factor de Frecuencia en el modal de edición de celda",
  "  observeEvent(input$btn_aplicar_factor_frecuencia_modal, {",
  "    ctx <- modal_celda_activa()",
  "    req(ctx)",
  "    dia_origen <- as.character(ctx$campo)",
  "    dia_destino <- as.character(input$modal_input_dia)",
  "    req(dia_origen, dia_destino, dia_origen != dia_destino)",
  "    ",
  "    dias_map <- setNames(1:6, c('Lunes', 'Martes', 'Miércoles', 'Jueves', 'Viernes', 'Sábado'))",
  "    diff_dias <- as.integer(dias_map[dia_destino] - dias_map[dia_origen])",
  "    ",
  "    frec_base <- 7",
  "    df_frec_sub <- tryCatch(rv_frecuencias(), error = function(e) NULL)",
  "    if (is.null(df_frec_sub) || nrow(df_frec_sub) == 0) df_frec_sub <- cargar_frecuencias_disco()",
  "    if (!is.null(df_frec_sub) && nrow(df_frec_sub) > 0) {",
  "      f_match <- df_frec_sub %>% filter(as.character(Módulo) == as.character(ctx$fila$Módulo),",
  "                                        limpiar_variedad(Variedad) == limpiar_variedad(ctx$fila$Variedad))",
  "      if (nrow(f_match) > 0 && !is.na(f_match$Frecuencia_Dias[1])) frec_base <- as.numeric(f_match$Frecuencia_Dias[1])",
  "    } else if (limpiar_variedad(ctx$fila$Variedad) == 'ROCIO') {",
  "      frec_base <- 4",
  "    }",
  "    ",
  "    elast_pct <- tryCatch(rv_elasticidad_frecuencia(), error = function(e) 70)",
  "    info_frec <- calcular_factor_frecuencia(diff_dias, frec_base, elast_pct)",
  "    ",
  "    modo <- input$modal_modo_ingreso",
  "    if (identical(modo, 'kgha')) {",
  "      kgha_orig <- coalesce(as.numeric(ctx$kgha_preset), 0)",
  "      if (kgha_orig > 0) {",
  "        kgha_sug <- round(kgha_orig * info_frec$factor, 1)",
  "        updateNumericInput(session, 'modal_input_kgha', value = kgha_sug)",
  "      }",
  "    } else {",
  "      kg_orig <- coalesce(as.numeric(ctx$valor_actual), 0)",
  "      if (kg_orig > 0) {",
  "        kg_sug <- round(kg_orig * info_frec$factor, 0)",
  "        updateNumericInput(session, 'modal_input_kilos', value = kg_sug)",
  "      }",
  "    }",
  "    ",
  "    motivo_sug <- sprintf('Ajuste por Frecuencia (%s a %s, %+d d, %+.1f%%)', dia_origen, dia_destino, diff_dias, info_frec$delta_pct)",
  "    updateTextInput(session, 'modal_input_motivo', value = motivo_sug)",
  "    showNotification(sprintf('✔ Factor de frecuencia aplicado (%+.1f%%). Valor actualizado.', info_frec$delta_pct), type = 'message', duration = 3)",
  "  })"
)

lines <- c(lines[1:idx_rv], block_rv, block_listeners, lines[(idx_rv + 1):length(lines)])
writeLines(lines, "Actualizacion Semanal/app.R", useBytes = TRUE)
cat("Successfully inserted reactive and listeners!\n")
