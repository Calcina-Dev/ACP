lines <- readLines("Actualizacion Semanal/app.R", encoding = "UTF-8")

# ------------------------------------------------------------------------------
# 1. UI: Agregar Tarjeta 3 (KPI Factor Rendimiento Aplicado al Turno)
# ------------------------------------------------------------------------------
idx_ui_flor <- grep('uiOutput\\("ui_kpi_factor_floral_semana"\\)', lines)[1]
cat(sprintf("1. UI floral card found at line %d\n", idx_ui_flor))

# The line after is line 5874: `)`
idx_tarjeta2_end <- idx_ui_flor + 1
cat(sprintf("Line %d is: %s\n", idx_tarjeta2_end, lines[idx_tarjeta2_end]))

tarjeta3_ui <- c(
  "                                      ),",
  "                                      # Tarjeta 3: Factor de rendimiento aplicado al turno para esa semana",
  "                                      div(class = \"slicer-card\", style = \"padding: 14px 16px; background: #FFFFFF; border-radius: 14px; border: 1px solid rgba(0,0,0,0.05); box-shadow: 0 1px 3px rgba(0,0,0,0.02);\",",
  "                                          uiOutput(\"ui_kpi_factor_rendimiento_turno_semana\")",
  "                                      )"
)

lines <- c(lines[1:(idx_tarjeta2_end - 1)], tarjeta3_ui, lines[(idx_tarjeta2_end + 1):length(lines)])

# ------------------------------------------------------------------------------
# 2. FIX: Modal Diagnóstico regex \1% -> \\1% y agregar Paso 3 de Frecuencia
# ------------------------------------------------------------------------------
idx_prem_txt <- grep('pct_prem_txt <- if \\(grepl\\("Premio floral', lines)[1]
idx_cast_txt <- grep('pct_cast_txt <- if \\(grepl\\("Castigo floral', lines)[1]
cat(sprintf("2. Diagnostic modal regex at %d and %d\n", idx_prem_txt, idx_cast_txt))

lines[idx_prem_txt + 1] <- '        sub(".*Premio floral ([+-]?[0-9.]+)%.*", "\\\\1%", motivo_str)'
lines[idx_cast_txt + 1] <- '        sub(".*Castigo floral ([+-]?[0-9.]+)%.*", "\\\\1%", motivo_str)'

idx_floral_neutro <- grep('desc = "La tasa de maduración se encuentra en línea con la curva teórica estándar de la variedad', lines)[1]
idx_floral_chain_end <- idx_floral_neutro + 2
while (!grepl("^\\s*\\}", lines[idx_floral_chain_end])) {
  idx_floral_chain_end <- idx_floral_chain_end + 1
}
cat(sprintf("Floral chain ends at line %d\n", idx_floral_chain_end))

paso_frecuencia_diag <- c(
  "    # Paso 3: Factor de Frecuencia de Cosecha (si aplica)",
  "    if (grepl(\"Frecuencia [0-9.]+d\", motivo_str)) {",
  "      txt_frec_aj <- if (grepl(\"Frecuencia ([0-9.]+d \\([+-]?[0-9.]+%%\\))\", motivo_str)) {",
  "        sub(\".*Frecuencia ([0-9.]+d \\([+-]?[0-9.]+%%\\)).*\", \"\\\\1\", motivo_str)",
  "      } else if (grepl(\"Frecuencia ([0-9.]+d)\", motivo_str)) {",
  "        sub(\".*Frecuencia ([0-9.]+d).*\", \"\\\\1\", motivo_str)",
  "      } else \"Frecuencia variable\"",
  "      pasos <- c(pasos, list(list(",
  "        num = as.character(length(pasos) + 1),",
  "        titulo = sprintf(\"Ajuste por Frecuencia de Cosecha (%s)\", txt_frec_aj),",
  "        desc = \"Modela el tiempo transcurrido desde el último pase en campo. Al diferir el intervalo respecto a la frecuencia base (7 días), las plantas acumulan o reducen proporcionalmente la carga de fruta madura en planta.\"",
  "      )))",
  "    }"
)

lines <- c(lines[1:idx_floral_chain_end], paso_frecuencia_diag, lines[(idx_floral_chain_end + 1):length(lines)])

# ------------------------------------------------------------------------------
# 3. MOTOR: Integrar Factor de Frecuencia en ejecutar_ajuste_semana
# ------------------------------------------------------------------------------
# 3a. En valvulas_calc: calcular dias_interv_valv
idx_idx_asig_rango <- grep('idx_asig_rango          = pmin\\(dia_fin_idx, pmax\\(dia_inicio_idx, idx_natural\\)\\),', lines)[1]
cat(sprintf("3a. idx_asig_rango at line %d\n", idx_idx_asig_rango))

lines[idx_idx_asig_rango] <- paste(
  "          idx_asig_rango          = pmin(dia_fin_idx, pmax(dia_inicio_idx, idx_natural)),",
  "          Dia_Asig_Nom            = dias_nombres[idx_asig_rango],",
  "          Dia_Ant_Ref             = coalesce(Dia_Rect_Ant, Dia_7d, Dia_Habitual),",
  "          dias_interv_valv        = {",
  "            dias_map_local <- setNames(1:6, dias_nombres)",
  "            idx_ant <- dias_map_local[Dia_Ant_Ref]",
  "            idx_act <- dias_map_local[Dia_Asig_Nom]",
  "            ifelse(!is.na(idx_ant) & !is.na(idx_act),",
  "                   (7L - as.integer(idx_ant)) + as.integer(idx_act),",
  "                   coalesce(as.integer(Frecuencia_Dias), 7L))",
  "          },",
  sep = "\n"
)

# 3b. En turno_avance_ha: agregar Frec_Turno_Sem
idx_valvs_tot <- grep('Valvulas_Total    = n\\(\\),', lines)[1]
cat(sprintf("3b. Valvulas_Total at line %d\n", idx_valvs_tot))

lines[idx_valvs_tot] <- paste(
  "          Frec_Turno_Sem    = round(weighted.mean(dias_interv_valv, w = Área, na.rm = TRUE), 1),",
  "          Valvulas_Total    = n(),",
  sep = "\n"
)

# 3c. En rend_turno: calcular Factor_Frecuencia
grep_rt <- grep('rend_turno <- rend_turno %>%', lines)
idx_rt_mutate <- grep_rt[grep_rt > idx_valvs_tot][1]
cat(sprintf("3c. rend_turno mutate at line %d\n", idx_rt_mutate))

frec_calc_block <- c(
  "      elast_frec_pct <- tryCatch(rv_elasticidad_frecuencia(), error = function(e) 70)",
  "      rend_turno <- rend_turno %>%",
  "        mutate(",
  "          Frec_Turno_Sem  = coalesce(Frec_Turno_Sem, 7.0),",
  "          Frec_Base_Turno = ifelse(limpiar_variedad(Variedad) == \"ROCIO\", 4, 7),",
  "          Diff_Dias_Frec  = Frec_Turno_Sem - Frec_Base_Turno,",
  "          Factor_Frecuencia = vapply(seq_along(Diff_Dias_Frec), function(k) {",
  "            calcular_factor_frecuencia(Diff_Dias_Frec[k], Frec_Base_Turno[k], elast_frec_pct)$factor",
  "          }, numeric(1)),",
  "          Delta_Frec_Pct = vapply(seq_along(Diff_Dias_Frec), function(k) {",
  "            calcular_factor_frecuencia(Diff_Dias_Frec[k], Frec_Base_Turno[k], elast_frec_pct)$delta_pct",
  "          }, numeric(1))",
  "        )",
  ""
)

lines <- c(lines[1:(idx_rt_mutate - 1)], frec_calc_block, lines[idx_rt_mutate:length(lines)])

# 3d. Multiplicar Factor_Frecuencia en Rend_Base_Con_Floral y Rend_S1_Floral
idx_rend_base_floral <- grep('TRUE                                                                                ~ round\\(Rend_Base_Turno \\* Factor_Floral, 2\\)', lines)[1]
cat(sprintf("3d. Rend_Base_Con_Floral at line %d\n", idx_rend_base_floral))
lines[idx_rend_base_floral] <- "            TRUE                                                                                ~ round(Rend_Base_Turno * Factor_Floral * Factor_Frecuencia, 2)"

idx_s1_floral <- grep('Rend_S1_Floral  = round\\(Rend_Ant_Turno \\* Factor_Floral, 2\\),', lines)[1]
cat(sprintf("3d. Rend_S1_Floral at line %d\n", idx_s1_floral))
lines[idx_s1_floral] <- "          Rend_S1_Floral  = round(Rend_Ant_Turno * Factor_Floral * Factor_Frecuencia, 2),"

# 3e. Detalle_Frecuencia y Motivo_Ajuste en rend_turno
idx_detalle_floral <- grep('Detalle_Floral = case_when\\(', lines)[1]
cat(sprintf("3e. Detalle_Floral at line %d\n", idx_detalle_floral))

idx_motivo_ajuste <- grep('Motivo_Ajuste = case_when\\(', lines)
idx_motivo_ajuste <- idx_motivo_ajuste[idx_motivo_ajuste > idx_detalle_floral][1]
cat(sprintf("3e. Motivo_Ajuste at line %d\n", idx_motivo_ajuste))

frec_motivo_block <- c(
  "          Detalle_Frecuencia = case_when(",
  "            !Esta_Proyectado | (Turno_Completo_Lun_Mar & !Es_Ciclo_Corto) | (Turno_Completo_Lun_Mar & Es_Ciclo_Corto) ~ \"\",",
  "            Semana_Ant_No_Finalizo & Tiene_Cosecha_Fresca & Prop_Fresca_Turno > prop_umbral_cfg ~ \"\",",
  "            Tiene_Cosecha_Act & Prop_Avance_Turno > prop_umbral_cfg ~ \"\",",
  "            Excede_25_Proy ~ \"\",",
  "            abs(Diff_Dias_Frec) >= 0.3 ~ sprintf(\"Frecuencia %.1fd (%+.1f%%)\", Frec_Turno_Sem, Delta_Frec_Pct),",
  "            TRUE ~ \"\"",
  "          ),"
)

lines <- c(lines[1:(idx_motivo_ajuste - 1)], frec_motivo_block, lines[idx_motivo_ajuste:length(lines)])

idx_nzchar_flor <- grep('nzchar\\(Detalle_Floral\\) ~ paste0\\(Motivo_Base, " • ", Detalle_Floral\\),', lines)[1]
cat(sprintf("3e. nzchar(Detalle_Floral) at line %d\n", idx_nzchar_flor))

lines[idx_nzchar_flor] <- paste(
  "            nzchar(Detalle_Floral) & nzchar(Detalle_Frecuencia) ~ paste0(Motivo_Base, \" • \", Detalle_Floral, \" • \", Detalle_Frecuencia),",
  "            nzchar(Detalle_Floral) ~ paste0(Motivo_Base, \" • \", Detalle_Floral),",
  "            nzchar(Detalle_Frecuencia) ~ paste0(Motivo_Base, \" • \", Detalle_Frecuencia),",
  sep = "\n"
)

# 3f. select in left_join
idx_sel_flor <- grep('Factor_Floral, Tipo_Efecto_Floral, Delta_Floral_Pct, Motivo_Ajuste', lines)[1]
cat(sprintf("3f. select in left_join at line %d\n", idx_sel_flor))
lines[idx_sel_flor] <- "            Factor_Floral, Tipo_Efecto_Floral, Delta_Floral_Pct, Factor_Frecuencia, Delta_Frec_Pct, Frec_Turno_Sem, Motivo_Ajuste"

# ------------------------------------------------------------------------------
# 4. FIX: Factor Floral KPI display bug (* 100)
# ------------------------------------------------------------------------------
idx_fac_medio <- grep('factor_medio   <- mean\\(df_fac\\$Ajuste_Floral_Pct, na.rm = TRUE\\)', lines)[1]
cat(sprintf("4. Factor medio at line %d\n", idx_fac_medio))
lines[idx_fac_medio]     <- "    factor_medio   <- mean(df_fac$Ajuste_Floral_Pct, na.rm = TRUE) * 100"
lines[idx_fac_medio + 1] <- "    delta_medio    <- mean(df_fac$Delta_Floral_Pct, na.rm = TRUE) * 100"

# ------------------------------------------------------------------------------
# 5. SERVER: Implementar output$ui_kpi_factor_rendimiento_turno_semana
# ------------------------------------------------------------------------------
idx_end_floral_kpi <- grep('output\\$ui_kpi_factor_floral_semana <- renderUI\\(\\{', lines)[1]
idx_close_floral_kpi <- idx_end_floral_kpi + 1
while (idx_close_floral_kpi <= length(lines)) {
  l <- lines[idx_close_floral_kpi]
  if (grepl("^\\s*\\}\\)", l)) break
  idx_close_floral_kpi <- idx_close_floral_kpi + 1
}
cat(sprintf("5. ui_kpi_factor_floral_semana closes at line %d\n", idx_close_floral_kpi))

kpi_rend_server <- c(
  "",
  "  # ==============================================================================",
  "  # KPI CARD: FACTOR DE RENDIMIENTO APLICADO AL TURNO PARA LA SEMANA (EMIL KOWALSKI)",
  "  # ==============================================================================",
  "  output$ui_kpi_factor_rendimiento_turno_semana <- renderUI({",
  "    req(input$filtro_campana, input$filtro_semana)",
  "    ",
  "    sem_sel  <- as.numeric(input$filtro_semana)",
  "    camp_sel <- as.character(input$filtro_campana)",
  "    sem_ant  <- sem_sel - 1",
  "    ",
  "    # 1. Base S-1 (Rendimiento observado semana previa)",
  "    df_w <- if (exists(\"df_hist_oficial_wide\", inherits = TRUE) && !is.null(df_hist_oficial_wide)) df_hist_oficial_wide else tibble()",
  "    dias_cols <- c(\"Lunes\", \"Martes\", \"Miércoles\", \"Jueves\", \"Viernes\", \"Sábado\")",
  "    ",
  "    sub_s1 <- df_w %>%",
  "      filter(!is.na(Codigo_Valv), as.numeric(Semana) == sem_ant)",
  "    ",
  "    if (!is.null(input$filtro_modulo) && length(input$filtro_modulo) > 0) {",
  "      sub_s1 <- sub_s1 %>% filter(trimws(as.character(Módulo_Norm)) %in% trimws(as.character(input$filtro_modulo)))",
  "    }",
  "    if (!is.null(input$filtro_turno) && length(input$filtro_turno) > 0) {",
  "      sub_s1 <- sub_s1 %>% filter(trimws(as.character(Turno_Norm)) %in% trimws(as.character(input$filtro_turno)))",
  "    }",
  "    if (!is.null(input$filtro_variedad) && length(input$filtro_variedad) > 0) {",
  "      sub_s1 <- sub_s1 %>% filter(limpiar_variedad(Variedad) %in% limpiar_variedad(input$filtro_variedad))",
  "    }",
  "    ",
  "    kg_s1 <- if (nrow(sub_s1) > 0) {",
  "      sum(sub_s1 %>% select(any_of(paste0(\"Kg_\", dias_cols))) %>% as.matrix(), na.rm = TRUE)",
  "    } else 0",
  "    area_s1 <- if (nrow(sub_s1) > 0) sum(sub_s1$Area_Ha, na.rm = TRUE) else 0",
  "    rend_s1 <- if (area_s1 > 0 && kg_s1 > 0) round(kg_s1 / area_s1, 1) else 0",
  "    ",
  "    # 2. Factor Floral",
  "    df_c_hist <- if (exists(\"df_conteo_historico\") && is.data.frame(df_conteo_historico) && nrow(df_conteo_historico) > 0) {",
  "      df_conteo_historico",
  "    } else if (file.exists(\"historico_conteo.rds\")) {",
  "      readRDS(\"historico_conteo.rds\")",
  "    } else if (file.exists(\"D:/ACP/Actualizacion Semanal/historico_conteo.rds\")) {",
  "      readRDS(\"D:/ACP/Actualizacion Semanal/historico_conteo.rds\")",
  "    } else {",
  "      tibble()",
  "    }",
  "    ",
  "    sens_pct   <- rv_sensibilidad_floral()",
  "    df_mad_mod <- rv_maduracion()",
  "    if (is.null(df_mad_mod) || nrow(df_mad_mod) == 0) df_mad_mod <- cargar_maduracion_disco()",
  "    df_mad_tur <- rv_maduracion_turno()",
  "    if (is.null(df_mad_tur) || nrow(df_mad_tur) == 0) df_mad_tur <- cargar_maduracion_turno_disco()",
  "    ",
  "    df_fac <- tryCatch({",
  "      calcular_factor_tendencia_floral(",
  "        df_conteo        = df_c_hist,",
  "        campana_sel      = camp_sel,",
  "        sem_cosecha      = sem_sel,",
  "        df_maduracion    = df_mad_mod,",
  "        sensibilidad_pct = sens_pct,",
  "        df_mad_turno     = df_mad_tur",
  "      )",
  "    }, error = function(e) tibble())",
  "    ",
  "    if (!is.null(input$filtro_modulo) && length(input$filtro_modulo) > 0) {",
  "      df_fac <- df_fac %>% filter(trimws(as.character(Módulo)) %in% trimws(as.character(input$filtro_modulo)))",
  "    }",
  "    if (!is.null(input$filtro_turno) && length(input$filtro_turno) > 0) {",
  "      df_fac <- df_fac %>% filter(trimws(as.character(Turno)) %in% trimws(as.character(input$filtro_turno)))",
  "    }",
  "    if (!is.null(input$filtro_variedad) && length(input$filtro_variedad) > 0) {",
  "      df_fac <- df_fac %>% filter(toupper(trimws(as.character(Variedad))) %in% toupper(trimws(as.character(input$filtro_variedad))))",
  "    }",
  "    ",
  "    factor_flor_val <- if (nrow(df_fac) > 0) mean(df_fac$Factor_Floral, na.rm = TRUE) else 1.0",
  "    delta_flor_pct  <- (factor_flor_val - 1) * 100",
  "    ",
  "    # 3. Factor Frecuencia",
  "    df_frec_camp <- tryCatch(frecuencia_campana_data(), error = function(e) tibble())",
  "    frec_dias_val <- 7.0",
  "    if (!is.null(df_frec_camp) && nrow(df_frec_camp) > 0) {",
  "      sub_fr <- df_frec_camp %>% filter(as.numeric(Semana) == sem_sel)",
  "      if (!is.null(input$filtro_modulo) && length(input$filtro_modulo) > 0) {",
  "        sub_fr <- sub_fr %>% filter(trimws(as.character(Módulo)) %in% trimws(as.character(input$filtro_modulo)))",
  "      }",
  "      if (!is.null(input$filtro_turno) && length(input$filtro_turno) > 0) {",
  "        sub_fr <- sub_fr %>% filter(trimws(as.character(Turno)) %in% trimws(as.character(input$filtro_turno)))",
  "      }",
  "      if (nrow(sub_fr) > 0 && \"Frecuencia\" %in% names(sub_fr)) {",
  "        frec_dias_val <- round(mean(sub_fr$Frecuencia, na.rm = TRUE), 1)",
  "      }",
  "    }",
  "    ",
  "    var_sel_chr <- if (!is.null(input$filtro_variedad) && length(input$filtro_variedad) > 0) limpiar_variedad(input$filtro_variedad[1]) else \"\"",
  "    frec_base_val <- if (var_sel_chr == \"ROCIO\") 4.0 else 7.0",
  "    diff_dias_val <- frec_dias_val - frec_base_val",
  "    elast_val     <- tryCatch(rv_elasticidad_frecuencia(), error = function(e) 70)",
  "    info_frec_kpi <- calcular_factor_frecuencia(diff_dias_val, frec_base_val, elast_val)",
  "    factor_frec_val <- info_frec_kpi$factor",
  "    delta_frec_pct  <- info_frec_kpi$delta_pct",
  "    ",
  "    # 4. Rendimiento Aplicado Resultante",
  "    rend_aplicado <- if (rend_s1 > 0) {",
  "      round(rend_s1 * factor_flor_val * factor_frec_val, 1)",
  "    } else 0",
  "    delta_total_pct <- if (rend_s1 > 0) round((rend_aplicado / rend_s1 - 1) * 100, 1) else 0",
  "    ",
  "    signo_tot <- if (delta_total_pct > 0) \"+\" else \"\"",
  "    str_delta_tot <- sprintf(\"%s%.1f%%\", signo_tot, delta_total_pct)",
  "    color_num_tot <- if (delta_total_pct > 0) \"#248A3D\" else if (delta_total_pct < 0) \"#D32F2F\" else \"#1C1C1E\"",
  "    ",
  "    tur_txt <- if (!is.null(input$filtro_turno) && length(input$filtro_turno) > 0) {",
  "      sprintf(\"Turno %s\", paste(input$filtro_turno, collapse = \", \"))",
  "    } else if (!is.null(input$filtro_modulo) && length(input$filtro_modulo) > 0) {",
  "      sprintf(\"Módulo %s\", paste(input$filtro_modulo, collapse = \", \"))",
  "    } else \"Global\"",
  "    ",
  "    div(",
  "      style = \"display: flex; flex-direction: column; gap: 8px;\",",
  "      div(",
  "        style = \"display: flex; justify-content: space-between; align-items: center;\",",
  "        div(",
  "          style = \"display: flex; align-items: center; gap: 6px;\",",
  "          span(style = \"width: 7px; height: 7px; border-radius: 50%; background: #007AFF; display: inline-block;\"),",
  "          span(style = \"font-size: 12px; font-weight: 600; color: #1C1C1E; letter-spacing: -0.01em;\", sprintf(\"Rendimiento Aplicado S%d\", sem_sel))",
  "        ),",
  "        span(",
  "          style = \"background: #F4F4F5; color: #1C1C1E; border: 1px solid #E4E4E7; font-size: 10px; font-weight: 600; padding: 1.5px 7px; border-radius: 5px; font-variant-numeric: tabular-nums;\",",
  "          tur_txt",
  "        )",
  "      ),",
  "      div(",
  "        style = \"display: flex; align-items: baseline; justify-content: space-between; margin-top: 1px;\",",
  "        div(",
  "          span(style = sprintf(\"font-size: 23px; font-weight: 700; color: %s; letter-spacing: -0.02em; font-variant-numeric: tabular-nums;\", color_num_tot),",
  "               if (rend_aplicado > 0) sprintf(\"%.1f\", rend_aplicado) else \"-\"),",
  "          span(style = \"font-size: 10.5px; color: #8E8E93; margin-left: 4px;\", \"kg/ha aj.\")",
  "        ),",
  "        div(",
  "          style = sprintf(\"font-size: 11px; font-weight: 600; color: %s; font-variant-numeric: tabular-nums;\", color_num_tot),",
  "          if (rend_s1 > 0) sprintf(\"%s vs S%d (%.1f)\", str_delta_tot, sem_ant, rend_s1) else \"Sin base S-1\"",
  "        )",
  "      ),",
  "      div(",
  "        style = \"background: #F9F9FB; border-radius: 8px; padding: 6px 10px; border: 1px solid rgba(0,0,0,0.04); display: flex; justify-content: space-between; align-items: center; font-size: 10px; color: #636366; font-variant-numeric: tabular-nums; flex-wrap: wrap; gap: 4px;\",",
  "        div(",
  "          span(style = \"color: #8E8E93;\", \"Base: \"),",
  "          strong(style = \"color: #1C1C1E;\", sprintf(\"%.1f\", rend_s1))",
  "        ),",
  "        div(",
  "          span(style = \"color: #8E8E93;\", \"× Flor: \"),",
  "          strong(style = if (delta_flor_pct < 0) \"color: #D32F2F;\" else \"color: #248A3D;\", sprintf(\"%+.1f%%\", delta_flor_pct))",
  "        ),",
  "        div(",
  "          span(style = \"color: #8E8E93;\", \"× Frec: \"),",
  "          strong(style = if (delta_frec_pct > 0) \"color: #5856D6;\" else \"color: #D32F2F;\", sprintf(\"%+.1f%% (%.1fd)\", delta_frec_pct, frec_dias_val))",
  "        )",
  "      )",
  "    )",
  "  })"
)

lines <- c(lines[1:idx_close_floral_kpi], kpi_rend_server, lines[(idx_close_floral_kpi + 1):length(lines)])

writeLines(lines, "Actualizacion Semanal/app.R", useBytes = TRUE)
cat("All changes applied successfully to Actualizacion Semanal/app.R!\n")
