file_app <- "D:/ACP/Actualizacion Semanal/app.R"
file_backup <- "D:/ACP/Actualizacion Semanal/app_backup_before_sr_patch.R"

# 1. Create backup
file.copy(file_app, file_backup, overwrite = TRUE)
cat("Backup created at:", file_backup, "\n")

lines <- readLines(file_app, encoding = "UTF-8", warn = FALSE)

# -------------------------------------------------------------
# PATCH 1: Lines around 1817 (df_rec columns)
# -------------------------------------------------------------
idx_desfase <- grep("Kg_Desfase       = 0,", lines, fixed = TRUE)[1]
stopifnot(!is.na(idx_desfase))

new_cols <- c(
  "        Kg_Lunes_sr      = 0,",
  "        Kg_Martes_sr     = 0,",
  "        Kg_Miercoles_sr  = 0,",
  "        Kg_Jueves_sr     = 0,",
  "        Kg_Viernes_sr    = 0,",
  "        Kg_Sabado_sr     = 0,",
  "        Kg_Sin_Ruta      = 0,",
  "        Kg_Desfase       = 0,"
)
lines <- c(lines[1:(idx_desfase - 1)], new_cols, lines[(idx_desfase + 1):length(lines)])
cat("Patch 1 applied.\n")

# -------------------------------------------------------------
# PATCH 2: Loop around line 1825 (now shifted by 7 lines)
# -------------------------------------------------------------
idx_loop_start <- grep("for (i in seq_len(nrow(df_rec))) {", lines, fixed = TRUE)[1]
idx_loop_end <- which(grepl("^\\s*return\\(df_rec\\)", lines))[1] - 2
# Let's verify idx_loop_end is "    }"
cat("idx_loop_start:", idx_loop_start, "lines[idx_loop_start]:", lines[idx_loop_start], "\n")
cat("idx_loop_end:", idx_loop_end, "lines[idx_loop_end]:", lines[idx_loop_end], "\n")
stopifnot(trimws(lines[idx_loop_end]) == "}")

new_loop <- c(
  "    for (i in seq_len(nrow(df_rec))) {",
  "      r <- df_rec[i, ]",
  "      ",
  "      # REGLA 1: SAP sin Ruta total -> Excluir de cálculos y pintado",
  "      if (!r$Tiene_Campo && r$Tiene_SAP) {",
  "        df_rec$Estado_Concil[i]        <- \"Kilos SAP sin Ruta\"",
  "        df_rec$Tipo_Alerta[i]          <- \"inconsistencia\"",
  "        df_rec$Diagnostico[i]          <- sprintf(\"Pesado en SAP (%s kg), pero no programado en ruta de campo. Excluido de mapa y cálculos.\", format(round(r$Kg_Total_sap, 0), big.mark = \",\"))",
  "        df_rec$Es_Valido_Operativo[i]  <- FALSE",
  "        df_rec$Kg_Sin_Ruta[i]          <- r$Kg_Total_sap",
  "        df_rec$Kg_Lunes_sr[i]          <- r$Kg_Lunes_sap",
  "        df_rec$Kg_Martes_sr[i]         <- r$Kg_Martes_sap",
  "        df_rec$Kg_Miercoles_sr[i]      <- r$Kg_Miercoles_sap",
  "        df_rec$Kg_Jueves_sr[i]         <- r$Kg_Jueves_sap",
  "        df_rec$Kg_Viernes_sr[i]        <- r$Kg_Viernes_sap",
  "        df_rec$Kg_Sabado_sr[i]         <- r$Kg_Sabado_sap",
  "        ",
  "        df_rec$Kg_Lunes_rec[i]         <- 0",
  "        df_rec$Kg_Martes_rec[i]        <- 0",
  "        df_rec$Kg_Miercoles_rec[i]     <- 0",
  "        df_rec$Kg_Jueves_rec[i]        <- 0",
  "        df_rec$Kg_Viernes_rec[i]       <- 0",
  "        df_rec$Kg_Sabado_rec[i]        <- 0",
  "        df_rec$Kg_Total_rec[i]         <- 0",
  "        next",
  "      }",
  "      ",
  "      # REGLA 3: Ruta sin Kilos en SAP -> Mantener en incidencias; en base real SAP es 0 kg hasta edición manual",
  "      if (r$Tiene_Campo && !r$Tiene_SAP) {",
  "        dias_c <- character(0)",
  "        if (r$Ha_Lunes_rc > 0) dias_c <- c(dias_c, \"Lunes\")",
  "        if (r$Ha_Martes_rc > 0) dias_c <- c(dias_c, \"Martes\")",
  "        if (r$Ha_Miercoles_rc > 0) dias_c <- c(dias_c, \"Miércoles\")",
  "        if (r$Ha_Jueves_rc > 0) dias_c <- c(dias_c, \"Jueves\")",
  "        if (r$Ha_Viernes_rc > 0) dias_c <- c(dias_c, \"Viernes\")",
  "        if (r$Ha_Sabado_rc > 0) dias_c <- c(dias_c, \"Sábado\")",
  "        ",
  "        tiene_previos <- any(dias_c %in% dias_cerrados_nom)",
  "        if (es_semana_actual && !tiene_previos) {",
  "          df_rec$Estado_Concil[i]        <- \"Cosecha Hoy (D+1)\"",
  "          df_rec$Tipo_Alerta[i]          <- \"en_proceso\"",
  "          df_rec$Diagnostico[i]          <- sprintf(\"Cosechado hoy (%s) en campo. Pendiente de recepción en balanza de packing mañana.\", paste(dias_c, collapse = \", \"))",
  "          df_rec$Es_Valido_Operativo[i]  <- TRUE",
  "          df_rec$Kg_Miercoles_rec[i]     <- 0",
  "          df_rec$Kg_Total_rec[i]         <- 0",
  "        } else {",
  "          df_rec$Estado_Concil[i]        <- \"Ruta sin Kilos SAP\"",
  "          df_rec$Tipo_Alerta[i]          <- \"inconsistencia\"",
  "          df_rec$Diagnostico[i]          <- sprintf(\"Cosechado en campo [%s], 0 kg en SAP tras cierre. En reporte de incidencias para edición manual en Excel.\",",
  "                                                    paste(dias_c, collapse = \", \"))",
  "          df_rec$Es_Valido_Operativo[i]  <- TRUE",
  "          df_rec$Kg_Lunes_rec[i]         <- 0",
  "          df_rec$Kg_Martes_rec[i]        <- 0",
  "          df_rec$Kg_Miercoles_rec[i]     <- 0",
  "          df_rec$Kg_Jueves_rec[i]        <- 0",
  "          df_rec$Kg_Viernes_rec[i]       <- 0",
  "          df_rec$Kg_Sabado_rec[i]        <- 0",
  "          df_rec$Kg_Total_rec[i]         <- 0",
  "        }",
  "        next",
  "      }",
  "      ",
  "      # REGLA 2: Desfase de Día y Kilos sin Ruta en días cerrados",
  "      if (r$Tiene_Campo && r$Tiene_SAP) {",
  "        kg_dias <- c(Lunes = r$Kg_Lunes_sap, Martes = r$Kg_Martes_sap, Miércoles = r$Kg_Miercoles_sap,",
  "                     Jueves = r$Kg_Jueves_sap, Viernes = r$Kg_Viernes_sap, Sábado = r$Kg_Sabado_sap)",
  "        ha_dias <- c(Lunes = r$Ha_Lunes_rc, Martes = r$Ha_Martes_rc, Miércoles = r$Ha_Miercoles_rc,",
  "                     Jueves = r$Ha_Jueves_rc, Viernes = r$Ha_Viernes_rc, Sábado = r$Ha_Sabado_rc)",
  "        ",
  "        dias_c_act <- names(ha_dias)[ha_dias > 0]",
  "        dias_s_act <- names(kg_dias)[kg_dias > 0]",
  "        ",
  "        dias_c_cerr <- dias_c_act[dias_c_act %in% dias_cerrados_nom]",
  "        dias_s_cerr <- dias_s_act[dias_s_act %in% dias_cerrados_nom]",
  "        ",
  "        # Evaluar cada día donde SAP reportó kilos",
  "        dias_match   <- character(0)",
  "        dias_desfase <- character(0) # D+1 (cosechó día anterior)",
  "        dias_sin_r   <- character(0) # Sin cosecha ese día ni el anterior",
  "        ",
  "        kg_sin_r_tot <- 0",
  "        kg_desf_tot  <- 0",
  "        ",
  "        for (ds in dias_s_cerr) {",
  "          if (ds %in% dias_c_cerr) {",
  "            dias_match <- c(dias_match, ds)",
  "          } else {",
  "            # Verificar si cosechó el día inmediatamente anterior (desfase D+1)",
  "            idx_s <- match(ds, dias_cols)",
  "            dia_ant <- if (idx_s > 1) dias_cols[idx_s - 1] else NA_character_",
  "            if (!is.na(dia_ant) && (dia_ant %in% dias_c_cerr)) {",
  "              dias_desfase <- c(dias_desfase, ds)",
  "              kg_desf_tot  <- kg_desf_tot + kg_dias[ds]",
  "            } else {",
  "              dias_sin_r   <- c(dias_sin_r, ds)",
  "              kg_sin_r_tot <- kg_sin_r_tot + kg_dias[ds]",
  "            }",
  "          }",
  "        }",
  "        ",
  "        # Procesar Sin Ruta",
  "        if (length(dias_sin_r) > 0) {",
  "          df_rec$Kg_Sin_Ruta[i] <- round(kg_sin_r_tot, 2)",
  "          for (dsr in dias_sin_r) {",
  "            col_rec <- paste0(\"Kg_\", tools::toTitleCase(chartr(\"áéíóúÁÉÍÓÚ\", \"aeiouAEIOU\", tolower(dsr))), \"_rec\")",
  "            col_sr  <- paste0(\"Kg_\", tools::toTitleCase(chartr(\"áéíóúÁÉÍÓÚ\", \"aeiouAEIOU\", tolower(dsr))), \"_sr\")",
  "            df_rec[[col_rec]][i] <- 0",
  "            df_rec[[col_sr]][i]  <- round(kg_dias[dsr], 2)",
  "          }",
  "        }",
  "        ",
  "        # Procesar Desfase D+1 (reasignar al día de campo previo)",
  "        if (length(dias_desfase) > 0) {",
  "          df_rec$Kg_Desfase[i] <- round(kg_desf_tot, 2)",
  "          for (dd in dias_desfase) {",
  "            idx_s <- match(dd, dias_cols)",
  "            dia_ant <- dias_cols[idx_s - 1]",
  "            col_dest <- paste0(\"Kg_\", tools::toTitleCase(chartr(\"áéíóúÁÉÍÓÚ\", \"aeiouAEIOU\", tolower(dia_ant))), \"_rec\")",
  "            col_orig <- paste0(\"Kg_\", tools::toTitleCase(chartr(\"áéíóúÁÉÍÓÚ\", \"aeiouAEIOU\", tolower(dd))), \"_rec\")",
  "            df_rec[[col_orig]][i] <- 0",
  "            df_rec[[col_dest]][i] <- round(df_rec[[col_dest]][i] + kg_dias[dd], 2)",
  "          }",
  "        }",
  "        ",
  "        # Actualizar Kg_Total_rec",
  "        df_rec$Kg_Total_rec[i] <- round(sum(c(df_rec$Kg_Lunes_rec[i], df_rec$Kg_Martes_rec[i], df_rec$Kg_Miercoles_rec[i],",
  "                                              df_rec$Kg_Jueves_rec[i], df_rec$Kg_Viernes_rec[i], df_rec$Kg_Sabado_rec[i])), 2)",
  "        ",
  "        # Estado final del registro",
  "        if (length(dias_sin_r) > 0) {",
  "          df_rec$Estado_Concil[i]        <- \"Kilos SAP sin Ruta\"",
  "          df_rec$Tipo_Alerta[i]          <- \"inconsistencia\"",
  "          df_rec$Diagnostico[i]          <- sprintf(\"Pesado en SAP sin ruta en campo en [%s] (%s kg). Excluido de mapa y cálculo agronómico.\",",
  "                                                    paste(dias_sin_r, collapse = \", \"), format(round(kg_sin_r_tot, 0), big.mark = \",\"))",
  "          # Si tiene campo en otros días válidos, mantenerlo operativo para esos días; si no tiene nada válido, FALSE",
  "          df_rec$Es_Valido_Operativo[i]  <- (df_rec$Kg_Total_rec[i] > 0 || any(ha_dias[setdiff(dias_cols, dias_sin_r)] > 0))",
  "        } else if (length(dias_desfase) > 0) {",
  "          df_rec$Estado_Concil[i]        <- \"Desfase Reasignado\"",
  "          df_rec$Tipo_Alerta[i]          <- \"conciliado\"",
  "          df_rec$Diagnostico[i]          <- sprintf(\"Desfase de %s kg en [%s] reasignado por coincidencia con ruta de campo previa. Usado operativamente (Azul).\",",
  "                                                    format(round(kg_desf_tot, 0), big.mark = \",\"), paste(dias_desfase, collapse = \", \"))",
  "          df_rec$Es_Valido_Operativo[i]  <- TRUE",
  "        } else if (length(dias_match) > 0) {",
  "          df_rec$Estado_Concil[i]        <- \"Conciliado OK\"",
  "          df_rec$Tipo_Alerta[i]          <- \"conciliado\"",
  "          df_rec$Diagnostico[i]          <- \"Campo y SAP coinciden plenamente en válvula y días.\"",
  "          df_rec$Es_Valido_Operativo[i]  <- TRUE",
  "        } else {",
  "          df_rec$Estado_Concil[i]        <- \"Desfase de Día\"",
  "          df_rec$Tipo_Alerta[i]          <- \"inconsistencia\"",
  "          df_rec$Diagnostico[i]          <- sprintf(\"Desfase total: Campo [%s] vs SAP [%s]. Kilos fuera de cálculo.\",",
  "                                                    paste(dias_c_cerr, collapse = \", \"), paste(dias_s_cerr, collapse = \", \"))",
  "          df_rec$Es_Valido_Operativo[i]  <- FALSE",
  "          df_rec$Kg_Total_rec[i]         <- 0",
  "        }",
  "        next",
  "      }",
  "    }"
)

lines <- c(lines[1:(idx_loop_start - 1)], new_loop, lines[(idx_loop_end + 1):length(lines)])
cat("Patch 2 applied.\n")

# -------------------------------------------------------------
# PATCH 3: ui_kpi_real_detalle (around line 6551)
# -------------------------------------------------------------
idx_kpi_sin_r <- grep("sin_r_mask <- !df_concil$Tiene_Campo & df_concil$Tiene_SAP", lines, fixed = TRUE)[1]
stopifnot(!is.na(idx_kpi_sin_r))
# Let's inspect the block around idx_kpi_sin_r:
# lines[idx_kpi_sin_r] is: "      sin_r_mask <- !df_concil$Tiene_Campo & df_concil$Tiene_SAP"
# lines[idx_kpi_sin_r + 1] is: "      if (any(sin_r_mask)) {"
# lines[idx_kpi_sin_r + 2] is: "        kg_sin_ruta_excl <- sum(coalesce(df_concil$Kg_Total_sap[sin_r_mask], 0), na.rm = TRUE)"
# lines[idx_kpi_sin_r + 3] is: "      }"

new_kpi_block <- c(
  "      if (\"Kg_Sin_Ruta\" %in% names(df_concil)) {",
  "        kg_sin_ruta_excl <- sum(coalesce(df_concil$Kg_Sin_Ruta, 0), na.rm = TRUE)",
  "      } else {",
  "        sin_r_mask <- (!df_concil$Tiene_Campo & df_concil$Tiene_SAP) | (coalesce(df_concil$Estado_Concil, \"\") == \"Kilos SAP sin Ruta\")",
  "        if (any(sin_r_mask)) {",
  "          kg_sin_ruta_excl <- sum(coalesce(df_concil$Kg_Total_sap[sin_r_mask], 0), na.rm = TRUE)",
  "        }",
  "      }"
)
lines <- c(lines[1:(idx_kpi_sin_r - 1)], new_kpi_block, lines[(idx_kpi_sin_r + 4):length(lines)])
cat("Patch 3 applied.\n")

# -------------------------------------------------------------
# PATCH 4: datos_resumen_base (around line 9818)
# -------------------------------------------------------------
idx_resumen_sr <- grep("sr_mask <- (!coalesce(df_concil$Tiene_Campo, FALSE) & coalesce(df_concil$Tiene_SAP, FALSE)) |", lines, fixed = TRUE)[1]
stopifnot(!is.na(idx_resumen_sr))
idx_sr_end <- which(grepl("select(Sector, Módulo, Turno, Variedad, Tipo, Proyección, Área, Lunes, Martes, Miércoles, Jueves, Viernes, Sábado, Ajuste, Diferencia)", lines, fixed = TRUE))[1]
stopifnot(!is.na(idx_sr_end))

new_resumen_block <- c(
  "      sr_mask <- coalesce(df_concil$Kg_Sin_Ruta, 0) > 0 |",
  "                 (!coalesce(df_concil$Tiene_Campo, FALSE) & coalesce(df_concil$Tiene_SAP, FALSE)) |",
  "                 coalesce(df_concil$Estado_Concil, \"\") == \"Kilos SAP sin Ruta\"",
  "      if (any(sr_mask)) {",
  "        df_sr_sub <- df_concil[sr_mask, ]",
  "        if (isTRUE(input$switch_alcance_resumen == \"filtrada\")) {",
  "          if (!is.null(input$filtro_variedad) && length(input$filtro_variedad) > 0) {",
  "            df_sr_sub <- df_sr_sub %>% filter(limpiar_variedad(Variedad) %in% limpiar_variedad(input$filtro_variedad))",
  "          }",
  "          if (!is.null(input$filtro_modulo) && length(input$filtro_modulo) > 0) {",
  "            df_sr_sub <- df_sr_sub %>% filter(as.character(Módulo) %in% as.character(input$filtro_modulo))",
  "          }",
  "          if (!is.null(input$filtro_turno) && length(input$filtro_turno) > 0) {",
  "            df_sr_sub <- df_sr_sub %>% filter(as.character(Turno) %in% as.character(input$filtro_turno))",
  "          }",
  "        }",
  "        if (nrow(df_sr_sub) > 0) {",
  "          tb_map <- tratamientos_base %>%",
  "            filter(as.character(Campaña) == as.character(input$filtro_campana)) %>%",
  "            distinct(Módulo = as.character(Módulo), Tipo)",
  "          ",
  "          df_sr_filtrado <- df_sr_sub %>%",
  "            left_join(tb_map, by = \"Módulo\") %>%",
  "            mutate(",
  "              Sector     = coalesce(as.character(Sector), \"Sin Ruta\"),",
  "              Módulo     = coalesce(as.character(Módulo), \"Sin Ruta\"),",
  "              Turno      = coalesce(as.character(Turno), \"0\"),",
  "              Variedad   = limpiar_variedad(Variedad),",
  "              Tipo       = coalesce(Tipo, \"Orgánico\"),",
  "              Proyección = 0,",
  "              Área       = 0,",
  "              Lunes      = if (\"Kg_Lunes_sr\" %in% names(.)) coalesce(as.numeric(Kg_Lunes_sr), 0) else ifelse(!Tiene_Campo, coalesce(as.numeric(Kg_Lunes_sap), 0), 0),",
  "              Martes     = if (\"Kg_Martes_sr\" %in% names(.)) coalesce(as.numeric(Kg_Martes_sr), 0) else ifelse(!Tiene_Campo, coalesce(as.numeric(Kg_Martes_sap), 0), 0),",
  "              Miércoles  = if (\"Kg_Miercoles_sr\" %in% names(.)) coalesce(as.numeric(Kg_Miercoles_sr), 0) else ifelse(!Tiene_Campo, coalesce(as.numeric(Kg_Miercoles_sap), 0), 0),",
  "              Jueves     = if (\"Kg_Jueves_sr\" %in% names(.)) coalesce(as.numeric(Kg_Jueves_sr), 0) else ifelse(!Tiene_Campo, coalesce(as.numeric(Kg_Jueves_sap), 0), 0),",
  "              Viernes    = if (\"Kg_Viernes_sr\" %in% names(.)) coalesce(as.numeric(Kg_Viernes_sr), 0) else ifelse(!Tiene_Campo, coalesce(as.numeric(Kg_Viernes_sap), 0), 0),",
  "              Sábado     = if (\"Kg_Sabado_sr\" %in% names(.)) coalesce(as.numeric(Kg_Sabado_sr), 0) else ifelse(!Tiene_Campo, coalesce(as.numeric(Kg_Sabado_sap), 0), 0),",
  "              Ajuste     = Lunes + Martes + Miércoles + Jueves + Viernes + Sábado,",
  "              Diferencia = Ajuste - Proyección",
  "            ) %>%",
  "            filter(Ajuste > 0) %>%",
  "            select(Sector, Módulo, Turno, Variedad, Tipo, Proyección, Área, Lunes, Martes, Miércoles, Jueves, Viernes, Sábado, Ajuste, Diferencia)"
)

lines <- c(lines[1:(idx_resumen_sr - 1)], new_resumen_block, lines[(idx_sr_end + 1):length(lines)])
cat("Patch 4 applied.\n")

# -------------------------------------------------------------
# PATCH 5: kpi_concil_sin_ruta_sub (around line 15152)
# -------------------------------------------------------------
idx_kpi_sub <- grep("kg_tot <- round(sum(sub_df$Kg_Total_sap, na.rm = TRUE), 0)", lines, fixed = TRUE)[1]
stopifnot(!is.na(idx_kpi_sub))

new_kpi_sub_line <- "    kg_tot <- if (\"Kg_Sin_Ruta\" %in% names(sub_df)) round(sum(coalesce(sub_df$Kg_Sin_Ruta, sub_df$Kg_Total_sap), na.rm = TRUE), 0) else round(sum(sub_df$Kg_Total_sap, na.rm = TRUE), 0)"
lines[idx_kpi_sub] <- new_kpi_sub_line
cat("Patch 5 applied.\n")

# Write modified lines
writeLines(lines, file_app, useBytes = TRUE)
cat("Wrote patched app.R successfully!\n")

# Verify syntax
res_parse <- tryCatch(parse(file_app), error = function(e) e)
if (inherits(res_parse, "error")) {
  cat("PARSE ERROR:\n", conditionMessage(res_parse), "\n")
  file.copy(file_backup, file_app, overwrite = TRUE)
  stop("Restored backup due to syntax error.")
} else {
  cat("PARSE SUCCESS: Syntax is 100% clean!\n")
}
