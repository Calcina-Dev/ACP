app_path <- "D:/ACP/Actualizacion Semanal/app.R"
lines <- readLines(app_path, warn = FALSE, encoding = "UTF-8")

# 1. cargar_ediciones_disco (replace lines 211:214)
lines[211:214] <- c(
  '      Ha_Asignada_1ra = if ("Ha_Asignada_1ra" %in% names(.)) {',
  '        ifelse(Variedad != "ROCIO", coalesce(suppressWarnings(as.numeric(Ha_Asignada)), suppressWarnings(as.numeric(Ha_Asignada_1ra))), suppressWarnings(as.numeric(Ha_Asignada_1ra)))',
  '      } else if (Variedad != "ROCIO") suppressWarnings(as.numeric(Ha_Asignada)) else NA_real_,',
  '      Kg_Asignado_1ra = if ("Kg_Asignado_1ra" %in% names(.)) {',
  '        ifelse(Variedad != "ROCIO", coalesce(suppressWarnings(as.numeric(Valor)), suppressWarnings(as.numeric(Kg_Asignado_1ra))), suppressWarnings(as.numeric(Kg_Asignado_1ra)))',
  '      } else if (Variedad != "ROCIO") suppressWarnings(as.numeric(Valor)) else NA_real_,',
  '      Ha_Asignada_2da = if ("Ha_Asignada_2da" %in% names(.)) {',
  '        ifelse(Variedad != "ROCIO", 0, suppressWarnings(as.numeric(Ha_Asignada_2da)))',
  '      } else 0,',
  '      Kg_Asignado_2da = if ("Kg_Asignado_2da" %in% names(.)) {',
  '        ifelse(Variedad != "ROCIO", 0, suppressWarnings(as.numeric(Kg_Asignado_2da)))',
  '      } else 0,'
)
cat("Step 1 done\n")

# Re-read or adjust indexing for guardar_ediciones_disco
# Let's search again dynamically to avoid line offset errors!
writeLines(lines, app_path, sep = "\r\n", useBytes = TRUE)
lines <- readLines(app_path, warn = FALSE, encoding = "UTF-8")

# 2. guardar_ediciones_disco
idx_ged <- which(grepl("^guardar_ediciones_disco <- function", lines))[1]
idx_g_mut <- idx_ged + which(grepl("Ha_Asignada_1ra = if", lines[idx_ged:(idx_ged + 30)]))[1] - 1
idx_g_end <- idx_ged + which(grepl("Kg_Asignado_2da = if", lines[idx_ged:(idx_ged + 30)]))[1] - 1

lines[idx_g_mut:idx_g_end] <- c(
  '      Ha_Asignada_1ra = if ("Ha_Asignada_1ra" %in% names(.)) {',
  '        ifelse(Variedad != "ROCIO", coalesce(suppressWarnings(as.numeric(Ha_Asignada)), suppressWarnings(as.numeric(Ha_Asignada_1ra))), suppressWarnings(as.numeric(Ha_Asignada_1ra)))',
  '      } else if (Variedad != "ROCIO") suppressWarnings(as.numeric(Ha_Asignada)) else NA_real_,',
  '      Kg_Asignado_1ra = if ("Kg_Asignado_1ra" %in% names(.)) {',
  '        ifelse(Variedad != "ROCIO", coalesce(suppressWarnings(as.numeric(Valor)), suppressWarnings(as.numeric(Kg_Asignado_1ra))), suppressWarnings(as.numeric(Kg_Asignado_1ra)))',
  '      } else if (Variedad != "ROCIO") suppressWarnings(as.numeric(Valor)) else NA_real_,',
  '      Ha_Asignada_2da = if ("Ha_Asignada_2da" %in% names(.)) {',
  '        ifelse(Variedad != "ROCIO", 0, suppressWarnings(as.numeric(Ha_Asignada_2da)))',
  '      } else 0,',
  '      Kg_Asignado_2da = if ("Kg_Asignado_2da" %in% names(.)) {',
  '        ifelse(Variedad != "ROCIO", 0, suppressWarnings(as.numeric(Kg_Asignado_2da)))',
  '      } else 0,'
)
cat("Step 2 done\n")

writeLines(lines, app_path, sep = "\r\n", useBytes = TRUE)
lines <- readLines(app_path, warn = FALSE, encoding = "UTF-8")

# 3. ha_ajustadas_ed
idx_haed <- which(grepl("^\\s*ha_ajustadas_ed <- ed_filtradas %>%", lines))[1]
idx_haed_mut <- idx_haed + which(grepl("Ha_1ra   = if", lines[idx_haed:(idx_haed + 25)]))[1] - 1
idx_haed_end <- idx_haed + which(grepl("Tiene_Detalle_Pas = any", lines[idx_haed:(idx_haed + 30)]))[1] - 1

lines[idx_haed_mut:idx_haed_end] <- c(
  '            es_rocio = (Var_k == "ROCIO"),',
  '            Ha_val   = if ("Ha_Asignada" %in% names(.)) suppressWarnings(as.numeric(Ha_Asignada)) else NA_real_,',
  '            Ha_1ra   = if ("Ha_Asignada_1ra" %in% names(.)) {',
  '              ifelse(es_rocio, suppressWarnings(as.numeric(Ha_Asignada_1ra)), suppressWarnings(as.numeric(Ha_Asignada)))',
  '            } else if (!es_rocio) suppressWarnings(as.numeric(Ha_Asignada)) else NA_real_,',
  '            Kg_1ra   = if ("Kg_Asignado_1ra" %in% names(.)) {',
  '              ifelse(es_rocio, suppressWarnings(as.numeric(Kg_Asignado_1ra)), suppressWarnings(as.numeric(Valor)))',
  '            } else if (!es_rocio) suppressWarnings(as.numeric(Valor)) else NA_real_,',
  '            Ha_2da   = if ("Ha_Asignada_2da" %in% names(.)) {',
  '              ifelse(es_rocio, suppressWarnings(as.numeric(Ha_Asignada_2da)), 0)',
  '            } else 0,',
  '            Kg_2da   = if ("Kg_Asignado_2da" %in% names(.)) {',
  '              ifelse(es_rocio, suppressWarnings(as.numeric(Kg_Asignado_2da)), 0)',
  '            } else 0,',
  '            Mot_val  = if ("Motivo_Ajuste" %in% names(.)) as.character(Motivo_Ajuste) else NA_character_',
  '          ) %>%',
  '          group_by(Módulo_k, Turno_k, Var_k) %>%',
  '          summarise(',
  '            Ha_Ajustada_Total = round(sum(Ha_val, na.rm = TRUE), 2),',
  '            Ha_Aj_1ra_Ed      = round(sum(Ha_1ra, na.rm = TRUE), 2),',
  '            Kg_Aj_1ra_Ed      = round(sum(Kg_1ra, na.rm = TRUE), 2),',
  '            Ha_Aj_2da_Ed      = round(sum(Ha_2da, na.rm = TRUE), 2),',
  '            Kg_Aj_2da_Ed      = round(sum(Kg_2da, na.rm = TRUE), 2),',
  '            Tiene_Ha_Asig     = any(!is.na(Ha_val) & Ha_val > 0),',
  '            Tiene_Detalle_Pas = first(es_rocio) & any(!is.na(Ha_1ra) | !is.na(Ha_2da)),'
)
cat("Step 3 done\n")

writeLines(lines, app_path, sep = "\r\n", useBytes = TRUE)
lines <- readLines(app_path, warn = FALSE, encoding = "UTF-8")

# 4. Ha Aj. 1ª Pas in tabla_detalle_turnos
idx_ha1pas <- which(grepl("^\\s*# 1ª Pasada Ajustada: Faltante para completar el turno físico", lines))[1]
idx_cwhen <- idx_ha1pas + which(grepl("Tipo == \"Testblock\" ~ 0,", lines[idx_ha1pas:(idx_ha1pas + 10)]))[1] - 1
lines[idx_cwhen:(idx_cwhen + 1)] <- c(
  '          Tipo == "Testblock" ~ 0,',
  '          !Tiene_Dias_Ajustados ~ 0,',
  '          limpiar_variedad(Variedad) != "ROCIO" & coalesce(Tiene_Ha_Asig, FALSE) & coalesce(Ha_Ajustada_Total, 0) > 0 ~ round(pmin(Área, Ha_Ajustada_Total), 2),'
)
cat("Step 4 done\n")

writeLines(lines, app_path, sep = "\r\n", useBytes = TRUE)
lines <- readLines(app_path, warn = FALSE, encoding = "UTF-8")

# 5. Kg_Aj_1ra in tabla_detalle_turnos
idx_kg1pas <- which(grepl("^\\s*Kg_Aj_1ra = case_when\\(", lines))[1]
idx_kg_cwhen <- idx_kg1pas + which(grepl("!Tiene_Dias_Ajustados ~ 0,", lines[idx_kg1pas:(idx_kg1pas + 5)]))[1] - 1
lines[idx_kg_cwhen] <- '          !Tiene_Dias_Ajustados ~ 0,\n          limpiar_variedad(Variedad) != "ROCIO" ~ kg_aj_neto_u,'
cat("Step 5 done\n")

writeLines(lines, app_path, sep = "\r\n", useBytes = TRUE)
lines <- readLines(app_path, warn = FALSE, encoding = "UTF-8")

# 6. pasada_sugerida
idx_psug <- which(grepl("^\\s*pasada_sugerida <- if \\(nrow\\(valvs_celda\\)", lines))[1]
lines[idx_psug] <- '    pasada_sugerida <- if (limpiar_variedad(fila$Variedad) != "ROCIO") 1L else if (nrow(valvs_celda) > 0 && any(valvs_celda$Num_Pasada == 2)) {'
cat("Step 6 done\n")

# 7. ui_modal_calculo_vivo pasada_activa & modal_btn_guardar pasada_activa
idx_pact1 <- which(grepl("^\\s*pasada_activa <- if \\(!is\\.null\\(input\\$modal_input_pasada\\)", lines))
for (ip in idx_pact1) {
  lines[ip] <- '    pasada_activa <- if (limpiar_variedad(ctx$fila$Variedad) == "ROCIO") { if (!is.null(input$modal_input_pasada)) as.integer(input$modal_input_pasada) else ctx$pasada_sug } else { 1L }'
}
cat("Step 7 done\n")

# 8. num_pasada_sel
idx_npas <- which(grepl("^\\s*num_pasada_sel <- pasada_activa", lines))[1]
lines[idx_npas] <- '    num_pasada_sel <- if (limpiar_variedad(ctx$fila$Variedad) == "ROCIO") pasada_activa else 1L'
cat("Step 8 done\n")

# 9. modal_btn_restablecer pasada_activa
idx_prest <- which(grepl("pasada_activa <- if \\(nrow\\(ctx\\$ed_celda\\) > 0 && coalesce\\(ctx\\$ed_celda\\$Ha_Asignada_2da", lines))[1]
lines[idx_prest] <- '    pasada_activa <- if (limpiar_variedad(ctx$fila$Variedad) == "ROCIO" && nrow(ctx$ed_celda) > 0 && coalesce(ctx$ed_celda$Ha_Asignada_2da[1], 0) > 0) 2L else if (limpiar_variedad(ctx$fila$Variedad) == "ROCIO") ctx$pasada_sug else 1L'
cat("Step 9 done\n")

# Save final
writeLines(lines, app_path, sep = "\r\n", useBytes = TRUE)
cat("Written to app.R successfully!\n")

# Parse check
p <- parse(app_path)
cat("PARSED SUCCESSFULLY! Total expressions:", length(p), "\n")
