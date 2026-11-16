# ==============================================================================
# SCRIPT DE CORRECCIÓN: EVITAR DUPLICACIÓN DE 2DA PASADA CUANDO YA ESTÁ COMPLETA
# ==============================================================================

aplicar_fix_2da <- function(app_path) {
  cat(sprintf("\n=== Procesando: %s ===\n", app_path))
  lines <- readLines(app_path, warn = FALSE)
  cat(sprintf("Líneas iniciales: %d\n", length(lines)))
  
  # 1. Insertar valvs_con_2da_rc antes de cols_ha_sap_exist
  idx_cols_sap <- grep("cols_ha_sap_exist <- intersect\\(cols_ha_prev, names\\(df_hist_oficial_wide\\)\\)", lines)[1]
  if (is.na(idx_cols_sap)) {
    stop("No se encontró cols_ha_sap_exist en ", app_path)
  }
  
  bloque_valvs_2da <- c(
    "      # Válvulas con 2da pasada ya registrada/ejecutada en rutas de campo de la semana actual",
    "      valvs_con_2da_rc <- if (nrow(rc_sem) > 0 && 'codigo' %in% names(rc_sem)) {",
    "        rc_sem %>%",
    "          filter(coalesce(suppressWarnings(as.numeric(Num_Pasada)), 1) == 2) %>%",
    "          mutate(codigo_norm = norm_codigo_valvula(codigo)) %>%",
    "          filter(coalesce(Ha_Total_Prog, 0) > 0 | coalesce(Kg_Total_Prog, 0) > 0) %>%",
    "          pull(codigo_norm) %>%",
    "          unique()",
    "      } else character(0)",
    ""
  )
  lines <- c(lines[1:(idx_cols_sap - 1)], bloque_valvs_2da, lines[idx_cols_sap:length(lines)])
  cat(sprintf("1. Insertado valvs_con_2da_rc antes de línea %d\n", idx_cols_sap))
  
  # 2. Insertar ha_real_2da_map antes de turno_avance_ha
  idx_turno_av <- grep("turno_avance_ha <- valvulas_calc %>%", lines)[1]
  if (is.na(idx_turno_av)) {
    stop("No se encontró turno_avance_ha en ", app_path)
  }
  
  bloque_map_2da <- c(
    "      # Resumen de 2da pasada real por turno desde universo_base",
    "      u_base_pre <- tryCatch(universo_base(), error = function(e) NULL)",
    "      ha_real_2da_map <- if (!is.null(u_base_pre) && nrow(u_base_pre) > 0) {",
    "        u_base_pre %>%",
    "          group_by(",
    "            Módulo = trimws(as.character(Módulo)),",
    "            Turno  = trimws(as.character(Turno)),",
    "            Variedad = limpiar_variedad(Variedad)",
    "          ) %>%",
    "          summarise(",
    "            Ha_Real_2da = sum(coalesce(`Ha Real 2ª Pas`, 0), na.rm = TRUE),",
    "            .groups = 'drop'",
    "          )",
    "      } else {",
    "        tibble(Módulo = character(), Turno = character(), Variedad = character(), Ha_Real_2da = numeric())",
    "      }",
    ""
  )
  lines <- c(lines[1:(idx_turno_av - 1)], bloque_map_2da, lines[idx_turno_av:length(lines)])
  cat(sprintf("2. Insertado ha_real_2da_map antes de línea %d\n", idx_turno_av))
  
  # 3. Actualizar turno_avance_ha para incluir Ha_Real_2da, Ha_Faltante_2da_Neto y Turno_Completo_2da
  idx_compl_1ra <- grep("Turno_Completo_1ra     = \\(Ha_Faltante_Turno_Neto <= 0.05\\)", lines)[1]
  if (is.na(idx_compl_1ra)) {
    stop("No se encontró Turno_Completo_1ra en ", app_path)
  }
  
  # Reemplazar mutate en turno_avance_ha
  # Buscar la línea de left_join(resumen_man_turno)
  idx_lj_resumen <- grep("left_join\\(resumen_man_turno, by = c\\(\"Módulo\", \"Turno\", \"Variedad\"\\)\\)", lines)[1]
  lines[idx_lj_resumen] <- "        left_join(resumen_man_turno, by = c(\"Módulo\", \"Turno\", \"Variedad\")) %>%\n        left_join(ha_real_2da_map, by = c(\"Módulo\", \"Turno\", \"Variedad\"))"
  
  lines[idx_compl_1ra] <- paste0(
    "          Turno_Completo_1ra     = (Ha_Faltante_Turno_Neto <= 0.05),\n",
    "          Ha_Real_2da            = coalesce(Ha_Real_2da, 0),\n",
    "          Ha_Faltante_2da_Neto   = pmax(0, Area_Total_Turno - Ha_Real_2da - Ha_Manual_2da),\n",
    "          Turno_Completo_2da     = (Ha_Faltante_2da_Neto <= 0.05)"
  )
  cat(sprintf("3. Actualizado turno_avance_ha con Ha_Faltante_2da_Neto y Turno_Completo_2da\n"))
  
  # 4. Actualizar select de rend_turno y mutate en valvulas_calc
  idx_sel_turnocompl <- grep("Area_Total_Turno, Ha_Manual_1ra, Ha_Manual_2da, Ha_Faltante_Turno_Neto, Turno_Completo_1ra,", lines)[1]
  lines[idx_sel_turnocompl] <- "            Area_Total_Turno, Ha_Manual_1ra, Ha_Manual_2da, Ha_Faltante_Turno_Neto, Turno_Completo_1ra, Ha_Real_2da, Ha_Faltante_2da_Neto, Turno_Completo_2da,"
  
  idx_valv_cerro <- grep("Valvula_Cerro_En_Lun_Mar = Semana_Ant_No_Finalizo & \\(Valvula_Cosecho_Lunes \\| Valvula_Cosecho_Martes\\),", lines)[1]
  lines[idx_valv_cerro] <- paste0(
    lines[idx_valv_cerro], "\n",
    "          Valvula_Tiene_2da_Real   = codigo_norm %in% valvs_con_2da_rc,"
  )
  cat("4. Actualizado valvulas_calc con Valvula_Tiene_2da_Real\n")
  
  # 5. Actualizar reg_2da_remanente
  idx_reg_rem <- grep("reg_2da_remanente <- valvulas_calc %>%", lines)[1]
  idx_filter_rem <- grep("filter\\(coalesce\\(Esta_Proyectado, FALSE\\), Semana_Ant_No_Finalizo, Es_Ciclo_Corto, Valvula_Cerro_En_Lun_Mar, Ha_Cerrada_Lun_Mar > 0\\)", lines)[1]
  
  # Reemplazar bloque reg_2da_remanente
  idx_rem_end <- grep("filter\\(Kg_Asignado > 0, Dia_Asignado %in% dias_ajustables_nom\\)", lines)
  idx_rem_end_1 <- idx_rem_end[idx_rem_end > idx_reg_rem][1]
  
  bloque_nuevo_rem <- c(
    "      reg_2da_remanente <- valvulas_calc %>%",
    "        filter(",
    "          coalesce(Esta_Proyectado, FALSE),",
    "          Semana_Ant_No_Finalizo,",
    "          Es_Ciclo_Corto,",
    "          Valvula_Cerro_En_Lun_Mar,",
    "          Ha_Cerrada_Lun_Mar > 0,",
    "          !coalesce(Turno_Completo_2da, FALSE),",
    "          !coalesce(Valvula_Tiene_2da_Real, FALSE)",
    "        ) %>%",
    "        group_by(Módulo, Turno, Variedad) %>%",
    "        mutate(",
    "          tot_2da_rem = sum(Ha_Cerrada_Lun_Mar, na.rm = TRUE),",
    "          Ha_2da = if_else(",
    "            tot_2da_rem > coalesce(Ha_Faltante_2da_Neto, tot_2da_rem) & tot_2da_rem > 0,",
    "            round(Ha_Cerrada_Lun_Mar * (coalesce(Ha_Faltante_2da_Neto, tot_2da_rem) / tot_2da_rem), 2),",
    "            Ha_Cerrada_Lun_Mar",
    "          ),",
    "          Dia_Cierre_Idx = ifelse(Valvula_Cosecho_Lunes, 1L, 2L),",
    "          Dia_Asignado = case_when(",
    "            Valvula_Cosecho_Lunes  ~ ifelse(dia_fin_idx >= 5L, 'Viernes', dia_fin_nom),",
    "            Valvula_Cosecho_Martes ~ ifelse(dia_fin_idx >= 6L, 'Sábado', dia_fin_nom),",
    "            TRUE                   ~ ifelse(dia_fin_idx >= 5L, 'Viernes', dia_fin_nom)",
    "          ),",
    "          Dia_Nuevo_Idx = match(Dia_Asignado, dias_nombres),",
    "          Dias_Crecimiento = pmax(1L, Dia_Nuevo_Idx - Dia_Cierre_Idx),",
    "          Factor_Crecimiento = case_when(",
    "            limpiar_variedad(Variedad) == 'ROCIO' & Valvula_Cosecho_Lunes  ~ coalesce(Pct_Pasada_Lun, 60) / 100,",
    "            limpiar_variedad(Variedad) == 'ROCIO' & Valvula_Cosecho_Martes ~ coalesce(Pct_Pasada_Mar, 60) / 100,",
    "            limpiar_variedad(Variedad) == 'ROCIO'                          ~ coalesce(Pct_Pasada_Lun, 60) / 100,",
    "            TRUE                                                           ~ pmin(1, Dias_Crecimiento / pmax(1, Frecuencia_Dias))",
    "          ),",
    "          Rend_Base_Dia  = ifelse(Valvula_Cosecho_Lunes, Rend_Valv_Lunes, Rend_Valv_Martes),",
    "          Kg_Asignado    = round(Ha_2da * coalesce(Rend_Base_Dia, 0) * Factor_Crecimiento, 2),",
    "          Ha_Asignada    = Ha_2da,",
    "          Num_Pasada     = 2L,",
    "          Es_Ruta_Sin_Kg = FALSE,",
    "          Motivo_Ajuste  = sprintf('2da pasada Cierre S-1 (%s • %d%% sin floración)', ",
    "                                   ifelse(Valvula_Cosecho_Lunes, 'Lunes', 'Martes'), round(Factor_Crecimiento * 100))",
    "        ) %>%",
    "        ungroup() %>%",
    "        filter(Kg_Asignado > 0, Ha_Asignada > 0, Dia_Asignado %in% dias_ajustables_nom)"
  )
  lines <- c(lines[1:(idx_reg_rem - 1)], bloque_nuevo_rem, lines[(idx_rem_end_1 + 1):length(lines)])
  cat("5. Actualizado reg_2da_remanente con exclusión si ya tiene 2da pasada\n")
  
  # 6. Actualizar reg_2da_lun
  idx_reg_lun <- grep("reg_2da_lun <- valvulas_calc %>%", lines)[1]
  idx_lun_end <- grep("filter\\(Kg_Asignado > 0, Dia_Asignado %in% dias_ajustables_nom\\)", lines)
  idx_lun_end_1 <- idx_lun_end[idx_lun_end > idx_reg_lun][1]
  
  bloque_nuevo_lun <- c(
    "      reg_2da_lun <- valvulas_calc %>%",
    "        filter(",
    "          coalesce(Esta_Proyectado, FALSE),",
    "          !Semana_Ant_No_Finalizo,",
    "          Es_Ciclo_Corto,",
    "          Ha_Cosechada_Lunes > 0,",
    "          !coalesce(Turno_Completo_2da, FALSE),",
    "          !coalesce(Valvula_Tiene_2da_Real, FALSE)",
    "        ) %>%",
    "        group_by(Módulo, Turno, Variedad) %>%",
    "        mutate(",
    "          tot_2da_lun = sum(Ha_Cosechada_Lunes, na.rm = TRUE),",
    "          Ha_2da_Corta = if_else(",
    "            tot_2da_lun > coalesce(Ha_Faltante_2da_Neto, tot_2da_lun) & tot_2da_lun > 0,",
    "            round(Ha_Cosechada_Lunes * (coalesce(Ha_Faltante_2da_Neto, tot_2da_lun) / tot_2da_lun), 2),",
    "            Ha_Cosechada_Lunes",
    "          ),",
    "          Dia_Base_Frec  = ifelse(codigo_norm %in% rectif_martes_ids, 'Martes', 'Lunes'),",
    "          Dia_Asignado   = as.character(desplazar_dia_frecuencia(Dia_Base_Frec, Frecuencia_Dias, dias_nombres, dia_inicio_idx, dia_inicio_nom, dia_fin_idx, dia_fin_nom)),",
    "          Pct_Rend       = if_else(limpiar_variedad(Variedad) == 'ROCIO', coalesce(Pct_Pasada_Lun, 60) / 100, 1.0),",
    "          Rend_2da_Aplicar = round(Rend_Valv_Lunes * Pct_Rend, 2),",
    "          Kg_Asignado    = round(Ha_2da_Corta * Rend_2da_Aplicar, 2),",
    "          Ha_Asignada    = Ha_2da_Corta,",
    "          Num_Pasada     = 2L,",
    "          Es_Ruta_Sin_Kg = FALSE,",
    "          Motivo_Ajuste  = sprintf('2da pasada Rocío (Rend Lunes: %s kg/ha • %d%% sin floración)', ",
    "                                   format(round(Rend_Valv_Lunes), big.mark = ','), round(Pct_Rend * 100))",
    "        ) %>%",
    "        ungroup() %>%",
    "        filter(Kg_Asignado > 0, Ha_Asignada > 0, Dia_Asignado %in% dias_ajustables_nom)"
  )
  lines <- c(lines[1:(idx_reg_lun - 1)], bloque_nuevo_lun, lines[(idx_lun_end_1 + 1):length(lines)])
  cat("6. Actualizado reg_2da_lun con exclusión si ya tiene 2da pasada\n")
  
  # 7. Actualizar reg_2da_mar
  idx_reg_mar <- grep("reg_2da_mar <- valvulas_calc %>%", lines)[1]
  idx_mar_end <- grep("filter\\(Kg_Asignado > 0, Dia_Asignado %in% dias_ajustables_nom\\)", lines)
  idx_mar_end_1 <- idx_mar_end[idx_mar_end > idx_reg_mar][1]
  
  bloque_nuevo_mar <- c(
    "      reg_2da_mar <- valvulas_calc %>%",
    "        filter(",
    "          coalesce(Esta_Proyectado, FALSE),",
    "          !Semana_Ant_No_Finalizo,",
    "          Es_Ciclo_Corto,",
    "          Ha_Cosechada_Martes > 0,",
    "          !coalesce(Turno_Completo_2da, FALSE),",
    "          !coalesce(Valvula_Tiene_2da_Real, FALSE)",
    "        ) %>%",
    "        group_by(Módulo, Turno, Variedad) %>%",
    "        mutate(",
    "          tot_2da_mar = sum(Ha_Cosechada_Martes, na.rm = TRUE),",
    "          Ha_2da_Corta = if_else(",
    "            tot_2da_mar > coalesce(Ha_Faltante_2da_Neto, tot_2da_mar) & tot_2da_mar > 0,",
    "            round(Ha_Cosechada_Martes * (coalesce(Ha_Faltante_2da_Neto, tot_2da_mar) / tot_2da_mar), 2),",
    "            Ha_Cosechada_Martes",
    "          ),",
    "          Dia_Asignado   = as.character(desplazar_dia_frecuencia('Martes', Frecuencia_Dias, dias_nombres, dia_inicio_idx, dia_inicio_nom, dia_fin_idx, dia_fin_nom)),",
    "          Pct_Rend       = if_else(limpiar_variedad(Variedad) == 'ROCIO', coalesce(Pct_Pasada_Mar, 60) / 100, 1.0),",
    "          Rend_2da_Aplicar = round(Rend_Valv_Martes * Pct_Rend, 2),",
    "          Kg_Asignado    = round(Ha_2da_Corta * Rend_2da_Aplicar, 2),",
    "          Ha_Asignada    = Ha_2da_Corta,",
    "          Num_Pasada     = 2L,",
    "          Es_Ruta_Sin_Kg = FALSE,",
    "          Motivo_Ajuste  = sprintf('2da pasada Rocío (Rend Martes: %s kg/ha • %d%% sin floración)', ",
    "                                   format(round(Rend_Valv_Martes), big.mark = ','), round(Pct_Rend * 100))",
    "        ) %>%",
    "        ungroup() %>%",
    "        filter(Kg_Asignado > 0, Ha_Asignada > 0, Dia_Asignado %in% dias_ajustables_nom)"
  )
  lines <- c(lines[1:(idx_reg_mar - 1)], bloque_nuevo_mar, lines[(idx_mar_end_1 + 1):length(lines)])
  cat("7. Actualizado reg_2da_mar con exclusión si ya tiene 2da pasada\n")
  
  # 8. Actualizar reg_2da_mie
  idx_reg_mie <- grep("reg_2da_mie <- if \\(nrow\\(ed_man_sem\\) > 0 && \"Miércoles\" %in% ed_man_sem\\$Campo", lines)[1]
  idx_filter_mie <- grep("limpiar_variedad\\(Variedad\\) == \"ROCIO\",", lines)
  idx_filter_mie_1 <- idx_filter_mie[idx_filter_mie > idx_reg_mie][1]
  
  lines[idx_filter_mie_1] <- "            limpiar_variedad(Variedad) == \"ROCIO\",\n            !coalesce(Turno_Completo_2da, FALSE),\n            !coalesce(Valvula_Tiene_2da_Real, FALSE),"
  cat("8. Actualizado reg_2da_mie con exclusión si ya tiene 2da pasada\n")
  
  # Guardar y validar sintaxis
  writeLines(lines, app_path)
  cat(sprintf("✔ Archivo guardado (%d líneas).\n", length(lines)))
  
  cat("🔍 Verificando sintaxis R con parse()...\n")
  parse(app_path)
  cat("✅ ¡Sintaxis verificada y correcta sin errores!\n")
}

# Aplicar a Actualizacion Semanal RNA/app.R
aplicar_fix_2da("Actualizacion Semanal RNA/app.R")

# Aplicar también a Actualizacion Semanal/app.R para que ambas apps queden corregidas
aplicar_fix_2da("Actualizacion Semanal/app.R")
