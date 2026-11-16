lines <- readLines("Actualizacion Semanal/app.R", encoding = "UTF-8", warn = FALSE)

# 1. Update lines 6317 to allow Ha Real 2ª Pas for any variety if Ha_Rc_2da > 0
idx_ha_2da <- which(grepl("^\\s*`Ha Real 2ª Pas`\\s*=\\s*ifelse\\(limpiar_variedad\\(Variedad\\)", lines))
cat("Found idx_ha_2da at:", idx_ha_2da, "\n")
if (length(idx_ha_2da) > 0) {
  lines[idx_ha_2da] <- "        `Ha Real 2ª Pas` = ifelse(coalesce(Ha_Rc_2da, 0) > 0, round(pmin(Área, Ha_Rc_2da), 2), 0),"
}

# 2. Update lines 6577-6586 to separate Kg_Real_2da for any variety if Kg_Real_2da_sap > 0
idx_kg_2da <- which(grepl("^\\s*Kg_Real_2da\\s*=\\s*case_when", lines))
cat("Found idx_kg_2da at:", idx_kg_2da, "\n")
if (length(idx_kg_2da) > 0) {
  # check the next few lines
  # lines[idx_kg_2da + 1] is: limpiar_variedad(Variedad) == "ROCIO" & coalesce(Kg_Real_2da_sap, 0) > 0 ~
  if (grepl("coalesce\\(Kg_Real_2da_sap, 0\\) > 0", lines[idx_kg_2da + 1])) {
    lines[idx_kg_2da + 1] <- "          coalesce(Kg_Real_2da_sap, 0) > 0 ~ "
  }
}

idx_kg_1ra <- which(grepl("^\\s*Kg_Real_1ra\\s*=\\s*case_when", lines))
cat("Found idx_kg_1ra at:", idx_kg_1ra, "\n")
if (length(idx_kg_1ra) > 0) {
  # replace lines[idx_kg_1ra : idx_kg_1ra+3] with simple pmax:
  # Kg_Real_1ra = round(pmax(0, Kg_SAP_Real - Kg_Real_2da), 2),
  lines[idx_kg_1ra] <- "        Kg_Real_1ra      = round(pmax(0, Kg_SAP_Real - Kg_Real_2da), 2),"
  lines[idx_kg_1ra + 1] <- "        # 1ª pasada neta"
  lines[idx_kg_1ra + 2] <- "        # (descontando 2ª pasada si hubo)"
  lines[idx_kg_1ra + 3] <- "        # end 1ra pasada"
}

# 3. In datos_comparativo_reactivo: replace evaluar_escenario and rc_avance_semana
idx_esc <- which(grepl("^\\s*df_esc\\s*<-\\s*rv_escenarios\\(\\)", lines))
# Take the one inside datos_comparativo_reactivo (the last one, line ~16817)
idx_esc_comp <- tail(idx_esc, 1)
cat("Found idx_esc_comp at:", idx_esc_comp, "\n")

# Find the start of PERSPECTIVA 1
idx_p1 <- which(grepl("^\\s*# PERSPECTIVA 1: COMPARAR 2 ESCENARIOS VS REAL", lines))
idx_p1 <- idx_p1[idx_p1 > idx_esc_comp][1]
cat("Found idx_p1 at:", idx_p1, "\n")

# The replacement block between idx_esc_comp and idx_p1
new_block <- c(
  '    df_esc   <- rv_escenarios()',
  '    dias_semana_nombres <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")',
  '    ',
  '    # Avance real en campo acumulado de toda la semana (Lunes a Sábado) desde rutas de cosecha',
  '    rc_file_cand <- tryCatch(obtener_rutas_campo_en_vivo(input$filtro_semana), error = function(e) cargar_rutas_cosecha())',
  '    if (is.null(rc_file_cand) || nrow(rc_file_cand) == 0) rc_file_cand <- tryCatch(cargar_rutas_cosecha(), error = function(e) NULL)',
  '    ',
  '    rc_sem_comp <- if (!is.null(rc_file_cand) && "Semana" %in% names(rc_file_cand)) {',
  '      rc_file_cand %>%',
  '        filter(as.numeric(Semana) == as.numeric(input$filtro_semana)) %>%',
  '        filter(!("Tipo_Registro" %in% names(.)) | Tipo_Registro != "Rectificacion")',
  '    } else tibble()',
  '    ',
  '    rc_avance_semana <- if (nrow(rc_sem_comp) > 0) {',
  '      cols_ha_dias <- intersect(c("Ha_Lunes", "Ha_Martes", "Ha_Miércoles", "Ha_Jueves", "Ha_Viernes", "Ha_Sábado"), names(rc_sem_comp))',
  '      rc_sem_comp %>%',
  '        mutate(',
  '          Turno_num_rc = suppressWarnings(as.numeric(Turno)),',
  '          Turno_clean  = ifelse(!is.na(Turno_num_rc), as.character(Turno_num_rc), trimws(as.character(Turno))),',
  '          Módulo_clean = case_when(',
  '            grepl("2026|^26", as.character(input$filtro_campana)) & as.character(Módulo) %in% c("13.1", "13.2") ~ "13",',
  '            grepl("2026|^26", as.character(input$filtro_campana)) & as.character(Módulo) %in% c("14.1", "14.2") ~ "14",',
  '            as.character(Módulo) == "9" & between(Turno_num_rc, 1, 11)  ~ "9.2",',
  '            as.character(Módulo) == "9" & Turno_num_rc >= 12            ~ "9.1",',
  '            as.character(Módulo) == "11" & between(Turno_num_rc, 1, 12) ~ "11.1",',
  '            as.character(Módulo) == "11" & Turno_num_rc >= 13           ~ "11.2",',
  '            TRUE                                                         ~ trimws(as.character(Módulo))',
  '          ),',
  '          Sector_clean = trimws(as.character(Sector)),',
  '          Var_clean    = toupper(trimws(limpiar_variedad(Variedad))),',
  '          num_pas_rc   = if ("Num_Pasada" %in% names(.)) suppressWarnings(as.numeric(Num_Pasada)) else 1,',
  '          Ha_Sem_Fila  = if (length(cols_ha_dias) > 0) rowSums(across(all_of(cols_ha_dias)), na.rm = TRUE) else 0',
  '        ) %>%',
  '        group_by(Módulo = Módulo_clean, Turno = Turno_clean, Variedad = Var_clean) %>%',
  '        summarise(',
  '          Ha_Real_Semana_1ra = sum(ifelse(coalesce(num_pas_rc, 1) == 1, Ha_Sem_Fila, 0), na.rm = TRUE),',
  '          Ha_Real_Semana_2da = sum(ifelse(coalesce(num_pas_rc, 1) == 2, Ha_Sem_Fila, 0), na.rm = TRUE),',
  '          Ha_Real_Semana_Tot = sum(Ha_Sem_Fila, na.rm = TRUE),',
  '          .groups = "drop"',
  '        )',
  '    } else {',
  '      tibble(Módulo = character(), Turno = character(), Variedad = character(),',
  '             Ha_Real_Semana_1ra = numeric(), Ha_Real_Semana_2da = numeric(), Ha_Real_Semana_Tot = numeric())',
  '    }',
  '    ',
  '    # Helper: Normalizar y evaluar un escenario sobre universo_base()',
  '    evaluar_escenario <- function(df_ed, esc_info = tibble()) {',
  '      dias_ajuste <- character(0)',
  '      if (!is.null(esc_info) && nrow(esc_info) > 0) {',
  '        d_desde <- as.character(esc_info$Dia_Desde[1])',
  '        d_hasta <- as.character(esc_info$Dia_Hasta[1])',
  '        idx_d <- match(d_desde, dias_semana_nombres)',
  '        idx_h <- match(d_hasta, dias_semana_nombres)',
  '        if (!is.na(idx_d) && !is.na(idx_h) && idx_d <= idx_h) {',
  '          dias_ajuste <- dias_semana_nombres[idx_d:idx_h]',
  '        } ',
  '      }',
  '      if (length(dias_ajuste) == 0 && !is.null(df_ed) && nrow(df_ed) > 0) {',
  '        dias_ajuste <- intersect(dias_semana_nombres, unique(as.character(df_ed$Campo)))',
  '      }',
  '      if (length(dias_ajuste) == 0) dias_ajuste <- dias_semana_nombres',
  '      ',
  '      min_idx <- min(match(dias_ajuste, dias_semana_nombres))',
  '      dias_previos <- if (min_idx > 1) dias_semana_nombres[1:(min_idx - 1)] else character(0)',
  '      ',
  '      ed_agg <- if (!is.null(df_ed) && nrow(df_ed) > 0) {',
  '        df_ed %>%',
  '          filter(Campo %in% dias_ajuste, coalesce(Valor, 0) > 0) %>%',
  '          mutate(',
  '            Turno_num    = suppressWarnings(as.numeric(Turno)),',
  '            Turno_clean  = ifelse(!is.na(Turno_num), as.character(Turno_num), trimws(as.character(Turno))),',
  '            Módulo_clean = trimws(as.character(Módulo)),',
  '            Sector_clean = trimws(as.character(Sector)),',
  '            Var_clean    = toupper(trimws(limpiar_variedad(Variedad)))',
  '          ) %>%',
  '          group_by(Sector = Sector_clean, Módulo = Módulo_clean, Turno = Turno_clean, Variedad = Var_clean) %>%',
  '          summarise(',
  '            Ha_Ajustada_Esc = round(sum(coalesce(Ha_Asignada, 0), na.rm = TRUE), 2),',
  '            Kg_Ajustado_Esc = round(sum(coalesce(Valor, 0), na.rm = TRUE), 1),',
  '            Motivo_Ajuste   = {',
  '              m <- if ("Motivo_Ajuste" %in% names(.)) na.omit(as.character(Motivo_Ajuste)) else character(0)',
  '              m <- m[nzchar(m) & m != "Sin ajuste programado"]',
  '              if (length(m) > 0) m[1] else "Ajuste programado"',
  '            },',
  '            .groups = "drop"',
  '          )',
  '      } else {',
  '        tibble(Sector = character(), Módulo = character(), Turno = character(), Variedad = character(), Ha_Ajustada_Esc = numeric(), Kg_Ajustado_Esc = numeric(), Motivo_Ajuste = character())',
  '      }',
  '      ',
  '      cols_prev_u <- intersect(dias_previos, names(u_base))',
  '      cols_sem_u  <- intersect(dias_semana_nombres, names(u_base))',
  '      ',
  '      res <- u_base %>%',
  '        mutate(',
  '          Turno_num    = suppressWarnings(as.numeric(Turno)),',
  '          Turno_clean  = ifelse(!is.na(Turno_num), as.character(Turno_num), trimws(as.character(Turno))),',
  '          Módulo_clean = trimws(as.character(Módulo)),',
  '          Sector_clean = trimws(as.character(Sector)),',
  '          Var_clean    = toupper(trimws(limpiar_variedad(Variedad))),',
  '          Pasada       = "1ª Pasada",',
  '          Kg_Real_Prev = if (length(cols_prev_u) > 0) round(rowSums(across(all_of(cols_prev_u)), na.rm = TRUE), 1) else 0,',
  '          Kg_Real_Semana = round(rowSums(across(all_of(cols_sem_u)), na.rm = TRUE), 1),',
  '          Kg_Proy_Oficial = round(coalesce(Proyección, 0), 1)',
  '        ) %>%',
  '        left_join(rc_avance_semana, by = c("Módulo_clean" = "Módulo", "Turno_clean" = "Turno", "Var_clean" = "Variedad")) %>%',
  '        mutate(',
  '          Sector             = Sector_clean,',
  '          Ha_Real_Semana_1ra = coalesce(Ha_Real_Semana_1ra, 0),',
  '          Ha_Real_Prev_U     = coalesce(`Ha Real 1ª Pas`, 0),',
  '          # En Comparativo se evalúa el cierre de la semana completa (Lunes a Sábado),',
  '          # evitando que quede en 0 si la cosecha fue en días posteriores al selector DESDE',
  '          Ha_Campo = case_when(',
  '            Ha_Real_Semana_1ra > 0 ~ round(pmin(Área, Ha_Real_Semana_1ra), 2),',
  '            Ha_Real_Prev_U > 0     ~ round(pmin(Área, Ha_Real_Prev_U), 2),',
  '            Kg_Real_Semana > 0 & Área > 0 ~ Área,',
  '            TRUE                   ~ 0',
  '          ),',
  '          Pct_Termino  = case_when(',
  '            Área > 0 & Ha_Campo >= (Área - 0.05) ~ 100,',
  '            Área > 0 & (Ha_Campo / Área) >= 0.98  ~ 100,',
  '            Área > 0 ~ round(pmin(100, (Ha_Campo / Área) * 100), 1),',
  '            TRUE ~ 0',
  '          )',
  '        ) %>%',
  '        left_join(ed_agg, by = c("Sector_clean" = "Sector", "Módulo_clean" = "Módulo", "Turno_clean" = "Turno", "Var_clean" = "Variedad")) %>%',
  '        mutate(',
  '          Ha_Ajustada_Esc    = coalesce(Ha_Ajustada_Esc, 0),',
  '          Kg_Ajustado_Esc    = coalesce(Kg_Ajustado_Esc, 0),',
  '          Kg_Ajustado_Semana = round(Kg_Real_Prev + Kg_Ajustado_Esc, 1),',
  '          Diferencia_Kg      = round(Kg_Ajustado_Semana - Kg_Real_Semana, 1),',
  '          Pct_Desvio         = ifelse(Kg_Real_Semana > 0, round((Diferencia_Kg / Kg_Real_Semana) * 100, 1), NA_real_),',
  '          Ha_Rend_Aj         = case_when(',
  '            Ha_Ajustada_Esc > 0 ~ Ha_Ajustada_Esc,',
  '            Ha_Campo > 0        ~ Ha_Campo,',
  '            Área > 0            ~ Área,',
  '            TRUE                ~ 0',
  '          ),',
  '          Rend_Ajustado      = ifelse(Ha_Rend_Aj > 0 & Kg_Ajustado_Semana > 0, round(Kg_Ajustado_Semana / Ha_Rend_Aj, 1), NA_real_),',
  '          Rend_Real          = ifelse(Ha_Campo > 0 & Kg_Real_Semana > 0, round(Kg_Real_Semana / Ha_Campo, 1), NA_real_),',
  '          Delta_Rend         = ifelse(!is.na(Rend_Ajustado) & !is.na(Rend_Real), round(Rend_Ajustado - Rend_Real, 1), NA_real_),',
  '          Turno_Label        = sprintf("M%s-T%s (%s)", Módulo_clean, Turno_clean, Var_clean),',
  '          Motivo_Ajuste      = coalesce(Motivo_Ajuste, "Sin ajuste programado"),',
  '          Módulo             = Módulo_clean,',
  '          Turno              = Turno_clean,',
  '          Variedad           = Var_clean',
  '        )',
  '      return(res)',
  '    }',
  '    ',
  '    # Helper: Filtros contextuales y globales comunes',
  '    aplicar_filtros <- function(df_in) {',
  '      res_df <- df_in',
  '      if (!is.null(input$filtro_grupo) && length(input$filtro_grupo) > 0) {',
  '        mods_permitidos <- unlist(grupos_modulos[input$filtro_grupo], use.names = FALSE)',
  '        mods_num <- suppressWarnings(as.numeric(sub("\\\\..*", "", as.character(res_df$Módulo))))',
  '        res_df <- res_df %>% filter(mods_num %in% mods_permitidos)',
  '      }',
  '      if (!is.null(input$filtro_modulo) && length(input$filtro_modulo) > 0) {',
  '        mods_sel <- as.character(input$filtro_modulo)',
  '        mods_expandidos <- unique(c(',
  '          mods_sel,',
  '          if ("11" %in% mods_sel) c("11.1", "11.2") else character(0),',
  '          if ("9" %in% mods_sel) c("9.1", "9.2") else character(0),',
  '          if ("13" %in% mods_sel) c("13.1", "13.2") else character(0),',
  '          if ("14" %in% mods_sel) c("14.1", "14.2") else character(0)',
  '        ))',
  '        res_df <- res_df %>% filter(as.character(Módulo) %in% mods_expandidos | sub("\\\\..*", "", as.character(Módulo)) %in% mods_sel)',
  '      }',
  '      if (!is.null(input$filtro_turno) && length(input$filtro_turno) > 0) {',
  '        turnos_sel <- as.character(as.numeric(input$filtro_turno))',
  '        res_df <- res_df %>% filter(as.character(Turno) %in% turnos_sel | as.character(Turno) %in% as.character(input$filtro_turno))',
  '      }',
  '      if (!is.null(input$filtro_variedad) && length(input$filtro_variedad) > 0) {',
  '        vars_sel <- toupper(trimws(limpiar_variedad(input$filtro_variedad)))',
  '        res_df <- res_df %>% filter(toupper(trimws(limpiar_variedad(Variedad))) %in% vars_sel)',
  '      }',
  '      if (f_pasada == "1") {',
  '        res_df <- res_df %>% filter(Pasada == "1ª Pasada")',
  '      } else if (f_pasada == "2") {',
  '        res_df <- res_df %>% filter(Pasada == "2ª Pasada")',
  '      }',
  '      if (f_term == "100") {',
  '        res_df <- res_df %>% filter(coalesce(Pct_Termino, 0) >= 99.5)',
  '      } else if (f_term == "en_curso") {',
  '        res_df <- res_df %>% filter(coalesce(Pct_Termino, 0) > 0 & coalesce(Pct_Termino, 0) < 99.5)',
  '      } else if (f_term == "sin_cosecha") {',
  '        res_df <- res_df %>% filter(coalesce(Pct_Termino, 0) == 0)',
  '      }',
  '      return(res_df)',
  '    }',
  '    '
)

# Splice the new block
lines <- c(lines[1:(idx_esc_comp - 1)], new_block, lines[idx_p1:length(lines)])

writeLines(lines, "Actualizacion Semanal/app.R", useBytes = TRUE)
cat("File written successfully. Total lines:", length(lines), "\n")
