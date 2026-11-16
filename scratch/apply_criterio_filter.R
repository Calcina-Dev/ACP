lines <- readLines("Actualizacion Semanal/app.R", encoding = "UTF-8", warn = FALSE)

# 1. UI: Add filtro_criterio_comp after filtro_pasada_comp
# Look for: selectInput("filtro_pasada_comp"
idx_pas <- which(grepl('selectInput\\("filtro_pasada_comp"', lines))[1]
cat("idx_pas:", idx_pas, "\n")

# Find the closing tag of that div (3 lines after idx_pas)
idx_pas_div_end <- idx_pas + which(grepl("^\\s*\\),", lines[idx_pas:(idx_pas+5)]))[1] - 1
cat("idx_pas_div_end:", idx_pas_div_end, "Line:", lines[idx_pas_div_end], "\n")

criterio_ui <- c(
  '                                   # Selector de Filtro por Criterio / Ajuste',
  '                                   div(style = "display: flex; flex-direction: column; gap: 4px; flex: 1 1 180px; min-width: 160px; max-width: 240px;",',
  '                                       span(style = "font-size: 11px; font-weight: 600; color: #71717A; text-transform: uppercase; letter-spacing: 0.05em; display: flex; align-items: center; gap: 5px;",',
  '                                            icon("sliders-h", style = "font-size: 10px; color: #8B5CF6;"), "Criterio"),',
  '                                       selectInput("filtro_criterio_comp", label = NULL,',
  '                                                   choices = c("Todos los turnos" = "todos",',
  '                                                               "Solo con ajuste programado" = "con_ajuste",',
  '                                                               "Sin ajuste programado" = "sin_ajuste",',
  '                                                               "Ajuste Manual" = "manual",',
  '                                                               "Avance > 15%" = "avance",',
  '                                                               "Sin avance previo" = "sin_avance"),',
  '                                                   selected = "todos", width = "100%")',
  '                                   ),'
)

lines <- c(lines[1:idx_pas_div_end], criterio_ui, lines[(idx_pas_div_end + 1):length(lines)])

# 2. Server: datos_comparativo_reactivo
idx_f_pas <- which(grepl('f_pasada <- if \\(!is.null\\(input\\$filtro_pasada_comp\\)\\)', lines))[1]
cat("idx_f_pas:", idx_f_pas, "\n")

criterio_server_init <- '    f_crit   <- if (!is.null(input$filtro_criterio_comp)) as.character(input$filtro_criterio_comp) else "todos"'
lines <- c(lines[1:idx_f_pas], criterio_server_init, lines[(idx_f_pas + 1):length(lines)])

# 3. Server: aplicar_filtros
idx_term_filter <- which(grepl('if \\(f_term == "100"\\)', lines))[1]
cat("idx_term_filter:", idx_term_filter, "\n")

criterio_filter_logic <- c(
  '      if (f_crit == "con_ajuste") {',
  '        res_df <- res_df %>% filter(',
  '          coalesce(Motivo_Ajuste, "") != "Sin ajuste programado" &',
  '          (coalesce(Kg_Ajustado_Esc, 0) > 0 | coalesce(Ha_Ajustada_Esc, 0) > 0)',
  '        )',
  '      } else if (f_crit == "sin_ajuste") {',
  '        res_df <- res_df %>% filter(',
  '          coalesce(Motivo_Ajuste, "") == "Sin ajuste programado" |',
  '          (coalesce(Kg_Ajustado_Esc, 0) == 0 & coalesce(Ha_Ajustada_Esc, 0) == 0)',
  '        )',
  '      } else if (f_crit == "manual") {',
  '        res_df <- res_df %>% filter(grepl("manual", coalesce(Motivo_Ajuste, ""), ignore.case = TRUE))',
  '      } else if (f_crit == "avance") {',
  '        res_df <- res_df %>% filter(grepl("avance", coalesce(Motivo_Ajuste, ""), ignore.case = TRUE))',
  '      } else if (f_crit == "sin_avance") {',
  '        res_df <- res_df %>% filter(grepl("sin avance", coalesce(Motivo_Ajuste, ""), ignore.case = TRUE))',
  '      }'
)

lines <- c(lines[1:(idx_term_filter - 1)], criterio_filter_logic, lines[idx_term_filter:length(lines)])

# 4. Server: vs_escenarios select should include Motivo_Ajuste, Kg_Ajustado_Esc, Ha_Ajustada_Esc
idx_vs_esc_sel <- which(grepl('Kg_Esc_A = Kg_Ajustado_Semana, Kg_Real_SAP = Kg_Real_Semana\\)', lines))[1]
cat("idx_vs_esc_sel:", idx_vs_esc_sel, "\n")
if (!is.na(idx_vs_esc_sel)) {
  lines[idx_vs_esc_sel] <- '               Kg_Esc_A = Kg_Ajustado_Semana, Kg_Real_SAP = Kg_Real_Semana,'
  lines <- c(lines[1:idx_vs_esc_sel],
             '               Motivo_Ajuste, Kg_Ajustado_Esc, Ha_Ajustada_Esc) %>%',
             lines[(idx_vs_esc_sel + 1):length(lines)])
}

# 5. Subtitulo badge in ui_comparativo_tabla_subtitulo
idx_badge_pas <- which(grepl('badge_pas <- if \\(f_pasada == "1"\\)', lines))[1]
cat("idx_badge_pas:", idx_badge_pas, "\n")

# Find where badge_pas block ends (before div(style = "display: flex; align-items: center;)
idx_div_sub <- idx_badge_pas + which(grepl('div\\(style = "display: flex; align-items: center;', lines[idx_badge_pas:(idx_badge_pas+20)]))[1] - 1
cat("idx_div_sub:", idx_div_sub, "\n")

criterio_sub_badge <- c(
  '    f_crit <- if (!is.null(input$filtro_criterio_comp)) as.character(input$filtro_criterio_comp) else "todos"',
  '    badge_crit <- if (f_crit == "con_ajuste") {',
  '      span(style = "margin-left: 6px; background: rgba(99, 102, 241, 0.12); color: #4F46E5; padding: 2px 8px; border-radius: 6px; font-weight: 600; font-size: 11px;", "● Con Ajuste")',
  '    } else if (f_crit == "sin_ajuste") {',
  '      span(style = "margin-left: 6px; background: rgba(142, 142, 147, 0.12); color: #8E8E93; padding: 2px 8px; border-radius: 6px; font-weight: 600; font-size: 11px;", "● Sin Ajuste")',
  '    } else if (f_crit == "manual") {',
  '      span(style = "margin-left: 6px; background: rgba(168, 85, 247, 0.12); color: #9333EA; padding: 2px 8px; border-radius: 6px; font-weight: 600; font-size: 11px;", "● Manual")',
  '    } else if (f_crit == "avance") {',
  '      span(style = "margin-left: 6px; background: rgba(16, 185, 129, 0.12); color: #059669; padding: 2px 8px; border-radius: 6px; font-weight: 600; font-size: 11px;", "● Avance > 15%")',
  '    } else if (f_crit == "sin_avance") {',
  '      span(style = "margin-left: 6px; background: rgba(245, 158, 11, 0.12); color: #D97706; padding: 2px 8px; border-radius: 6px; font-weight: 600; font-size: 11px;", "● Sin Avance Previo")',
  '    } else NULL',
  '    '
)

lines <- c(lines[1:(idx_div_sub - 1)], criterio_sub_badge, lines[idx_div_sub:length(lines)])

# Also update the div in ui_comparativo_tabla_subtitulo to include badge_crit
idx_div_ret <- which(grepl('badge_filtro,', lines))[1]
cat("idx_div_ret:", idx_div_ret, "\n")
# The line after badge_filtro is badge_pas)
if (grepl("badge_pas", lines[idx_div_ret + 1])) {
  lines[idx_div_ret + 1] <- "        badge_pas,"
  lines <- c(lines[1:(idx_div_ret + 1)],
             "        badge_crit",
             lines[(idx_div_ret + 2):length(lines)])
}

writeLines(lines, "Actualizacion Semanal/app.R", useBytes = TRUE)
cat("File written successfully. Total lines:", length(lines), "\n")
