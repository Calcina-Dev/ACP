app_path <- "Actualizacion Semanal/app.R"
lines <- readLines(app_path, encoding = "UTF-8", warn = FALSE)

# Find start of evaluar_escenario
idx_start <- grep("evaluar_escenario <- function\\(df_ed, esc_info = tibble\\(\\)\\) \\{", lines)
if (length(idx_start) == 0) {
  stop("Could not find start of evaluar_escenario")
}
idx_start <- idx_start[1]
cat("Found evaluar_escenario at line:", idx_start, "\n")

# Find the line where ed_p1 starts
idx_ed_p1 <- grep("^\\s*ed_p1 <- if \\(nrow\\(ed_filtrada\\) > 0\\)", lines[idx_start:min(idx_start + 60, length(lines))])
if (length(idx_ed_p1) == 0) {
  stop("Could not find start of ed_p1")
}
idx_end <- idx_start + idx_ed_p1[1] - 2

cat("Replacing from line", idx_start, "to", idx_end, "\n")

new_block <- c(
  "    evaluar_escenario <- function(df_ed, esc_info = tibble()) {",
  "      dias_ajuste  <- character(0)",
  "      dias_previos <- character(0)",
  "      ",
  "      # 1. Escenario guardado con rango explícito",
  "      if (!is.null(esc_info) && nrow(esc_info) > 0 && !is.na(esc_info$Dia_Desde[1]) && nzchar(as.character(esc_info$Dia_Desde[1]))) {",
  "        d_desde <- as.character(esc_info$Dia_Desde[1])",
  "        d_hasta <- if (\"Dia_Hasta\" %in% names(esc_info) && !is.na(esc_info$Dia_Hasta[1]) && nzchar(as.character(esc_info$Dia_Hasta[1]))) {",
  "          as.character(esc_info$Dia_Hasta[1])",
  "        } else {",
  "          \"Sábado\"",
  "        }",
  "        idx_d <- match(d_desde, dias_semana_nombres)",
  "        idx_h <- match(d_hasta, dias_semana_nombres)",
  "        if (is.na(idx_d)) idx_d <- 1",
  "        if (is.na(idx_h)) idx_h <- 6",
  "        if (idx_h < idx_d) idx_h <- idx_d",
  "        dias_ajuste <- dias_semana_nombres[idx_d:idx_h]",
  "        if (idx_d > 1) dias_previos <- dias_semana_nombres[1:(idx_d - 1)]",
  "      } else {",
  "        # 2. Ajuste en curso: tomar el rango activo [DESDE, HASTA] de la simulación",
  "        d_desde <- tryCatch({",
  "          if (!is.null(input$ajustar_desde_dia) && as.character(input$ajustar_desde_dia) %in% dias_semana_nombres) {",
  "            as.character(input$ajustar_desde_dia)",
  "          } else {",
  "            NULL",
  "          }",
  "        }, error = function(e) NULL)",
  "        ",
  "        d_hasta <- tryCatch({",
  "          if (!is.null(input$ajustar_hasta_dia) && as.character(input$ajustar_hasta_dia) %in% dias_semana_nombres) {",
  "            as.character(input$ajustar_hasta_dia)",
  "          } else {",
  "            NULL",
  "          }",
  "        }, error = function(e) NULL)",
  "        ",
  "        if (!is.null(d_desde) && !is.null(d_hasta)) {",
  "          idx_d <- match(d_desde, dias_semana_nombres)",
  "          idx_h <- match(d_hasta, dias_semana_nombres)",
  "          if (is.na(idx_d)) idx_d <- 1",
  "          if (is.na(idx_h)) idx_h <- 6",
  "          if (idx_h < idx_d) idx_h <- idx_d",
  "          dias_ajuste <- dias_semana_nombres[idx_d:idx_h]",
  "          if (idx_d > 1) dias_previos <- dias_semana_nombres[1:(idx_d - 1)]",
  "        } else if (!is.null(df_ed) && nrow(df_ed) > 0) {",
  "          # Fallback para pruebas sin inputs reactivos activos: excluir tipo ruta_sin_kg",
  "          df_ed_calc <- if (\"Tipo_Edicion\" %in% names(df_ed)) df_ed %>% filter(coalesce(Tipo_Edicion, \"\") != \"ruta_sin_kg\") else df_ed",
  "          dias_ajuste <- intersect(dias_semana_nombres, unique(as.character(df_ed_calc$Campo)))",
  "          if (length(dias_ajuste) == 0) dias_ajuste <- dias_semana_nombres",
  "          min_idx <- min(match(dias_ajuste, dias_semana_nombres))",
  "          if (min_idx > 1) dias_previos <- dias_semana_nombres[1:(min_idx - 1)]",
  "        } else {",
  "          dias_ajuste <- dias_semana_nombres",
  "        }",
  "      }",
  "      ",
  "      # Separar df_ed en 1ra y 2da pasada para coherencia con filtros de pasada",
  "      ed_filtrada <- if (!is.null(df_ed) && nrow(df_ed) > 0) {",
  "        df_base_ed <- if (\"Tipo_Edicion\" %in% names(df_ed)) {",
  "          df_ed %>% filter(coalesce(Tipo_Edicion, \"\") != \"ruta_sin_kg\")",
  "        } else {",
  "          df_ed",
  "        }",
  "        df_base_ed %>%",
  "          filter(Campo %in% dias_ajuste, coalesce(Valor, 0) > 0) %>%",
  "          mutate(",
  "            Turno_num    = suppressWarnings(as.numeric(Turno)),",
  "            Turno_clean  = ifelse(!is.na(Turno_num), as.character(Turno_num), trimws(as.character(Turno))),",
  "            Módulo_clean = trimws(as.character(Módulo)),",
  "            Sector_clean = trimws(as.character(Sector)),",
  "            Var_clean    = toupper(trimws(limpiar_variedad(Variedad))),",
  "            es_rocio     = (Var_clean == \"ROCIO\"),",
  "            es_p2        = (es_rocio & (",
  "              coalesce(Kg_Asignado_2da, 0) > 0 |",
  "              coalesce(Ha_Asignada_2da, 0) > 0 |",
  "              grepl(\"2da pasada|2ª pasada\", coalesce(Motivo_Ajuste, \"\"), ignore.case = TRUE)",
  "            )),",
  "            kg_p1        = ifelse(!es_p2, coalesce(Kg_Asignado_1ra, Valor), 0),",
  "            ha_p1        = ifelse(!es_p2, coalesce(Ha_Asignada_1ra, Ha_Asignada), 0),",
  "            kg_p2        = ifelse(es_p2, coalesce(Kg_Asignado_2da, Valor), 0),",
  "            ha_p2        = ifelse(es_p2, coalesce(Ha_Asignada_2da, Ha_Asignada), 0)",
  "          )",
  "      } else {",
  "        tibble()",
  "      }"
)

lines_updated <- c(lines[1:(idx_start - 1)], new_block, lines[(idx_end + 1):length(lines)])
writeLines(lines_updated, app_path, useBytes = TRUE)
cat("Successfully updated app.R!\n")
