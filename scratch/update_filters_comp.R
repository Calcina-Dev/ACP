file_path <- "D:/ACP/Actualizacion Semanal/app.R"
lines <- readLines(file_path, encoding = "UTF-8", warn = FALSE)

# 1. Perspective 1 (vs_escenarios)
target_1 <- '      # Filtros globales\n      if (!is.null(input$filtro_modulo) && input$filtro_modulo != "Todos") comp_df <- comp_df %>% filter(as.character(Módulo) == norm_mod(input$filtro_modulo))\n      if (!is.null(input$filtro_turno) && input$filtro_turno != "Todos") comp_df <- comp_df %>% filter(as.character(Turno) == as.character(input$filtro_turno))\n      if (!is.null(input$filtro_variedad) && length(input$filtro_variedad) > 0 && !"Todas" %in% input$filtro_variedad) {\n        comp_df <- comp_df %>% filter(toupper(trimws(as.character(Variedad))) %in% toupper(trimws(input$filtro_variedad)))\n      }'

# 2. Perspective 2 (semana_completa)
target_2 <- '      if (!is.null(input$filtro_modulo) && input$filtro_modulo != "Todos") comp_sem <- comp_sem %>% filter(as.character(Módulo) == norm_mod(input$filtro_modulo))\n      if (!is.null(input$filtro_turno) && input$filtro_turno != "Todos") comp_sem <- comp_sem %>% filter(as.character(Turno) == as.character(input$filtro_turno))\n      if (!is.null(input$filtro_variedad) && length(input$filtro_variedad) > 0 && !"Todas" %in% input$filtro_variedad) {\n        comp_sem <- comp_sem %>% filter(toupper(trimws(as.character(Variedad))) %in% toupper(trimws(input$filtro_variedad)))\n      }'

# 3. Perspective 3 (rango_escenario)
target_3 <- '    if (!is.null(input$filtro_modulo) && input$filtro_modulo != "Todos") comp_df <- comp_df %>% filter(as.character(Módulo) == norm_mod(input$filtro_modulo))\n    if (!is.null(input$filtro_turno) && input$filtro_turno != "Todos") comp_df <- comp_df %>% filter(as.character(Turno) == as.character(input$filtro_turno))\n    if (!is.null(input$filtro_variedad) && length(input$filtro_variedad) > 0 && !"Todas" %in% input$filtro_variedad) {\n      comp_df <- comp_df %>% filter(toupper(trimws(as.character(Variedad))) %in% toupper(trimws(input$filtro_variedad)))\n    }'

# Badge
target_badge <- '    mod_sel <- if (!is.null(input$filtro_modulo) && input$filtro_modulo != "Todos") sprintf("Módulo %s", input$filtro_modulo) else "Toda la Empresa"'

replacement_1 <- '      # Filtros globales sincronizados
      if (!is.null(input$filtro_grupo) && length(input$filtro_grupo) > 0) {
        mods_permitidos <- unlist(grupos_modulos[input$filtro_grupo], use.names = FALSE)
        comp_df <- comp_df %>% filter(suppressWarnings(as.numeric(as.character(Módulo))) %in% mods_permitidos)
      }
      if (!is.null(input$filtro_modulo) && length(input$filtro_modulo) > 0) {
        mods_sel <- norm_mod(input$filtro_modulo)
        comp_df  <- comp_df %>% filter(as.character(Módulo) %in% mods_sel)
      }
      if (!is.null(input$filtro_turno) && length(input$filtro_turno) > 0) {
        comp_df <- comp_df %>% filter(as.character(Turno) %in% as.character(input$filtro_turno))
      }
      if (!is.null(input$filtro_variedad) && length(input$filtro_variedad) > 0) {
        comp_df <- comp_df %>% filter(toupper(trimws(as.character(Variedad))) %in% toupper(trimws(input$filtro_variedad)))
      }'

replacement_2 <- '      # Filtros globales sincronizados
      if (!is.null(input$filtro_grupo) && length(input$filtro_grupo) > 0) {
        mods_permitidos <- unlist(grupos_modulos[input$filtro_grupo], use.names = FALSE)
        comp_sem <- comp_sem %>% filter(suppressWarnings(as.numeric(as.character(Módulo))) %in% mods_permitidos)
      }
      if (!is.null(input$filtro_modulo) && length(input$filtro_modulo) > 0) {
        mods_sel <- norm_mod(input$filtro_modulo)
        comp_sem <- comp_sem %>% filter(as.character(Módulo) %in% mods_sel)
      }
      if (!is.null(input$filtro_turno) && length(input$filtro_turno) > 0) {
        comp_sem <- comp_sem %>% filter(as.character(Turno) %in% as.character(input$filtro_turno))
      }
      if (!is.null(input$filtro_variedad) && length(input$filtro_variedad) > 0) {
        comp_sem <- comp_sem %>% filter(toupper(trimws(as.character(Variedad))) %in% toupper(trimws(input$filtro_variedad)))
      }'

replacement_3 <- '    # Filtros globales sincronizados
    if (!is.null(input$filtro_grupo) && length(input$filtro_grupo) > 0) {
      mods_permitidos <- unlist(grupos_modulos[input$filtro_grupo], use.names = FALSE)
      comp_df <- comp_df %>% filter(suppressWarnings(as.numeric(as.character(Módulo))) %in% mods_permitidos)
    }
    if (!is.null(input$filtro_modulo) && length(input$filtro_modulo) > 0) {
      mods_sel <- norm_mod(input$filtro_modulo)
      comp_df  <- comp_df %>% filter(as.character(Módulo) %in% mods_sel)
    }
    if (!is.null(input$filtro_turno) && length(input$filtro_turno) > 0) {
      comp_df <- comp_df %>% filter(as.character(Turno) %in% as.character(input$filtro_turno))
    }
    if (!is.null(input$filtro_variedad) && length(input$filtro_variedad) > 0) {
      comp_df <- comp_df %>% filter(toupper(trimws(as.character(Variedad))) %in% toupper(trimws(input$filtro_variedad)))
    }'

replacement_badge <- '    mod_sel <- if (!is.null(input$filtro_modulo) && length(input$filtro_modulo) > 0) sprintf("Módulo %s", paste(input$filtro_modulo, collapse = ", ")) else "Toda la Empresa"'

content <- paste(lines, collapse = "\n")

if (grepl(target_1, content, fixed = TRUE)) {
  content <- sub(target_1, replacement_1, content, fixed = TRUE)
  cat("Replaced filter 1\n")
} else {
  cat("Target 1 not found\n")
}

if (grepl(target_2, content, fixed = TRUE)) {
  content <- sub(target_2, replacement_2, content, fixed = TRUE)
  cat("Replaced filter 2\n")
} else {
  cat("Target 2 not found\n")
}

if (grepl(target_3, content, fixed = TRUE)) {
  content <- sub(target_3, replacement_3, content, fixed = TRUE)
  cat("Replaced filter 3\n")
} else {
  cat("Target 3 not found\n")
}

if (grepl(target_badge, content, fixed = TRUE)) {
  content <- sub(target_badge, replacement_badge, content, fixed = TRUE)
  cat("Replaced badge\n")
} else {
  cat("Target badge not found\n")
}

writeLines(strsplit(content, "\n")[[1]], file_path, useBytes = TRUE)
cat("Updated file successfully\n")
