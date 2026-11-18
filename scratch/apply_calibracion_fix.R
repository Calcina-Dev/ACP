options(warn = 1)
app_path <- "D:/ACP/Actualizacion Semanal/app.R"

# Read file lines
lines <- readLines(app_path, encoding = "UTF-8", warn = FALSE)
full_text <- paste(lines, collapse = "\n")

# Backup
writeLines(lines, paste0(app_path, ".bak_calib"), useBytes = TRUE)
cat("Backup created.\n")

# TARGET 1: optimizar_dias_maduracion
target1 <- 'optimizar_dias_maduracion <- function(df_cosecha, df_conteo, campana = 2026, modulo, turno, variedad = NULL, min_dias = 35, max_dias = 120) {
  if (is.null(df_cosecha) || is.null(df_conteo) || nrow(df_cosecha) == 0 || nrow(df_conteo) == 0) {
    return(list(opt_dias = 45L, r = NA_real_, sem_shift = 6L, mensaje = "Sin datos"))
  }
  
  camp_chr <- as.character(campana)
  camp_num <- suppressWarnings(as.numeric(camp_chr))
  mod_chr  <- trimws(as.character(modulo))
  tur_chr  <- trimws(as.character(turno))
  
  df_y <- df_cosecha %>%
    filter(
      (as.character(Campaña) == camp_chr | (!is.na(camp_num) & Campaña == camp_num)),
      trimws(as.character(Módulo)) == mod_chr,
      trimws(as.character(Turno)) == tur_chr,
      !coalesce(Es_Testblock, FALSE),
      `Kg Total` > 0
    ) %>%
    mutate(Semana_Num = suppressWarnings(as.numeric(Semana))) %>%
    filter(!is.na(Semana_Num)) %>%
    group_by(Semana_Num) %>%
    summarise(
      Kg_Total = sum(`Kg Total`, na.rm = TRUE),
      Area = first(na.omit(Área)),
      Rend = ifelse(!is.na(Area) && Area > 0, Kg_Total / Area, Kg_Total),
      .groups = "drop"
    )
  
  if (nrow(df_y) < 3) {
    df_y <- df_cosecha %>%
      filter(
        (as.character(Campaña) == camp_chr | (!is.na(camp_num) & Campaña == camp_num)),
        trimws(as.character(Módulo)) == mod_chr,
        !coalesce(Es_Testblock, FALSE),
        `Kg Total` > 0
      ) %>%
      mutate(Semana_Num = suppressWarnings(as.numeric(Semana))) %>%
      filter(!is.na(Semana_Num)) %>%
      group_by(Semana_Num) %>%
      summarise(
        Kg_Total = sum(`Kg Total`, na.rm = TRUE),
        Area = sum(first(na.omit(Área)), na.rm = TRUE),
        Rend = ifelse(!is.na(Area) && Area > 0, Kg_Total / Area, Kg_Total),
        .groups = "drop"
      )
  }
  
  df_c <- df_conteo %>%
    filter(
      (as.character(Campaña) == camp_chr | (!is.na(camp_num) & Campaña == camp_num)),
      trimws(as.character(Módulo)) == mod_chr,
      trimws(as.character(Turno)) == tur_chr
    ) %>%
    mutate(Semana_Num = suppressWarnings(as.numeric(Semana))) %>%
    filter(!is.na(Semana_Num)) %>%
    group_by(Semana_Num) %>%
    summarise(
      Organos = sum(Boton + Flor, na.rm = TRUE),
      Muestra = sum(MuestraF, na.rm = TRUE),
      Carga = ifelse(Muestra > 0, Organos / Muestra, mean(Total_Floral_Planta, na.rm = TRUE)),
      .groups = "drop"
    )
  
  if (nrow(df_c) < 3) {
    df_c <- df_conteo %>%
      filter(
        (as.character(Campaña) == camp_chr | (!is.na(camp_num) & Campaña == camp_num)),
        trimws(as.character(Módulo)) == mod_chr
      ) %>%
      mutate(Semana_Num = suppressWarnings(as.numeric(Semana))) %>%
      filter(!is.na(Semana_Num)) %>%
      group_by(Semana_Num) %>%
      summarise(
        Organos = sum(Boton + Flor, na.rm = TRUE),
        Muestra = sum(MuestraF, na.rm = TRUE),
        Carga = ifelse(Muestra > 0, Organos / Muestra, mean(Total_Floral_Planta, na.rm = TRUE)),
        .groups = "drop"
      )
  }'

replacement1 <- 'optimizar_dias_maduracion <- function(df_cosecha, df_conteo, campana = 2026, modulo, turno, variedad = NULL, min_dias = 35, max_dias = 120) {
  if (is.null(df_cosecha) || is.null(df_conteo) || nrow(df_cosecha) == 0 || nrow(df_conteo) == 0) {
    return(list(opt_dias = 45L, r = NA_real_, sem_shift = 6L, mensaje = "Sin datos"))
  }
  
  camp_chr  <- as.character(campana)
  camp_num  <- suppressWarnings(as.numeric(camp_chr))
  mod_chr   <- trimws(as.character(modulo))
  tur_chr   <- trimws(as.character(turno))
  var_clean <- if (!is.null(variedad) && nzchar(variedad)) toupper(trimws(as.character(variedad))) else NULL
  
  df_y_sub <- df_cosecha %>%
    filter(
      (as.character(Campaña) == camp_chr | (!is.na(camp_num) & Campaña == camp_num)),
      trimws(as.character(Módulo)) == mod_chr,
      trimws(as.character(Turno)) == tur_chr,
      !coalesce(Es_Testblock, FALSE),
      `Kg Total` > 0
    )
  if (!is.null(var_clean)) {
    df_y_sub <- df_y_sub %>% filter(limpiar_variedad(Variedad) == limpiar_variedad(var_clean))
  }
  
  df_y <- df_y_sub %>%
    mutate(Semana_Num = suppressWarnings(as.numeric(Semana))) %>%
    filter(!is.na(Semana_Num)) %>%
    group_by(Semana_Num) %>%
    summarise(
      Kg_Total = sum(`Kg Total`, na.rm = TRUE),
      Area = first(na.omit(Área)),
      Rend = ifelse(!is.na(Area) && Area > 0, Kg_Total / Area, Kg_Total),
      .groups = "drop"
    )
  
  if (nrow(df_y) < 3) {
    df_y_mod <- df_cosecha %>%
      filter(
        (as.character(Campaña) == camp_chr | (!is.na(camp_num) & Campaña == camp_num)),
        trimws(as.character(Módulo)) == mod_chr,
        !coalesce(Es_Testblock, FALSE),
        `Kg Total` > 0
      )
    if (!is.null(var_clean)) {
      df_y_mod <- df_y_mod %>% filter(limpiar_variedad(Variedad) == limpiar_variedad(var_clean))
    }
    df_y <- df_y_mod %>%
      mutate(Semana_Num = suppressWarnings(as.numeric(Semana))) %>%
      filter(!is.na(Semana_Num)) %>%
      group_by(Semana_Num) %>%
      summarise(
        Kg_Total = sum(`Kg Total`, na.rm = TRUE),
        Area = sum(first(na.omit(Área)), na.rm = TRUE),
        Rend = ifelse(!is.na(Area) && Area > 0, Kg_Total / Area, Kg_Total),
        .groups = "drop"
      )
  }
  
  df_c_sub <- df_conteo %>%
    filter(
      (as.character(Campaña) == camp_chr | (!is.na(camp_num) & Campaña == camp_num)),
      trimws(as.character(Módulo)) == mod_chr,
      trimws(as.character(Turno)) == tur_chr
    )
  if (!is.null(var_clean)) {
    df_c_sub <- df_c_sub %>% filter(limpiar_variedad(Variedad) == limpiar_variedad(var_clean))
  }
  
  df_c <- df_c_sub %>%
    mutate(Semana_Num = suppressWarnings(as.numeric(Semana))) %>%
    filter(!is.na(Semana_Num)) %>%
    group_by(Semana_Num) %>%
    summarise(
      Organos = sum(Boton + Flor, na.rm = TRUE),
      Muestra = sum(MuestraF, na.rm = TRUE),
      Carga = ifelse(Muestra > 0, Organos / Muestra, mean(Total_Floral_Planta, na.rm = TRUE)),
      .groups = "drop"
    )
  
  if (nrow(df_c) < 3) {
    df_c_mod <- df_conteo %>%
      filter(
        (as.character(Campaña) == camp_chr | (!is.na(camp_num) & Campaña == camp_num)),
        trimws(as.character(Módulo)) == mod_chr
      )
    if (!is.null(var_clean)) {
      df_c_mod <- df_c_mod %>% filter(limpiar_variedad(Variedad) == limpiar_variedad(var_clean))
    }
    df_c <- df_c_mod %>%
      mutate(Semana_Num = suppressWarnings(as.numeric(Semana))) %>%
      filter(!is.na(Semana_Num)) %>%
      group_by(Semana_Num) %>%
      summarise(
        Organos = sum(Boton + Flor, na.rm = TRUE),
        Muestra = sum(MuestraF, na.rm = TRUE),
        Carga = ifelse(Muestra > 0, Organos / Muestra, mean(Total_Floral_Planta, na.rm = TRUE)),
        .groups = "drop"
      )
  }'

if (!grepl(target1, full_text, fixed = TRUE)) {
  stop("TARGET 1 not found in app.R!")
}
cat("TARGET 1 found! Replacing...\n")
full_text <- sub(target1, replacement1, full_text, fixed = TRUE)

# TARGET 2: grafico_calce_maduracion df_y and df_c filtering by variedad
target2 <- '    # 1. Cosecha real
    df_cos <- if (exists("df_cosecha") && is.data.frame(df_cosecha) && nrow(df_cosecha) > 0) df_cosecha else tibble()
    df_y <- df_cos %>%
      filter(
        (as.character(Campaña) == camp_chr | (!is.na(camp_num) & Campaña == camp_num)),
        trimws(as.character(Módulo)) == trimws(as.character(m)),
        trimws(as.character(Turno)) == trimws(as.character(t)),
        !coalesce(Es_Testblock, FALSE),
        `Kg Total` > 0
      ) %>%
      mutate(Semana_Num = suppressWarnings(as.numeric(Semana))) %>%
      filter(!is.na(Semana_Num)) %>%
      group_by(Semana_Num) %>%
      summarise(
        Kg_Total = sum(`Kg Total`, na.rm = TRUE),
        Area     = first(na.omit(Área)),
        Rend     = ifelse(!is.na(Area) && Area > 0, round(Kg_Total / Area, 1), round(Kg_Total, 1)),
        .groups  = "drop"
      ) %>%
      arrange(Semana_Num)
    
    if (nrow(df_y) == 0) {
      df_y <- df_cos %>%
        filter(
          (as.character(Campaña) == camp_chr | (!is.na(camp_num) & Campaña == camp_num)),
          trimws(as.character(Módulo)) == trimws(as.character(m)),
          !coalesce(Es_Testblock, FALSE),
          `Kg Total` > 0
        ) %>%
        mutate(Semana_Num = suppressWarnings(as.numeric(Semana))) %>%
        filter(!is.na(Semana_Num)) %>%
        group_by(Semana_Num) %>%
        summarise(
          Kg_Total = sum(`Kg Total`, na.rm = TRUE),
          Area     = sum(first(na.omit(Área)), na.rm = TRUE),
          Rend     = ifelse(!is.na(Area) && Area > 0, round(Kg_Total / Area, 1), round(Kg_Total, 1)),
          .groups  = "drop"
        ) %>%
        arrange(Semana_Num)
    }
    
    # 2. Conteo floral
    df_c_hist <- if (exists("df_conteo_historico") && is.data.frame(df_conteo_historico) && nrow(df_conteo_historico) > 0) df_conteo_historico else tibble()
    df_c <- df_c_hist %>%
      filter(
        (as.character(Campaña) == camp_chr | (!is.na(camp_num) & Campaña == camp_num)),
        trimws(as.character(Módulo)) == trimws(as.character(m)),
        trimws(as.character(Turno)) == trimws(as.character(t))
      ) %>%
      mutate(Semana_Num = suppressWarnings(as.numeric(Semana))) %>%
      filter(!is.na(Semana_Num)) %>%
      group_by(Semana_Num) %>%
      summarise(
        Organos = sum(Boton + Flor, na.rm = TRUE),
        Muestra = sum(MuestraF, na.rm = TRUE),
        Carga   = ifelse(Muestra > 0, round(Organos / Muestra, 1), round(mean(Total_Floral_Planta, na.rm = TRUE), 1)),
        .groups = "drop"
      ) %>%
      arrange(Semana_Num)
    
    if (nrow(df_c) == 0) {
      df_c <- df_c_hist %>%
        filter(
          (as.character(Campaña) == camp_chr | (!is.na(camp_num) & Campaña == camp_num)),
          trimws(as.character(Módulo)) == trimws(as.character(m))
        ) %>%
        mutate(Semana_Num = suppressWarnings(as.numeric(Semana))) %>%
        filter(!is.na(Semana_Num)) %>%
        group_by(Semana_Num) %>%
        summarise(
          Organos = sum(Boton + Flor, na.rm = TRUE),
          Muestra = sum(MuestraF, na.rm = TRUE),
          Carga   = ifelse(Muestra > 0, round(Organos / Muestra, 1), round(mean(Total_Floral_Planta, na.rm = TRUE), 1)),
          .groups = "drop"
        ) %>%
        arrange(Semana_Num)
    }'

replacement2 <- '    # 1. Cosecha real (filtrada por Módulo, Turno y Variedad específica)
    df_cos <- if (exists("df_cosecha") && is.data.frame(df_cosecha) && nrow(df_cosecha) > 0) df_cosecha else tibble()
    df_y <- df_cos %>%
      filter(
        (as.character(Campaña) == camp_chr | (!is.na(camp_num) & Campaña == camp_num)),
        trimws(as.character(Módulo)) == trimws(as.character(m)),
        trimws(as.character(Turno)) == trimws(as.character(t)),
        limpiar_variedad(Variedad) == limpiar_variedad(v),
        !coalesce(Es_Testblock, FALSE),
        `Kg Total` > 0
      ) %>%
      mutate(Semana_Num = suppressWarnings(as.numeric(Semana))) %>%
      filter(!is.na(Semana_Num)) %>%
      group_by(Semana_Num) %>%
      summarise(
        Kg_Total = sum(`Kg Total`, na.rm = TRUE),
        Area     = first(na.omit(Área)),
        Rend     = ifelse(!is.na(Area) && Area > 0, round(Kg_Total / Area, 1), round(Kg_Total, 1)),
        .groups  = "drop"
      ) %>%
      arrange(Semana_Num)
    
    if (nrow(df_y) == 0) {
      df_y <- df_cos %>%
        filter(
          (as.character(Campaña) == camp_chr | (!is.na(camp_num) & Campaña == camp_num)),
          trimws(as.character(Módulo)) == trimws(as.character(m)),
          limpiar_variedad(Variedad) == limpiar_variedad(v),
          !coalesce(Es_Testblock, FALSE),
          `Kg Total` > 0
        ) %>%
        mutate(Semana_Num = suppressWarnings(as.numeric(Semana))) %>%
        filter(!is.na(Semana_Num)) %>%
        group_by(Semana_Num) %>%
        summarise(
          Kg_Total = sum(`Kg Total`, na.rm = TRUE),
          Area     = sum(first(na.omit(Área)), na.rm = TRUE),
          Rend     = ifelse(!is.na(Area) && Area > 0, round(Kg_Total / Area, 1), round(Kg_Total, 1)),
          .groups  = "drop"
        ) %>%
        arrange(Semana_Num)
    }
    
    # 2. Conteo floral (filtrado por Módulo, Turno y Variedad específica)
    df_c_hist <- if (exists("df_conteo_historico") && is.data.frame(df_conteo_historico) && nrow(df_conteo_historico) > 0) df_conteo_historico else tibble()
    df_c <- df_c_hist %>%
      filter(
        (as.character(Campaña) == camp_chr | (!is.na(camp_num) & Campaña == camp_num)),
        trimws(as.character(Módulo)) == trimws(as.character(m)),
        trimws(as.character(Turno)) == trimws(as.character(t)),
        limpiar_variedad(Variedad) == limpiar_variedad(v)
      ) %>%
      mutate(Semana_Num = suppressWarnings(as.numeric(Semana))) %>%
      filter(!is.na(Semana_Num)) %>%
      group_by(Semana_Num) %>%
      summarise(
        Organos = sum(Boton + Flor, na.rm = TRUE),
        Muestra = sum(MuestraF, na.rm = TRUE),
        Carga   = ifelse(Muestra > 0, round(Organos / Muestra, 1), round(mean(Total_Floral_Planta, na.rm = TRUE), 1)),
        .groups = "drop"
      ) %>%
      arrange(Semana_Num)
    
    if (nrow(df_c) == 0) {
      df_c <- df_c_hist %>%
        filter(
          (as.character(Campaña) == camp_chr | (!is.na(camp_num) & Campaña == camp_num)),
          trimws(as.character(Módulo)) == trimws(as.character(m)),
          limpiar_variedad(Variedad) == limpiar_variedad(v)
        ) %>%
        mutate(Semana_Num = suppressWarnings(as.numeric(Semana))) %>%
        filter(!is.na(Semana_Num)) %>%
        group_by(Semana_Num) %>%
        summarise(
          Organos = sum(Boton + Flor, na.rm = TRUE),
          Muestra = sum(MuestraF, na.rm = TRUE),
          Carga   = ifelse(Muestra > 0, round(Organos / Muestra, 1), round(mean(Total_Floral_Planta, na.rm = TRUE), 1)),
          .groups = "drop"
        ) %>%
        arrange(Semana_Num)
    }'

if (!grepl(target2, full_text, fixed = TRUE)) {
  stop("TARGET 2 not found in app.R!")
}
cat("TARGET 2 found! Replacing...\n")
full_text <- sub(target2, replacement2, full_text, fixed = TRUE)

# TARGET 3: ui_calibrar_kpis_calce
target3 <- '    df_y <- df_cos %>%
      filter(
        (as.character(Campaña) == camp_chr | (!is.na(camp_num) & Campaña == camp_num)),
        trimws(as.character(Módulo)) == trimws(as.character(m)),
        trimws(as.character(Turno)) == trimws(as.character(t)),
        !coalesce(Es_Testblock, FALSE),
        `Kg Total` > 0
      ) %>%
      mutate(Semana_Num = suppressWarnings(as.numeric(Semana))) %>%
      filter(!is.na(Semana_Num)) %>%
      group_by(Semana_Num) %>%
      summarise(
        Kg_Total = sum(`Kg Total`, na.rm = TRUE),
        Area     = first(na.omit(Área)),
        Rend     = ifelse(!is.na(Area) && Area > 0, Kg_Total / Area, Kg_Total),
        .groups  = "drop"
      )
    
    df_c <- df_c_hist %>%
      filter(
        (as.character(Campaña) == camp_chr | (!is.na(camp_num) & Campaña == camp_num)),
        trimws(as.character(Módulo)) == trimws(as.character(m)),
        trimws(as.character(Turno)) == trimws(as.character(t))
      ) %>%
      mutate(Semana_Num = suppressWarnings(as.numeric(Semana))) %>%
      filter(!is.na(Semana_Num)) %>%
      group_by(Semana_Num) %>%
      summarise(
        Organos = sum(Boton + Flor, na.rm = TRUE),
        Muestra = sum(MuestraF, na.rm = TRUE),
        Carga   = ifelse(Muestra > 0, Organos / Muestra, mean(Total_Floral_Planta, na.rm = TRUE)),
        .groups = "drop"
      )'

replacement3 <- '    df_y <- df_cos %>%
      filter(
        (as.character(Campaña) == camp_chr | (!is.na(camp_num) & Campaña == camp_num)),
        trimws(as.character(Módulo)) == trimws(as.character(m)),
        trimws(as.character(Turno)) == trimws(as.character(t)),
        limpiar_variedad(Variedad) == limpiar_variedad(v),
        !coalesce(Es_Testblock, FALSE),
        `Kg Total` > 0
      ) %>%
      mutate(Semana_Num = suppressWarnings(as.numeric(Semana))) %>%
      filter(!is.na(Semana_Num)) %>%
      group_by(Semana_Num) %>%
      summarise(
        Kg_Total = sum(`Kg Total`, na.rm = TRUE),
        Area     = first(na.omit(Área)),
        Rend     = ifelse(!is.na(Area) && Area > 0, Kg_Total / Area, Kg_Total),
        .groups  = "drop"
      )
    
    if (nrow(df_y) == 0) {
      df_y <- df_cos %>%
        filter(
          (as.character(Campaña) == camp_chr | (!is.na(camp_num) & Campaña == camp_num)),
          trimws(as.character(Módulo)) == trimws(as.character(m)),
          limpiar_variedad(Variedad) == limpiar_variedad(v),
          !coalesce(Es_Testblock, FALSE),
          `Kg Total` > 0
        ) %>%
        mutate(Semana_Num = suppressWarnings(as.numeric(Semana))) %>%
        filter(!is.na(Semana_Num)) %>%
        group_by(Semana_Num) %>%
        summarise(
          Kg_Total = sum(`Kg Total`, na.rm = TRUE),
          Area     = sum(first(na.omit(Área)), na.rm = TRUE),
          Rend     = ifelse(!is.na(Area) && Area > 0, Kg_Total / Area, Kg_Total),
          .groups  = "drop"
        )
    }
    
    df_c <- df_c_hist %>%
      filter(
        (as.character(Campaña) == camp_chr | (!is.na(camp_num) & Campaña == camp_num)),
        trimws(as.character(Módulo)) == trimws(as.character(m)),
        trimws(as.character(Turno)) == trimws(as.character(t)),
        limpiar_variedad(Variedad) == limpiar_variedad(v)
      ) %>%
      mutate(Semana_Num = suppressWarnings(as.numeric(Semana))) %>%
      filter(!is.na(Semana_Num)) %>%
      group_by(Semana_Num) %>%
      summarise(
        Organos = sum(Boton + Flor, na.rm = TRUE),
        Muestra = sum(MuestraF, na.rm = TRUE),
        Carga   = ifelse(Muestra > 0, Organos / Muestra, mean(Total_Floral_Planta, na.rm = TRUE)),
        .groups = "drop"
      )
    
    if (nrow(df_c) == 0) {
      df_c <- df_c_hist %>%
        filter(
          (as.character(Campaña) == camp_chr | (!is.na(camp_num) & Campaña == camp_num)),
          trimws(as.character(Módulo)) == trimws(as.character(m)),
          limpiar_variedad(Variedad) == limpiar_variedad(v)
        ) %>%
        mutate(Semana_Num = suppressWarnings(as.numeric(Semana))) %>%
        filter(!is.na(Semana_Num)) %>%
        group_by(Semana_Num) %>%
        summarise(
          Organos = sum(Boton + Flor, na.rm = TRUE),
          Muestra = sum(MuestraF, na.rm = TRUE),
          Carga   = ifelse(Muestra > 0, Organos / Muestra, mean(Total_Floral_Planta, na.rm = TRUE)),
          .groups = "drop"
        )
    }'

if (!grepl(target3, full_text, fixed = TRUE)) {
  stop("TARGET 3 not found in app.R!")
}
cat("TARGET 3 found! Replacing...\n")
full_text <- sub(target3, replacement3, full_text, fixed = TRUE)

# TARGET 4: btn_auto_ajustar_calce auto-save
target4 <- '  # Observer: Ajustar Automáticamente
  observeEvent(input$btn_auto_ajustar_calce, {
    m <- calibrador_state$modulo
    v <- calibrador_state$variedad
    t <- input$calibrar_mad_turno_sel
    req(m, t)
    
    camp_sel <- if (!is.null(input$filtro_campana)) input$filtro_campana else 2026
    df_c_hist <- if (exists("df_conteo_historico") && is.data.frame(df_conteo_historico) && nrow(df_conteo_historico) > 0) df_conteo_historico else tibble()
    df_cos    <- if (exists("df_cosecha") && is.data.frame(df_cosecha) && nrow(df_cosecha) > 0) df_cosecha else tibble()
    
    opt <- optimizar_dias_maduracion(df_cos, df_c_hist, campana = camp_sel, modulo = m, turno = t, variedad = v)
    
    if (!is.na(opt$r)) {
      updateSliderInput(session, "calibrar_slider_dias", value = opt$opt_dias)
      showNotification(
        sprintf("✔ Calce óptimo para Turno %s: %d días (%d semanas, r = %+.3f).", t, opt$opt_dias, opt$sem_shift, opt$r),
        type = "message", duration = 4
      )
    } else {
      showNotification("No se pudo calcular una correlación suficiente para este turno.", type = "warning", duration = 4)
    }
  })'

replacement4 <- '  # Observer: Ajustar Automáticamente (Sincroniza Picos y Guarda Inmediatamente)
  observeEvent(input$btn_auto_ajustar_calce, {
    m <- calibrador_state$modulo
    v <- calibrador_state$variedad
    t <- input$calibrar_mad_turno_sel
    req(m, t)
    
    camp_sel <- if (!is.null(input$filtro_campana)) input$filtro_campana else 2026
    df_c_hist <- if (exists("df_conteo_historico") && is.data.frame(df_conteo_historico) && nrow(df_conteo_historico) > 0) df_conteo_historico else tibble()
    df_cos    <- if (exists("df_cosecha") && is.data.frame(df_cosecha) && nrow(df_cosecha) > 0) df_cosecha else tibble()
    
    opt <- optimizar_dias_maduracion(df_cos, df_c_hist, campana = camp_sel, modulo = m, turno = t, variedad = v)
    
    if (!is.na(opt$r)) {
      updateSliderInput(session, "calibrar_slider_dias", value = opt$opt_dias)
      
      # Persistir inmediatamente la calibración para este turno
      df_mad_t <- rv_maduracion_turno()
      if (is.null(df_mad_t) || !is.data.frame(df_mad_t)) df_mad_t <- cargar_maduracion_turno_disco()
      
      df_mad_t <- df_mad_t %>%
        filter(!(Módulo == m & Turno == t & Variedad == v))
      
      nueva_fila <- tibble(
        Módulo             = m,
        Turno              = t,
        Variedad           = v,
        Dias_Maduracion    = opt$opt_dias,
        Correlacion        = round(opt$r, 3),
        Fecha_Calibracion  = as.character(Sys.Date())
      )
      df_mad_t <- bind_rows(df_mad_t, nueva_fila)
      
      rv_maduracion_turno(df_mad_t)
      guardar_maduracion_turno_disco(df_mad_t)
      calibrador_state$dias <- opt$opt_dias
      
      showNotification(
        sprintf("✔ Calce óptimo sincronizado y guardado para Turno %s (%s): %d días (%d semanas, r = %+.3f).", t, v, opt$opt_dias, opt$sem_shift, opt$r),
        type = "message", duration = 4
      )
    } else {
      showNotification("No se pudo calcular una correlación suficiente para este turno.", type = "warning", duration = 4)
    }
  })'

if (!grepl(target4, full_text, fixed = TRUE)) {
  stop("TARGET 4 not found in app.R!")
}
cat("TARGET 4 found! Replacing...\n")
full_text <- sub(target4, replacement4, full_text, fixed = TRUE)

# Write to file
writeLines(full_text, app_path, useBytes = TRUE)
cat("File written successfully.\n")

# Syntax validation
parse(app_path)
cat("Syntax validation: 100% OK!\n")
