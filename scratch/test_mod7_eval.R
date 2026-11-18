suppressPackageStartupMessages(library(dplyr))
load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")

optimizar_dias_maduracion_ORIG <- function(df_cosecha, df_conteo, campana = 2026, modulo, turno, variedad = NULL, min_dias = 35, max_dias = 120) {
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
    
  sem_shifts <- unique(round(seq(min_dias, max_dias, by = 1) / 7))
  
  evals <- lapply(sem_shifts, function(sw) {
    df_join <- df_y %>%
      mutate(Semana_Flor = Semana_Num - sw) %>%
      inner_join(df_c, by = c("Semana_Flor" = "Semana_Num")) %>%
      filter(!is.na(Rend), !is.na(Carga))
    
    if (nrow(df_join) < 3) return(list(dias = sw * 7L, r = NA_real_, sw = sw, n = nrow(df_join)))
    r_val <- suppressWarnings(cor(df_join$Rend, df_join$Carga))
    list(dias = sw * 7L, r = r_val, sw = sw, n = nrow(df_join))
  })
  
  evals_valid <- evals[!is.na(sapply(evals, function(x) x$r))]
  if (length(evals_valid) == 0) return(list(opt_dias = 45L, r = NA_real_, sem_shift = 6L, mensaje = "Sin correlacion"))
  
  best <- evals_valid[[which.max(sapply(evals_valid, function(x) x$r))]]
  return(list(opt_dias = as.integer(best$dias), r = round(best$r, 3), sem_shift = as.integer(best$sw)))
}

optimizar_dias_maduracion_CON_VARIEDAD <- function(df_cosecha, df_conteo, campana = 2026, modulo, turno, variedad = NULL, min_dias = 35, max_dias = 120) {
  camp_chr <- as.character(campana)
  camp_num <- suppressWarnings(as.numeric(camp_chr))
  mod_chr  <- trimws(as.character(modulo))
  tur_chr  <- trimws(as.character(turno))
  var_clean <- if (!is.null(variedad) && nzchar(variedad)) toupper(trimws(as.character(variedad))) else NULL
  
  df_y_filt <- df_cosecha %>%
    filter(
      (as.character(Campaña) == camp_chr | (!is.na(camp_num) & Campaña == camp_num)),
      trimws(as.character(Módulo)) == mod_chr,
      trimws(as.character(Turno)) == tur_chr,
      !coalesce(Es_Testblock, FALSE),
      `Kg Total` > 0
    )
  if (!is.null(var_clean)) {
    df_y_filt <- df_y_filt %>% filter(toupper(trimws(as.character(Variedad))) == var_clean)
  }
  
  df_y <- df_y_filt %>%
    mutate(Semana_Num = suppressWarnings(as.numeric(Semana))) %>%
    filter(!is.na(Semana_Num)) %>%
    group_by(Semana_Num) %>%
    summarise(
      Kg_Total = sum(`Kg Total`, na.rm = TRUE),
      Area = first(na.omit(Área)),
      Rend = ifelse(!is.na(Area) && Area > 0, Kg_Total / Area, Kg_Total),
      .groups = "drop"
    )
  
  df_c_filt <- df_conteo %>%
    filter(
      (as.character(Campaña) == camp_chr | (!is.na(camp_num) & Campaña == camp_num)),
      trimws(as.character(Módulo)) == mod_chr,
      trimws(as.character(Turno)) == tur_chr
    )
  if (!is.null(var_clean)) {
    df_c_filt <- df_c_filt %>% filter(toupper(trimws(as.character(Variedad))) == var_clean)
  }
  
  df_c <- df_c_filt %>%
    mutate(Semana_Num = suppressWarnings(as.numeric(Semana))) %>%
    filter(!is.na(Semana_Num)) %>%
    group_by(Semana_Num) %>%
    summarise(
      Organos = sum(Boton + Flor, na.rm = TRUE),
      Muestra = sum(MuestraF, na.rm = TRUE),
      Carga = ifelse(Muestra > 0, Organos / Muestra, mean(Total_Floral_Planta, na.rm = TRUE)),
      .groups = "drop"
    )
    
  sem_shifts <- unique(round(seq(min_dias, max_dias, by = 1) / 7))
  
  evals <- lapply(sem_shifts, function(sw) {
    df_join <- df_y %>%
      mutate(Semana_Flor = Semana_Num - sw) %>%
      inner_join(df_c, by = c("Semana_Flor" = "Semana_Num")) %>%
      filter(!is.na(Rend), !is.na(Carga))
    
    if (nrow(df_join) < 3) return(list(dias = sw * 7L, r = NA_real_, sw = sw, n = nrow(df_join)))
    r_val <- suppressWarnings(cor(df_join$Rend, df_join$Carga))
    list(dias = sw * 7L, r = r_val, sw = sw, n = nrow(df_join))
  })
  
  evals_valid <- evals[!is.na(sapply(evals, function(x) x$r))]
  if (length(evals_valid) == 0) return(list(opt_dias = 45L, r = NA_real_, sem_shift = 6L, mensaje = "Sin correlacion"))
  
  best <- evals_valid[[which.max(sapply(evals_valid, function(x) x$r))]]
  return(list(opt_dias = as.integer(best$dias), r = round(best$r, 3), sem_shift = as.integer(best$sw)))
}

cat("=== COMPARACIÓN PARA MODULO 7 VENTURA ===\n")
for (t in c("1", "2", "3", "4")) {
  opt_orig <- optimizar_dias_maduracion_ORIG(df_cosecha, df_conteo_historico, campana = 2026, modulo = "7", turno = t, variedad = "VENTURA")
  opt_var  <- optimizar_dias_maduracion_CON_VARIEDAD(df_cosecha, df_conteo_historico, campana = 2026, modulo = "7", turno = t, variedad = "VENTURA")
  cat(sprintf("Turno %s:\n  SIN filtro variedad: dias = %d, r = %s\n  CON filtro variedad: dias = %d, r = %s\n\n",
              t, opt_orig$opt_dias, opt_orig$r, opt_var$opt_dias, opt_var$r))
}
