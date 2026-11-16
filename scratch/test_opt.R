library(dplyr)
load("Actualizacion Semanal/datos_maestros_cosecha_2026.RData")

optimizar_dias_maduracion <- function(df_cosecha, df_conteo, campana = 2026, modulo, turno, variedad = NULL, min_dias = 35, max_dias = 120) {
  if (is.null(df_cosecha) || is.null(df_conteo) || nrow(df_cosecha) == 0 || nrow(df_conteo) == 0) {
    return(list(opt_dias = 45L, r = NA_real_, sem_shift = 6L))
  }
  
  camp_chr <- as.character(campana)
  camp_num <- suppressWarnings(as.numeric(camp_chr))
  mod_chr  <- trimws(as.character(modulo))
  tur_chr  <- trimws(as.character(turno))
  
  # Cosecha real por semana
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
  
  # Si no hay datos suficientes de cosecha en el turno, buscar a nivel módulo
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
  
  # Conteo floral por semana
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
  
  # Si no hay conteo en el turno, fallback a nivel módulo
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
  }
  
  if (nrow(df_y) < 3 || nrow(df_c) < 3) {
    return(list(opt_dias = 45L, r = NA_real_, sem_shift = 6L, mensaje = "Datos insuficientes"))
  }
  
  # Evaluar desfases de semanas entre round(min_dias/7) y round(max_dias/7)
  sem_shifts <- unique(round(seq(min_dias, max_dias, by = 1) / 7))
  
  evals <- lapply(sem_shifts, function(sw) {
    df_join <- df_y %>%
      mutate(Semana_Flor = Semana_Num - sw) %>%
      inner_join(df_c, by = c("Semana_Flor" = "Semana_Num")) %>%
      filter(!is.na(Rend), !is.na(Carga))
    
    if (nrow(df_join) < 3) {
      return(tibble(sw = sw, r = NA_real_, n = nrow(df_join)))
    }
    
    # Pearson r
    r_val <- suppressWarnings(cor(df_join$Rend, df_join$Carga))
    tibble(sw = sw, r = r_val, n = nrow(df_join))
  }) %>% bind_rows() %>% filter(!is.na(r))
  
  if (nrow(evals) == 0) {
    return(list(opt_dias = 45L, r = NA_real_, sem_shift = 6L, mensaje = "Sin correlación calculable"))
  }
  
  best <- evals %>% arrange(desc(r)) %>% slice(1)
  opt_sem <- best$sw[1]
  opt_r   <- round(best$r[1], 3)
  opt_dias <- as.integer(opt_sem * 7)
  
  return(list(opt_dias = opt_dias, r = opt_r, sem_shift = opt_sem, mensaje = "OK"))
}

# Probar en varios turnos
cat("--- Test Módulo 2 Turno 1 ---\n")
print(optimizar_dias_maduracion(df_cosecha, df_conteo_historico, 2026, "2", "1"))

cat("--- Test Módulo 2 Turno 2 ---\n")
print(optimizar_dias_maduracion(df_cosecha, df_conteo_historico, 2026, "2", "2"))

cat("--- Test Módulo 6 Turno 1 ---\n")
print(optimizar_dias_maduracion(df_cosecha, df_conteo_historico, 2026, "6", "1"))
