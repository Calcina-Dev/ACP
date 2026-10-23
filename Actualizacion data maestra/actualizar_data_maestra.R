# ==============================================================================
# SCRIPT MAESTRO DE ACTUALIZACIÓN DE DATOS (ARÁNDANO 2026)
# Ubicación: D:/ACP/Actualizacion data maestra/actualizar_data_maestra.R
# Propósito:
#    1. Ingestar la cosecha diaria de SAP (Cosecha_SAP_2026_dash.xlsx)
#    2. Integrar datos en df_cosecha (Control Semanal) y df_hist_oficial_wide (Mapa)
#    3. Escanear y consolidar TODAS las proyecciones semanales (V_max de S21 a S44+)
#    4. Unificar todo en un ÚNICO archivo RData para carga instantánea en shinyapps.io y local
# ==============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(lubridate)
})

# Forzar zona horaria de Perú (UTC-5) para consistencia local y en shinyapps.io
Sys.setenv(TZ = "America/Lima")

t_inicio <- lubridate::with_tz(Sys.time(), tzone = "America/Lima")
cat("\n")
cat("==============================================================================\n")
cat("🚀 INICIANDO ACTUALIZACIÓN MAESTRA DE DATOS - COSECHA Y PROYECCIÓN 2026\n")
cat(sprintf("⏰ Hora de inicio (Perú): %s\n", format(t_inicio, "%Y-%m-%d %H:%M:%S")))
cat("==============================================================================\n\n")

# ------------------------------------------------------------------------------
# 1. DEFINICIÓN DE RUTAS Y FUNCIONES AUXILIARES
# ------------------------------------------------------------------------------
ruta_drive_raiz <- "G:/Unidades compartidas/Compartido Proyecciones/3.- Arándano"

# Funciones de normalización estándar
normalizar_texto <- function(x) {
  x_str <- toupper(trimws(as.character(x)))
  chartr("ÁÉÍÓÚÜ", "AEIOUU", x_str)
}

limpiar_variedad <- function(x) {
  v <- normalizar_texto(x)
  dplyr::case_when(
    grepl("EB\\s*9\\s*[-_.]\\s*2", v, ignore.case = TRUE) ~ "MAGICA",
    grepl("^MEGA\\s*CRISP|^MEGACRISP", v, ignore.case = TRUE) ~ "MEGA CRISP",
    grepl("^MILAGRO", v, ignore.case = TRUE) ~ "MILAGROS",
    TRUE ~ v
  )
}

norm_codigo_valvula <- function(x) {
  sub("^M0*([0-9]+)[-_]T0*([0-9]+)[-_]V0*([0-9]+)$", "M\\1-T\\2-V\\3", toupper(trimws(as.character(x))))
}

# Subdivisión geométrica de válvulas compuestas por múltiples variedades (ej: M12-T17-V59)
aplicar_particiones_multivariedad <- function(df) {
  ruta_part <- "D:/ACP/Rutas de Cosecha/particiones_multivariedad.rds"
  if (!file.exists(ruta_part) && exists("ruta_drive_raiz")) ruta_part <- file.path(ruta_drive_raiz, "particiones_multivariedad.rds")
  if (!file.exists(ruta_part)) ruta_part <- "particiones_multivariedad.rds"
  if (!file.exists(ruta_part)) return(df)
  parts_def <- tryCatch(readRDS(ruta_part), error = function(e) NULL)
  if (is.null(parts_def) || nrow(parts_def) == 0) return(df)
  
  codigos_part <- unique(parts_def[["codigo"]])
  for (cod in codigos_part) {
    idx_cod <- which(df[["codigo"]] == cod)
    # Solo subdividir si la válvula existe actualmente como 1 registro único original indiviso
    if (length(idx_cod) == 1) {
      row_orig <- df[idx_cod, ]
      parts <- parts_def[parts_def[["codigo"]] == cod, ]
      parts <- parts[order(parts[["orden"]]), ]
      
      sum_ha <- sum(parts[["area_ha"]])
      w_orig <- as.numeric(row_orig[["w"]])
      w_subs <- round(w_orig * (parts[["area_ha"]] / sum_ha), 2)
      diff_w <- w_orig - sum(w_subs)
      w_subs[length(w_subs)] <- w_subs[length(w_subs)] + diff_w
      x_subs <- as.numeric(row_orig[["x"]]) + c(0, cumsum(w_subs)[-length(w_subs)])
      
      sub_rows <- do.call(rbind, lapply(seq_len(nrow(parts)), function(i) {
        r <- row_orig
        var_name <- parts[["variedad"]][i]
        var_tag <- gsub("[^a-zA-Z0-9]", "", var_name)
        
        count_var <- sum(parts[["variedad"]][1:i] == var_name)
        total_var <- sum(parts[["variedad"]] == var_name)
        if (total_var > 1) {
          var_tag <- paste0(var_tag, "_", count_var)
        }
        
        r[["id"]] <- paste0(r[["id"]], "_", var_tag)
        r[["Variedad"]] <- var_name
        r[["Área"]] <- parts[["area_ha"]][i]
        r[["x"]] <- x_subs[i]
        r[["w"]] <- w_subs[i]
        r
      }))
      df <- rbind(df[-idx_cod, ], sub_rows)
    }
  }
  df
}

# Limpiador de texto de Módulo para evitar errores de punto flotante de Excel
limpiar_modulo_str <- function(x) {
  x_chr <- trimws(as.character(x))
  x_num <- suppressWarnings(as.numeric(x_chr))
  ifelse(
    !is.na(x_num),
    sub("\\.0$", "", sprintf("%.1f", round(x_num, 1))),
    x_chr
  )
}

asignar_grupo_modulo <- function(mod_vec) {
  mod_num <- suppressWarnings(as.numeric(sub("\\..*", "", as.character(mod_vec))))
  dplyr::case_when(
    mod_num %in% c(1, 2, 3)         ~ "COSECHA MOD 1,2y3 / 2026",
    mod_num %in% c(4, 5, 6, 7, 12) ~ "COSECHA - LABORES SECTOR 8",
    mod_num %in% c(8, 9, 11)        ~ "REPORTES COSECHA S9",
    mod_num %in% c(10, 13, 14)     ~ "Cosecha M10-M13-M14",
    TRUE                           ~ "OTROS"
  )
}

# ------------------------------------------------------------------------------
# 2. CARGAR BASE HISTÓRICA MULTICAMPAÑA, CATÁLOGO Y CACHÉ PREVIO
# ------------------------------------------------------------------------------
cat("📂 [1/5] Cargando bases históricas y caché previo...\n")

ruta_rdata_base <- "D:/ACP/Actualizacion data maestra/datos_historicos_arandano.RData"
if (!file.exists(ruta_rdata_base)) {
  ruta_rdata_base <- file.path(ruta_drive_raiz, "datos_historicos_arandano.RData")
}

dir_hist_cosecha <- file.path(ruta_drive_raiz, "Histórico de Cosecha")
arch_bi_cosecha2 <- if (dir.exists(dir_hist_cosecha)) {
  list.files(dir_hist_cosecha, pattern = "^BI_Cosecha2.*\\.(xlsx|xls)$", full.names = TRUE, ignore.case = TRUE)[1]
} else NA_character_

if (file.exists(ruta_rdata_base)) {
  env_hist <- new.env()
  load(ruta_rdata_base, envir = env_hist)
  
  if (!is.na(arch_bi_cosecha2) && file.exists(arch_bi_cosecha2)) {
    mtime_bi2   <- file.info(arch_bi_cosecha2)$mtime
    mtime_rdata <- file.info(ruta_rdata_base)$mtime
    if (!is.na(mtime_bi2) && !is.na(mtime_rdata) && mtime_bi2 > mtime_rdata) {
      cat(sprintf("   🔄 BI_Cosecha2.xlsx en Drive (%s) es más reciente que el RData base (%s). Sincronizando histórico...\n",
                  format(mtime_bi2, "%Y-%m-%d %H:%M"), format(mtime_rdata, "%Y-%m-%d %H:%M")))
      tryCatch({
        df_bi2_nuevo <- suppressMessages(read_excel(arch_bi_cosecha2))
        if (nrow(df_bi2_nuevo) > 0 && "Campaña" %in% names(df_bi2_nuevo)) {
          env_hist$df_cosecha <- df_bi2_nuevo
          save(list = ls(envir = env_hist), file = "D:/ACP/Actualizacion data maestra/datos_historicos_arandano.RData", envir = env_hist)
          cat(sprintf("   ✔ RData histórico actualizado automáticamente (%d filas en BI_Cosecha2)\n", nrow(df_bi2_nuevo)))
        }
      }, error = function(e) {
        cat(sprintf("   ⚠️ Aviso al leer BI_Cosecha2.xlsx: %s (se usa base previa)\n", e$message))
      })
    }
  }
  df_cosecha      <- env_hist$df_cosecha
  df_tratamientos <- env_hist$df_tratamientos
} else {
  stop("❌ No se encontró datos_historicos_arandano.RData en D:/ACP/Actualizacion data maestra/ ni en Drive")
}

cat(sprintf("   ✔ df_cosecha histórico cargado: %d registros (Campañas: %s)\n", 
            nrow(df_cosecha), paste(sort(unique(df_cosecha$Campaña)), collapse = ", ")))

# Cargar RData maestro previo
ruta_rdata_prev <- "D:/ACP/Actualizacion data maestra/datos_maestros_cosecha_2026.RData"
if (!file.exists(ruta_rdata_prev)) {
  ruta_rdata_prev <- file.path(ruta_drive_raiz, "datos_maestros_cosecha_2026.RData")
}
df_proyecciones_prev  <- tibble()
df_manifest_proy_prev <- tibble()
df_cosecha_prev       <- tibble()

if (file.exists(ruta_rdata_prev)) {
  env_prev <- new.env()
  tryCatch({
    load(ruta_rdata_prev, envir = env_prev)
    if (exists("df_cosecha", envir = env_prev) && is.data.frame(env_prev$df_cosecha)) {
      df_cosecha_prev <- env_prev$df_cosecha
    }
    if (exists("df_proyecciones", envir = env_prev) && is.data.frame(env_prev$df_proyecciones)) {
      df_proyecciones_prev <- env_prev$df_proyecciones %>%
        mutate(
          Modulo_Raw  = limpiar_modulo_str(Modulo_Raw),
          Módulo_join = limpiar_modulo_str(Módulo_join)
        ) %>%
        group_by(Campaña, Semana, Version, Sector, Sector_join, Modulo_Raw, Módulo_join, Módulo, Turno, Turno_join, Variedad, Variedad_join) %>%
        summarise(
          Proyección    = sum(Proyección, na.rm = TRUE),
          Kg_Proy_Turno = Proyección,
          .groups       = "drop"
        )
    }
    if (exists("df_manifest_proy", envir = env_prev) && is.data.frame(env_prev$df_manifest_proy)) {
      df_manifest_proy_prev <- env_prev$df_manifest_proy
    } else if (exists("metadata_actualizacion", envir = env_prev) &&
               !is.null(env_prev$metadata_actualizacion$Matriz_Control_Proyecciones)) {
      df_manifest_proy_prev <- env_prev$metadata_actualizacion$Matriz_Control_Proyecciones
    }
  }, error = function(e) NULL)
}

if (nrow(df_cosecha_prev) > 0) {
  sems_base_2026 <- unique(df_cosecha$Semana[df_cosecha$Campaña == 2026])
  df_extra_prev <- df_cosecha_prev %>%
    filter(Campaña == 2026, !Semana %in% sems_base_2026)
  if (nrow(df_extra_prev) > 0) {
    df_cosecha <- bind_rows(df_cosecha, df_extra_prev)
    cat(sprintf("   ✔ Se preservaron %d filas de semanas 2026 previas (%s)\n",
                nrow(df_extra_prev), paste(sort(unique(df_extra_prev$Semana)), collapse = ", ")))
  }
}

# Catálogo de válvulas normalizadas
ruta_valvs <- "D:/ACP/Rutas de Cosecha/valvulas_normalizadas.rds"
if (!file.exists(ruta_valvs)) {
  stop("❌ No se encontró valvulas_normalizadas.rds en D:/ACP/Rutas de Cosecha/")
}
df_valvulas <- readRDS(ruta_valvs) %>%
  filter(!grepl("^S[0-9]", codigo)) %>%
  mutate(
    id          = as.character(id),
    codigo_norm = norm_codigo_valvula(codigo),
    Módulo      = as.character(suppressWarnings(as.numeric(sub("\\..*", "", as.character(Módulo))))),
    Turno       = as.character(suppressWarnings(as.numeric(Turno))),
    Variedad    = limpiar_variedad(Variedad),
    Área        = suppressWarnings(as.numeric(Área)),
    Grupo       = asignar_grupo_modulo(Módulo)
  )
df_valvulas <- aplicar_particiones_multivariedad(df_valvulas)
cat(sprintf("   ✔ Catálogo de válvulas cargado: %d válvulas activas\n", nrow(df_valvulas)))

# Histórico wide previo
ruta_hist_wide_prev <- "D:/ACP/Rutas de Cosecha/historico_oficial_wide_2026.rds"
df_hist_oficial_wide <- if (file.exists(ruta_hist_wide_prev)) {
  readRDS(ruta_hist_wide_prev)
} else {
  tibble()
}
cat(sprintf("   ✔ Histórico wide previo cargado: %d registros\n\n", nrow(df_hist_oficial_wide)))


# ------------------------------------------------------------------------------
# 3. INGESTAR Y PROCESAR COSECHA DIARIA DE SAP
# ------------------------------------------------------------------------------
cat("📊 [2/5] Buscando y procesando archivo SAP diario...\n")

ruta_sap <- file.path(ruta_drive_raiz, "Campaña 26-27", "Cosecha", "BI", "Cosecha_SAP_2026_dash.xlsx")
tiene_sap <- file.exists(ruta_sap)

if (tiene_sap) {
  cat(sprintf("   ✔ Archivo SAP detectado: %s\n", basename(ruta_sap)))
  
  df_sap_raw <- suppressMessages(read_excel(ruta_sap))
  if ("IDCULTIVO" %in% names(df_sap_raw)) {
    df_sap_raw <- df_sap_raw %>% filter(IDCULTIVO == "AR")
  }
  
  raw_fechas <- df_sap_raw$FECHA
  fechas_parsed <- if (inherits(raw_fechas, "Date")) {
    raw_fechas
  } else if (inherits(raw_fechas, "POSIXt")) {
    as.Date(raw_fechas)
  } else if (is.numeric(raw_fechas)) {
    as.Date(raw_fechas, origin = "1899-12-30")
  } else {
    txt_norm <- gsub("\\.", "/", as.character(raw_fechas))
    suppressWarnings(as.Date(parse_date_time(txt_norm, orders = c("ymd", "dmy", "mdy", "Y-m-d", "Ymd"))))
  }
  
  df_sap_raw$Fecha_Parsed <- fechas_parsed
  df_sap_raw$Semana_ISO   <- lubridate::isoweek(fechas_parsed)
  df_sap_raw$Anio_ISO     <- lubridate::isoyear(fechas_parsed)
  
  semanas_en_sap <- sort(unique(na.omit(df_sap_raw$Semana_ISO)))
  rango_fechas   <- range(na.omit(fechas_parsed))
  cat(sprintf("   ✔ Registros SAP: %d filas | Semanas: %s | Rango: %s a %s\n",
              nrow(df_sap_raw), paste(semanas_en_sap, collapse = ", "),
              format(rango_fechas[1]), format(rango_fechas[2])))
  
  consum_vals <- as.character(df_sap_raw$CONSUMIDOR)
  sec_vec <- suppressWarnings(as.numeric(sub(".*\\bS(\\d+).*", "\\1", consum_vals)))
  mod_vec <- suppressWarnings(as.character(as.numeric(sub(".*\\bM(\\d+).*", "\\1", consum_vals))))
  tur_vec <- suppressWarnings(as.numeric(sub(".*\\bT(\\d+).*", "\\1", consum_vals)))
  
  df_sap_turno <- df_sap_raw %>%
    mutate(
      Fecha    = Fecha_Parsed,
      Real     = suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(PESONETO)))),
      Sector   = sec_vec,
      Módulo   = mod_vec,
      Turno    = tur_vec,
      Variedad = limpiar_variedad(sub("^.*\\|\\s*", "", as.character(VARIEDAD))),
      Semana   = Semana_ISO,
      Campaña  = 2026
    ) %>%
    filter(!is.na(Real), Real > 0, !is.na(Fecha)) %>%
    mutate(
      Sector = ifelse(Variedad == "ROCIO" & Módulo == "8", 3, Sector),
      Módulo = case_when(
        Módulo == "9" & between(Turno, 1, 11)  ~ "9.2",
        Módulo == "9" & Turno >= 12            ~ "9.1",
        Módulo == "11" & between(Turno, 1, 12) ~ "11.1",
        Módulo == "11" & Turno >= 13           ~ "11.2",
        Módulo == "13" & between(Turno, 1, 9)  ~ "13.1",
        Módulo == "13" & Turno >= 10           ~ "13.2",
        Módulo == "14" & between(Turno, 1, 8)  ~ "14.1",
        Módulo == "14" & Turno >= 9            ~ "14.2",
        Variedad == "ROCIO" & Módulo == "8"    ~ "13.2",
        TRUE                                   ~ Módulo
      )
    ) %>%
    group_by(Fecha, Campaña, Semana, Sector, Módulo, Turno, Variedad) %>%
    summarise(
      `Kg Total` = sum(Real, na.rm = TRUE),
      .groups    = "drop"
    )
  
  fechas_sap_unicas <- unique(df_sap_turno$Fecha)
  df_cosecha <- df_cosecha %>%
    filter(!(Campaña == 2026 & as.Date(Fecha) %in% fechas_sap_unicas)) %>%
    bind_rows(df_sap_turno)
  
  cat(sprintf("   ✔ df_cosecha actualizado con SAP: %d filas añadidas/reemplazadas (Total: %.1f Kg)\n",
              nrow(df_sap_turno), sum(df_sap_turno$`Kg Total`, na.rm = TRUE)))
  
  df_sap_valvs_raw <- df_sap_raw %>%
    mutate(
      Fecha_Date  = Fecha_Parsed,
      Semana      = Semana_ISO,
      Dia_Num     = lubridate::wday(Fecha_Date, week_start = 1),
      Kg          = suppressWarnings(as.numeric(PESONETO)),
      Mod_Num     = suppressWarnings(as.numeric(sub("^M0*", "", MODULO))),
      Tur_Num     = suppressWarnings(as.numeric(sub("^T0*", "", TURNO))),
      Val_Num     = suppressWarnings(as.numeric(sub("^0*", "", coalesce(NROVALVULA, VALVULA_CAMPO)))),
      codigo_norm = sprintf("M%d-T%d-V%d", Mod_Num, Tur_Num, Val_Num),
      Variedad    = limpiar_variedad(sub("^.*\\|\\s*", "", as.character(VARIEDAD)))
    ) %>%
    filter(!is.na(Kg), Kg > 0, !is.na(Mod_Num), !is.na(Tur_Num), !is.na(Val_Num), !is.na(Dia_Num))
  
  kg_domingo <- sum(df_sap_valvs_raw$Kg[df_sap_valvs_raw$Dia_Num == 7], na.rm = TRUE)
  if (kg_domingo > 0) {
    cat(sprintf("   ⚠️ Aviso: Se detectaron %.1f Kg cosechados en día Domingo en SAP; se acumularán en 'Sábado'.\n", kg_domingo))
  }
  
  df_sap_valvs <- df_sap_valvs_raw %>%
    mutate(
      dia_col = case_when(
        Dia_Num == 1 ~ "Lunes",
        Dia_Num == 2 ~ "Martes",
        Dia_Num == 3 ~ "Miércoles",
        Dia_Num == 4 ~ "Jueves",
        Dia_Num == 5 ~ "Viernes",
        Dia_Num %in% c(6, 7) ~ "Sábado",
        TRUE ~ "Otro"
      )
    ) %>%
    filter(dia_col != "Otro")
  
  auditoria_sap_huerfanas <- df_sap_valvs %>%
    filter(!codigo_norm %in% df_valvulas$codigo_norm) %>%
    group_by(Semana, codigo_norm) %>%
    summarise(Kg_Huerfanos = round(sum(Kg, na.rm = TRUE), 2), Filas = n(), .groups = "drop")
  
  if (nrow(auditoria_sap_huerfanas) > 0) {
    cat(sprintf("   ⚠️ ALERTA: %d válvula(s) en SAP NO existen en el catálogo del mapa (%.1f Kg sin polígono):\n",
                nrow(auditoria_sap_huerfanas), sum(auditoria_sap_huerfanas$Kg_Huerfanos)))
    for (idx_h in seq_len(nrow(auditoria_sap_huerfanas))) {
      cat(sprintf("      ❌ Sem %d | %s -> %.1f Kg (%d reg)\n",
                  auditoria_sap_huerfanas$Semana[idx_h],
                  auditoria_sap_huerfanas$codigo_norm[idx_h],
                  auditoria_sap_huerfanas$Kg_Huerfanos[idx_h],
                  auditoria_sap_huerfanas$Filas[idx_h]))
    }
  } else {
    cat("   ✔ Auditoría SAP vs Mapa: 100% de las válvulas SAP existen en el mapa (0.0 Kg huérfanos)\n")
  }
  
  resumen_sap_wide <- df_sap_valvs %>%
    group_by(Semana, codigo_norm, Variedad, dia_col) %>%
    summarise(Kg_Dia = sum(Kg, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = dia_col, values_from = Kg_Dia, values_fill = 0, names_prefix = "Kg_")
  
  for (cd in c("Kg_Lunes", "Kg_Martes", "Kg_Miércoles", "Kg_Jueves", "Kg_Viernes", "Kg_Sábado")) {
    if (!cd %in% names(resumen_sap_wide)) resumen_sap_wide[[cd]] <- 0
  }
  
  codigos_multi <- df_valvulas %>%
    group_by(codigo_norm) %>%
    filter(n() > 1) %>%
    pull(codigo_norm) %>%
    unique()
  
  df_valv_multi <- df_valvulas %>%
    filter(codigo_norm %in% codigos_multi) %>%
    select(codigo_norm, Variedad, Sector, Módulo, Turno, Área)
  
  df_valv_uni <- df_valvulas %>%
    filter(!codigo_norm %in% codigos_multi) %>%
    distinct(codigo_norm, .keep_all = TRUE) %>%
    select(codigo_norm, Variedad_Cat = Variedad, Sector, Módulo, Turno, Área)
  
  df_multi <- resumen_sap_wide %>%
    filter(codigo_norm %in% codigos_multi) %>%
    inner_join(df_valv_multi, by = c("codigo_norm", "Variedad")) %>%
    group_by(Semana, codigo_norm, Variedad) %>%
    mutate(
      prop_sub = Área / pmax(0.0001, sum(Área)),
      across(starts_with("Kg_"), ~ round(. * prop_sub, 2))
    ) %>%
    select(-prop_sub) %>%
    ungroup()
  
  df_uni <- resumen_sap_wide %>%
    filter(!codigo_norm %in% codigos_multi) %>%
    inner_join(df_valv_uni, by = "codigo_norm") %>%
    mutate(Variedad = coalesce(Variedad_Cat, Variedad)) %>%
    select(-Variedad_Cat)
  
  df_sap_wide_full <- bind_rows(df_multi, df_uni) %>%
    mutate(
      Campaña       = 2026,
      Pasada        = 1,
      Sector        = suppressWarnings(as.numeric(Sector)),
      Módulo_Norm   = as.character(Módulo),
      Turno_Norm    = as.character(Turno),
      Codigo_Valv   = codigo_norm,
      Variedad      = as.character(Variedad),
      Area_Ha       = suppressWarnings(as.numeric(Área)),
      Kg_Total_Sem  = round(Kg_Lunes + Kg_Martes + Kg_Miércoles + Kg_Jueves + Kg_Viernes + Kg_Sábado, 3),
      Pct_Lunes     = ifelse(Kg_Total_Sem > 0, round((Kg_Lunes / Kg_Total_Sem) * 100), 0),
      Pct_Martes    = ifelse(Kg_Total_Sem > 0, round((Kg_Martes / Kg_Total_Sem) * 100), 0),
      Pct_Miércoles = ifelse(Kg_Total_Sem > 0, round((Kg_Miércoles / Kg_Total_Sem) * 100), 0),
      Pct_Jueves    = ifelse(Kg_Total_Sem > 0, round((Kg_Jueves / Kg_Total_Sem) * 100), 0),
      Pct_Viernes   = ifelse(Kg_Total_Sem > 0, round((Kg_Viernes / Kg_Total_Sem) * 100), 0),
      Pct_Sábado    = ifelse(Kg_Total_Sem > 0, round((Kg_Sábado / Kg_Total_Sem) * 100), 0)
    )
  
  if (nrow(df_sap_wide_full) > 0) {
    mat_kg  <- as.matrix(df_sap_wide_full[, c("Kg_Lunes", "Kg_Martes", "Kg_Miércoles", "Kg_Jueves", "Kg_Viernes", "Kg_Sábado")])
    mat_pct <- as.matrix(df_sap_wide_full[, c("Pct_Lunes", "Pct_Martes", "Pct_Miércoles", "Pct_Jueves", "Pct_Viernes", "Pct_Sábado")])
    for (r_i in seq_len(nrow(mat_pct))) {
      if (df_sap_wide_full$Kg_Total_Sem[r_i] > 0) {
        diff_pct <- 100 - sum(mat_pct[r_i, ])
        if (diff_pct != 0) {
          idx_max <- which.max(mat_kg[r_i, ])
          mat_pct[r_i, idx_max] <- mat_pct[r_i, idx_max] + diff_pct
        }
      }
    }
    df_sap_wide_full$Pct_Lunes     <- mat_pct[, 1]
    df_sap_wide_full$Pct_Martes    <- mat_pct[, 2]
    df_sap_wide_full$Pct_Miércoles <- mat_pct[, 3]
    df_sap_wide_full$Pct_Jueves    <- mat_pct[, 4]
    df_sap_wide_full$Pct_Viernes   <- mat_pct[, 5]
    df_sap_wide_full$Pct_Sábado    <- mat_pct[, 6]
  }
  
  df_sap_wide_full <- df_sap_wide_full %>%
    mutate(
      Ha_Lunes      = round(Area_Ha * (Pct_Lunes / 100), 2),
      Ha_Martes     = round(Area_Ha * (Pct_Martes / 100), 2),
      Ha_Miércoles  = round(Area_Ha * (Pct_Miércoles / 100), 2),
      Ha_Jueves     = round(Area_Ha * (Pct_Jueves / 100), 2),
      Ha_Viernes    = round(Area_Ha * (Pct_Viernes / 100), 2),
      Ha_Sábado     = round(Area_Ha * (Pct_Sábado / 100), 2),
      Ha_Total_Sem  = Area_Ha,
      Modo          = "Cosecha Oficial",
      Evaluador     = "Sistema Packing Oficial SAP",
      Fecha_Hora    = format(lubridate::with_tz(Sys.time(), tzone = "America/Lima"), "%Y-%m-%d %H:%M:%S")
    ) %>%
    select(
      Campaña, Semana, Pasada, Sector, Módulo_Norm, Turno_Norm, Codigo_Valv, Variedad, Area_Ha,
      Kg_Miércoles, Kg_Viernes, Kg_Jueves, Kg_Martes, Kg_Lunes, Kg_Sábado, Kg_Total_Sem,
      Pct_Lunes, Pct_Martes, Pct_Miércoles, Pct_Jueves, Pct_Viernes, Pct_Sábado,
      Ha_Lunes, Ha_Martes, Ha_Miércoles, Ha_Jueves, Ha_Viernes, Ha_Sábado, Ha_Total_Sem,
      Modo, Evaluador, Fecha_Hora
    )
  
  if (nrow(df_hist_oficial_wide) > 0) {
    df_hist_oficial_wide <- df_hist_oficial_wide %>%
      filter(!Semana %in% semanas_en_sap) %>%
      bind_rows(df_sap_wide_full)
  } else {
    df_hist_oficial_wide <- df_sap_wide_full
  }
  
  cat(sprintf("   ✔ df_hist_oficial_wide actualizado con válvulas SAP: %d registros totales\n\n",
              nrow(df_hist_oficial_wide)))
} else {
  auditoria_sap_huerfanas <- tibble()
  cat("   ⚠️ Archivo SAP no encontrado en Drive. Se conservan datos históricos existentes.\n\n")
}


# ------------------------------------------------------------------------------
# 4. ESCANEAR Y ACTUALIZAR INCREMENTALMENTE LAS PROYECCIONES SEMANALES (V_MAX)
# ------------------------------------------------------------------------------
cat("📈 [3/5] Verificando matriz de control de proyecciones semanales (Incremental)...\n")

dir_proy_det <- file.path(ruta_drive_raiz, "Campaña 26-27", "Proyección Semanal", "Proyección Semanal Detallada")

leer_proyeccion_excel <- function(f_p, s_num, v_num) {
  hojas <- excel_sheets(f_p)
  h_calc <- hojas[grepl("^c[aá]lculo$", trimws(hojas), ignore.case = TRUE)][1]
  if (is.na(h_calc)) h_calc <- hojas[grepl("calculo", hojas, ignore.case = TRUE)][1]
  if (is.na(h_calc)) h_calc <- hojas[1]
  
  raw_p <- suppressMessages(read_excel(f_p, sheet = h_calc, skip = 3))
  nms   <- names(raw_p)
  
  col_s <- nms[grepl("^s$|^sector$", nms, ignore.case = TRUE)][1]
  if (is.na(col_s) && ncol(raw_p) >= 3) col_s <- nms[3]
  
  col_m <- nms[grepl("^m$|^m[oó]dulo$", nms, ignore.case = TRUE)][1]
  if (is.na(col_m) && ncol(raw_p) >= 4) col_m <- nms[4]
  
  col_t <- nms[grepl("^t$|^turno$", nms, ignore.case = TRUE)][1]
  if (is.na(col_t) && ncol(raw_p) >= 5) col_t <- nms[5]
  
  col_v <- nms[grepl("^variedad$", nms, ignore.case = TRUE)][1]
  if (is.na(col_v) && ncol(raw_p) >= 7) col_v <- nms[7]
  
  col_p <- nms[grepl("proyecci[oó]n", nms, ignore.case = TRUE) & !grepl("ha", nms, ignore.case = TRUE)][1]
  if (is.na(col_p) && ncol(raw_p) >= 13) col_p <- nms[13]
  
  raw_p %>%
    filter(!is.na(.data[[col_s]]), !is.na(.data[[col_t]])) %>%
    mutate(
      Campaña       = 2026,
      Semana        = s_num,
      Version       = v_num,
      Sector        = trimws(as.character(.data[[col_s]])),
      Sector_join   = Sector,
      Modulo_Raw    = limpiar_modulo_str(.data[[col_m]]),
      Módulo_join   = Modulo_Raw,
      Módulo        = as.character(suppressWarnings(as.numeric(sub("\\..*", "", Modulo_Raw)))),
      Turno         = as.character(suppressWarnings(as.numeric(trimws(as.character(.data[[col_t]]))))),
      Turno_join    = Turno,
      Variedad      = limpiar_variedad(.data[[col_v]]),
      Variedad_join = Variedad,
      Proyección    = suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(.data[[col_p]])))),
      Kg_Proy_Turno = Proyección
    ) %>%
    filter(!is.na(Turno), !is.na(Proyección), Proyección > 0) %>%
    group_by(Campaña, Semana, Version, Sector, Sector_join, Modulo_Raw, Módulo_join, Módulo, Turno, Turno_join, Variedad, Variedad_join) %>%
    summarise(
      Proyección    = sum(Proyección, na.rm = TRUE),
      Kg_Proy_Turno = Proyección,
      .groups       = "drop"
    )
}

if (dir.exists(dir_proy_det)) {
  archivos_proy <- list.files(dir_proy_det, pattern = "\\.(xlsx|xls)$", full.names = TRUE)
  archivos_proy <- archivos_proy[!grepl("^~\\$", basename(archivos_proy))]
  
  sems_proy <- regmatches(basename(archivos_proy), regexec("Semana\\s*([0-9]+)", basename(archivos_proy), ignore.case = TRUE))
  sems_num  <- sapply(sems_proy, function(x) if (length(x) >= 2) as.numeric(x[2]) else NA)
  
  vers_proy <- sapply(basename(archivos_proy), function(x) {
    v <- regmatches(x, regexec("_V([0-9]+)", x, ignore.case = TRUE))[[1]]
    if (length(v) >= 2) as.numeric(v[2]) else 1
  })
  
  f_info <- file.info(archivos_proy)
  
  df_archivos_proy <- tibble(
    archivo   = archivos_proy,
    nombre    = basename(archivos_proy),
    semana    = sems_num,
    version   = vers_proy,
    mtime     = f_info$mtime,
    mtime_str = format(f_info$mtime, "%Y-%m-%d %H:%M:%S"),
    fsize     = as.numeric(f_info$size)
  ) %>%
    filter(!is.na(semana)) %>%
    arrange(semana, desc(version), desc(mtime)) %>%
    group_by(semana) %>%
    slice(1) %>%
    ungroup() %>%
    arrange(semana)
  
  tiene_cache_valido <- nrow(df_proyecciones_prev) > 0 &&
    nrow(df_manifest_proy_prev) > 0 &&
    all(c("semana", "nombre", "version", "mtime_str", "fsize") %in% names(df_manifest_proy_prev))
  
  if (tiene_cache_valido) {
    df_comp <- df_archivos_proy %>%
      left_join(
        df_manifest_proy_prev %>%
          select(semana, nombre_prev = nombre, version_prev = version, mtime_prev = mtime_str, fsize_prev = fsize),
        by = "semana"
      ) %>%
      mutate(
        en_datos_prev = semana %in% unique(df_proyecciones_prev$Semana),
        requiere_lectura = is.na(nombre_prev) |
          !en_datos_prev |
          (nombre != nombre_prev) |
          (version != version_prev) |
          (mtime_str != mtime_prev) |
          (fsize != fsize_prev)
      )
  } else {
    df_comp <- df_archivos_proy %>%
      mutate(requiere_lectura = TRUE)
  }
  
  sems_a_leer   <- df_comp %>% filter(requiere_lectura)
  sems_en_cache <- df_comp %>% filter(!requiere_lectura)
  
  cat(sprintf("   ✔ Semanas detectadas: %d | En caché sin cambios: %d | Nuevas o modificadas a leer: %d\n",
              nrow(df_comp), nrow(sems_en_cache), nrow(sems_a_leer)))
  
  df_proy_conservadas <- if (nrow(sems_en_cache) > 0 && nrow(df_proyecciones_prev) > 0) {
    df_proyecciones_prev %>% filter(Semana %in% sems_en_cache$semana)
  } else {
    tibble()
  }
  
  if (nrow(sems_a_leer) > 0) {
    lista_nuevas <- vector("list", nrow(sems_a_leer))
    for (i in seq_len(nrow(sems_a_leer))) {
      f_p   <- sems_a_leer$archivo[i]
      s_num <- sems_a_leer$semana[i]
      v_num <- sems_a_leer$version[i]
      cat(sprintf("      🔄 Leyendo Sem %02d (V%d): %s [Modificado: %s]\n",
                  s_num, v_num, sems_a_leer$nombre[i], sems_a_leer$mtime_str[i]))
      lista_nuevas[[i]] <- leer_proyeccion_excel(f_p, s_num, v_num)
    }
    df_proy_nuevas <- bind_rows(lista_nuevas)
  } else {
    cat("      ⚡ Todas las semanas de proyección están al día en caché (0 archivos Excel leídos).\n")
    df_proy_nuevas <- tibble()
  }
  
  df_proyecciones <- bind_rows(df_proy_conservadas, df_proy_nuevas) %>%
    arrange(Semana, suppressWarnings(as.numeric(Sector)), suppressWarnings(as.numeric(Módulo)), suppressWarnings(as.numeric(Turno)))
  
  resumen_kg_sem <- df_proyecciones %>%
    group_by(Semana) %>%
    summarise(Filas = n(), Total_Kg_Proy = round(sum(Proyección, na.rm = TRUE), 0), .groups = "drop")
  
  df_manifest_proy <- df_archivos_proy %>%
    select(semana, version, nombre, mtime_str, fsize) %>%
    left_join(resumen_kg_sem, by = c("semana" = "Semana"))
  
  cat(sprintf("   ✔ Proyecciones consolidadas: %d registros | Total Kilos: %s\n\n",
              nrow(df_proyecciones), format(round(sum(df_proyecciones$Proyección)), big.mark = ",")))
} else {
  cat("   ⚠️ No se encontró la carpeta de Proyecciones Detalladas. Se conserva caché previo.\n\n")
  df_proyecciones  <- df_proyecciones_prev
  df_manifest_proy <- df_manifest_proy_prev
}


# ------------------------------------------------------------------------------
# 5. GENERAR METADATA DE CONTROL
# ------------------------------------------------------------------------------
metadata_actualizacion <- list(
  Fecha_Hora_Actualizacion        = lubridate::with_tz(Sys.time(), tzone = "America/Lima"),
  Fecha_Hora_Actualizacion_Str    = format(lubridate::with_tz(Sys.time(), tzone = "America/Lima"), "%d/%m/%Y %H:%M:%S"),
  Zona_Horaria                    = "America/Lima",
  Total_Kilos_Cosecha_2026        = sum(df_cosecha %>% filter(Campaña == 2026) %>% pull(`Kg Total`), na.rm = TRUE),
  Total_Kilos_Proyeccion          = if (nrow(df_proyecciones) > 0) sum(df_proyecciones$Proyección, na.rm = TRUE) else 0,
  Semanas_Cosecha_2026            = sort(unique((df_cosecha %>% filter(Campaña == 2026))$Semana)),
  Semanas_Proyeccion              = if (nrow(df_proyecciones) > 0) sort(unique(df_proyecciones$Semana)) else numeric(0),
  Total_Valvulas_Mapa             = nrow(df_valvulas),
  Filas_Hist_Wide_Mapa            = nrow(df_hist_oficial_wide),
  Filas_Proyecciones              = nrow(df_proyecciones),
  Filas_Cosecha_Multicampana      = nrow(df_cosecha),
  Matriz_Control_Proyecciones     = df_manifest_proy,
  Auditoria_Valvulas_Huerfanas    = auditoria_sap_huerfanas
)


# ------------------------------------------------------------------------------
# 6. GUARDAR EL ARCHIVO RDATA MAESTRO UNIFICADO
# ------------------------------------------------------------------------------
cat("💾 [4/5] Guardando datos maestros unificados...\n")

ruta_rdata_drive <- file.path(ruta_drive_raiz, "datos_maestros_cosecha_2026.RData")
ruta_rdata_d1    <- "D:/ACP/Actualizacion data maestra/datos_maestros_cosecha_2026.RData"
ruta_rdata_d2    <- "D:/ACP/Rutas de Cosecha/datos_maestros_cosecha_2026.RData"

save(
  df_cosecha,
  df_tratamientos,
  df_valvulas,
  df_hist_oficial_wide,
  df_proyecciones,
  df_manifest_proy,
  metadata_actualizacion,
  file = ruta_rdata_d1
)
cat(sprintf("   ✔ Guardado local exitoso en: %s (Tamaño: %.2f MB)\n", 
            ruta_rdata_d1, file.size(ruta_rdata_d1) / (1024 * 1024)))

file.copy(ruta_rdata_d1, ruta_rdata_d2, overwrite = TRUE)
cat(sprintf("   ✔ Espejo local actualizado en: %s\n", ruta_rdata_d2))

saveRDS(df_hist_oficial_wide, "D:/ACP/Rutas de Cosecha/historico_oficial_wide_2026.rds")
saveRDS(df_valvulas, "D:/ACP/Rutas de Cosecha/valvulas_normalizadas.rds")

guardado_drive <- FALSE
tryCatch({
  file.copy(ruta_rdata_d1, ruta_rdata_drive, overwrite = TRUE)
  cat(sprintf("   ✔ Guardado exitoso en Google Drive: %s\n", ruta_rdata_drive))
  guardado_drive <- TRUE
}, error = function(e) {
  cat(sprintf("   ⚠️ No se pudo copiar a Google Drive directamente: %s\n", e$message))
})

t_fin <- Sys.time()
duracion_seg <- as.numeric(t_fin - t_inicio, units = "secs")

cat("\n")
cat("==============================================================================\n")
cat("✅ ACTUALIZACIÓN MAESTRA FINALIZADA CON ÉXITO\n")
cat(sprintf("⏱️ Tiempo total de procesamiento: %.2f segundos\n", duracion_seg))
cat(sprintf("📦 Archivo generado: datos_maestros_cosecha_2026.RData (%.2f MB)\n", file.size(ruta_rdata_d1) / (1024 * 1024)))
cat(sprintf("📊 Semanas Proyección consolidadas: %s\n", paste(metadata_actualizacion$Semanas_Proyeccion, collapse = ", ")))
cat(sprintf("🍇 Semanas Cosecha 2026 disponibles: %s\n", paste(metadata_actualizacion$Semanas_Cosecha_2026, collapse = ", ")))
cat(sprintf("🗺️ Válvulas con cosecha oficial en mapa: %d registros\n", nrow(df_hist_oficial_wide)))
cat("==============================================================================\n\n")