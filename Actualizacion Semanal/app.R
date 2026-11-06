# ==============================================================================
# SHINY APP: CONTROL SEMANAL DE COSECHA (VERSIÓN CORREGIDA Y OPTIMIZADA)
# ==============================================================================

paquetes <- c("shiny", "dplyr", "tidyr", "lubridate", "DT", "readxl", "readr", "stringi", "plotly", "writexl", "httr", "jsonlite")
nuevos <- paquetes[!(paquetes %in% installed.packages()[, "Package"])]
if (length(nuevos)) install.packages(nuevos)

library(shiny)
library(dplyr)
library(tidyr)
library(lubridate)
library(DT)
library(readxl)
library(readr)
library(stringi)
library(plotly)
library(writexl)

# Forzar zona horaria de Perú (UTC-5) tanto en local como en shinyapps.io (Linux UTC)
Sys.setenv(TZ = "America/Lima")

formatear_hora_peru <- function(t, fmt = "%d/%m/%Y %H:%M:%S") {
  if (is.null(t) || length(t) == 0 || all(is.na(t))) return("Desconocida")
  t_peru <- tryCatch(lubridate::with_tz(as.POSIXct(t), tzone = "America/Lima"), error = function(e) as.POSIXct(t))
  format(t_peru, fmt)
}

# Función auxiliar para remover tildes y mayúsculas en cruces
normalizar_texto <- function(x) {
  if (is.null(x) || length(x) == 0) return(character(0))
  x_str <- toupper(trimws(as.character(x)))
  as.character(chartr("ÁÉÍÓÚÜ", "AEIOUU", x_str))
}

# Función auxiliar para homologar nombres de variedad
limpiar_variedad <- function(x) {
  if (is.null(x) || length(x) == 0) return(character(0))
  v <- normalizar_texto(x)
  as.character(ifelse(grepl("EB\\s*9\\s*[-_.]\\s*2", v, ignore.case = TRUE), "MAGICA", v))
}

# ==============================================================================
# FUNCIONES AUXILIARES PARA RESUMEN SEMANAL (VARIEDAD, MÓDULO, PROGRAMA)
# ==============================================================================
formatear_var_resumen <- function(var, tipo) {
  if (is.null(var) || length(var) == 0) return(character(0))
  v_upper <- toupper(trimws(as.character(var)))
  t_low   <- tolower(trimws(as.character(tipo)))
  
  if (v_upper %in% c("VARIEDADES", "MILAGROS")) return("Variedades")
  if (v_upper %in% c("SEKOYA POP", "MAGICA")) {
    v_clean <- tools::toTitleCase(tolower(v_upper))
    suf <- if (grepl("org", t_low)) " O." else " C."
    return(paste0(v_clean, suf))
  }
  if (v_upper %in% c("BILOXI", "VENTURA", "SEKOYA BEAUTY")) {
    v_clean <- tools::toTitleCase(tolower(v_upper))
    suf <- if (grepl("conv", t_low)) " C." else " O."
    return(paste0(v_clean, suf))
  }
  return(tools::toTitleCase(tolower(v_upper)))
}

clasificar_programa_comercial <- function(var, tipo, mod) {
  v <- toupper(trimws(as.character(var)))
  t <- tolower(trimws(as.character(tipo)))
  
  if (v %in% c("MADEIRA", "MALIBU", "MANILA")) return("Kg Planasa")
  if (v %in% c("ATLAS BLUE", "TEST BLOCK") || grepl("test|tb", t)) return("Kg vivero varios (Test Block)")
  if (v == "MAGICA" && grepl("conv", t)) return("Kg Convencional OZ Blue")
  if (v == "MAGICA" && grepl("org", t)) return("Kg Orgánico OZ Blue")
  if (v == "SEKOYA POP" && grepl("conv", t)) return("Kg Convencional North Bay")
  if (v %in% c("SEKOYA POP", "SEKOYA BEAUTY") && grepl("org", t)) return("Kg Orgánico North Bay")
  if (v %in% c("BILOXI", "VENTURA")) return("Kg Orgánico ACP")
  if (v == "IMPERIAL" && grepl("conv", t)) return("Kg Convencional Hortifrut")
  if (v %in% c("ROCIO", "IMPERIAL", "MILAGROS", "VARIEDADES")) return("Kg Orgánico Hortifrut")
  if (grepl("org", t)) paste0("Kg Orgánico ", v) else paste0("Kg Convencional ", v)
}

orden_variedades_resumen <- c(
  "Biloxi O.", "Ventura O.", "Rocio", "Atlas Blue", "Imperial",
  "Madeira", "Malibu", "Manila", "Variedades",
  "Sekoya Pop C.", "Sekoya Beauty O.", "Sekoya Pop O.",
  "Magica C.", "Magica O."
)

orden_programas_comercial <- c(
  "Kg Convencional North Bay",
  "Kg Convencional Hortifrut",
  "Kg Convencional OZ Blue",
  "Kg Orgánico OZ Blue",
  "Kg Orgánico ACP",
  "Kg Orgánico Hortifrut",
  "Kg Planasa",
  "Kg vivero varios (Test Block)",
  "Kg Orgánico North Bay"
)

norm_codigo_valvula <- function(x) {
  sub("^M0*([0-9]+)[-_]T0*([0-9]+)[-_]V0*([0-9]+)$", "M\\1-T\\2-V\\3", toupper(trimws(as.character(x))))
}

# Función para ordenar semanas cronológicamente en campañas que cruzan de año (ej. S25..S53 -> S1..S19)
orden_semana_campana <- function(sem) {
  sem_num <- suppressWarnings(as.numeric(sem))
  ifelse(!is.na(sem_num) & sem_num < 20, sem_num + 53, sem_num)
}

# Función para asignar colores y texto a las celdas del heatmap de frecuencia (estilo Emil Kowalski)
color_frecuencia <- function(val) {
  if (is.null(val) || length(val) == 0 || is.na(val[1]) || !is.finite(val[1])) {
    return(list(bg = "#F4F4F6", text = "#C7C7CC", str = "-"))
  }
  
  v_num <- suppressWarnings(as.numeric(val[1]))
  if (is.na(v_num) || !is.finite(v_num)) {
    return(list(bg = "#F4F4F6", text = "#C7C7CC", str = "-"))
  }
  
  # Clamping estricto de ratio entre 0.0 y 1.0 para evitar NAs en colorRamp
  ratio <- pmax(0.0, pmin(1.0, (v_num - 3.0) / 4.0))
  
  col_func <- colorRamp(c("#C2E7C7", "#D5E593", "#E3D17A", "#DA9B5F", "#BF6A3B"))
  rgb_mat  <- col_func(ratio)
  
  bg_hex <- if (any(is.na(rgb_mat))) {
    "#BF6A3B"
  } else {
    rgb(rgb_mat[1, 1], rgb_mat[1, 2], rgb_mat[1, 3], maxColorValue = 255)
  }
  
  text_col <- if (v_num >= 6.4) "#FFFFFF" else "#1C1C1E"
  
  list(bg = bg_hex, text = text_col, str = sprintf("%.1f", v_num))
}

# Definir la ruta raíz de Arándano
ruta_raiz_arandano <- "G:/Unidades compartidas/Compartido Proyecciones/3.- Arándano"

# Rutas para persistencia de ediciones manuales (Conexión exclusiva a Google Drive)
ruta_ediciones_principal <- file.path(ruta_raiz_arandano, "ediciones_manuales_cosecha.rds")
ruta_ediciones_nube      <- "ediciones_manuales_cosecha.rds"

# Carga unificada de rutas_cosecha_mapa.rds (compatible con Unidad G:, D: y local)
cargar_rutas_cosecha <- function() {
  rutas_candidatas <- c(
    file.path(ruta_raiz_arandano, "rutas_cosecha_mapa.rds"),
    "D:/ACP/Rutas de Cosecha/rutas_cosecha_mapa.rds",
    "rutas_cosecha_mapa.rds"
  )
  for (r in rutas_candidatas) {
    if (file.exists(r)) {
      df <- tryCatch(readRDS(r), error = function(e) NULL)
      if (!is.null(df) && is.data.frame(df) && nrow(df) > 0) {
        return(df)
      }
    }
  }
  return(NULL)
}

cargar_ediciones_disco <- function() {
  df <- NULL
  if (file.exists(ruta_ediciones_principal)) {
    tryCatch({
      df <- readRDS(ruta_ediciones_principal)
      cat("[EDICIONES] Cargadas exitosamente desde Google Drive:", nrow(df), "registros\n")
    }, error = function(e) {
      cat("[AVISO EDICIONES] Error leyendo de Google Drive:", e$message, "\n")
    })
  } else if (file.exists(ruta_ediciones_nube)) {
    tryCatch({
      df <- readRDS(ruta_ediciones_nube)
      cat("[EDICIONES] Cargadas exitosamente desde contenedor local (shinyapps.io):", nrow(df), "registros\n")
    }, error = function(e) {
      cat("[AVISO EDICIONES] Error leyendo desde contenedor local:", e$message, "\n")
    })
  }
  if (is.null(df) || nrow(df) == 0) {
    return(tibble(
      Campaña       = character(),
      Semana        = numeric(),
      Sector        = character(),
      Módulo        = character(),
      Turno         = character(),
      Variedad      = character(),
      Campo         = character(),
      Valor         = numeric(),
      Ha_Asignada     = numeric(),
      Ha_Asignada_1ra = numeric(),
      Kg_Asignado_1ra = numeric(),
      Ha_Asignada_2da = numeric(),
      Kg_Asignado_2da = numeric(),
      Tipo_Edicion    = character(),
      Motivo_Ajuste   = character(),
      Fecha_Edicion   = as.POSIXct(character())
    ))
  }
  df %>%
    mutate(
      Campaña         = trimws(as.character(Campaña)),
      Semana          = suppressWarnings(as.numeric(Semana)),
      Sector          = trimws(as.character(Sector)),
      Módulo          = trimws(as.character(Módulo)),
      Turno           = trimws(as.character(Turno)),
      Variedad        = limpiar_variedad(Variedad),
      Campo           = trimws(as.character(Campo)),
      Valor           = suppressWarnings(as.numeric(Valor)),
      Ha_Asignada     = if ("Ha_Asignada" %in% names(.)) suppressWarnings(as.numeric(Ha_Asignada)) else NA_real_,
      Ha_Asignada_1ra = if ("Ha_Asignada_1ra" %in% names(.)) suppressWarnings(as.numeric(Ha_Asignada_1ra)) else NA_real_,
      Kg_Asignado_1ra = if ("Kg_Asignado_1ra" %in% names(.)) suppressWarnings(as.numeric(Kg_Asignado_1ra)) else NA_real_,
      Ha_Asignada_2da = if ("Ha_Asignada_2da" %in% names(.)) suppressWarnings(as.numeric(Ha_Asignada_2da)) else NA_real_,
      Kg_Asignado_2da = if ("Kg_Asignado_2da" %in% names(.)) suppressWarnings(as.numeric(Kg_Asignado_2da)) else NA_real_,
      Tipo_Edicion    = if ("Tipo_Edicion" %in% names(.)) coalesce(as.character(Tipo_Edicion), "manual") else "manual",
      Motivo_Ajuste   = if ("Motivo_Ajuste" %in% names(.)) as.character(Motivo_Ajuste) else NA_character_,
      Fecha_Edicion   = as.POSIXct(Fecha_Edicion)
    )
}

guardar_ediciones_disco <- function(df_ediciones) {
  guardado <- FALSE
  df_norm <- df_ediciones %>%
    mutate(
      Campaña         = trimws(as.character(Campaña)),
      Semana          = suppressWarnings(as.numeric(Semana)),
      Sector          = trimws(as.character(Sector)),
      Módulo          = trimws(as.character(Módulo)),
      Turno           = trimws(as.character(Turno)),
      Variedad        = limpiar_variedad(Variedad),
      Campo           = trimws(as.character(Campo)),
      Valor           = suppressWarnings(as.numeric(Valor)),
      Ha_Asignada     = if ("Ha_Asignada" %in% names(.)) suppressWarnings(as.numeric(Ha_Asignada)) else NA_real_,
      Ha_Asignada_1ra = if ("Ha_Asignada_1ra" %in% names(.)) suppressWarnings(as.numeric(Ha_Asignada_1ra)) else NA_real_,
      Kg_Asignado_1ra = if ("Kg_Asignado_1ra" %in% names(.)) suppressWarnings(as.numeric(Kg_Asignado_1ra)) else NA_real_,
      Ha_Asignada_2da = if ("Ha_Asignada_2da" %in% names(.)) suppressWarnings(as.numeric(Ha_Asignada_2da)) else NA_real_,
      Kg_Asignado_2da = if ("Kg_Asignado_2da" %in% names(.)) suppressWarnings(as.numeric(Kg_Asignado_2da)) else NA_real_,
      Tipo_Edicion    = if ("Tipo_Edicion" %in% names(.)) coalesce(as.character(Tipo_Edicion), "manual") else "manual",
      Motivo_Ajuste   = if ("Motivo_Ajuste" %in% names(.)) as.character(Motivo_Ajuste) else NA_character_,
      Fecha_Edicion   = as.POSIXct(Fecha_Edicion)
    )
  if (dir.exists(dirname(ruta_ediciones_principal))) {
    tryCatch({
      saveRDS(df_norm, ruta_ediciones_principal)
      cat("[EDICIONES] Guardadas en Google Drive exitosamente:", nrow(df_norm), "registros\n")
      guardado <- TRUE
    }, error = function(e) {
      cat("[AVISO EDICIONES] No se pudo guardar en Google Drive:", e$message, "\n")
    })
  }
  tryCatch({
    saveRDS(df_norm, ruta_ediciones_nube)
    guardado <- TRUE
  }, error = function(e) NULL)
  return(guardado)
}

# Rutas y funciones para persistencia de Válvulas Ajustadas por Semana (para visualización en mapa de ruta)
ruta_ajuste_valvulas_principal <- file.path(ruta_raiz_arandano, "ajuste_valvulas_semana.rds")
ruta_ajuste_valvulas_nube      <- "ajuste_valvulas_semana.rds"

cargar_ajuste_valvulas_disco <- function() {
  df <- NULL
  if (file.exists(ruta_ajuste_valvulas_principal)) {
    tryCatch({
      df <- readRDS(ruta_ajuste_valvulas_principal)
    }, error = function(e) NULL)
  } else if (file.exists(ruta_ajuste_valvulas_nube)) {
    tryCatch({
      df <- readRDS(ruta_ajuste_valvulas_nube)
    }, error = function(e) NULL)
  }
  if (is.null(df) || !is.data.frame(df)) {
    df <- tibble(
      Campaña         = character(),
      Semana          = numeric(),
      id              = character(),
      codigo          = character(),
      Sector          = character(),
      Módulo          = character(),
      Turno           = character(),
      Variedad        = character(),
      Dia_Asignado    = character(),
      Kg_Asignado     = numeric(),
      Ha_Asignada     = numeric(),
      Num_Pasada      = integer(),
      Tipo_Asignacion = character(),
      Dia_Desde       = character(),
      Dia_Hasta       = character(),
      Motivo_Ajuste   = character(),
      Fecha_Ajuste    = as.POSIXct(character())
    )
  }
  return(df)
}

guardar_ajuste_valvulas_disco <- function(df) {
  if (is.null(df) || !is.data.frame(df)) return(invisible(FALSE))
  if (dir.exists(dirname(ruta_ajuste_valvulas_principal))) {
    tryCatch({
      saveRDS(df, ruta_ajuste_valvulas_principal)
    }, error = function(e) NULL)
  }
  tryCatch({
    saveRDS(df, ruta_ajuste_valvulas_nube)
  }, error = function(e) NULL)
  invisible(TRUE)
}

# Rutas y funciones para persistencia de Válvulas Ajustadas por Semana (para visualización en mapa de ruta)
ruta_ajuste_valvulas_principal <- file.path(ruta_raiz_arandano, "ajuste_valvulas_semana.rds")
ruta_ajuste_valvulas_nube      <- "ajuste_valvulas_semana.rds"

cargar_ajuste_valvulas_disco <- function() {
  df <- NULL
  if (file.exists(ruta_ajuste_valvulas_principal)) {
    tryCatch({
      df <- readRDS(ruta_ajuste_valvulas_principal)
    }, error = function(e) NULL)
  } else if (file.exists(ruta_ajuste_valvulas_nube)) {
    tryCatch({
      df <- readRDS(ruta_ajuste_valvulas_nube)
    }, error = function(e) NULL)
  }
  if (is.null(df) || !is.data.frame(df)) {
    df <- tibble(
      Campaña         = character(),
      Semana          = numeric(),
      id              = character(),
      codigo          = character(),
      Sector          = character(),
      Módulo          = character(),
      Turno           = character(),
      Variedad        = character(),
      Dia_Asignado    = character(),
      Kg_Asignado     = numeric(),
      Ha_Asignada     = numeric(),
      Num_Pasada      = integer(),
      Tipo_Asignacion = character(),
      Dia_Desde       = character(),
      Dia_Hasta       = character(),
      Motivo_Ajuste   = character(),
      Fecha_Ajuste    = as.POSIXct(character())
    )
  }
  return(df)
}

guardar_ajuste_valvulas_disco <- function(df) {
  if (is.null(df) || !is.data.frame(df)) return(invisible(FALSE))
  if (dir.exists(dirname(ruta_ajuste_valvulas_principal))) {
    tryCatch({
      saveRDS(df, ruta_ajuste_valvulas_principal)
    }, error = function(e) NULL)
  }
  tryCatch({
    saveRDS(df, ruta_ajuste_valvulas_nube)
  }, error = function(e) NULL)
  invisible(TRUE)
}

# Rutas y funciones para persistencia de Frecuencias de Cosecha por Módulo y Variedad
ruta_frecuencias_principal        <- file.path(ruta_raiz_arandano, "frecuencias_modulo_variedad_cosecha.rds")
ruta_frecuencias_nube             <- "frecuencias_modulo_variedad_cosecha.rds"
ruta_frecuencias_legacy_principal <- file.path(ruta_raiz_arandano, "frecuencias_variedad_cosecha.rds")
ruta_frecuencias_legacy_nube      <- "frecuencias_variedad_cosecha.rds"

ruta_modo_frec_principal <- file.path(ruta_raiz_arandano, "modo_frecuencia_cosecha.rds")
ruta_modo_frec_nube      <- "modo_frecuencia_cosecha.rds"

id_input_frec_mod <- function(m, v) {
  mod_clean <- gsub("[^A-Za-z0-9]", "_", trimws(as.character(m)))
  var_clean <- gsub("[^A-Za-z0-9]", "_", limpiar_variedad(v))
  paste0("frec_mv_", mod_clean, "_", var_clean)
}

id_input_frec <- function(v) {
  paste0("frec_var_", gsub("[^A-Z0-9]", "_", limpiar_variedad(v)))
}

cargar_modo_frecuencia_disco <- function() {
  m <- NULL
  if (file.exists(ruta_modo_frec_principal)) {
    m <- tryCatch(readRDS(ruta_modo_frec_principal), error = function(e) NULL)
  } else if (file.exists(ruta_modo_frec_nube)) {
    m <- tryCatch(readRDS(ruta_modo_frec_nube), error = function(e) NULL)
  }
  if (!is.null(m) && is.character(m) && length(m) == 1 && m %in% c("automatico", "manual")) {
    return(m)
  }
  return("automatico")
}

guardar_modo_frecuencia_disco <- function(modo) {
  if (is.null(modo) || !is.character(modo) || length(modo) != 1) return(FALSE)
  if (dir.exists(dirname(ruta_modo_frec_principal))) {
    tryCatch(saveRDS(modo, ruta_modo_frec_principal), error = function(e) NULL)
  }
  tryCatch(saveRDS(modo, ruta_modo_frec_nube), error = function(e) NULL)
  return(TRUE)
}

desplazar_dia_frecuencia <- function(dia_base_vec, frec_vec, dias_nombres, dia_inicio_idx, dia_inicio_nom, dia_fin_idx = 6L, dia_fin_nom = "Sábado") {
  idx_base <- match(dia_base_vec, dias_nombres)
  frec_int <- as.integer(frec_vec)
  desp <- ifelse(frec_int <= 4L, frec_int, frec_int - 7L)
  idx_target <- pmin(dia_fin_idx, pmax(1L, idx_base + desp))
  dia_target <- ifelse(is.na(idx_target), dia_inicio_nom, dias_nombres[idx_target])
  dia_res <- ifelse(is.na(idx_target) | idx_target < dia_inicio_idx, dia_inicio_nom, dia_target)
  idx_res <- match(dia_res, dias_nombres)
  ifelse(!is.na(idx_res) & idx_res > dia_fin_idx, dia_fin_nom, dia_res)
}

cargar_frecuencias_disco <- function(combos_default = NULL) {
  df <- NULL
  if (file.exists(ruta_frecuencias_principal)) {
    df <- tryCatch(readRDS(ruta_frecuencias_principal), error = function(e) NULL)
  } else if (file.exists(ruta_frecuencias_nube)) {
    df <- tryCatch(readRDS(ruta_frecuencias_nube), error = function(e) NULL)
  } else if (file.exists(ruta_frecuencias_legacy_principal)) {
    df <- tryCatch(readRDS(ruta_frecuencias_legacy_principal), error = function(e) NULL)
  } else if (file.exists(ruta_frecuencias_legacy_nube)) {
    df <- tryCatch(readRDS(ruta_frecuencias_legacy_nube), error = function(e) NULL)
  }
  
  combos_base <- if (!is.null(combos_default) && is.data.frame(combos_default) && nrow(combos_default) > 0) {
    combos_default %>% distinct(Módulo = trimws(as.character(Módulo)), Variedad = limpiar_variedad(Variedad))
  } else if (exists("df_valvulas") && is.data.frame(df_valvulas) && all(c("Módulo", "Variedad") %in% names(df_valvulas))) {
    df_valvulas %>%
      mutate(
        Turno_num = suppressWarnings(as.numeric(Turno)),
        Módulo_c = case_when(
          Módulo %in% c("13.1", "13.2") ~ "13",
          Módulo %in% c("14.1", "14.2") ~ "14",
          Módulo == "9" & between(Turno_num, 1, 11)  ~ "9.2",
          Módulo == "9" & Turno_num >= 12            ~ "9.1",
          Módulo == "11" & between(Turno_num, 1, 12) ~ "11.1",
          Módulo == "11" & Turno_num >= 13           ~ "11.2",
          TRUE                                       ~ trimws(as.character(Módulo))
        ),
        Variedad_c = limpiar_variedad(Variedad)
      ) %>%
      filter(!is.na(Módulo_c), nzchar(Módulo_c), !is.na(Variedad_c), nzchar(Variedad_c)) %>%
      distinct(Módulo = Módulo_c, Variedad = Variedad_c)
  } else {
    tibble(
      Módulo   = c("13", "14", "6", "8", "10"),
      Variedad = c("ROCIO", "ROCIO", "MAGICA", "BILOXI", "MADEIRA")
    )
  }
  
  df_def <- combos_base %>%
    mutate(
      Frecuencia_Dias = ifelse(Variedad == "ROCIO", 3L, 7L)
    )
  
  if (!is.null(df) && is.data.frame(df) && all(c("Módulo", "Variedad", "Frecuencia_Dias") %in% names(df))) {
    df_clean <- df %>%
      mutate(
        Módulo          = trimws(as.character(Módulo)),
        Variedad        = limpiar_variedad(Variedad),
        Frecuencia_Dias = suppressWarnings(as.integer(Frecuencia_Dias))
      ) %>%
      filter(!is.na(Módulo), nzchar(Módulo), !is.na(Variedad), nzchar(Variedad), !is.na(Frecuencia_Dias), Frecuencia_Dias >= 1L, Frecuencia_Dias <= 21L) %>%
      distinct(Módulo, Variedad, .keep_all = TRUE)
    
    df_def <- df_def %>%
      left_join(df_clean, by = c("Módulo", "Variedad"), suffix = c("_def", "")) %>%
      mutate(Frecuencia_Dias = coalesce(Frecuencia_Dias, Frecuencia_Dias_def)) %>%
      select(Módulo, Variedad, Frecuencia_Dias)
  } else if (!is.null(df) && is.data.frame(df) && all(c("Variedad", "Frecuencia_Dias") %in% names(df))) {
    df_clean <- df %>%
      mutate(
        Variedad        = limpiar_variedad(Variedad),
        Frecuencia_Dias = suppressWarnings(as.integer(Frecuencia_Dias))
      ) %>%
      filter(!is.na(Variedad), nzchar(Variedad), !is.na(Frecuencia_Dias)) %>%
      distinct(Variedad, .keep_all = TRUE)
    
    df_def <- df_def %>%
      left_join(df_clean, by = "Variedad", suffix = c("_def", "")) %>%
      mutate(Frecuencia_Dias = coalesce(Frecuencia_Dias, Frecuencia_Dias_def)) %>%
      select(Módulo, Variedad, Frecuencia_Dias)
  }
  return(df_def)
}

guardar_frecuencias_disco <- function(df_frec) {
  if (is.null(df_frec) || !is.data.frame(df_frec)) return(FALSE)
  if (dir.exists(dirname(ruta_frecuencias_principal))) {
    tryCatch(saveRDS(df_frec, ruta_frecuencias_principal), error = function(e) NULL)
  }
  tryCatch(saveRDS(df_frec, ruta_frecuencias_nube), error = function(e) NULL)
  return(TRUE)
}

# Algoritmo para calcular frecuencias medianas observadas de campo por Módulo y Variedad
calcular_frecuencias_historicas_mv <- function(df_w_data, df_c_data, combos_mv_target, camp_sel = 2026) {
  all_ev <- NULL
  if (!is.null(df_w_data) && is.data.frame(df_w_data) && nrow(df_w_data) > 0 && "Codigo_Valv" %in% names(df_w_data)) {
    dias_cols <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
    dias_map  <- setNames(1:6, dias_cols)
    df_w_sub  <- df_w_data %>% filter(!is.na(Codigo_Valv), nzchar(as.character(Codigo_Valv)))
    
    events_list <- list()
    for (d in dias_cols) {
      kg_col <- paste0("Kg_", d)
      if (kg_col %in% names(df_w_sub)) {
        sub_d <- df_w_sub %>%
          filter(!is.na(.data[[kg_col]]), .data[[kg_col]] > 0) %>%
          select(Semana, Módulo = Módulo_Norm, Turno = Turno_Norm, Variedad, Codigo_Valv, Kg = all_of(kg_col)) %>%
          mutate(Dia_Num = dias_map[d], Dia_Abs = (as.numeric(Semana) - 1) * 7 + dias_map[d])
        events_list[[d]] <- sub_d
      }
    }
    if (length(events_list) > 0) {
      all_ev <- bind_rows(events_list) %>% arrange(Codigo_Valv, Dia_Abs)
      all_ev <- all_ev %>%
        group_by(Codigo_Valv) %>%
        mutate(diff_days = Dia_Abs - lag(Dia_Abs)) %>%
        ungroup() %>%
        filter(!is.na(diff_days), diff_days >= 1, diff_days <= 35)
    }
  }
  
  if (is.null(all_ev) || nrow(all_ev) == 0) {
    if (!is.null(df_c_data) && is.data.frame(df_c_data) && nrow(df_c_data) > 0) {
      df_c_sub <- df_c_data %>%
        filter(
          as.character(Campaña) == as.character(camp_sel) | Campaña == suppressWarnings(as.numeric(camp_sel)),
          coalesce(`Kg Total`, 0) > 0,
          coalesce(Es_Testblock, "NO") != "SI"
        ) %>%
        mutate(
          Fecha_d    = as.Date(Fecha),
          Semana_n   = as.numeric(Semana),
          Módulo_c   = as.character(Módulo),
          Turno_c    = as.character(Turno),
          Valvula_c  = as.character(Válvula),
          Variedad_c = limpiar_variedad(Variedad)
        )
      if (nrow(df_c_sub) > 0) {
        valv_ev <- df_c_sub %>%
          group_by(Módulo_c, Turno_c, Valvula_c, Variedad_c, Fecha_d, Semana_n) %>%
          summarise(Kg = sum(`Kg Total`, na.rm = TRUE), .groups = "drop") %>%
          arrange(Módulo_c, Turno_c, Valvula_c, Fecha_d) %>%
          group_by(Módulo_c, Turno_c, Valvula_c, Variedad_c) %>%
          mutate(diff_days = as.numeric(Fecha_d - lag(Fecha_d))) %>%
          ungroup() %>%
          filter(!is.na(diff_days), diff_days >= 1, diff_days <= 35)
        
        all_ev <- valv_ev %>%
          select(Semana = Semana_n, Módulo = Módulo_c, Turno = Turno_c, Variedad = Variedad_c, diff_days, Kg)
      }
    }
  }
  
  if (is.null(all_ev) || nrow(all_ev) == 0) {
    return(combos_mv_target %>% mutate(Frecuencia_Dias = ifelse(Variedad == "ROCIO", 3L, 7L)))
  }
  
  all_ev <- all_ev %>%
    mutate(
      Turno_num = suppressWarnings(as.numeric(Turno)),
      Módulo_c = case_when(
        as.character(Módulo) %in% c("13.1", "13.2") ~ "13",
        as.character(Módulo) %in% c("14.1", "14.2") ~ "14",
        as.character(Módulo) == "9" & between(Turno_num, 1, 11)  ~ "9.2",
        as.character(Módulo) == "9" & Turno_num >= 12            ~ "9.1",
        as.character(Módulo) == "11" & between(Turno_num, 1, 12) ~ "11.1",
        as.character(Módulo) == "11" & Turno_num >= 13           ~ "11.2",
        TRUE                                                     ~ trimws(as.character(Módulo))
      ),
      Variedad_c = limpiar_variedad(Variedad),
      Semana_num = suppressWarnings(as.numeric(Semana))
    )
  
  max_s <- max(all_ev$Semana_num, na.rm = TRUE)
  if (is.na(max_s) || max_s <= 0) max_s <- 44
  
  frec_mv_hist <- all_ev %>%
    filter(Semana_num >= (max_s - 3)) %>%
    group_by(Módulo = Módulo_c, Variedad = Variedad_c) %>%
    summarise(
      Frec_Med = round(median(diff_days)),
      .groups  = "drop"
    )
  
  frec_v_fallback <- all_ev %>%
    filter(Semana_num >= (max_s - 3)) %>%
    group_by(Variedad = Variedad_c) %>%
    summarise(
      Frec_Var_Med = round(median(diff_days)),
      .groups      = "drop"
    )
  
  combos_mv_target %>%
    left_join(frec_mv_hist, by = c("Módulo", "Variedad")) %>%
    left_join(frec_v_fallback, by = "Variedad") %>%
    mutate(
      Frecuencia_Dias = pmin(15L, pmax(1L, as.integer(coalesce(
        Frec_Med,
        Frec_Var_Med,
        ifelse(Variedad == "ROCIO", 3, 7)
      ))))
    ) %>%
    select(Módulo, Variedad, Frecuencia_Dias)
}

ruta_pct_rocio_principal <- file.path(Sys.getenv("USERPROFILE"), "Desktop", "porcentajes_rocio_modulo.rds")
ruta_pct_rocio_nube      <- "porcentajes_rocio_modulo.rds"

id_input_pct_rocio_lun <- function(mod) {
  paste0("pct_rocio_lun_mod_", gsub("[^A-Za-z0-9]", "_", trimws(as.character(mod))))
}

id_input_pct_rocio_mar <- function(mod) {
  paste0("pct_rocio_mar_mod_", gsub("[^A-Za-z0-9]", "_", trimws(as.character(mod))))
}

cargar_pct_rocio_disco <- function(modulos_default = NULL) {
  df <- NULL
  if (file.exists(ruta_pct_rocio_principal)) {
    df <- tryCatch(readRDS(ruta_pct_rocio_principal), error = function(e) NULL)
  } else if (file.exists(ruta_pct_rocio_nube)) {
    df <- tryCatch(readRDS(ruta_pct_rocio_nube), error = function(e) NULL)
  }
  
  mods_base <- if (!is.null(modulos_default) && length(modulos_default) > 0) {
    sort(unique(trimws(as.character(modulos_default))))
  } else if (exists("df_valvulas") && is.data.frame(df_valvulas) && "Variedad" %in% names(df_valvulas) && "Módulo" %in% names(df_valvulas)) {
    sort(unique(trimws(as.character(df_valvulas$Módulo[limpiar_variedad(df_valvulas$Variedad) == "ROCIO"]))))
  } else {
    c("8", "9", "10", "11", "12", "13", "14")
  }
  mods_base <- mods_base[!is.na(mods_base) & nzchar(mods_base)]
  if (length(mods_base) == 0) mods_base <- c("8", "9", "10", "11", "12", "13", "14")
  
  df_def <- tibble(
    Módulo         = mods_base,
    Pct_Pasada_Lun = 60,
    Pct_Pasada_Mar = 60
  )
  
  if (!is.null(df) && is.data.frame(df) && "Módulo" %in% names(df)) {
    df_clean <- df %>%
      mutate(
        Módulo         = trimws(as.character(Módulo)),
        Pct_Pasada_Lun = suppressWarnings(as.numeric(Pct_Pasada_Lun)),
        Pct_Pasada_Mar = suppressWarnings(as.numeric(Pct_Pasada_Mar))
      ) %>%
      filter(!is.na(Módulo), nzchar(Módulo)) %>%
      distinct(Módulo, .keep_all = TRUE)
    
    df_def <- df_def %>%
      left_join(df_clean, by = "Módulo", suffix = c("_def", "")) %>%
      mutate(
        Pct_Pasada_Lun = coalesce(Pct_Pasada_Lun, Pct_Pasada_Lun_def, 60),
        Pct_Pasada_Mar = coalesce(Pct_Pasada_Mar, Pct_Pasada_Mar_def, 60)
      ) %>%
      select(Módulo, Pct_Pasada_Lun, Pct_Pasada_Mar)
    
    mods_extra <- setdiff(df_clean$Módulo, df_def$Módulo)
    if (length(mods_extra) > 0) {
      df_def <- bind_rows(df_def, df_clean %>% filter(Módulo %in% mods_extra) %>% select(Módulo, Pct_Pasada_Lun, Pct_Pasada_Mar))
    }
  }
  return(df_def)
}

guardar_pct_rocio_disco <- function(df_pct) {
  if (is.null(df_pct) || !is.data.frame(df_pct)) return(FALSE)
  if (dir.exists(dirname(ruta_pct_rocio_principal))) {
    tryCatch(saveRDS(df_pct, ruta_pct_rocio_principal), error = function(e) NULL)
  }
  tryCatch(saveRDS(df_pct, ruta_pct_rocio_nube), error = function(e) NULL)
  return(TRUE)
}

# ==============================================================================
# CONFIGURACIÓN DE DÍAS DE MADURACIÓN POR MÓDULO Y VARIEDAD (PERSISTENCIA)
# ==============================================================================
ruta_maduracion_principal <- file.path(Sys.getenv("USERPROFILE"), "Desktop", "dias_maduracion_modulo_variedad.rds")
ruta_maduracion_local     <- "dias_maduracion_modulo_variedad.rds"
ruta_maduracion_nube      <- "G:/Unidades compartidas/Compartido Proyecciones/3.- Arándano/dias_maduracion_modulo_variedad.rds"

id_input_maduracion <- function(modulo, variedad) {
  mod_clean <- gsub("[^A-Za-z0-9]", "_", trimws(as.character(modulo)))
  var_clean <- gsub("[^A-Za-z0-9]", "_", trimws(as.character(variedad)))
  paste0("dias_mad_", mod_clean, "_", var_clean)
}

# Algoritmo de cálculo de días de maduración a partir de curvas fenológicas de campo
calcular_dias_maduracion_curvas <- function(campana_target = 2026) {
  rutas <- c(
    file.path(ruta_raiz_arandano, "Data Histórica", "Maduración", "Curvas de Maduracion Campaigns.csv"),
    "G:/Unidades compartidas/Compartido Proyecciones/3.- Arándano/Data Histórica/Maduración/Curvas de Maduracion Campaigns.csv",
    "D:/ACP/Actualizacion Semanal/Curvas de Maduracion Campaigns.csv",
    "D:/ACP/Actualizacion data maestra/Curvas de Maduracion Campaigns.csv",
    "D:/ACP/Rutas de Cosecha/Curvas de Maduracion Campaigns.csv"
  )
  ruta_valida <- NULL
  for (r in rutas) {
    if (file.exists(r)) {
      ruta_valida <- r
      break
    }
  }
  if (is.null(ruta_valida)) {
    return(tibble(Módulo = character(), Variedad = character(), Dias_Maduracion = integer()))
  }
  
  df_raw <- suppressMessages(readr::read_delim(ruta_valida, delim = ";", locale = readr::locale(encoding = "ISO-8859-1"), show_col_types = FALSE))
  if (nrow(df_raw) == 0) return(tibble(Módulo = character(), Variedad = character(), Dias_Maduracion = integer()))
  
  df_raw$state_norm <- toupper(trimws(stringi::stri_trans_general(df_raw$state, "Latin-ASCII")))
  fases_obligatorias <- c("FLOR", "PEQUENA", "VERDE", "INICIO FASE 1", "INICIO FASE 2", "CREMA", "MADURA")
  
  norm_mod_local <- function(m) {
    m_str <- as.character(m)
    m_num <- suppressWarnings(as.numeric(m_str))
    if (!is.na(m_num)) {
      if (m_num == round(m_num)) as.character(as.integer(m_num)) else sprintf("%.1f", m_num)
    } else {
      m_str
    }
  }
  
  camp_num <- suppressWarnings(as.numeric(campana_target))
  df_camp <- df_raw %>%
    filter(
      Campaign == campana_target |
        as.character(Campaign) == as.character(campana_target) |
        (!is.na(camp_num) & Campaign == camp_num)
    )
  if (nrow(df_camp) == 0) {
    df_camp <- df_raw %>% filter(Campaign == max(Campaign, na.rm = TRUE))
  }
  
  df_camp <- df_camp %>%
    mutate(
      Modulo_Norm = sapply(module, norm_mod_local),
      Variedad_Norm = toupper(trimws(as.character(variant)))
    )
  
  promos <- df_camp %>%
    group_by(Modulo_Norm, Variedad_Norm, study_group, trial_start_week) %>%
    summarise(
      fases = list(unique(state_norm)),
      tiene_obligatorias = all(fases_obligatorias %in% unlist(fases)),
      dias_totales = sum(sojourn_days, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(Modulo_Norm, Variedad_Norm, trial_start_week)
  
  combos <- promos %>% distinct(Modulo_Norm, Variedad_Norm)
  res_list <- lapply(seq_len(nrow(combos)), function(i) {
    m <- combos$Modulo_Norm[i]
    v <- combos$Variedad_Norm[i]
    
    df_sub <- promos %>%
      filter(Modulo_Norm == m, Variedad_Norm == v, tiene_obligatorias) %>%
      arrange(desc(trial_start_week))
    
    if (nrow(df_sub) == 0) return(NULL)
    
    sem_ult <- df_sub$trial_start_week[1]
    dias_ult <- df_sub$dias_totales[1]
    
    # Semana contigua anterior inmediata (separada por 2 semanas: sem_ult - 2)
    sem_ant_esp <- sem_ult - 2
    fila_ant <- df_sub %>% filter(trial_start_week == sem_ant_esp)
    
    if (nrow(fila_ant) > 0) {
      dias_ant <- fila_ant$dias_totales[1]
      dias_final <- as.integer(round(mean(c(dias_ult, dias_ant))))
      tibble(
        Módulo = m,
        Variedad = v,
        Dias_Maduracion = dias_final,
        Semana_Ultima = sem_ult,
        Semana_Penultima = sem_ant_esp,
        Metodo = "Promedio 2 semanas contiguas"
      )
    } else {
      dias_final <- as.integer(round(dias_ult))
      tibble(
        Módulo = m,
        Variedad = v,
        Dias_Maduracion = dias_final,
        Semana_Ultima = sem_ult,
        Semana_Penultima = NA_real_,
        Metodo = "Solo última semana completa"
      )
    }
  })
  
  bind_rows(res_list)
}

cargar_maduracion_disco <- function(combos_default = NULL) {
  df <- NULL
  rutas_candidatas <- c(
    "D:/ACP/Actualizacion Semanal/dias_maduracion_modulo_variedad.rds",
    ruta_maduracion_nube,
    ruta_maduracion_principal,
    ruta_maduracion_local
  )
  for (rc in rutas_candidatas) {
    if (file.exists(rc)) {
      df <- tryCatch(readRDS(rc), error = function(e) NULL)
      if (!is.null(df) && is.data.frame(df) && nrow(df) > 0) break
    }
  }
  
  combos_base <- if (!is.null(combos_default) && is.data.frame(combos_default) && nrow(combos_default) > 0) {
    combos_default %>%
      distinct(Módulo = trimws(as.character(Módulo)), Variedad = trimws(as.character(Variedad)))
  } else if (exists("df_valvulas") && is.data.frame(df_valvulas) && all(c("Módulo", "Variedad") %in% names(df_valvulas))) {
    df_valvulas %>%
      filter(!is.na(Módulo), nzchar(trimws(as.character(Módulo))),
             !is.na(Variedad), nzchar(trimws(as.character(Variedad)))) %>%
      distinct(Módulo = trimws(as.character(Módulo)), Variedad = trimws(as.character(Variedad)))
  } else {
    tibble(Módulo = character(), Variedad = character())
  }
  
  if (nrow(combos_base) == 0) {
    combos_base <- tibble(
      Módulo   = c("1", "2", "3", "4", "5", "6", "7", "7", "8", "9", "10", "11", "12", "13", "14"),
      Variedad = c("SEKOYA POP", "SEKOYA POP", "SEKOYA POP", "MAGICA", "MAGICA", "MAGICA", "BILOXI", "VENTURA", "BILOXI", "IMPERIAL", "ROCIO", "SEKOYA POP", "ROCIO", "ROCIO", "ROCIO")
    )
  }
  
  df_def <- combos_base %>%
    arrange(suppressWarnings(as.numeric(Módulo)), Módulo, Variedad) %>%
    mutate(Dias_Maduracion = 45L)
  
  # Integrar valores calculados de curvas de maduración si están disponibles
  ruta_calc_rds <- "D:/ACP/Actualizacion Semanal/dias_maduracion_calculados.rds"
  if (file.exists(ruta_calc_rds)) {
    df_c_mad <- tryCatch(readRDS(ruta_calc_rds), error = function(e) NULL)
    if (!is.null(df_c_mad) && nrow(df_c_mad) > 0) {
      df_def <- df_def %>%
        left_join(df_c_mad %>% select(Módulo, Variedad, Dias_Calc = Dias_Maduracion), by = c("Módulo", "Variedad")) %>%
        mutate(Dias_Maduracion = coalesce(Dias_Calc, Dias_Maduracion, 45L)) %>%
        select(Módulo, Variedad, Dias_Maduracion)
    }
  }
  
  if (!is.null(df) && is.data.frame(df) && all(c("Módulo", "Variedad") %in% names(df))) {
    df_clean <- df %>%
      mutate(
        Módulo          = trimws(as.character(Módulo)),
        Variedad        = trimws(as.character(Variedad)),
        Dias_Maduracion = suppressWarnings(as.integer(Dias_Maduracion))
      ) %>%
      filter(!is.na(Módulo), nzchar(Módulo), !is.na(Variedad), nzchar(Variedad), !is.na(Dias_Maduracion), Dias_Maduracion >= 0L) %>%
      distinct(Módulo, Variedad, .keep_all = TRUE)
    
    df_def <- df_def %>%
      left_join(df_clean, by = c("Módulo", "Variedad"), suffix = c("_def", "")) %>%
      mutate(Dias_Maduracion = coalesce(Dias_Maduracion, Dias_Maduracion_def, 45L)) %>%
      select(Módulo, Variedad, Dias_Maduracion)
    
    combos_def_keys <- paste0(df_def$Módulo, "___", df_def$Variedad)
    combos_clean_keys <- paste0(df_clean$Módulo, "___", df_clean$Variedad)
    extras <- which(!combos_clean_keys %in% combos_def_keys)
    if (length(extras) > 0) {
      df_def <- bind_rows(df_def, df_clean[extras, c("Módulo", "Variedad", "Dias_Maduracion")])
    }
  }
  return(df_def)
}

guardar_maduracion_disco <- function(df_mad) {
  if (is.null(df_mad) || !is.data.frame(df_mad)) return(FALSE)
  tryCatch(saveRDS(df_mad, "D:/ACP/Actualizacion Semanal/dias_maduracion_modulo_variedad.rds"), error = function(e) NULL)
  tryCatch(saveRDS(df_mad, ruta_maduracion_local), error = function(e) NULL)
  if (dir.exists(dirname(ruta_maduracion_principal))) {
    tryCatch(saveRDS(df_mad, ruta_maduracion_principal), error = function(e) NULL)
  }
  if (dir.exists(dirname(ruta_maduracion_nube))) {
    tryCatch(saveRDS(df_mad, ruta_maduracion_nube), error = function(e) NULL)
  }
  return(TRUE)
}

ruta_sensibilidad_floral <- "D:/ACP/Actualizacion Semanal/sensibilidad_maduracion_floral.rds"

cargar_sensibilidad_floral <- function() {
  rutas_candidatas <- c(
    ruta_sensibilidad_floral,
    file.path(ruta_raiz_arandano, "sensibilidad_maduracion_floral.rds"),
    "sensibilidad_maduracion_floral.rds"
  )
  for (rc in rutas_candidatas) {
    if (file.exists(rc)) {
      val <- tryCatch(readRDS(rc), error = function(e) NULL)
      if (!is.null(val) && is.numeric(val) && !is.na(val) && val >= 0 && val <= 100) {
        return(as.numeric(val))
      }
    }
  }
  50 # 50% por defecto (recomendado para amortiguar variación biológica)
}

guardar_sensibilidad_floral <- function(val) {
  if (!is.numeric(val) || is.na(val)) return(FALSE)
  tryCatch(saveRDS(val, ruta_sensibilidad_floral), error = function(e) NULL)
  tryCatch(saveRDS(val, "sensibilidad_maduracion_floral.rds"), error = function(e) NULL)
  if (dir.exists(ruta_raiz_arandano)) {
    tryCatch(saveRDS(val, file.path(ruta_raiz_arandano, "sensibilidad_maduracion_floral.rds")), error = function(e) NULL)
  }
  return(TRUE)
}

# ==============================================================================
# MOTOR DE FRECUENCIA AUTOMÁTICA POR VÁLVULA (HISTÓRICO 2 SEMANAS: S-2 vs S-1)
# ==============================================================================
calcular_frecuencias_valvula_2semanas <- function(df_cosecha, campana_sel, sem_actual) {
  if (is.null(df_cosecha) || !is.data.frame(df_cosecha) || nrow(df_cosecha) == 0) return(tibble())
  
  sem_act <- as.numeric(sem_actual)
  if (is.na(sem_act) || sem_act <= 0) sem_act <- lubridate::isoweek(Sys.Date())
  
  ord_sel <- orden_semana_campana(sem_act)
  camp_chr <- as.character(campana_sel)
  camp_num <- suppressWarnings(as.numeric(camp_chr))
  
  df_c_camp <- df_cosecha %>%
    filter(
      as.character(Campaña) == camp_chr | (!is.na(camp_num) & Campaña == camp_num),
      coalesce(Tipo_Cosecha, "Comercial") == "Comercial",
      `Kg Total` > 0
    )
  
  if (nrow(df_c_camp) == 0) return(tibble())
  
  sems_disponibles <- sort(unique(suppressWarnings(as.numeric(df_c_camp$Semana))))
  ords_disponibles <- orden_semana_campana(sems_disponibles)
  
  ords_prev <- sort(ords_disponibles[ords_disponibles < ord_sel], decreasing = TRUE)
  if (length(ords_prev) == 0) return(tibble())
  
  ord_s1 <- ords_prev[1]
  ord_s2 <- if (length(ords_prev) >= 2) ords_prev[2] else NA_real_
  
  sem_s1 <- sems_disponibles[match(ord_s1, ords_disponibles)]
  sem_s2 <- if (!is.na(ord_s2)) sems_disponibles[match(ord_s2, ords_disponibles)] else NA_real_
  
  dias_nombres <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Domingo")
  
  df_sub <- df_c_camp %>%
    filter(Semana %in% c(sem_s1, sem_s2)) %>%
    mutate(
      Fecha_D     = as.Date(Fecha),
      Turno_num   = suppressWarnings(as.numeric(Turno)),
      Módulo_c    = case_when(
        Módulo %in% c("13.1", "13.2") ~ "13",
        Módulo %in% c("14.1", "14.2") ~ "14",
        Módulo == "9" & between(Turno_num, 1, 11)  ~ "9.2",
        Módulo == "9" & Turno_num >= 12            ~ "9.1",
        Módulo == "11" & between(Turno_num, 1, 12) ~ "11.1",
        Módulo == "11" & Turno_num >= 13           ~ "11.2",
        TRUE                                       ~ trimws(as.character(Módulo))
      ),
      codigo_norm = norm_codigo_valvula(paste(Módulo_c, Turno, Válvula, sep = "-")),
      Semana_num  = suppressWarnings(as.numeric(Semana)),
      Variedad_c  = limpiar_variedad(Variedad)
    )
  
  if (nrow(df_sub) == 0) return(tibble())
  
  v_s2 <- if (!is.na(sem_s2)) {
    df_sub %>%
      filter(Semana_num == sem_s2) %>%
      group_by(codigo_norm) %>%
      summarise(Fecha_S2 = max(Fecha_D), .groups = "drop")
  } else {
    tibble(codigo_norm = character(), Fecha_S2 = as.Date(character()))
  }
  
  v_s1 <- df_sub %>%
    filter(Semana_num == sem_s1) %>%
    group_by(codigo_norm, Variedad = Variedad_c) %>%
    summarise(
      Fecha_S1   = max(Fecha_D),
      Dia_Nom_S1 = dias_nombres[lubridate::wday(max(Fecha_D), week_start = 1)],
      .groups    = "drop"
    )
  
  # Frecuencia mediana por variedad (fallback si la válvula no estuvo en S-2)
  frec_var_fallback <- v_s1 %>%
    left_join(v_s2, by = "codigo_norm") %>%
    filter(!is.na(Fecha_S2)) %>%
    mutate(diff_d = as.numeric(Fecha_S1 - Fecha_S2)) %>%
    filter(diff_d >= 2, diff_d <= 18) %>%
    group_by(Variedad) %>%
    summarise(
      Frec_Mediana_Var = round(median(diff_d)),
      .groups = "drop"
    )
  
  res <- v_s1 %>%
    left_join(v_s2, by = "codigo_norm") %>%
    left_join(frec_var_fallback, by = "Variedad") %>%
    mutate(
      Frec_Mediana_Var   = coalesce(Frec_Mediana_Var, ifelse(Variedad == "ROCIO", 4, 7)),
      Intervalo_Real     = as.numeric(Fecha_S1 - Fecha_S2),
      Tiene_Intervalo_2S = !is.na(Intervalo_Real) & Intervalo_Real >= 2 & Intervalo_Real <= 18,
      Dias_Frecuencia    = ifelse(Tiene_Intervalo_2S, as.integer(round(Intervalo_Real)), as.integer(Frec_Mediana_Var)),
      # Fecha proyectada para la semana actual según frecuencia real
      Fecha_Proyectada_S = Fecha_S1 + Dias_Frecuencia,
      Wday_Proy          = lubridate::wday(Fecha_Proyectada_S, week_start = 1),
      Dia_Proyectado_Nom = ifelse(Wday_Proy <= 7, dias_nombres[Wday_Proy], "Sábado"),
      Metodo_Frec        = ifelse(Tiene_Intervalo_2S, "2 Semanas Histórico (Válvula)", "Mediana Variedad Reciente")
    )
  
  res %>% select(codigo_norm, Variedad, Fecha_S1, Fecha_S2, Dias_Frecuencia, Dia_Proyectado_Nom, Metodo_Frec, Tiene_Intervalo_2S)
}

# ==============================================================================
# MOTOR DE TENDENCIA FLORAL SEGÚN MADURACIÓN (PREMIO / CASTIGO)
# ==============================================================================
calcular_factor_tendencia_floral <- function(df_conteo, campana_sel, sem_cosecha, df_maduracion, sensibilidad_pct = 50) {
  if (is.null(df_conteo) || !is.data.frame(df_conteo) || nrow(df_conteo) == 0) return(tibble())
  
  sens <- as.numeric(sensibilidad_pct) / 100
  if (is.na(sens) || sens < 0) sens <- 0.50
  
  camp_chr <- as.character(campana_sel)
  camp_num <- suppressWarnings(as.numeric(camp_chr))
  
  df_f <- df_conteo %>%
    filter(toupper(trimws(coalesce(Fundo, "ACP"))) == "ACP") %>%
    filter(
      as.character(Campaña) == camp_chr |
        as.character(Campaña_Original) == camp_chr |
        (!is.na(camp_num) & Campaña == camp_num)
    ) %>%
    mutate(
      Modulo_chr = trimws(as.character(Módulo)),
      Turno_chr  = trimws(as.character(Turno)),
      Var_clean  = toupper(trimws(as.character(Variedad))),
      Sem_num    = suppressWarnings(as.numeric(Semana))
    ) %>%
    filter(!is.na(Sem_num))
  
  if (nrow(df_f) == 0) return(tibble())
  
  # Resumen a nivel Módulo, Turno, Variedad, Semana
  df_sum_turno <- df_f %>%
    group_by(Modulo_chr, Turno_chr, Var_clean, Sem_num) %>%
    summarise(
      Total_Organos = sum(Boton, na.rm = TRUE) + sum(Flor, na.rm = TRUE),
      Muestra_Total = sum(MuestraF, na.rm = TRUE),
      Carga_Planta  = ifelse(Muestra_Total > 0, Total_Organos / Muestra_Total, mean(Total_Floral_Planta, na.rm = TRUE)),
      .groups = "drop"
    )
  
  # Resumen a nivel Módulo, Variedad, Semana (Fallback)
  df_sum_modulo <- df_f %>%
    group_by(Modulo_chr, Var_clean, Sem_num) %>%
    summarise(
      Total_Organos = sum(Boton, na.rm = TRUE) + sum(Flor, na.rm = TRUE),
      Muestra_Total = sum(MuestraF, na.rm = TRUE),
      Carga_Planta  = ifelse(Muestra_Total > 0, Total_Organos / Muestra_Total, mean(Total_Floral_Planta, na.rm = TRUE)),
      .groups = "drop"
    )
  
  combos_mtv <- df_f %>%
    distinct(Modulo_chr, Turno_chr, Var_clean)
  
  df_mad_clean <- if (!is.null(df_maduracion) && is.data.frame(df_maduracion) && nrow(df_maduracion) > 0) {
    df_maduracion %>%
      mutate(
        Modulo_chr = trimws(as.character(Módulo)),
        Var_clean  = toupper(trimws(as.character(Variedad))),
        Dias_Mad   = suppressWarnings(as.numeric(Dias_Maduracion))
      ) %>%
      filter(!is.na(Dias_Mad), Dias_Mad > 0) %>%
      distinct(Modulo_chr, Var_clean, .keep_all = TRUE)
  } else {
    tibble(Modulo_chr = character(), Var_clean = character(), Dias_Mad = numeric())
  }
  
  sem_cos <- as.numeric(sem_cosecha)
  if (is.na(sem_cos) || sem_cos <= 0) sem_cos <- lubridate::isoweek(Sys.Date())
  
  res <- combos_mtv %>%
    left_join(df_mad_clean %>% select(Modulo_chr, Var_clean, Dias_Mad), by = c("Modulo_chr", "Var_clean")) %>%
    mutate(
      Dias_Mad     = coalesce(Dias_Mad, 45),
      Sem_Flor     = sem_cos - round(Dias_Mad / 7),
      Sem_Flor_Ant = Sem_Flor - 1
    )
  
  res <- res %>%
    left_join(
      df_sum_turno %>% select(Modulo_chr, Turno_chr, Var_clean, Sem_Flor = Sem_num, Carga_Flor_T = Carga_Planta),
      by = c("Modulo_chr", "Turno_chr", "Var_clean", "Sem_Flor")
    ) %>%
    left_join(
      df_sum_turno %>% select(Modulo_chr, Turno_chr, Var_clean, Sem_Flor_Ant = Sem_num, Carga_Flor_Ant_T = Carga_Planta),
      by = c("Modulo_chr", "Turno_chr", "Var_clean", "Sem_Flor_Ant")
    ) %>%
    left_join(
      df_sum_modulo %>% select(Modulo_chr, Var_clean, Sem_Flor = Sem_num, Carga_Flor_M = Carga_Planta),
      by = c("Modulo_chr", "Var_clean", "Sem_Flor")
    ) %>%
    left_join(
      df_sum_modulo %>% select(Modulo_chr, Var_clean, Sem_Flor_Ant = Sem_num, Carga_Flor_Ant_M = Carga_Planta),
      by = c("Modulo_chr", "Var_clean", "Sem_Flor_Ant")
    ) %>%
    mutate(
      Nivel_Dato = case_when(
        !is.na(Carga_Flor_T) & !is.na(Carga_Flor_Ant_T) ~ "Turno",
        !is.na(Carga_Flor_M) & !is.na(Carga_Flor_Ant_M) ~ "Módulo",
        TRUE ~ "Sin Dato"
      ),
      Carga_Origen = case_when(
        Nivel_Dato == "Turno"  ~ Carga_Flor_T,
        Nivel_Dato == "Módulo" ~ Carga_Flor_M,
        TRUE                   ~ NA_real_
      ),
      Carga_Anterior = case_when(
        Nivel_Dato == "Turno"  ~ Carga_Flor_Ant_T,
        Nivel_Dato == "Módulo" ~ Carga_Flor_Ant_M,
        TRUE                   ~ NA_real_
      ),
      Delta_Floral_Pct = case_when(
        !is.na(Carga_Origen) & !is.na(Carga_Anterior) & Carga_Anterior > 0 ~ (Carga_Origen - Carga_Anterior) / Carga_Anterior,
        TRUE ~ 0
      ),
      Ajuste_Floral_Pct = pmin(0.40, pmax(-0.40, Delta_Floral_Pct * sens)),
      Factor_Floral     = round(1 + Ajuste_Floral_Pct, 4),
      Tipo_Efecto_Floral = case_when(
        Delta_Floral_Pct > 0.005  ~ "Premio",
        Delta_Floral_Pct < -0.005 ~ "Castigo",
        TRUE                      ~ "Neutro"
      ),
      Módulo   = Modulo_chr,
      Turno    = Turno_chr,
      Variedad = Var_clean
    ) %>%
    select(
      Módulo, Turno, Variedad, Dias_Maduracion = Dias_Mad,
      Sem_Flor, Sem_Flor_Ant, Nivel_Dato,
      Carga_Origen, Carga_Anterior,
      Delta_Floral_Pct, Ajuste_Floral_Pct, Factor_Floral, Tipo_Efecto_Floral
    )
  
  res
}

# ID de Google Drive para datos_maestros_cosecha_2026.RData (para shinyapps.io / entornos sin unidad G:)
id_drive_data_maestra <- "1W2OXKfCIXtuOesplFXp4nzLrPpTvkeRc"

descargar_data_maestra_drive <- function(id_archivo, destino = "datos_maestros_cosecha_2026.RData") {
  if (!nzchar(id_archivo)) return(FALSE)
  id_limpio <- sub(".*[?&]id=([^&]+).*", "\\1", id_archivo)
  id_limpio <- sub(".*/d/([^/]+).*", "\\1", id_limpio)
  url_drive <- sprintf("https://drive.usercontent.google.com/download?id=%s&export=download&confirm=t&nocache=%d",
                       id_limpio, as.integer(Sys.time()))
  
  # Intento 1: httr::GET (maneja redirects y headers de Google Drive)
  tryCatch({
    cat("[GOOGLE DRIVE] Descargando datos_maestros_cosecha_2026.RData desde Google Drive (httr)...\n")
    res <- httr::GET(
      url_drive,
      httr::write_disk(destino, overwrite = TRUE),
      httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"),
      httr::timeout(90)
    )
    if (httr::status_code(res) == 200 && file.exists(destino) && file.size(destino) > 50000) {
      cat(sprintf("[GOOGLE DRIVE] Descarga completada con éxito vía httr (%.2f MB)\n", file.size(destino) / (1024 * 1024)))
      return(TRUE)
    }
  }, error = function(e) {
    cat("[AVISO GOOGLE DRIVE] Error en descarga httr:", e$message, "\n")
  })
  
  # Intento 2: download.file (fallback)
  tryCatch({
    cat("[GOOGLE DRIVE] Intentando descarga con download.file...\n")
    metodo <- if (capabilities("libcurl")) "libcurl" else "auto"
    download.file(url_drive, destfile = destino, mode = "wb", method = metodo, quiet = TRUE)
    if (file.exists(destino) && file.size(destino) > 50000) {
      cat(sprintf("[GOOGLE DRIVE] Descarga completada con éxito vía download.file (%.2f MB)\n", file.size(destino) / (1024 * 1024)))
      return(TRUE)
    }
    return(FALSE)
  }, error = function(e) {
    cat("[AVISO GOOGLE DRIVE] Error en download.file:", e$message, "\n")
    return(FALSE)
  })
}

# CONEXIÓN EXCLUSIVA A GOOGLE DRIVE:
# 1. En local con Google Drive para escritorio: Lee directo de Unidad G:
# 2. En la nube (shinyapps.io) o máquinas sin G:: Descarga directamente de Google Drive
ruta_rdata_drive_g <- file.path(ruta_raiz_arandano, "datos_maestros_cosecha_2026.RData")
origen_rdata_actual <- "DESCONOCIDO"
ruta_rdata_cargada <- ""
ultima_descarga_nube_ts <- as.POSIXct(NA)

if (file.exists(ruta_rdata_drive_g)) {
  ruta_rdata_cargada  <- ruta_rdata_drive_g
  origen_rdata_actual <- "DRIVE_LOCAL_G"
} else if (nzchar(id_drive_data_maestra)) {
  cat("[GOOGLE DRIVE] Unidad G: no detectada. Conectando y descargando datos maestros directamente desde Google Drive...\n")
  exito_dl <- descargar_data_maestra_drive(id_drive_data_maestra, "datos_maestros_cosecha_2026.RData")
  if (exito_dl) {
    ruta_rdata_cargada     <- "datos_maestros_cosecha_2026.RData"
    origen_rdata_actual    <- "NUBE_DRIVE"
    ultima_descarga_nube_ts <- Sys.time()
  } else if (file.exists("datos_maestros_cosecha_2026.RData")) {
    ruta_rdata_cargada  <- "datos_maestros_cosecha_2026.RData"
    origen_rdata_actual <- "EMPAQUETADO_NUBE"
  }
}

if (nzchar(ruta_rdata_cargada) && file.exists(ruta_rdata_cargada)) {
  load(ruta_rdata_cargada)
  cat("[DATA] Cargado exitosamente desde Google Drive:", ruta_rdata_cargada, "| Origen:", origen_rdata_actual, "\n")
  
  # Carga segura de base histórica de conteo floral si no vino en el RData
  if (!exists("df_conteo_historico") || !is.data.frame(df_conteo_historico) || nrow(df_conteo_historico) == 0) {
    rutas_candidatas_conteo <- c(
      file.path(ruta_raiz_arandano, "historico_conteo.rds"),
      "D:/ACP/Actualizacion Semanal/historico_conteo.rds",
      "historico_conteo.rds"
    )
    for (rc in rutas_candidatas_conteo) {
      if (file.exists(rc)) {
        df_conteo_historico <- tryCatch(readRDS(rc), error = function(e) NULL)
        if (!is.null(df_conteo_historico) && nrow(df_conteo_historico) > 0) {
          cat("[DATA] Conteo histórico cargado desde RDS:", rc, "| Filas:", nrow(df_conteo_historico), "\n")
          break
        }
      }
    }
  }
  if (!exists("df_conteo_historico") || !is.data.frame(df_conteo_historico)) {
    df_conteo_historico <- tibble()
  }
  
  # ==============================================================================
  # CATÁLOGO Y MOTOR DE DATOS PARA MAPA 2D DE COSECHA OFICIAL
  # ==============================================================================
  if (exists("df_valvulas") && is.data.frame(df_valvulas)) {
    df_valvulas <- df_valvulas %>%
      mutate(
        Turno_num = suppressWarnings(as.numeric(Turno)),
        Sector = case_when(
          as.character(Módulo) %in% c("9", "9.1", "9.2", "11", "11.1", "11.2", "12") ~ "9",
          TRUE ~ as.character(Sector)
        ),
        Módulo = case_when(
          as.character(Módulo) %in% c("13.1", "13.2") ~ "13",
          as.character(Módulo) %in% c("14.1", "14.2") ~ "14",
          as.character(Módulo) == "9" & between(Turno_num, 1, 11)  ~ "9.2",
          as.character(Módulo) == "9" & Turno_num >= 12            ~ "9.1",
          as.character(Módulo) == "11" & between(Turno_num, 1, 12) ~ "11.1",
          as.character(Módulo) == "11" & Turno_num >= 13           ~ "11.2",
          TRUE ~ as.character(Módulo)
        )
      ) %>%
      select(-any_of("Turno_num"))
    if (!"Grupo" %in% names(df_valvulas)) {
      df_valvulas <- df_valvulas %>% mutate(Grupo = asignar_grupo_modulo(Módulo))
    }
    if (!"Módulo_Sub" %in% names(df_valvulas)) {
      df_valvulas <- df_valvulas %>%
        mutate(
          Turno_num_aux = suppressWarnings(as.numeric(Turno)),
          Módulo_Sub = case_when(
            as.character(Módulo) == "9" & between(Turno_num_aux, 1, 11)  ~ "9.2",
            as.character(Módulo) == "9" & Turno_num_aux >= 12            ~ "9.1",
            as.character(Módulo) == "11" & between(Turno_num_aux, 1, 12) ~ "11.1",
            as.character(Módulo) == "11" & Turno_num_aux >= 13           ~ "11.2",
            TRUE                                                         ~ as.character(Módulo)
          )
        ) %>%
        select(-Turno_num_aux)
    }
    df_valvulas_mapa <- df_valvulas %>%
      mutate(
        codigo_norm   = norm_codigo_valvula(codigo),
        variedad_norm = limpiar_variedad(Variedad)
      )
    json_valvulas_mapa <- jsonlite::toJSON(df_valvulas_mapa, dataframe = "rows", auto_unbox = TRUE)
  } else {
    df_valvulas_mapa <- tibble()
    json_valvulas_mapa <- "[]"
  }
  
  # Carga segura del motor JavaScript Canvas (compatible con local y shinyapps.io)
  ruta_js_mapa <- if (file.exists("mapa_lectura.js")) {
    "mapa_lectura.js"
  } else if (file.exists(file.path("Actualizacion Semanal", "mapa_lectura.js"))) {
    file.path("Actualizacion Semanal", "mapa_lectura.js")
  } else {
    file.path(getwd(), "mapa_lectura.js")
  }
  codigo_js_mapa_archivo <- if (file.exists(ruta_js_mapa)) {
    paste(readLines(ruta_js_mapa, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  } else {
    ""
  }
  
  # ==============================================================================
  # CONEXIÓN EN TIEMPO REAL CON GOOGLE SHEETS PARA RUTAS DE COSECHA CAMPO
  # ==============================================================================
  url_webhook_sheets <- "https://script.google.com/macros/s/AKfycby6wa7fNcT-ofGLceojaX_HraGwZrJj4GzLyEZHTg-hQ8OrfzMjf-kUOHYz8PLfXKdubA/exec"
  cache_rutas_campo  <- new.env(parent = emptyenv())
  
  obtener_rutas_campo_en_vivo <- function(sem_num = NULL) {
    if (is.null(sem_num) || is.na(sem_num)) return(NULL)
    sem_key <- as.character(as.numeric(sem_num))
    
    if (exists(sem_key, envir = cache_rutas_campo, inherits = FALSE)) {
      return(get(sem_key, envir = cache_rutas_campo))
    }
    
    url <- sprintf("%s?campana=2026&semana=%s", url_webhook_sheets, sem_key)
    
    df_remoto <- tryCatch({
      res <- httr::GET(url, httr::timeout(15))
      if (httr::status_code(res) != 200) return(NULL)
      txt <- httr::content(res, as = "text", encoding = "UTF-8")
      if (!nzchar(txt) || txt == "[]" || txt == "null") return(NULL)
      raw_df <- jsonlite::fromJSON(txt)
      if (!is.data.frame(raw_df) || nrow(raw_df) == 0) return(NULL)
      
      nms <- names(raw_df)
      get_col_raw <- function(pattern) {
        c_match <- nms[grepl(pattern, nms, ignore.case = TRUE)][1]
        if (!is.na(c_match)) raw_df[[c_match]] else NULL
      }
      get_num <- function(pattern, def = 0) {
        val <- get_col_raw(pattern)
        if (is.null(val)) rep(def, nrow(raw_df)) else {
          res <- suppressWarnings(as.numeric(val))
          res[is.na(res)] <- def
          res
        }
      }
      get_chr <- function(pattern, def = "") {
        val <- get_col_raw(pattern)
        if (is.null(val)) rep(def, nrow(raw_df)) else {
          res <- trimws(as.character(val))
          res[is.na(res) | res == ""] <- def
          res
        }
      }
      
      df_clean <- tibble(
        Campaña        = get_chr("^campa[nñ]a$", "2026"),
        Semana         = get_num("^semana$", NA_real_),
        Modo_Raw       = get_chr("^modo$", "Proyeccion"),
        Num_Pasada     = get_num("^pasada$", 1),
        Codigo_Valvula = get_chr("^codigo.*valvula$|^codigo$", ""),
        Variedad_Sheet = get_chr("^variedad$", ""),
        Area_Sheet     = round(get_num("^area.*fisica|^area", 0), 2),
        Lunes          = get_num("pct.*lun", 0),
        Martes         = get_num("pct.*mar", 0),
        Miércoles      = get_num("pct.*mie", 0),
        Jueves         = get_num("pct.*jue", 0),
        Viernes        = get_num("pct.*vie", 0),
        Sábado         = get_num("pct.*sab", 0),
        Kg_Lunes       = get_num("kg.*lun", 0),
        Kg_Martes      = get_num("kg.*mar", 0),
        Kg_Miércoles   = get_num("kg.*mie", 0),
        Kg_Jueves      = get_num("kg.*jue", 0),
        Kg_Viernes     = get_num("kg.*vie", 0),
        Kg_Sábado      = get_num("kg.*sab", 0),
        Cells_Str_Sheet = get_chr("^cells.*str$", "")
      ) %>%
        filter(!is.na(Semana), nzchar(Codigo_Valvula)) %>%
        mutate(
          Tipo_Registro = case_when(
            grepl("rectif", Modo_Raw, ignore.case = TRUE) ~ "Rectificacion",
            grepl("cosecha.*campo|cosecha", Modo_Raw, ignore.case = TRUE) ~ "CosechaCampo",
            grepl("oficial", Modo_Raw, ignore.case = TRUE) ~ "CosechaOficial",
            TRUE ~ "Proyeccion"
          ),
          codigo_norm   = norm_codigo_valvula(Codigo_Valvula),
          variedad_norm = limpiar_variedad(Variedad_Sheet)
        )
      
      if (nrow(df_clean) == 0 || !exists("df_valvulas") || !is.data.frame(df_valvulas)) return(NULL)
      
      df_valv_map <- df_valvulas %>%
        mutate(
          codigo_norm   = norm_codigo_valvula(codigo),
          variedad_norm = limpiar_variedad(Variedad),
          Area_Round    = round(Área, 2)
        ) %>%
        select(id, codigo, Sector, Módulo, Turno, Variedad, Área, Area_Round, codigo_norm, variedad_norm)
      
      codigos_multi <- df_valv_map %>%
        group_by(codigo_norm) %>%
        filter(n() > 1) %>%
        pull(codigo_norm) %>%
        unique()
      
      df_multi_clean <- df_clean %>% filter(codigo_norm %in% codigos_multi)
      df_res_multi <- df_multi_clean %>%
        inner_join(df_valv_map, by = c("codigo_norm", "variedad_norm", "Area_Sheet" = "Area_Round")) %>%
        select(-Area_Sheet)
      
      if (nrow(df_res_multi) < nrow(df_multi_clean)) {
        faltantes <- df_multi_clean %>% filter(!codigo_norm %in% df_res_multi$codigo_norm)
        if (nrow(faltantes) > 0) {
          df_recup <- faltantes %>%
            inner_join(df_valv_map, by = c("codigo_norm", "variedad_norm")) %>%
            select(-Area_Sheet)
          df_res_multi <- bind_rows(df_res_multi, df_recup)
        }
      }
      
      df_res_uni <- df_clean %>%
        filter(!codigo_norm %in% codigos_multi) %>%
        inner_join(df_valv_map %>% distinct(codigo_norm, .keep_all = TRUE) %>% select(-variedad_norm, -Area_Round), by = "codigo_norm") %>%
        select(-any_of(c("Area_Sheet")))
      
      res_df <- bind_rows(df_res_multi, df_res_uni) %>%
        mutate(
          Ha_Lunes            = round(Área * (Lunes / 100), 2),
          Ha_Martes           = round(Área * (Martes / 100), 2),
          Ha_Miércoles        = round(Área * (Miércoles / 100), 2),
          Ha_Jueves           = round(Área * (Jueves / 100), 2),
          Ha_Viernes          = round(Área * (Viernes / 100), 2),
          Ha_Sábado           = round(Área * (Sábado / 100), 2),
          Ha_Total_Prog       = round(Ha_Lunes + Ha_Martes + Ha_Miércoles + Ha_Jueves + Ha_Viernes + Ha_Sábado, 2),
          Kg_Total_Prog       = round(Kg_Lunes + Kg_Martes + Kg_Miércoles + Kg_Jueves + Kg_Viernes + Kg_Sábado, 2),
          cells_str           = coalesce(as.character(Cells_Str_Sheet), ""),
          Fecha_Actualizacion = Sys.time()
        ) %>%
        select(-any_of("Cells_Str_Sheet"))
      res_df
    }, error = function(e) {
      cat("[AVISO GOOGLE SHEETS]", e$message, "\n")
      NULL
    })
    
    if (!is.null(df_remoto) && nrow(df_remoto) > 0) {
      assign(sem_key, df_remoto, envir = cache_rutas_campo)
      
      # Actualizar respaldos en disco de rutas_cosecha_mapa.rds si existen
      tryCatch({
        rutas_loc <- c(
          file.path(ruta_raiz_arandano, "rutas_cosecha_mapa.rds"),
          "D:/ACP/Rutas de Cosecha/rutas_cosecha_mapa.rds"
        )
        for (r_guardar in rutas_loc) {
          if (file.exists(r_guardar)) {
            df_existente <- tryCatch(readRDS(r_guardar), error = function(e) NULL)
            if (!is.null(df_existente) && is.data.frame(df_existente)) {
              df_rem_save <- df_remoto
              if ("cells_str" %in% names(df_existente)) {
                prev_cells <- df_existente %>%
                  filter(Campaña == "2026", Semana == as.numeric(sem_key), !is.na(cells_str), nzchar(cells_str)) %>%
                  select(id, Num_Pasada, Tipo_Registro, cells_str_prev = cells_str) %>%
                  distinct(id, Num_Pasada, Tipo_Registro, .keep_all = TRUE)
                df_rem_save <- df_rem_save %>%
                  left_join(prev_cells, by = c("id", "Num_Pasada", "Tipo_Registro")) %>%
                  mutate(
                    cells_str = case_when(
                      !is.na(cells_str) & nzchar(cells_str) ~ cells_str,
                      !is.na(cells_str_prev) & nzchar(cells_str_prev) ~ cells_str_prev,
                      TRUE ~ coalesce(as.character(cells_str), "")
                    )
                  ) %>%
                  select(-any_of("cells_str_prev"))
              }
              df_act <- df_existente %>%
                filter(!(Campaña == "2026" & Semana == as.numeric(sem_key) & Tipo_Registro %in% c("Proyeccion", "CosechaCampo", "Cosecha", "Rectificacion"))) %>%
                bind_rows(df_rem_save)
              tryCatch(saveRDS(df_act, r_guardar), error = function(e) NULL)
            }
          }
        }
      }, error = function(e) NULL)
      
      return(df_remoto)
    }
    
    # Si falló la consulta en vivo, cargar desde respaldo en disco
    df_disco <- tryCatch(cargar_rutas_cosecha(), error = function(e) NULL)
    if (!is.null(df_disco) && nrow(df_disco) > 0) {
      df_disco_sem <- df_disco %>% filter(as.numeric(Semana) == as.numeric(sem_key))
      if (nrow(df_disco_sem) > 0) return(df_disco_sem)
    }
    
    return(NULL)
  }
  
  # ==============================================================================
  # MOTOR CENTRAL DE CONCILIACIÓN Y RECONCILIACIÓN DE COSECHA (CAMPO VS SAP)
  # Reglas operativas:
  # 1. SAP sin Ruta: excluido de cálculos operativos y pintado de mapa.
  # 2. Desfase de Día: suma kilos contiguos y asigna al día con más kilos si coincide con Campo.
  # 3. Ruta sin SAP: asigna kilos de ruta provisoriamente, manteniéndolo en reporte de incidencias.
  # ==============================================================================
  obtener_cosecha_conciliada_semana <- function(sem_num, campana_sel = "2026", pasada_filtro = "Todas") {
    sem_num <- as.numeric(sem_num)
    campana_sel <- as.character(campana_sel)
    pasada_str <- if (!is.null(pasada_filtro) && nzchar(as.character(pasada_filtro))) as.character(pasada_filtro) else "Todas"
    
    fecha_hoy        <- Sys.Date()
    sem_actual_num   <- lubridate::isoweek(fecha_hoy)
    wday_hoy         <- lubridate::wday(fecha_hoy, week_start = 1)
    es_semana_actual <- (sem_num == sem_actual_num)
    dias_cols        <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
    
    dias_cerrados_nom <- if (es_semana_actual) {
      if (wday_hoy > 1) dias_cols[1:min(wday_hoy - 1, 6)] else character(0)
    } else if (sem_num < sem_actual_num) {
      dias_cols
    } else {
      character(0)
    }
    
    extraer_num_col_segura <- function(df, pref, dia_til, dia_sin) {
      c1 <- paste0(pref, dia_til); c2 <- paste0(pref, dia_sin)
      if (c1 %in% names(df)) suppressWarnings(as.numeric(df[[c1]]))
      else if (c2 %in% names(df)) suppressWarnings(as.numeric(df[[c2]]))
      else rep(0, nrow(df))
    }
    
    norm_col_dia_rec <- function(dia) {
      d_sin <- chartr("áéíóúÁÉÍÓÚ", "aeiouAEIOU", tolower(dia))
      paste0("Kg_", tools::toTitleCase(d_sin), "_rec")
    }
    
    # 1. Rutas de Campo (en vivo desde Google Sheets o fallback en disco)
    rc_raw <- tryCatch(obtener_rutas_campo_en_vivo(sem_num), error = function(e) NULL)
    if (is.null(rc_raw) || !is.data.frame(rc_raw) || nrow(rc_raw) == 0) {
      rc_raw <- tryCatch(cargar_rutas_cosecha(), error = function(e) NULL)
      if (!is.null(rc_raw) && "Semana" %in% names(rc_raw)) {
        rc_raw <- rc_raw %>% filter(as.numeric(Semana) == sem_num)
      }
    }
    
    # Identificar válvulas con Num_Pasada == 2 por cada día de la semana
    valvs_dias_p2 <- list()
    if (!is.null(rc_raw) && is.data.frame(rc_raw) && nrow(rc_raw) > 0) {
      rc_p2 <- rc_raw %>%
        filter(!("Tipo_Registro" %in% names(.)) | Tipo_Registro != "Rectificacion") %>%
        filter(coalesce(suppressWarnings(as.numeric(Num_Pasada)), 1) == 2)
      col_c_p2 <- intersect(c("Codigo_Valvula", "codigo", "Codigo_Valv"), names(rc_p2))[1]
      if (!is.na(col_c_p2) && nrow(rc_p2) > 0) {
        rc_p2 <- rc_p2 %>% mutate(cod_norm_temp = norm_codigo_valvula(.data[[col_c_p2]]))
        for (d in dias_cols) {
          d_sin <- chartr("áéíóúÁÉÍÓÚ", "aeiouAEIOU", tolower(d))
          has_adv <- (if (d %in% names(rc_p2)) coalesce(rc_p2[[d]], 0) > 0 else FALSE) |
                     (if (paste0("Ha_", d) %in% names(rc_p2)) coalesce(rc_p2[[paste0("Ha_", d)]], 0) > 0 else FALSE) |
                     (if (paste0("Ha_", tools::toTitleCase(d_sin)) %in% names(rc_p2)) coalesce(rc_p2[[paste0("Ha_", tools::toTitleCase(d_sin))]], 0) > 0 else FALSE)
          valvs_dias_p2[[d]] <- unique(na.omit(rc_p2$cod_norm_temp[has_adv]))
        }
      }
    }
    for (d in dias_cols) {
      if (is.null(valvs_dias_p2[[d]])) valvs_dias_p2[[d]] <- character(0)
    }
    
    df_campo <- if (!is.null(rc_raw) && is.data.frame(rc_raw) && nrow(rc_raw) > 0) {
      rc_sub <- rc_raw
      if (pasada_str %in% c("1", "2")) {
        rc_sub <- rc_sub %>% filter(coalesce(as.character(Num_Pasada), "1") == pasada_str)
      }
      col_cod <- intersect(c("Codigo_Valvula", "codigo", "Codigo_Valv"), names(rc_sub))[1]
      
      ha_lun <- coalesce(extraer_num_col_segura(rc_sub, "Ha_", "Lunes", "Lunes"), 0)
      ha_mar <- coalesce(extraer_num_col_segura(rc_sub, "Ha_", "Martes", "Martes"), 0)
      ha_mie <- coalesce(extraer_num_col_segura(rc_sub, "Ha_", "Miércoles", "Miercoles"), 0)
      ha_jue <- coalesce(extraer_num_col_segura(rc_sub, "Ha_", "Jueves", "Jueves"), 0)
      ha_vie <- coalesce(extraer_num_col_segura(rc_sub, "Ha_", "Viernes", "Viernes"), 0)
      ha_sab <- coalesce(extraer_num_col_segura(rc_sub, "Ha_", "Sábado", "Sabado"), 0)
      
      pct_lun <- pmin(100, coalesce(extraer_num_col_segura(rc_sub, "", "Lunes", "Lunes"), 0))
      pct_mar <- pmin(100, coalesce(extraer_num_col_segura(rc_sub, "", "Martes", "Martes"), 0))
      pct_mie <- pmin(100, coalesce(extraer_num_col_segura(rc_sub, "", "Miércoles", "Miercoles"), 0))
      pct_jue <- pmin(100, coalesce(extraer_num_col_segura(rc_sub, "", "Jueves", "Jueves"), 0))
      pct_vie <- pmin(100, coalesce(extraer_num_col_segura(rc_sub, "", "Viernes", "Viernes"), 0))
      pct_sab <- pmin(100, coalesce(extraer_num_col_segura(rc_sub, "", "Sábado", "Sabado"), 0))
      
      kg_p_lun <- coalesce(extraer_num_col_segura(rc_sub, "Kg_", "Lunes", "Lunes"), 0)
      kg_p_mar <- coalesce(extraer_num_col_segura(rc_sub, "Kg_", "Martes", "Martes"), 0)
      kg_p_mie <- coalesce(extraer_num_col_segura(rc_sub, "Kg_", "Miércoles", "Miercoles"), 0)
      kg_p_jue <- coalesce(extraer_num_col_segura(rc_sub, "Kg_", "Jueves", "Jueves"), 0)
      kg_p_vie <- coalesce(extraer_num_col_segura(rc_sub, "Kg_", "Viernes", "Viernes"), 0)
      kg_sab <- coalesce(extraer_num_col_segura(rc_sub, "Kg_", "Sábado", "Sabado"), 0)
      
      cells_vec <- if ("cells_str" %in% names(rc_sub)) as.character(rc_sub$cells_str) else rep("", nrow(rc_sub))
      
      tibble(
        codigo_norm       = norm_codigo_valvula(rc_sub[[col_cod]]),
        Pasada_rc         = if ("Num_Pasada" %in% names(rc_sub)) as.character(rc_sub$Num_Pasada) else "1",
        Ha_Lunes_rc       = ha_lun,
        Ha_Martes_rc      = ha_mar,
        Ha_Miercoles_rc   = ha_mie,
        Ha_Jueves_rc      = ha_jue,
        Ha_Viernes_rc     = ha_vie,
        Ha_Sabado_rc      = ha_sab,
        Ha_Total_rc       = ha_lun + ha_mar + ha_mie + ha_jue + ha_vie + ha_sab,
        Pct_Lunes_rc      = pct_lun,
        Pct_Martes_rc     = pct_mar,
        Pct_Miercoles_rc  = pct_mie,
        Pct_Jueves_rc     = pct_jue,
        Pct_Viernes_rc    = pct_vie,
        Pct_Sabado_rc     = pct_sab,
        cells_str_rc      = cells_vec,
        Kg_Prog_Lunes     = kg_p_lun,
        Kg_Prog_Martes    = kg_p_mar,
        Kg_Prog_Miercoles = kg_p_mie,
        Kg_Prog_Jueves    = kg_p_jue,
        Kg_Prog_Viernes   = kg_p_vie,
        Kg_Prog_Sabado    = kg_sab,
        Kg_Prog_Total     = kg_p_lun + kg_p_mar + kg_p_mie + kg_p_jue + kg_p_vie + kg_sab
      ) %>%
        filter(!is.na(codigo_norm), nzchar(codigo_norm), Ha_Total_rc > 0) %>%
        group_by(codigo_norm) %>%
        summarise(
          Pasada_rc         = first(Pasada_rc),
          Ha_Lunes_rc       = sum(Ha_Lunes_rc, na.rm = TRUE),
          Ha_Martes_rc      = sum(Ha_Martes_rc, na.rm = TRUE),
          Ha_Miercoles_rc   = sum(Ha_Miercoles_rc, na.rm = TRUE),
          Ha_Jueves_rc      = sum(Ha_Jueves_rc, na.rm = TRUE),
          Ha_Viernes_rc     = sum(Ha_Viernes_rc, na.rm = TRUE),
          Ha_Sabado_rc      = sum(Ha_Sabado_rc, na.rm = TRUE),
          Ha_Total_rc       = sum(Ha_Total_rc, na.rm = TRUE),
          Pct_Lunes_rc      = pmin(100, sum(Pct_Lunes_rc, na.rm = TRUE)),
          Pct_Martes_rc     = pmin(100, sum(Pct_Martes_rc, na.rm = TRUE)),
          Pct_Miercoles_rc  = pmin(100, sum(Pct_Miercoles_rc, na.rm = TRUE)),
          Pct_Jueves_rc     = pmin(100, sum(Pct_Jueves_rc, na.rm = TRUE)),
          Pct_Viernes_rc    = pmin(100, sum(Pct_Viernes_rc, na.rm = TRUE)),
          Pct_Sabado_rc     = pmin(100, sum(Pct_Sabado_rc, na.rm = TRUE)),
          cells_str_rc      = {
            cs <- na.omit(cells_str_rc[nzchar(trimws(cells_str_rc))])
            if (length(cs) > 0) cs[1] else ""
          },
          Kg_Prog_Lunes     = sum(Kg_Prog_Lunes, na.rm = TRUE),
          Kg_Prog_Martes    = sum(Kg_Prog_Martes, na.rm = TRUE),
          Kg_Prog_Miercoles = sum(Kg_Prog_Miercoles, na.rm = TRUE),
          Kg_Prog_Jueves    = sum(Kg_Prog_Jueves, na.rm = TRUE),
          Kg_Prog_Viernes   = sum(Kg_Prog_Viernes, na.rm = TRUE),
          Kg_Prog_Sabado    = sum(Kg_Prog_Sabado, na.rm = TRUE),
          Kg_Prog_Total     = sum(Kg_Prog_Total, na.rm = TRUE),
          .groups           = "drop"
        )
    } else {
      tibble(codigo_norm = character(), Pasada_rc = character(), Ha_Lunes_rc = numeric(), Ha_Martes_rc = numeric(), Ha_Miercoles_rc = numeric(),
             Ha_Jueves_rc = numeric(), Ha_Viernes_rc = numeric(), Ha_Sabado_rc = numeric(), Ha_Total_rc = numeric(),
             Pct_Lunes_rc = numeric(), Pct_Martes_rc = numeric(), Pct_Miercoles_rc = numeric(), Pct_Jueves_rc = numeric(), Pct_Viernes_rc = numeric(), Pct_Sabado_rc = numeric(), cells_str_rc = character(),
             Kg_Prog_Lunes = numeric(), Kg_Prog_Martes = numeric(), Kg_Prog_Miercoles = numeric(), Kg_Prog_Jueves = numeric(), Kg_Prog_Viernes = numeric(), Kg_Prog_Sabado = numeric(), Kg_Prog_Total = numeric())
    }
    
    # 2. SAP oficial (df_hist_oficial_wide)
    df_sap <- if (exists("df_hist_oficial_wide") && is.data.frame(df_hist_oficial_wide) && nrow(df_hist_oficial_wide) > 0) {
      sub_w <- df_hist_oficial_wide %>% filter(as.numeric(Semana) == sem_num)
      if (nrow(sub_w) > 0) {
        kg_lun <- coalesce(extraer_num_col_segura(sub_w, "Kg_", "Lunes", "Lunes"), 0)
        kg_mar <- coalesce(extraer_num_col_segura(sub_w, "Kg_", "Martes", "Martes"), 0)
        kg_mie <- coalesce(extraer_num_col_segura(sub_w, "Kg_", "Miércoles", "Miercoles"), 0)
        kg_jue <- coalesce(extraer_num_col_segura(sub_w, "Kg_", "Jueves", "Jueves"), 0)
        kg_vie <- coalesce(extraer_num_col_segura(sub_w, "Kg_", "Viernes", "Viernes"), 0)
        kg_sab <- coalesce(extraer_num_col_segura(sub_w, "Kg_", "Sábado", "Sabado"), 0)
        
        cod_norms_sap <- norm_codigo_valvula(sub_w$Codigo_Valv)
        
        # Filtrar kilos diarios por pasada usando los días que campo recorrió cada pasada
        if (pasada_str == "2") {
          kg_lun <- ifelse(cod_norms_sap %in% valvs_dias_p2[["Lunes"]], kg_lun, 0)
          kg_mar <- ifelse(cod_norms_sap %in% valvs_dias_p2[["Martes"]], kg_mar, 0)
          kg_mie <- ifelse(cod_norms_sap %in% valvs_dias_p2[["Miércoles"]], kg_mie, 0)
          kg_jue <- ifelse(cod_norms_sap %in% valvs_dias_p2[["Jueves"]], kg_jue, 0)
          kg_vie <- ifelse(cod_norms_sap %in% valvs_dias_p2[["Viernes"]], kg_vie, 0)
          kg_sab <- ifelse(cod_norms_sap %in% valvs_dias_p2[["Sábado"]], kg_sab, 0)
        } else if (pasada_str == "1") {
          kg_lun <- ifelse(cod_norms_sap %in% valvs_dias_p2[["Lunes"]], 0, kg_lun)
          kg_mar <- ifelse(cod_norms_sap %in% valvs_dias_p2[["Martes"]], 0, kg_mar)
          kg_mie <- ifelse(cod_norms_sap %in% valvs_dias_p2[["Miércoles"]], 0, kg_mie)
          kg_jue <- ifelse(cod_norms_sap %in% valvs_dias_p2[["Jueves"]], 0, kg_jue)
          kg_vie <- ifelse(cod_norms_sap %in% valvs_dias_p2[["Viernes"]], 0, kg_vie)
          kg_sab <- ifelse(cod_norms_sap %in% valvs_dias_p2[["Sábado"]], 0, kg_sab)
        }
        
        tibble(
          codigo_norm      = cod_norms_sap,
          Variedad_sap     = as.character(sub_w$Variedad),
          Pasada_sap       = if (pasada_str %in% c("1", "2")) pasada_str else "1",
          Kg_Lunes_sap     = kg_lun,
          Kg_Martes_sap    = kg_mar,
          Kg_Miercoles_sap = kg_mie,
          Kg_Jueves_sap    = kg_jue,
          Kg_Viernes_sap   = kg_vie,
          Kg_Sabado_sap    = kg_sab,
          Kg_Total_sap     = round(kg_lun + kg_mar + kg_mie + kg_jue + kg_vie + kg_sab, 2)
        ) %>%
          filter(!is.na(codigo_norm), nzchar(codigo_norm), Kg_Total_sap > 0) %>%
          group_by(codigo_norm) %>%
          summarise(
            Variedad_sap     = first(na.omit(Variedad_sap)),
            Pasada_sap       = first(Pasada_sap),
            Kg_Lunes_sap     = sum(Kg_Lunes_sap, na.rm = TRUE),
            Kg_Martes_sap    = sum(Kg_Martes_sap, na.rm = TRUE),
            Kg_Miercoles_sap = sum(Kg_Miercoles_sap, na.rm = TRUE),
            Kg_Jueves_sap    = sum(Kg_Jueves_sap, na.rm = TRUE),
            Kg_Viernes_sap   = sum(Kg_Viernes_sap, na.rm = TRUE),
            Kg_Sabado_sap    = sum(Kg_Sabado_sap, na.rm = TRUE),
            Kg_Total_sap     = sum(Kg_Total_sap, na.rm = TRUE),
            .groups          = "drop"
          )
      } else {
        tibble(codigo_norm = character(), Variedad_sap = character(), Pasada_sap = character(),
               Kg_Lunes_sap = numeric(), Kg_Martes_sap = numeric(), Kg_Miercoles_sap = numeric(),
               Kg_Jueves_sap = numeric(), Kg_Viernes_sap = numeric(), Kg_Sabado_sap = numeric(), Kg_Total_sap = numeric())
      }
    } else {
      tibble(codigo_norm = character(), Variedad_sap = character(), Pasada_sap = character(),
             Kg_Lunes_sap = numeric(), Kg_Martes_sap = numeric(), Kg_Miercoles_sap = numeric(),
             Kg_Jueves_sap = numeric(), Kg_Viernes_sap = numeric(), Kg_Sabado_sap = numeric(), Kg_Total_sap = numeric())
    }
    
    todos_codigos <- unique(c(df_campo$codigo_norm, df_sap$codigo_norm))
    if (length(todos_codigos) == 0) return(tibble())
    
    # Cruce preliminar
    df_rec <- tibble(codigo_norm = todos_codigos) %>%
      left_join(df_campo, by = "codigo_norm") %>%
      left_join(df_sap, by = "codigo_norm") %>%
      mutate(
        across(starts_with("Ha_"), ~ coalesce(.x, 0)),
        across(starts_with("Kg_"), ~ coalesce(.x, 0)),
        across(starts_with("Pct_"), ~ coalesce(.x, 0)),
        cells_str_rc = coalesce(cells_str_rc, ""),
        Tiene_Campo  = Ha_Total_rc > 0,
        Tiene_SAP    = Kg_Total_sap > 0,
        Pasada_rc    = coalesce(Pasada_rc, Pasada_sap, "1")
      )
    
    # Integrar catastro para georreferenciación y agrupación
    catastro <- if (exists("df_valvulas") && is.data.frame(df_valvulas) && nrow(df_valvulas) > 0) {
      df_valvulas %>%
        mutate(
          codigo_norm = norm_codigo_valvula(codigo),
          Turno_num   = suppressWarnings(as.numeric(Turno)),
          Sector      = case_when(as.character(Módulo) %in% c("9", "9.1", "9.2", "11", "11.1", "11.2", "12") ~ "9", TRUE ~ as.character(Sector)),
          Módulo      = case_when(
            as.character(Módulo) %in% c("13.1", "13.2") ~ "13",
            as.character(Módulo) %in% c("14.1", "14.2") ~ "14",
            as.character(Módulo) == "9" & between(Turno_num, 1, 11)  ~ "9.2",
            as.character(Módulo) == "9" & Turno_num >= 12            ~ "9.1",
            as.character(Módulo) == "11" & between(Turno_num, 1, 12) ~ "11.1",
            as.character(Módulo) == "11" & Turno_num >= 13           ~ "11.2",
            TRUE ~ as.character(Módulo)
          )
        ) %>%
        distinct(codigo_norm, .keep_all = TRUE) %>%
        select(codigo_norm, Sector, Módulo, Turno, Válvula, Variedad_cat = Variedad, Area_ha = Área)
    } else {
      tibble(codigo_norm = character(), Sector = character(), Módulo = character(), Turno = character(), Válvula = character(), Variedad_cat = character(), Area_ha = numeric())
    }
    
    df_rec <- df_rec %>%
      left_join(catastro, by = "codigo_norm") %>%
      mutate(
        Sector           = coalesce(Sector, sub(".*S0*([0-9]+).*", "\1", codigo_norm)),
        Módulo           = coalesce(as.character(Módulo), sub("^M0*([0-9]+).*", "\1", codigo_norm)),
        Turno            = coalesce(as.character(Turno), sub(".*-T0*([0-9]+).*", "\1", codigo_norm)),
        Válvula          = coalesce(as.character(Válvula), sub(".*-V0*([0-9]+)$", "\1", codigo_norm)),
        Variedad         = coalesce(Variedad_sap, Variedad_cat, "Desconocida"),
        Area_ha          = coalesce(Area_ha, 0),
        
        Kg_Lunes_rec     = Kg_Lunes_sap,
        Kg_Martes_rec    = Kg_Martes_sap,
        Kg_Miercoles_rec = Kg_Miercoles_sap,
        Kg_Jueves_rec    = Kg_Jueves_sap,
        Kg_Viernes_rec   = Kg_Viernes_sap,
        Kg_Sabado_rec    = Kg_Sabado_sap,
        Kg_Total_rec     = Kg_Total_sap,
        Kg_Desfase       = 0,
        
        Estado_Concil        = "Conciliado OK",
        Tipo_Alerta          = "conciliado",
        Diagnostico          = "Campo y SAP coinciden plenamente.",
        Es_Valido_Operativo  = TRUE
      )
    
    for (i in seq_len(nrow(df_rec))) {
      r <- df_rec[i, ]
      
      # REGLA 1: SAP sin Ruta -> Excluir de cálculos y pintado
      if (!r$Tiene_Campo && r$Tiene_SAP) {
        df_rec$Estado_Concil[i]        <- "Kilos SAP sin Ruta"
        df_rec$Tipo_Alerta[i]          <- "inconsistencia"
        df_rec$Diagnostico[i]          <- sprintf("Pesado en SAP (%s kg), pero no programado en ruta de campo. Excluido de mapa y cálculos.", format(round(r$Kg_Total_sap, 0), big.mark = ","))
        df_rec$Es_Valido_Operativo[i]  <- FALSE
        df_rec$Kg_Lunes_rec[i]         <- 0
        df_rec$Kg_Martes_rec[i]        <- 0
        df_rec$Kg_Miercoles_rec[i]     <- 0
        df_rec$Kg_Jueves_rec[i]        <- 0
        df_rec$Kg_Viernes_rec[i]       <- 0
        df_rec$Kg_Sabado_rec[i]        <- 0
        df_rec$Kg_Total_rec[i]         <- 0
        next
      }
      
      # REGLA 3: Ruta sin Kilos en SAP -> Mantener en incidencias; en base real SAP es 0 kg hasta edición manual
      if (r$Tiene_Campo && !r$Tiene_SAP) {
        dias_c <- character(0)
        if (r$Ha_Lunes_rc > 0) dias_c <- c(dias_c, "Lunes")
        if (r$Ha_Martes_rc > 0) dias_c <- c(dias_c, "Martes")
        if (r$Ha_Miercoles_rc > 0) dias_c <- c(dias_c, "Miércoles")
        if (r$Ha_Jueves_rc > 0) dias_c <- c(dias_c, "Jueves")
        if (r$Ha_Viernes_rc > 0) dias_c <- c(dias_c, "Viernes")
        if (r$Ha_Sabado_rc > 0) dias_c <- c(dias_c, "Sábado")
        
        tiene_previos <- any(dias_c %in% dias_cerrados_nom)
        if (es_semana_actual && !tiene_previos) {
          df_rec$Estado_Concil[i]        <- "Cosecha Hoy (D+1)"
          df_rec$Tipo_Alerta[i]          <- "en_proceso"
          df_rec$Diagnostico[i]          <- sprintf("Cosechado hoy (%s) en campo. Pendiente de recepción en balanza de packing mañana.", paste(dias_c, collapse = ", "))
          df_rec$Es_Valido_Operativo[i]  <- TRUE
          df_rec$Kg_Miercoles_rec[i]     <- 0
          df_rec$Kg_Total_rec[i]         <- 0
        } else {
          df_rec$Estado_Concil[i]        <- "Ruta sin Kilos SAP"
          df_rec$Tipo_Alerta[i]          <- "inconsistencia"
          df_rec$Diagnostico[i]          <- sprintf("Cosechado en campo [%s], 0 kg en SAP tras cierre. En reporte de incidencias para edición manual en Excel.",
                                                    paste(dias_c, collapse = ", "))
          df_rec$Es_Valido_Operativo[i]  <- TRUE
          df_rec$Kg_Lunes_rec[i]         <- 0
          df_rec$Kg_Martes_rec[i]        <- 0
          df_rec$Kg_Miercoles_rec[i]     <- 0
          df_rec$Kg_Jueves_rec[i]        <- 0
          df_rec$Kg_Viernes_rec[i]       <- 0
          df_rec$Kg_Sabado_rec[i]        <- 0
          df_rec$Kg_Total_rec[i]         <- 0
        }
        next
      }
      # REGLA 2: Desfase de Día -> Días cerrados donde SAP tiene kilos pero Campo no reportó cosecha
      if (r$Tiene_Campo && r$Tiene_SAP) {
        kg_dias <- c(Lunes = r$Kg_Lunes_sap, Martes = r$Kg_Martes_sap, Miércoles = r$Kg_Miercoles_sap,
                     Jueves = r$Kg_Jueves_sap, Viernes = r$Kg_Viernes_sap, Sábado = r$Kg_Sabado_sap)
        ha_dias <- c(Lunes = r$Ha_Lunes_rc, Martes = r$Ha_Martes_rc, Miércoles = r$Ha_Miercoles_rc,
                     Jueves = r$Ha_Jueves_rc, Viernes = r$Ha_Viernes_rc, Sábado = r$Ha_Sabado_rc)
        
        dias_c_act <- names(ha_dias)[ha_dias > 0]
        dias_s_act <- names(kg_dias)[kg_dias > 0]
        
        dias_c_cerr <- dias_c_act[dias_c_act %in% dias_cerrados_nom]
        dias_s_cerr <- dias_s_act[dias_s_act %in% dias_cerrados_nom]
        
        if (es_semana_actual) {
          dias_desfase_sap <- setdiff(dias_s_cerr, dias_c_cerr)
          
          if (length(dias_desfase_sap) > 0 && length(dias_c_cerr) > 0) {
            # Hay kilos en SAP en días cerrados donde campo no reportó cosecha (jabas rezagadas / desfase)
            kg_desfase_excl  <- round(sum(kg_dias[dias_desfase_sap]), 2)
            kg_coincidente   <- round(sum(kg_dias[dias_c_cerr]), 2)
            kg_sum_contiguos <- round(kg_coincidente + kg_desfase_excl, 2)
            
            # El usuario instruye: el desfase ahora sí cuadra, agregarlo a la fecha donde tiene más kilos
            # que coincide con cosecha campo, y ahora es AZUL porque sí se usará.
            dia_destino <- dias_c_cerr[1]
            
            df_rec$Estado_Concil[i]        <- "Desfase Reasignado"
            df_rec$Tipo_Alerta[i]          <- "conciliado"
            df_rec$Diagnostico[i]          <- sprintf("Desfase de %s kg en [%s] reasignado a [%s] por coincidencia con ruta de campo (Total: %s kg). Usado operativamente (Azul).",
                                                      format(round(kg_desfase_excl, 0), big.mark = ","),
                                                      paste(dias_desfase_sap, collapse = ", "),
                                                      dia_destino,
                                                      format(round(kg_sum_contiguos, 0), big.mark = ","))
            df_rec$Es_Valido_Operativo[i]  <- TRUE
            df_rec$Kg_Desfase[i]           <- kg_desfase_excl
            
            # Asignar todos los kilos al día coincidente con campo; el día desfasado queda en 0
            for (dc in dias_desfase_sap) {
              col_n <- norm_col_dia_rec(dc)
              df_rec[[col_n]][i] <- 0
            }
            col_dest <- norm_col_dia_rec(dia_destino)
            df_rec[[col_dest]][i] <- kg_sum_contiguos
            
            df_rec$Kg_Total_rec[i] <- round(sum(c(df_rec$Kg_Lunes_rec[i], df_rec$Kg_Martes_rec[i], df_rec$Kg_Miercoles_rec[i],
                                                  df_rec$Kg_Jueves_rec[i], df_rec$Kg_Viernes_rec[i], df_rec$Kg_Sabado_rec[i])), 2)
          } else if (length(dias_c_cerr) > 0 && length(dias_s_cerr) > 0 && length(dias_desfase_sap) == 0) {
            # Coincidencia plena en todos los días cerrados (ej. cosechó Lunes y Martes y SAP tiene Lunes y Martes)
            df_rec$Estado_Concil[i]        <- "Conciliado OK"
            df_rec$Tipo_Alerta[i]          <- "conciliado"
            df_rec$Diagnostico[i]          <- "Campo y SAP coinciden plenamente en válvula y días."
            df_rec$Es_Valido_Operativo[i]  <- TRUE
            df_rec$Kg_Desfase[i]           <- 0
            for (dc in dias_c_cerr) {
              col_n <- norm_col_dia_rec(dc)
              df_rec[[col_n]][i] <- round(kg_dias[dc], 2)
            }
            df_rec$Kg_Total_rec[i] <- round(sum(c(df_rec$Kg_Lunes_rec[i], df_rec$Kg_Martes_rec[i], df_rec$Kg_Miercoles_rec[i],
                                                  df_rec$Kg_Jueves_rec[i], df_rec$Kg_Viernes_rec[i], df_rec$Kg_Sabado_rec[i])), 2)
          } else {
            df_rec$Estado_Concil[i]        <- "Desfase de Día"
            df_rec$Tipo_Alerta[i]          <- "inconsistencia"
            df_rec$Diagnostico[i]          <- sprintf("Desfase total: Campo [%s] vs SAP [%s]. Kilos fuera de cálculo (Rojo).",
                                                      paste(dias_c_cerr, collapse = ", "), paste(dias_s_cerr, collapse = ", "))
            df_rec$Es_Valido_Operativo[i]  <- FALSE
            df_rec$Kg_Desfase[i]           <- round(sum(kg_dias[dias_s_cerr]), 2)
            df_rec$Kg_Lunes_rec[i]         <- 0
            df_rec$Kg_Martes_rec[i]        <- 0
            df_rec$Kg_Miercoles_rec[i]     <- 0
            df_rec$Kg_Jueves_rec[i]        <- 0
            df_rec$Kg_Viernes_rec[i]       <- 0
            df_rec$Kg_Sabado_rec[i]        <- 0
            df_rec$Kg_Total_rec[i]         <- 0
          }
        } else {
          df_rec$Estado_Concil[i]        <- "Conciliado OK"
          df_rec$Tipo_Alerta[i]          <- "conciliado"
          df_rec$Diagnostico[i]          <- "Semana histórica o sin desfases."
          df_rec$Es_Valido_Operativo[i]  <- TRUE
        }
        next
      }
    }
    
    return(df_rec)
  }
  
  obtener_datos_mapa_semana <- function(sem_num, 
                                        pasada_filtro = "1",
                                        grupos_filtro = character(0),
                                        mods_filtro   = character(0),
                                        turnos_filtro = character(0),
                                        vars_filtro     = character(0),
                                        modo_mapa     = "real",
                                        df_ajuste_valvs = NULL,
                                        df_ediciones  = NULL) {
    sem_num <- as.numeric(sem_num)
    pasada_str <- if (!is.null(pasada_filtro) && nzchar(as.character(pasada_filtro))) as.character(pasada_filtro) else "1"
    if (!exists("df_valvulas") || !is.data.frame(df_valvulas) || nrow(df_valvulas) == 0) {
      return(list(semana = sem_num, pasada = pasada_str, ids_filtrados = NULL, kpis = list(ha_cosechada = 0, ha_fisica = 0, kg_total = 0, rend_promedio = 0, dias = list()), valvulas = list()))
    }
    
    df_valv_ref <- df_valvulas %>%
      mutate(
        codigo_norm   = norm_codigo_valvula(codigo),
        variedad_norm = limpiar_variedad(Variedad)
      )
    
    # 1. Cosecha Oficial SAP (Kilos Oficiales y Ha/Pct de fallback)
    df_sub <- if (exists("df_hist_oficial_wide") && is.data.frame(df_hist_oficial_wide)) {
      tmp <- df_hist_oficial_wide %>% filter(as.numeric(Semana) == sem_num)
      if (pasada_str %in% c("1", "2")) {
        tmp <- tmp %>% filter(coalesce(as.character(Pasada), "1") == pasada_str)
      }
      tmp
    } else {
      tibble()
    }
    
    codigos_multi <- df_valv_ref %>%
      group_by(codigo_norm) %>%
      filter(n() > 1) %>%
      pull(codigo_norm) %>%
      unique()
    
    df_oficial_cruzado <- if (nrow(df_sub) > 0) {
      df_sub <- df_sub %>%
        mutate(
          codigo_norm   = norm_codigo_valvula(Codigo_Valv),
          variedad_norm = limpiar_variedad(Variedad)
        )
      
      df_multi <- df_sub %>%
        filter(codigo_norm %in% codigos_multi) %>%
        inner_join(
          df_valv_ref %>% select(id, codigo_norm, variedad_norm, Área),
          by = c("codigo_norm", "variedad_norm")
        ) %>%
        group_by(Semana, codigo_norm, variedad_norm, Pasada) %>%
        mutate(
          prop_sub = Área / pmax(0.0001, sum(Área)),
          across(starts_with("Kg_"), ~ round(. * prop_sub, 2)),
          across(starts_with("Ha_"), ~ round(. * prop_sub, 2))
        ) %>%
        ungroup() %>%
        select(-prop_sub)
      
      df_uni <- df_sub %>%
        filter(!codigo_norm %in% codigos_multi) %>%
        inner_join(
          df_valv_ref %>% distinct(codigo_norm, .keep_all = TRUE) %>% select(id, codigo_norm),
          by = "codigo_norm"
        )
      
      bind_rows(df_multi, df_uni) %>%
        group_by(id) %>%
        summarise(
          Kg_Lunes_ofic     = sum(coalesce(as.numeric(Kg_Lunes), 0)),
          Kg_Martes_ofic    = sum(coalesce(as.numeric(Kg_Martes), 0)),
          Kg_Miercoles_ofic = sum(coalesce(as.numeric(Kg_Miércoles), 0)),
          Kg_Jueves_ofic    = sum(coalesce(as.numeric(Kg_Jueves), 0)),
          Kg_Viernes_ofic   = sum(coalesce(as.numeric(Kg_Viernes), 0)),
          Kg_Sabado_ofic    = sum(coalesce(as.numeric(Kg_Sábado), 0)),
          Kg_Total_ofic     = sum(coalesce(as.numeric(Kg_Total_Sem), 0)),
          
          Pct_Lunes_ofic     = pmin(100, sum(coalesce(as.numeric(Pct_Lunes), 0))),
          Pct_Martes_ofic    = pmin(100, sum(coalesce(as.numeric(Pct_Martes), 0))),
          Pct_Miercoles_ofic = pmin(100, sum(coalesce(as.numeric(Pct_Miércoles), 0))),
          Pct_Jueves_ofic    = pmin(100, sum(coalesce(as.numeric(Pct_Jueves), 0))),
          Pct_Viernes_ofic   = pmin(100, sum(coalesce(as.numeric(Pct_Viernes), 0))),
          Pct_Sabado_ofic    = pmin(100, sum(coalesce(as.numeric(Pct_Sábado), 0))),
          
          Ha_Lunes_ofic     = sum(coalesce(as.numeric(Ha_Lunes), 0)),
          Ha_Martes_ofic    = sum(coalesce(as.numeric(Ha_Martes), 0)),
          Ha_Miercoles_ofic = sum(coalesce(as.numeric(Ha_Miércoles), 0)),
          Ha_Jueves_ofic    = sum(coalesce(as.numeric(Ha_Jueves), 0)),
          Ha_Viernes_ofic   = sum(coalesce(as.numeric(Ha_Viernes), 0)),
          Ha_Sabado_ofic    = sum(coalesce(as.numeric(Ha_Sábado), 0)),
          Ha_Total_ofic     = sum(coalesce(as.numeric(Ha_Total_Sem), 0)),
          .groups           = "drop"
        )
    } else {
      tibble(
        id                 = character(),
        Kg_Lunes_ofic      = numeric(),
        Kg_Martes_ofic     = numeric(),
        Kg_Miercoles_ofic  = numeric(),
        Kg_Jueves_ofic     = numeric(),
        Kg_Viernes_ofic    = numeric(),
        Kg_Sabado_ofic     = numeric(),
        Kg_Total_ofic      = numeric(),
        Pct_Lunes_ofic     = numeric(),
        Pct_Martes_ofic    = numeric(),
        Pct_Miercoles_ofic = numeric(),
        Pct_Jueves_ofic    = numeric(),
        Pct_Viernes_ofic   = numeric(),
        Pct_Sabado_ofic    = numeric(),
        Ha_Lunes_ofic      = numeric(),
        Ha_Martes_ofic     = numeric(),
        Ha_Miercoles_ofic  = numeric(),
        Ha_Jueves_ofic     = numeric(),
        Ha_Viernes_ofic    = numeric(),
        Ha_Sabado_ofic     = numeric(),
        Ha_Total_ofic      = numeric()
      )
    }
    
    # 2. Rutas Cosecha Campo (Prioridad para Hectáreas y % de avance, en vivo desde Google Sheets con fallback local)
    rc_raw <- obtener_rutas_campo_en_vivo(sem_num)
    rc_cruzado <- if (!is.null(rc_raw) && is.data.frame(rc_raw) && nrow(rc_raw) > 0) {
      rc_sem <- rc_raw %>% filter(as.numeric(Semana) == sem_num)
      if (pasada_str %in% c("1", "2")) {
        rc_sem <- rc_sem %>% filter(coalesce(as.character(Num_Pasada), "1") == pasada_str)
      }
      rc_campo <- rc_sem %>% filter(Tipo_Registro %in% c("CosechaCampo", "Cosecha"))
      if (nrow(rc_campo) == 0) {
        rc_campo <- rc_sem %>% filter(Tipo_Registro == "Proyeccion")
      }
      if (nrow(rc_campo) > 0) {
        rc_campo %>%
          group_by(id) %>%
          summarise(
            Ha_Lunes_rc     = sum(coalesce(as.numeric(Ha_Lunes), 0)),
            Ha_Martes_rc    = sum(coalesce(as.numeric(Ha_Martes), 0)),
            Ha_Miercoles_rc = sum(coalesce(as.numeric(Ha_Miércoles), 0)),
            Ha_Jueves_rc    = sum(coalesce(as.numeric(Ha_Jueves), 0)),
            Ha_Viernes_rc   = sum(coalesce(as.numeric(Ha_Viernes), 0)),
            Ha_Sabado_rc    = sum(coalesce(as.numeric(Ha_Sábado), 0)),
            Ha_Total_rc     = sum(coalesce(as.numeric(Ha_Total_Prog), 0)),
            
            Pct_Lunes_rc     = pmin(100, sum(coalesce(as.numeric(Lunes), 0))),
            Pct_Martes_rc    = pmin(100, sum(coalesce(as.numeric(Martes), 0))),
            Pct_Miercoles_rc = pmin(100, sum(coalesce(as.numeric(Miércoles), 0))),
            Pct_Jueves_rc    = pmin(100, sum(coalesce(as.numeric(Jueves), 0))),
            Pct_Viernes_rc   = pmin(100, sum(coalesce(as.numeric(Viernes), 0))),
            Pct_Sabado_rc    = pmin(100, sum(coalesce(as.numeric(Sábado), 0))),
            cells_str_rc     = {
              cs <- as.character(cells_str)
              cs <- cs[!is.na(cs) & nzchar(trimws(cs))]
              if (length(cs) > 0) cs[1] else ""
            },
            .groups          = "drop"
          )
      } else {
        tibble(
          id               = character(),
          Ha_Lunes_rc      = numeric(),
          Ha_Martes_rc     = numeric(),
          Ha_Miercoles_rc  = numeric(),
          Ha_Jueves_rc     = numeric(),
          Ha_Viernes_rc    = numeric(),
          Ha_Sabado_rc     = numeric(),
          Ha_Total_rc      = numeric(),
          Pct_Lunes_rc     = numeric(),
          Pct_Martes_rc    = numeric(),
          Pct_Miercoles_rc = numeric(),
          Pct_Jueves_rc    = numeric(),
          Pct_Viernes_rc   = numeric(),
          Pct_Sabado_rc    = numeric(),
          cells_str_rc     = character()
        )
      }
    } else {
      tibble(
        id               = character(),
        Ha_Lunes_rc      = numeric(),
        Ha_Martes_rc     = numeric(),
        Ha_Miercoles_rc  = numeric(),
        Ha_Jueves_rc     = numeric(),
        Ha_Viernes_rc    = numeric(),
        Ha_Sabado_rc     = numeric(),
        Ha_Total_rc      = numeric(),
        Pct_Lunes_rc     = numeric(),
        Pct_Martes_rc    = numeric(),
        Pct_Miercoles_rc = numeric(),
        Pct_Jueves_rc    = numeric(),
        Pct_Viernes_rc   = numeric(),
        Pct_Sabado_rc    = numeric(),
        cells_str_rc     = character()
      )
    }
    
    # 3. Fusión Unificada: Kilos Oficiales + Ha Cosecha Campo (con fallback oficial)
    # 3. Fusión Reconciliada (Reglas: 1. SAP sin ruta excluido; 2. Desfase contiguo unificado; 3. Ruta sin SAP asignado provisorio)
    df_concil <- tryCatch(obtener_cosecha_conciliada_semana(sem_num, "2026", pasada_filtro = pasada_str), error = function(e) NULL)
    
    df_merged <- if (!is.null(df_concil) && nrow(df_concil) > 0) {
      if (pasada_str %in% c("1", "2")) {
        df_concil <- df_concil %>% filter(coalesce(as.character(Pasada_rc), coalesce(as.character(Pasada_sap), "1")) == pasada_str)
      }
      df_valv_ref %>%
        left_join(df_concil, by = "codigo_norm") %>%
        group_by(codigo_norm) %>%
        mutate(
          prop_valv = if (n() > 1 && sum(Área, na.rm = TRUE) > 0) Área / sum(Área, na.rm = TRUE) else 1,
          across(starts_with("Kg_") & ends_with("_rec"), ~ round(. * prop_valv, 2)),
          across(starts_with("Ha_") & ends_with("_rc"), ~ round(. * prop_valv, 2))
        ) %>%
        ungroup() %>%
        mutate(
          es_valido   = coalesce(Es_Valido_Operativo, FALSE),
          tiene_campo = coalesce(Tiene_Campo, FALSE),
          
          # REGLA 1: SAP sin ruta excluido por completo (kilos = 0, ha = 0, % = 0)
          Kg_Lunes     = ifelse(es_valido, coalesce(Kg_Lunes_rec, 0), 0),
          Kg_Martes    = ifelse(es_valido, coalesce(Kg_Martes_rec, 0), 0),
          Kg_Miercoles = ifelse(es_valido, coalesce(Kg_Miercoles_rec, 0), 0),
          Kg_Jueves    = ifelse(es_valido, coalesce(Kg_Jueves_rec, 0), 0),
          Kg_Viernes   = ifelse(es_valido, coalesce(Kg_Viernes_rec, 0), 0),
          Kg_Sabado    = ifelse(es_valido, coalesce(Kg_Sabado_rec, 0), 0),
          Kg_Total     = round(Kg_Lunes + Kg_Martes + Kg_Miercoles + Kg_Jueves + Kg_Viernes + Kg_Sabado, 2),
          
          Ha_Lunes     = ifelse(es_valido & tiene_campo, coalesce(Ha_Lunes_rc, 0), 0),
          Ha_Martes    = ifelse(es_valido & tiene_campo, coalesce(Ha_Martes_rc, 0), 0),
          Ha_Miercoles = ifelse(es_valido & tiene_campo, coalesce(Ha_Miercoles_rc, 0), 0),
          Ha_Jueves    = ifelse(es_valido & tiene_campo, coalesce(Ha_Jueves_rc, 0), 0),
          Ha_Viernes   = ifelse(es_valido & tiene_campo, coalesce(Ha_Viernes_rc, 0), 0),
          Ha_Sabado    = ifelse(es_valido & tiene_campo, coalesce(Ha_Sabado_rc, 0), 0),
          
          Pct_Lunes     = ifelse(es_valido & tiene_campo, coalesce(Pct_Lunes_rc, 0), 0),
          Pct_Martes    = ifelse(es_valido & tiene_campo, coalesce(Pct_Martes_rc, 0), 0),
          Pct_Miercoles = ifelse(es_valido & tiene_campo, coalesce(Pct_Miercoles_rc, 0), 0),
          Pct_Jueves    = ifelse(es_valido & tiene_campo, coalesce(Pct_Jueves_rc, 0), 0),
          Pct_Viernes   = ifelse(es_valido & tiene_campo, coalesce(Pct_Viernes_rc, 0), 0),
          Pct_Sabado    = ifelse(es_valido & tiene_campo, coalesce(Pct_Sabado_rc, 0), 0),
          cells_str     = ifelse(es_valido & tiene_campo, coalesce(as.character(cells_str_rc), ""), ""),
          
          Pct_Total    = pmin(100, Pct_Lunes + Pct_Martes + Pct_Miercoles + Pct_Jueves + Pct_Viernes + Pct_Sabado),
          Ha_Cosechada = ifelse(es_valido & tiene_campo, coalesce(Ha_Total_rc, Ha_Lunes + Ha_Martes + Ha_Miercoles + Ha_Jueves + Ha_Viernes + Ha_Sabado), 0)
        )
    } else {
      df_valv_ref %>%
        left_join(df_oficial_cruzado, by = "id") %>%
        left_join(rc_cruzado, by = "id") %>%
        mutate(
          Kg_Lunes     = coalesce(Kg_Lunes_ofic, 0),
          Kg_Martes    = coalesce(Kg_Martes_ofic, 0),
          Kg_Miercoles = coalesce(Kg_Miercoles_ofic, 0),
          Kg_Jueves    = coalesce(Kg_Jueves_ofic, 0),
          Kg_Viernes   = coalesce(Kg_Viernes_ofic, 0),
          Kg_Sabado    = coalesce(Kg_Sabado_ofic, 0),
          Kg_Total     = round(Kg_Lunes + Kg_Martes + Kg_Miercoles + Kg_Jueves + Kg_Viernes + Kg_Sabado, 2),
          tiene_campo  = (!is.na(Ha_Total_rc) & Ha_Total_rc > 0),
          Ha_Lunes     = ifelse(tiene_campo, coalesce(Ha_Lunes_rc, 0), 0),
          Ha_Martes    = ifelse(tiene_campo, coalesce(Ha_Martes_rc, 0), 0),
          Ha_Miercoles = ifelse(tiene_campo, coalesce(Ha_Miercoles_rc, 0), 0),
          Ha_Jueves    = ifelse(tiene_campo, coalesce(Ha_Jueves_rc, 0), 0),
          Ha_Viernes   = ifelse(tiene_campo, coalesce(Ha_Viernes_rc, 0), 0),
          Ha_Sabado    = ifelse(tiene_campo, coalesce(Ha_Sabado_rc, 0), 0),
          Pct_Lunes     = ifelse(tiene_campo, coalesce(Pct_Lunes_rc, 0), 0),
          Pct_Martes    = ifelse(tiene_campo, coalesce(Pct_Martes_rc, 0), 0),
          Pct_Miercoles = ifelse(tiene_campo, coalesce(Pct_Miercoles_rc, 0), 0),
          Pct_Jueves    = ifelse(tiene_campo, coalesce(Pct_Jueves_rc, 0), 0),
          Pct_Viernes   = ifelse(tiene_campo, coalesce(Pct_Viernes_rc, 0), 0),
          Pct_Sabado    = ifelse(tiene_campo, coalesce(Pct_Sabado_rc, 0), 0),
          cells_str     = ifelse(tiene_campo, coalesce(as.character(cells_str_rc), ""), ""),
          Pct_Total    = pmin(100, Pct_Lunes + Pct_Martes + Pct_Miercoles + Pct_Jueves + Pct_Viernes + Pct_Sabado),
          Ha_Cosechada = ifelse(tiene_campo, coalesce(Ha_Total_rc, Ha_Lunes + Ha_Martes + Ha_Miercoles + Ha_Jueves + Ha_Viernes + Ha_Sabado), 0)
        )
    }
  # Incorporar ediciones manuales de celdas al mapa de ajuste
  ed_sem_man <- if (!is.null(df_ediciones) && is.data.frame(df_ediciones) && nrow(df_ediciones) > 0) {
    df_ediciones %>% filter(as.numeric(Semana) == sem_num)
  } else if (exists("cargar_ediciones_disco")) {
    tryCatch(cargar_ediciones_disco() %>% filter(as.numeric(Semana) == sem_num), error = function(e) tibble())
  } else {
    tibble()
  }
  
  man_filas <- if (nrow(ed_sem_man) > 0) {
    ed_sem_man %>% 
      filter(Tipo_Edicion == "manual", Campo %in% c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado"), coalesce(Valor, 0) > 0)
  } else {
    tibble()
  }
  
  df_valvs_man <- tibble()
  if (nrow(man_filas) > 0) {
    valvs_man_list <- list()
    for (i in seq_len(nrow(man_filas))) {
      ed <- man_filas[i, ]
      mod_e <- trimws(as.character(ed$Módulo))
      tur_e <- trimws(as.character(ed$Turno))
      dia_e <- trimws(as.character(ed$Campo))
      val_e <- as.numeric(ed$Valor)
      
      vt <- df_valv_ref %>% 
        filter(
          trimws(as.character(Módulo)) == mod_e,
          trimws(as.character(Turno))  == tur_e
        )
      if (nrow(vt) == 0) next
      
      a_tot <- sum(vt$Área, na.rm = TRUE)
      v_man <- vt %>%
        transmute(
          id              = id,
          codigo          = codigo,
          Módulo          = mod_e,
          Turno           = tur_e,
          Dia_Asignado    = dia_e,
          Kg_Asignado     = round(val_e * (Área / pmax(0.01, a_tot)), 2),
          Ha_Asignada     = Área,
          Num_Pasada      = 1L,
          Tipo_Asignacion = "manual",
          Dia_Desde       = "Miércoles",
          Dia_Hasta       = "Sábado"
        )
      valvs_man_list[[length(valvs_man_list) + 1]] <- v_man
    }
    if (length(valvs_man_list) > 0) {
      df_valvs_man <- bind_rows(valvs_man_list)
    }
  }
  
  # Combinar df_ajuste_valvs con df_valvs_man (las ediciones manuales sobreescriben asignaciones automáticas de ese turno y día)
  df_aj_comb <- if (!is.null(df_ajuste_valvs) && is.data.frame(df_ajuste_valvs) && nrow(df_ajuste_valvs) > 0) {
    if (nrow(df_valvs_man) > 0 && "Módulo" %in% names(df_ajuste_valvs) && "Turno" %in% names(df_ajuste_valvs)) {
      claves_man <- paste(trimws(as.character(df_valvs_man$Módulo)),
                          trimws(as.character(df_valvs_man$Turno)),
                          trimws(as.character(df_valvs_man$Dia_Asignado)), sep = "___")
      claves_aj <- paste(trimws(as.character(df_ajuste_valvs$Módulo)),
                         trimws(as.character(df_ajuste_valvs$Turno)),
                         trimws(as.character(df_ajuste_valvs$Dia_Asignado)), sep = "___")
      bind_rows(df_ajuste_valvs %>% filter(!claves_aj %in% claves_man), df_valvs_man)
    } else {
      df_ajuste_valvs
    }
  } else {
    df_valvs_man
  }
  
  if (identical(modo_mapa, "ajustado") && nrow(df_aj_comb) > 0) {
    df_aj <- df_aj_comb
    if (pasada_str %in% c("1", "2")) {
      df_aj <- df_aj %>% filter(coalesce(as.character(Num_Pasada), "1") == pasada_str)
    }
    
    dias_todos <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
    d_ini <- if ("Dia_Desde" %in% names(df_aj) && nrow(df_aj) > 0 && !is.na(df_aj$Dia_Desde[1])) df_aj$Dia_Desde[1] else "Miércoles"
    d_fin <- if ("Dia_Hasta" %in% names(df_aj) && nrow(df_aj) > 0 && !is.na(df_aj$Dia_Hasta[1])) df_aj$Dia_Hasta[1] else "Sábado"
    idx_ini <- match(d_ini, dias_todos)
    idx_fin <- match(d_fin, dias_todos)
    if (is.na(idx_ini)) idx_ini <- 3
    if (is.na(idx_fin)) idx_fin <- 6
    if (idx_fin < idx_ini) idx_fin <- idx_ini
    dias_ventana_ajuste <- dias_todos[idx_ini:idx_fin]
    
    df_aj_wide <- df_aj %>%
      mutate(
        Dia_Asignado_norm = case_when(
          Dia_Asignado %in% c("Lunes", "lunes") ~ "Lunes",
          Dia_Asignado %in% c("Martes", "martes") ~ "Martes",
          Dia_Asignado %in% c("Miércoles", "Miercoles", "miercoles") ~ "Miercoles",
          Dia_Asignado %in% c("Jueves", "jueves") ~ "Jueves",
          Dia_Asignado %in% c("Viernes", "viernes") ~ "Viernes",
          Dia_Asignado %in% c("Sábado", "Sabado", "sabado") ~ "Sabado",
          TRUE ~ Dia_Asignado
        )
      ) %>%
      group_by(id) %>%
      summarise(
        Kg_Lunes_aj     = sum(ifelse(Dia_Asignado_norm == "Lunes", coalesce(Kg_Asignado, 0), 0), na.rm = TRUE),
        Ha_Lunes_aj     = sum(ifelse(Dia_Asignado_norm == "Lunes", coalesce(Ha_Asignada, 0), 0), na.rm = TRUE),
        Kg_Martes_aj    = sum(ifelse(Dia_Asignado_norm == "Martes", coalesce(Kg_Asignado, 0), 0), na.rm = TRUE),
        Ha_Martes_aj    = sum(ifelse(Dia_Asignado_norm == "Martes", coalesce(Ha_Asignada, 0), 0), na.rm = TRUE),
        Kg_Miercoles_aj = sum(ifelse(Dia_Asignado_norm == "Miercoles", coalesce(Kg_Asignado, 0), 0), na.rm = TRUE),
        Ha_Miercoles_aj = sum(ifelse(Dia_Asignado_norm == "Miercoles", coalesce(Ha_Asignada, 0), 0), na.rm = TRUE),
        Kg_Jueves_aj    = sum(ifelse(Dia_Asignado_norm == "Jueves", coalesce(Kg_Asignado, 0), 0), na.rm = TRUE),
        Ha_Jueves_aj    = sum(ifelse(Dia_Asignado_norm == "Jueves", coalesce(Ha_Asignada, 0), 0), na.rm = TRUE),
        Kg_Viernes_aj   = sum(ifelse(Dia_Asignado_norm == "Viernes", coalesce(Kg_Asignado, 0), 0), na.rm = TRUE),
        Ha_Viernes_aj   = sum(ifelse(Dia_Asignado_norm == "Viernes", coalesce(Ha_Asignada, 0), 0), na.rm = TRUE),
        Kg_Sabado_aj    = sum(ifelse(Dia_Asignado_norm == "Sabado", coalesce(Kg_Asignado, 0), 0), na.rm = TRUE),
        Ha_Sabado_aj    = sum(ifelse(Dia_Asignado_norm == "Sabado", coalesce(Ha_Asignada, 0), 0), na.rm = TRUE),
        .groups         = "drop"
      )
      
    df_merged <- df_merged %>%
      left_join(df_aj_wide, by = "id") %>%
      mutate(
        Kg_Lunes = if ("Lunes" %in% dias_ventana_ajuste) coalesce(Kg_Lunes_aj, 0) else ifelse(coalesce(Kg_Lunes, 0) == 0 & coalesce(Kg_Lunes_aj, 0) > 0, Kg_Lunes_aj, Kg_Lunes),
        Ha_Lunes = if ("Lunes" %in% dias_ventana_ajuste) coalesce(Ha_Lunes_aj, 0) else Ha_Lunes,
        Pct_Lunes = if ("Lunes" %in% dias_ventana_ajuste) round(pmin(100, (Ha_Lunes / pmax(0.01, Área)) * 100)) else Pct_Lunes,
        
        Kg_Martes = if ("Martes" %in% dias_ventana_ajuste) coalesce(Kg_Martes_aj, 0) else ifelse(coalesce(Kg_Martes, 0) == 0 & coalesce(Kg_Martes_aj, 0) > 0, Kg_Martes_aj, Kg_Martes),
        Ha_Martes = if ("Martes" %in% dias_ventana_ajuste) coalesce(Ha_Martes_aj, 0) else Ha_Martes,
        Pct_Martes = if ("Martes" %in% dias_ventana_ajuste) round(pmin(100, (Ha_Martes / pmax(0.01, Área)) * 100)) else Pct_Martes,
        
        Kg_Miercoles = if ("Miércoles" %in% dias_ventana_ajuste) coalesce(Kg_Miercoles_aj, 0) else Kg_Miercoles,
        Ha_Miercoles = if ("Miércoles" %in% dias_ventana_ajuste) coalesce(Ha_Miercoles_aj, 0) else Ha_Miercoles,
        Pct_Miercoles = if ("Miércoles" %in% dias_ventana_ajuste) round(pmin(100, (Ha_Miercoles / pmax(0.01, Área)) * 100)) else Pct_Miercoles,
        
        Kg_Jueves = if ("Jueves" %in% dias_ventana_ajuste) coalesce(Kg_Jueves_aj, 0) else Kg_Jueves,
        Ha_Jueves = if ("Jueves" %in% dias_ventana_ajuste) coalesce(Ha_Jueves_aj, 0) else Ha_Jueves,
        Pct_Jueves = if ("Jueves" %in% dias_ventana_ajuste) round(pmin(100, (Ha_Jueves / pmax(0.01, Área)) * 100)) else Pct_Jueves,
        
        Kg_Viernes = if ("Viernes" %in% dias_ventana_ajuste) coalesce(Kg_Viernes_aj, 0) else Kg_Viernes,
        Ha_Viernes = if ("Viernes" %in% dias_ventana_ajuste) coalesce(Ha_Viernes_aj, 0) else Ha_Viernes,
        Pct_Viernes = if ("Viernes" %in% dias_ventana_ajuste) round(pmin(100, (Ha_Viernes / pmax(0.01, Área)) * 100)) else Pct_Viernes,
        
        Kg_Sabado = if ("Sábado" %in% dias_ventana_ajuste) coalesce(Kg_Sabado_aj, 0) else Kg_Sabado,
        Ha_Sabado = if ("Sábado" %in% dias_ventana_ajuste) coalesce(Ha_Sabado_aj, 0) else Ha_Sabado,
        Pct_Sabado = if ("Sábado" %in% dias_ventana_ajuste) round(pmin(100, (Ha_Sabado / pmax(0.01, Área)) * 100)) else Pct_Sabado,
        
        cells_str = "",
        
        Kg_Total = round(Kg_Lunes + Kg_Martes + Kg_Miercoles + Kg_Jueves + Kg_Viernes + Kg_Sabado, 2),
        Pct_Total = pmin(100, Pct_Lunes + Pct_Martes + Pct_Miercoles + Pct_Jueves + Pct_Viernes + Pct_Sabado),
        Ha_Cosechada = round(Ha_Lunes + Ha_Martes + Ha_Miercoles + Ha_Jueves + Ha_Viernes + Ha_Sabado, 2)
      ) %>%
      mutate(
        Ha_Cosechada = ifelse(Ha_Cosechada <= 0 & Pct_Total > 0 & Área > 0, round(Área * (Pct_Total / 100), 2), Ha_Cosechada),
        Pct_Total    = ifelse(Kg_Total > 0 & Pct_Total <= 0, 100, Pct_Total),
        Ha_Cosechada = ifelse(Kg_Total > 0 & Ha_Cosechada <= 0, Área, Ha_Cosechada)
      )
  }
    
    # 4. Filtrado por selección activa en UI
    tiene_filtro <- (!is.null(grupos_filtro) && length(grupos_filtro) > 0) ||
      (!is.null(mods_filtro) && length(mods_filtro) > 0) ||
      (!is.null(turnos_filtro) && length(turnos_filtro) > 0) ||
      (!is.null(vars_filtro) && length(vars_filtro) > 0)
    
    if (tiene_filtro) {
      df_valv_f <- df_valv_ref
      if (!is.null(grupos_filtro) && length(grupos_filtro) > 0) {
        mods_grupo <- as.character(unlist(grupos_modulos[grupos_filtro], use.names = FALSE))
        df_valv_f <- df_valv_f %>%
          filter(suppressWarnings(as.numeric(sub("\\..*", "", as.character(Módulo)))) %in% as.numeric(mods_grupo))
      }
      if (!is.null(mods_filtro) && length(mods_filtro) > 0) {
        mods_filtro_chr <- as.character(mods_filtro)
        if ("Módulo_Sub" %in% names(df_valv_f)) {
          df_valv_f <- df_valv_f %>%
            filter(as.character(Módulo) %in% mods_filtro_chr | as.character(Módulo_Sub) %in% mods_filtro_chr)
        } else {
          df_valv_f <- df_valv_f %>%
            filter(as.character(Módulo) %in% mods_filtro_chr)
        }
      }
      if (!is.null(turnos_filtro) && length(turnos_filtro) > 0) {
        df_valv_f <- df_valv_f %>%
          filter(as.character(Turno) %in% as.character(turnos_filtro))
      }
      if (!is.null(vars_filtro) && length(vars_filtro) > 0) {
        df_valv_f <- df_valv_f %>%
          filter(limpiar_variedad(Variedad) %in% limpiar_variedad(vars_filtro))
      }
      ids_filtrados <- unique(as.character(df_valv_f$id))
      df_kpi <- df_merged %>% filter(as.character(id) %in% ids_filtrados)
    } else {
      ids_filtrados <- NULL
      df_kpi <- df_merged
    }
    
    tot_kg     <- sum(df_kpi$Kg_Total, na.rm = TRUE)
    tot_ha_cos <- sum(df_kpi$Ha_Cosechada, na.rm = TRUE)
    tot_ha_fis <- sum(df_kpi$Área, na.rm = TRUE)
    
    list(
      semana        = sem_num,
      ids_filtrados = ids_filtrados,
      kpis = list(
        ha_cosechada  = round(tot_ha_cos, 2),
        ha_fisica     = round(tot_ha_fis, 2),
        kg_total      = round(tot_kg),
        rend_promedio = round(if (tot_ha_cos > 0) tot_kg / tot_ha_cos else 0),
        dias = list(
          Lunes     = list(kg = round(sum(df_kpi$Kg_Lunes, na.rm = TRUE)),     ha = round(sum(df_kpi$Ha_Lunes, na.rm = TRUE), 2)),
          Martes    = list(kg = round(sum(df_kpi$Kg_Martes, na.rm = TRUE)),    ha = round(sum(df_kpi$Ha_Martes, na.rm = TRUE), 2)),
          Miercoles = list(kg = round(sum(df_kpi$Kg_Miercoles, na.rm = TRUE)), ha = round(sum(df_kpi$Ha_Miercoles, na.rm = TRUE), 2)),
          Jueves    = list(kg = round(sum(df_kpi$Kg_Jueves, na.rm = TRUE)),    ha = round(sum(df_kpi$Ha_Jueves, na.rm = TRUE), 2)),
          Viernes   = list(kg = round(sum(df_kpi$Kg_Viernes, na.rm = TRUE)),   ha = round(sum(df_kpi$Ha_Viernes, na.rm = TRUE), 2)),
          Sabado    = list(kg = round(sum(df_kpi$Kg_Sabado, na.rm = TRUE)),    ha = round(sum(df_kpi$Ha_Sabado, na.rm = TRUE), 2))
        )
      ),
      valvulas = purrr::transpose(
        df_merged %>%
          select(
            id, codigo, Pct_Lunes, Pct_Martes, Pct_Miercoles, Pct_Jueves, Pct_Viernes, Pct_Sabado,
            Kg_Lunes, Kg_Martes, Kg_Miercoles, Kg_Jueves, Kg_Viernes, Kg_Sabado,
            Kg_Total, Pct_Total, Ha_Cosechada, cells_str
          )
      )
    )
  }
} else {
  stop("ERROR CRÍTICO: Conexión exclusiva a Google Drive falló. No se encontró datos_maestros_cosecha_2026.RData en la unidad G: ni en Google Drive nube.")
}

# Identificador canónico de registros Testblock / Incubadora / Vivero / Sector 4
es_fila_testblock <- function(sector_vec, modulo_vec, tipo_vec = NULL, es_tb_vec = NULL) {
  res <- trimws(as.character(sector_vec)) %in% c("4", "04", "S04", "S4") |
         grepl("test|incub|vivero|block|^tb", as.character(modulo_vec), ignore.case = TRUE)
  if (!is.null(tipo_vec)) {
    res <- res | toupper(trimws(coalesce(as.character(tipo_vec), ""))) == "TESTBLOCK"
  }
  if (!is.null(es_tb_vec)) {
    res <- res | coalesce(as.logical(es_tb_vec), FALSE) |
           toupper(trimws(coalesce(as.character(es_tb_vec), ""))) %in% c("SI", "TRUE", "1")
  }
  coalesce(res, FALSE)
}

limpiar_hist_wide_sin_testblock <- function(df_w) {
  if (is.null(df_w) || !is.data.frame(df_w) || nrow(df_w) == 0) return(df_w)
  df_w %>%
    filter(
      !trimws(as.character(Sector)) %in% c("4", "04", "S04", "S4"),
      !is.na(Módulo_Norm),
      !grepl("test|incub|vivero|block|^tb|^MNA", paste(Módulo_Norm, Codigo_Valv), ignore.case = TRUE)
    )
}

if (exists("df_hist_oficial_wide") && is.data.frame(df_hist_oficial_wide)) {
  df_hist_oficial_wide <- limpiar_hist_wide_sin_testblock(df_hist_oficial_wide)
}

# Homologar EB 9-2 a MAGICA, remover tildes y etiquetar Testblock desde el origen
df_cosecha_base <- df_cosecha %>%
  mutate(
    Variedad     = limpiar_variedad(Variedad),
    Es_Testblock = es_fila_testblock(Sector, Módulo, if ("Tipo_Cosecha" %in% names(.)) Tipo_Cosecha else NULL, if ("Es_Testblock" %in% names(.)) Es_Testblock else NULL),
    Tipo_Cosecha = ifelse(Es_Testblock, "Testblock", "Comercial")
  )

# 2. Pre-calcular el Área consolidada por Turno (exclusivamente lotes comerciales)
areas_base <- df_cosecha_base %>%
  filter(!Es_Testblock, !is.na(Área), Área > 0) %>%
  distinct(Campaña, Sector, Módulo, Turno, Válvula, Variedad, Área) %>%
  mutate(
    Campaña  = trimws(as.character(Campaña)),
    Sector   = trimws(as.character(Sector)),
    Módulo   = trimws(as.character(Módulo)),
    Turno    = trimws(as.character(Turno)),
    Variedad = limpiar_variedad(Variedad)
  ) %>%
  group_by(Campaña, Sector, Módulo, Turno, Variedad) %>%
  summarise(Área = sum(Área, na.rm = TRUE), .groups = "drop")

# 3. Preparar Tratamientos (Tipo)
col_campana <- intersect(c("Campaña", "Campania", "campaña", "campania"), names(df_tratamientos))[1]
col_modulo  <- intersect(c("Módulo", "Modulo", "módulo", "modulo"), names(df_tratamientos))[1]
col_tipo    <- intersect(c("Tratamiento", "tratamiento", "Tipo", "tipo"), names(df_tratamientos))[1]

tratamientos_base <- df_tratamientos %>%
  mutate(
    Campaña = trimws(as.character(.data[[col_campana]])),
    Módulo  = trimws(as.character(.data[[col_modulo]])),
    Tipo    = case_when(
      grepl("org[aá]n", as.character(.data[[col_tipo]]), ignore.case = TRUE) ~ "Orgánico",
      grepl("conv", as.character(.data[[col_tipo]]), ignore.case = TRUE)     ~ "Convencional",
      TRUE ~ tools::toTitleCase(tolower(trimws(as.character(.data[[col_tipo]]))))
    )
  ) %>%
  mutate(
    Campaña = ifelse(grepl("/", Campaña), 
                     as.character(as.numeric(sub("/.*", "", Campaña)) + 2000), 
                     Campaña)
  ) %>%
  select(Campaña, Módulo, Tipo) %>%
  filter(!is.na(Campaña), !is.na(Módulo)) %>%
  distinct(Campaña, Módulo, .keep_all = TRUE)


# ==============================================================================
# FUNCIÓN AUXILIAR: EXTRAER TODAS LAS SEMANAS DE PROYECCIÓN DISPONIBLES
# ==============================================================================
obtener_semanas_proyeccion <- function(ruta_raiz, campana) {
  if (exists("df_proyecciones") && !is.null(df_proyecciones) && nrow(df_proyecciones) > 0) {
    es_2026 <- grepl("2026|^26", as.character(campana))
    if (es_2026) {
      sems <- sort(unique(df_proyecciones$Semana))
      if (length(sems) > 0) return(sems)
    }
  }
  tryCatch({
    nums <- regmatches(as.character(campana), gregexpr("[0-9]+", as.character(campana)))[[1]]
    if (length(nums) == 0) return(numeric(0))
    y1 <- as.numeric(nums[1]) %% 100
    
    carpetas <- list.dirs(ruta_raiz, recursive = FALSE, full.names = TRUE)
    patron_campana <- sprintf("Campa[nñ]a\\s*0*%02d\\s*[-/]", y1)
    carpeta_campana <- carpetas[grepl(patron_campana, basename(carpetas), ignore.case = TRUE)][1]
    if (is.na(carpeta_campana)) return(numeric(0))
    
    dirs_sub1 <- list.dirs(carpeta_campana, recursive = FALSE, full.names = TRUE)
    dir_proy <- dirs_sub1[grepl("Proyecci[oó]n.*Semanal", basename(dirs_sub1), ignore.case = TRUE)][1]
    if (is.na(dir_proy)) return(numeric(0))
    
    dirs_sub2 <- list.dirs(dir_proy, recursive = FALSE, full.names = TRUE)
    dir_detallada <- dirs_sub2[grepl("Detallada", basename(dirs_sub2), ignore.case = TRUE)][1]
    if (is.na(dir_detallada)) return(numeric(0))
    
    archivos <- list.files(dir_detallada, pattern = "\\.(xlsx|xls)$", full.names = FALSE)
    archivos <- archivos[!grepl("^~\\$", archivos)]
    
    semanas <- regmatches(archivos, regexec("Semana\\s*([0-9]+)", archivos, ignore.case = TRUE))
    semanas_num <- sapply(semanas, function(x) if (length(x) >= 2) as.numeric(x[2]) else NA)
    return(na.omit(unique(semanas_num)))
  }, error = function(e) {
    return(numeric(0))
  })
}


# ==============================================================================
# FUNCIÓN: CARGAR COSECHA ACTUAL DE SAP CON REGLAS DE NEGOCIO OFICIALES
# ==============================================================================
cargar_cosecha_actual <- function(ruta_raiz, campana_actual = 2026) {
  cat("\n=======================================================\n")
  cat("[COSECHA SEMANA ACTUAL] Buscando archivo de SAP...\n")
  
  tryCatch({
    fecha_hoy     <- Sys.Date()
    semana_actual <- lubridate::isoweek(fecha_hoy)
    anio_actual   <- lubridate::isoyear(fecha_hoy)
    cat(sprintf("  -> Fecha hoy: %s | Año ISO: %d | Semana ISO actual: %d\n", fecha_hoy, anio_actual, semana_actual))
    
    # Usar el año de la campaña (ej. 2026 -> 26) para que en Enero 2027 siga buscando en Campaña 26-27
    y1 <- as.numeric(campana_actual) %% 100
    carpetas <- list.dirs(ruta_raiz, recursive = FALSE, full.names = TRUE)
    patron_campana <- sprintf("Campa[nñ]a\\s*0*%02d\\s*[-/]", y1)
    carpeta_campana <- carpetas[grepl(patron_campana, basename(carpetas), ignore.case = TRUE)][1]
    
    if (is.na(carpeta_campana) || !dir.exists(carpeta_campana)) {
      carpeta_campana <- file.path(ruta_raiz, "Campaña 26-27")
    }
    
    ruta_bi <- file.path(carpeta_campana, "Cosecha", "BI")
    if (!dir.exists(ruta_bi)) return(NULL)
    
    archivos_bi <- list.files(ruta_bi, pattern = "\\.(xlsx|xls|csv)$", full.names = TRUE)
    archivos_bi <- archivos_bi[!grepl("^~\\$", basename(archivos_bi))]
    
    archivo_sap <- archivos_bi[grepl("Cosecha_SAP_.*dash", basename(archivos_bi), ignore.case = TRUE)][1]
    if (is.na(archivo_sap) || !file.exists(archivo_sap)) return(NULL)
    cat("  -> Archivo SAP detectado:", basename(archivo_sap), "\n")
    
    df_raw <- if (grepl("\\.csv$", archivo_sap, ignore.case = TRUE)) {
      read.csv(archivo_sap, stringsAsFactors = FALSE)
    } else {
      suppressMessages(read_excel(archivo_sap, sheet = 1))
    }
    
    if ("IDCULTIVO" %in% names(df_raw)) {
      df_raw <- df_raw %>% filter(IDCULTIVO == "AR")
    }
    
    nms <- names(df_raw)
    col_fecha        <- nms[grepl("^fecha$", nms, ignore.case = TRUE)][1]
    col_peso         <- nms[grepl("^pesoneto$", nms, ignore.case = TRUE)][1]
    col_consum       <- nms[grepl("^consumidor$", nms, ignore.case = TRUE)][1]
    col_sector       <- nms[grepl("^sector$", nms, ignore.case = TRUE)][1]
    col_modulo       <- nms[grepl("^modulo$|^m[oó]dulo$", nms, ignore.case = TRUE)][1]
    col_turno        <- nms[grepl("^turno$", nms, ignore.case = TRUE)][1]
    col_variedad     <- nms[grepl("^variedad$", nms, ignore.case = TRUE)][1]
    col_consum_campo <- nms[grepl("consumidor.*campo|campo.*consumidor", nms, ignore.case = TRUE)][1]
    
    if (is.na(col_fecha) || is.na(col_peso)) return(NULL)
    
    raw_fechas <- df_raw[[col_fecha]]
    fechas_parsed <- if (inherits(raw_fechas, "Date")) {
      raw_fechas
    } else if (inherits(raw_fechas, "POSIXt")) {
      as.Date(raw_fechas)
    } else if (is.numeric(raw_fechas)) {
      as.Date(raw_fechas, origin = "1899-12-30")
    } else {
      txt_norm <- gsub("\\.", "/", as.character(raw_fechas))
      res <- suppressWarnings(as.Date(parse_date_time(txt_norm, orders = c("dmy", "ymd", "mdy", "d/m/Y", "Y-m-d", "Ymd"))))
      idx_na <- which(is.na(res))
      if (length(idx_na) > 0) {
        res[idx_na] <- suppressWarnings(as.Date(as.character(raw_fechas)[idx_na], format = "%Y%m%d"))
      }
      res
    }
    
    semanas_filas <- lubridate::isoweek(fechas_parsed)
    anios_filas   <- lubridate::isoyear(fechas_parsed)
    
    cat("  -> Semanas presentes en el archivo SAP:", paste(sort(unique(na.omit(semanas_filas))), collapse = ", "), "\n")
    
    es_semana_actual <- (!is.na(semanas_filas) & semanas_filas == semana_actual & anios_filas == anio_actual)
    n_actuales <- sum(es_semana_actual, na.rm = TRUE)
    
    if (n_actuales == 0) {
      cat(sprintf("  [AVISO] Archivo SAP sin registros de la Semana actual (%d). Se usa solo histórico.\n", semana_actual))
      cat("=======================================================\n\n")
      return(NULL)
    }
    
    df_actual <- df_raw[which(es_semana_actual), ]
    
    if (!is.na(col_consum)) {
      consum_vals <- as.character(df_actual[[col_consum]])
      sec_vec <- suppressWarnings(as.numeric(sub(".*\\bS(\\d+).*", "\\1", consum_vals)))
      mod_vec <- suppressWarnings(as.character(as.numeric(sub(".*\\bM(\\d+).*", "\\1", consum_vals))))
      tur_vec <- suppressWarnings(as.numeric(sub(".*\\bT(\\d+).*", "\\1", consum_vals)))
    } else {
      sec_vec <- suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(df_actual[[col_sector]]))))
      mod_vec <- sub("^[A-Za-z]+0*", "", trimws(as.character(df_actual[[col_modulo]])))
      tur_vec <- suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(df_actual[[col_turno]]))))
      consum_vals <- rep("", nrow(df_actual))
    }
    
    # Detección de Testblock / Ensayos (Sector 4 / TB en CONSUMIDOR_CAMPO o CONSUMIDOR)
    es_tb <- if (!is.na(col_consum_campo) && col_consum_campo %in% names(df_actual)) {
      grepl("S04|TB", as.character(df_actual[[col_consum_campo]]), ignore.case = TRUE)
    } else {
      rep(FALSE, nrow(df_actual))
    }
    if (!is.na(col_consum)) {
      es_tb <- es_tb | grepl("^TB|\\bS0*4\\b", consum_vals, ignore.case = TRUE)
    }
    tipo_cosecha_vec <- ifelse(es_tb, "Testblock", "Comercial")
    
    df_actual <- df_actual %>%
      mutate(
        Fecha        = fechas_parsed[which(es_semana_actual)],
        Real         = suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(.data[[col_peso]])))),
        Sector       = ifelse(es_tb, 4, sec_vec),
        Módulo       = ifelse(es_tb, "Test Block", mod_vec),
        Turno        = ifelse(es_tb, 0, tur_vec),
        Variedad     = limpiar_variedad(sub("^.*\\|\\s*", "", as.character(.data[[col_variedad]]))),
        Semana       = semana_actual,
        Campaña      = campana_actual,
        Tipo_Cosecha = tipo_cosecha_vec,
        Es_Testblock = es_tb
      ) %>%
      filter(!is.na(Real), Real > 0) %>%
      mutate(
        Sector = ifelse(!Es_Testblock & Variedad == "ROCIO" & Módulo == "8", 3, Sector),
        Módulo = case_when(
          Es_Testblock                           ~ "Test Block",
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
      group_by(Fecha, Campaña, Semana, Sector, Módulo, Turno, Variedad, Tipo_Cosecha) %>%
      summarise(
        `Kg Total`   = sum(Real, na.rm = TRUE),
        Es_Testblock = any(Es_Testblock),
        .groups      = "drop"
      )
    
    kg_comercial <- sum(df_actual$`Kg Total`[df_actual$Tipo_Cosecha == "Comercial"], na.rm = TRUE)
    kg_testblock <- sum(df_actual$`Kg Total`[df_actual$Tipo_Cosecha == "Testblock"], na.rm = TRUE)
    cat(sprintf("  -> Éxito SAP: %d grupos consolidados de Semana %d (Comercial: %.1f Kg | Testblock: %.1f Kg)\n", 
                nrow(df_actual), semana_actual, kg_comercial, kg_testblock))
    cat("=======================================================\n\n")
    return(df_actual)
    
  }, error = function(e) {
    cat("[ERROR SAP]:", e$message, "\n")
    return(NULL)
  })
}

# Función para integrar la cosecha actual de SAP sobre la base histórica
integrar_sap_en_historico <- function(df_hist_base, ruta_raiz, campana_actual = 2026) {
  df_sap_actual <- cargar_cosecha_actual(ruta_raiz, campana_actual = campana_actual)
  if (!is.null(df_sap_actual) && nrow(df_sap_actual) > 0) {
    semana_actual_num <- lubridate::isoweek(Sys.Date())
    
    if (!"Tipo_Cosecha" %in% names(df_hist_base)) {
      df_hist_base$Tipo_Cosecha <- "Comercial"
    }
    if (!"Es_Testblock" %in% names(df_hist_base)) {
      df_hist_base$Es_Testblock <- FALSE
    }
    
    for (col in intersect(names(df_sap_actual), names(df_hist_base))) {
      clase_destino <- class(df_hist_base[[col]])[1]
      if (clase_destino %in% c("numeric", "double")) {
        df_sap_actual[[col]] <- suppressWarnings(as.numeric(df_sap_actual[[col]]))
      } else if (clase_destino == "integer") {
        df_sap_actual[[col]] <- suppressWarnings(as.integer(df_sap_actual[[col]]))
      } else if (clase_destino == "character") {
        df_sap_actual[[col]] <- as.character(df_sap_actual[[col]])
      } else if (clase_destino %in% c("Date", "POSIXct", "POSIXt")) {
        df_sap_actual[[col]] <- as.Date(df_sap_actual[[col]])
      }
    }
    
    df_hist_base <- df_hist_base %>%
      filter(!(as.character(Campaña) == as.character(campana_actual) & Semana == semana_actual_num)) %>%
      bind_rows(df_sap_actual)
  }
  tb_f <- es_fila_testblock(df_hist_base$Sector, df_hist_base$Módulo, df_hist_base$Tipo_Cosecha, df_hist_base$Es_Testblock)
  df_hist_base$Es_Testblock <- tb_f
  df_hist_base$Tipo_Cosecha <- ifelse(tb_f, "Testblock", "Comercial")
  return(df_hist_base)
}

# Carga inicial al arrancar la app: separar desde el inicio Comercial vs Testblock
df_cosecha_inicial_full <- if (exists("metadata_actualizacion") && "Tipo_Cosecha" %in% names(df_cosecha_base)) {
  cat("[COSECHA INICIAL] Base cargada al día directamente desde RData maestro (Arranque ultra-rápido)\n")
  df_cosecha_base
} else {
  integrar_sap_en_historico(df_cosecha_base, ruta_raiz_arandano, campana_actual = 2026)
}
tb_ini_flag <- es_fila_testblock(df_cosecha_inicial_full$Sector, df_cosecha_inicial_full$Módulo, df_cosecha_inicial_full$Tipo_Cosecha, df_cosecha_inicial_full$Es_Testblock)
df_cosecha_inicial_full$Es_Testblock <- tb_ini_flag
df_cosecha_inicial_full$Tipo_Cosecha <- ifelse(tb_ini_flag, "Testblock", "Comercial")

# Base 100% Comercial para tabla, ajustes, frecuencias y rendimientos; Testblock aislado solo para KPI de Cosecha Real
df_cosecha_inicial       <- df_cosecha_inicial_full %>% filter(!Es_Testblock)
df_cosecha_testblock_ini <- df_cosecha_inicial_full %>% filter(Es_Testblock)


# ==============================================================================
# FUNCIÓN AUXILIAR: CARGAR PROYECCIÓN
# ==============================================================================
cargar_proyeccion <- function(ruta_raiz, campana, semana) {
  cat("\n=======================================================\n")
  cat("[DIAGNÓSTICO PROYECCIÓN] Iniciando búsqueda...\n")
  cat("  -> Campaña solicitada:", campana, "\n")
  cat("  -> Semana solicitada :", semana, "\n")
  
  # 1. Búsqueda instantánea en RData consolidado
  if (exists("df_proyecciones") && !is.null(df_proyecciones) && nrow(df_proyecciones) > 0) {
    df_sub <- df_proyecciones %>%
      filter(Semana == as.numeric(semana)) %>%
      mutate(
        Módulo_join = ifelse(
          !is.na(suppressWarnings(as.numeric(Módulo_join))),
          sub("\\.0$", "", sprintf("%.1f", round(suppressWarnings(as.numeric(Módulo_join)), 1))),
          trimws(as.character(Módulo_join))
        )
      ) %>%
      group_by(Sector_join, Módulo_join, Turno_join, Variedad_join) %>%
      summarise(Variedad = first(Variedad), Proyección = sum(Proyección, na.rm = TRUE), .groups = "drop")
    if (nrow(df_sub) > 0) {
      cat(sprintf("  -> [PROYECCIÓN RDATA] Semana %s cargada instantáneamente (%d registros)\n", semana, nrow(df_sub)))
      cat("=======================================================\n\n")
      return(df_sub %>% select(Sector_join, Módulo_join, Turno_join, Variedad_join, Variedad, Proyección))
    }
  }
  
  # 2. Respaldo por lectura de Excel en Google Drive
  tryCatch({
    nums <- regmatches(as.character(campana), gregexpr("[0-9]+", as.character(campana)))[[1]]
    if (length(nums) == 0) return(NULL)
    y1 <- as.numeric(nums[1]) %% 100
    
    carpetas <- list.dirs(ruta_raiz, recursive = FALSE, full.names = TRUE)
    patron_campana <- sprintf("Campa[nñ]a\\s*0*%02d\\s*[-/]", y1)
    carpeta_campana <- carpetas[grepl(patron_campana, basename(carpetas), ignore.case = TRUE)][1]
    
    if (is.na(carpeta_campana)) {
      patron_campana_alt <- sprintf("Campa[nñ]a.*%02d", y1)
      carpeta_campana <- carpetas[grepl(patron_campana_alt, basename(carpetas), ignore.case = TRUE)][1]
    }
    
    if (is.na(carpeta_campana) || !dir.exists(carpeta_campana)) return(NULL)
    
    dirs_sub1 <- list.dirs(carpeta_campana, recursive = FALSE, full.names = TRUE)
    dir_proy <- dirs_sub1[grepl("Proyecci[oó]n.*Semanal", basename(dirs_sub1), ignore.case = TRUE)][1]
    if (is.na(dir_proy)) return(NULL)
    
    dirs_sub2 <- list.dirs(dir_proy, recursive = FALSE, full.names = TRUE)
    dir_detallada <- dirs_sub2[grepl("Detallada", basename(dirs_sub2), ignore.case = TRUE)][1]
    if (is.na(dir_detallada)) return(NULL)
    
    archivos <- list.files(dir_detallada, pattern = "\\.(xlsx|xls)$", full.names = TRUE)
    archivos <- archivos[!grepl("^~\\$", basename(archivos))]
    
    patron_semana <- sprintf("Semana\\s*0*%s(\\b|[^0-9])", semana)
    archivos_semana <- archivos[grepl(patron_semana, basename(archivos), ignore.case = TRUE)]
    if (length(archivos_semana) == 0) return(NULL)
    
    versiones <- sapply(archivos_semana, function(f) {
      v <- regmatches(f, regexec("_V([0-9]+)", f, ignore.case = TRUE))[[1]]
      if (length(v) >= 2) as.numeric(v[2]) else 1
    })
    
    orden <- order(versiones, file.mtime(archivos_semana), decreasing = TRUE)
    archivo_sel <- archivos_semana[orden[1]]
    cat("  -> ARCHIVO PROYECCIÓN:", basename(archivo_sel), "\n")
    
    hojas <- excel_sheets(archivo_sel)
    hoja_calculo <- hojas[grepl("^c[aá]lculo$", trimws(hojas), ignore.case = TRUE)][1]
    if (is.na(hoja_calculo)) hoja_calculo <- hojas[grepl("calculo", hojas, ignore.case = TRUE)][1]
    if (is.na(hoja_calculo)) hoja_calculo <- hojas[1]
    
    raw_proy <- suppressMessages(read_excel(archivo_sel, sheet = hoja_calculo, skip = 3))
    nms <- names(raw_proy)
    
    col_s <- nms[grepl("^s$|^sector$", nms, ignore.case = TRUE)][1]
    if (is.na(col_s) && ncol(raw_proy) >= 3) col_s <- nms[3]
    
    col_m <- nms[grepl("^m$|^m[oó]dulo$", nms, ignore.case = TRUE)][1]
    if (is.na(col_m) && ncol(raw_proy) >= 4) col_m <- nms[4]
    
    col_t <- nms[grepl("^t$|^turno$", nms, ignore.case = TRUE)][1]
    if (is.na(col_t) && ncol(raw_proy) >= 5) col_t <- nms[5]
    
    col_v <- nms[grepl("^variedad$", nms, ignore.case = TRUE)][1]
    if (is.na(col_v) && ncol(raw_proy) >= 7) col_v <- nms[7]
    
    col_p <- nms[grepl("proyecci[oó]n", nms, ignore.case = TRUE) & !grepl("ha", nms, ignore.case = TRUE)][1]
    if (is.na(col_p) && ncol(raw_proy) >= 13) col_p <- nms[13]
    
    df_limpio <- raw_proy %>%
      filter(!is.na(.data[[col_s]]), !is.na(.data[[col_t]])) %>%
      mutate(
        Sector_join   = trimws(as.character(.data[[col_s]])),
        Módulo_join   = trimws(as.character(.data[[col_m]])),
        Turno_join    = trimws(as.character(.data[[col_t]])),
        var_limpia    = limpiar_variedad(.data[[col_v]]),
        Variedad_join = var_limpia,
        proy_num      = suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(.data[[col_p]]))))
      ) %>%
      filter(!is.na(proy_num)) %>%
      group_by(Sector_join, Módulo_join, Turno_join, Variedad_join) %>%
      summarise(
        Variedad   = first(var_limpia),
        Proyección = sum(proy_num, na.rm = TRUE), 
        .groups    = "drop"
      )
    
    cat("  -> Registros procesados de proyección:", nrow(df_limpio), "\n")
    cat("=======================================================\n\n")
    return(df_limpio)
  }, error = function(e) {
    cat("[ERROR PROYECCIÓN]:", e$message, "\n")
    return(NULL)
  })
}


# ==============================================================================
# DEFINICIÓN DE GRUPOS DE MÓDULOS
# ==============================================================================
grupos_modulos <- list(
  "COSECHA MOD 1,2y3 / 2026"   = c(1, 2, 3),
  "COSECHA - LABORES SECTOR 8" = c(4, 5, 6, 7, 12),
  "REPORTES COSECHA S9"        = c(8, 9, 11),
  "Cosecha M10-M13-M14"        = c(10, 13, 14)
)


# ==============================================================================
# UI
# ==============================================================================

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap');
      
      body {
        background-color: #F2F2F7;
        font-family: -apple-system, BlinkMacSystemFont, 'Inter', 'SF Pro Text', sans-serif;
        color: #1C1C1E;
        -webkit-font-smoothing: antialiased;
        padding-bottom: 60px;
      }
      
      .ios-header {
        background: rgba(255, 255, 255, 0.85);
        backdrop-filter: blur(20px);
        -webkit-backdrop-filter: blur(20px);
        border-bottom: 1px solid rgba(0, 0, 0, 0.08);
        padding: 16px 32px;
        margin-bottom: 24px;
        border-radius: 0 0 20px 20px;
        display: flex;
        justify-content: space-between;
        align-items: center;
      }
      
      .ios-title {
        font-size: 20px;
        font-weight: 700;
        letter-spacing: -0.5px;
        margin: 0;
        color: #000;
      }
      
      .ios-subtitle {
        font-size: 13px;
        color: #8E8E93;
        margin-top: 2px;
      }
      
      .ios-card {
        background: #FFFFFF;
        border-radius: 18px;
        padding: 20px 24px;
        border: 1px solid rgba(0, 0, 0, 0.06);
        box-shadow: 0 4px 20px -2px rgba(0, 0, 0, 0.03);
        margin-bottom: 20px;
      }
      
      .kpi-container {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(190px, 1fr));
        gap: 16px;
        margin-bottom: 20px;
      }
      
      .kpi-card {
        background: #FFFFFF;
        border-radius: 16px;
        padding: 18px 20px;
        border: 1px solid rgba(0, 0, 0, 0.05);
        box-shadow: 0 2px 8px rgba(0, 0, 0, 0.02);
      }
      
      .kpi-label {
        font-size: 11px;
        font-weight: 600;
        text-transform: uppercase;
        letter-spacing: 0.5px;
        color: #8E8E93;
        margin-bottom: 6px;
      }
      
      .kpi-value {
        font-size: 26px;
        font-weight: 700;
        letter-spacing: -0.6px;
        color: #1C1C1E;
        font-variant-numeric: tabular-nums;
      }
      
      .kpi-badge {
        display: inline-block;
        font-size: 11px;
        font-weight: 600;
        padding: 2px 8px;
        border-radius: 8px;
        background: rgba(0, 122, 255, 0.1);
        color: #007AFF;
        margin-top: 4px;
      }
      
      /* Segmented Control iOS Style Tabs */
      .nav-pills {
        background: #E5E5EA !important;
        padding: 4px !important;
        border-radius: 12px !important;
        display: inline-flex !important;
        gap: 4px !important;
        border: none !important;
        margin-bottom: 16px !important;
      }
      
      .nav-pills > li > a {
        border-radius: 9px !important;
        padding: 8px 20px !important;
        font-size: 13px !important;
        font-weight: 600 !important;
        color: #8E8E93 !important;
        background: transparent !important;
        border: none !important;
        transition: all 0.2s ease !important;
      }
      
      .nav-pills > li > a:hover {
        color: #1C1C1E !important;
        background: rgba(255, 255, 255, 0.5) !important;
      }
      
      .nav-pills > li.active > a,
      .nav-pills > li.active > a:hover,
      .nav-pills > li.active > a:focus {
        background: #FFFFFF !important;
        color: #1C1C1E !important;
        box-shadow: 0 3px 10px rgba(0, 0, 0, 0.08) !important;
        border: none !important;
      }

      /* Barra superior derecha a la altura de los selectores de pestañas */
      .tabs-top-right-bar {
        position: absolute;
        top: 20px;
        right: 24px;
        z-index: 100;
        display: inline-flex;
        align-items: center;
        gap: 8px;
      }
      
      .unit-switch-inline {
        display: inline-flex;
        align-items: center;
        gap: 8px;
        margin-right: 4px;
      }
      
      .unit-switch-label {
        font-size: 12px;
        font-weight: 600;
        color: #8E8E93;
        margin: 0;
      }
      
      .ios-segmented-switch .shiny-input-container {
        margin: 0 !important;
        width: auto !important;
      }
      
      .ios-segmented-switch .shiny-options-group {
        background: #E5E5EA;
        padding: 3px;
        border-radius: 10px;
        display: inline-flex;
        gap: 2px;
      }
      
      .ios-segmented-switch .radio-inline {
        margin: 0 !important;
        padding: 0 !important;
      }
      
      .ios-segmented-switch .radio-inline input[type='radio'] {
        display: none;
      }
      
      .ios-segmented-switch .radio-inline span {
        display: inline-block;
        padding: 6px 14px;
        font-size: 12.5px;
        font-weight: 600;
        color: #8E8E93;
        border-radius: 8px;
        cursor: pointer;
        transition: all 0.2s ease;
        user-select: none;
      }
      
      .ios-segmented-switch .radio-inline input[type='radio']:checked + span {
        background: #FFFFFF;
        color: #007AFF;
        box-shadow: 0 2px 8px rgba(0, 0, 0, 0.10);
      }

      .agrupar-dropdown-wrapper {
        position: relative;
        display: inline-block;
      }

      .btn-agrupar-por, .btn-config-frecuencias {
        display: inline-flex;
        align-items: center;
        gap: 6px;
        height: 38px;
        padding: 0 14px;
        box-sizing: border-box;
        border-radius: 10px;
        font-size: 12px;
        font-weight: 600;
        background: #F2F2F7;
        color: #1C1C1E;
        border: 1px solid rgba(0, 0, 0, 0.10);
        box-shadow: 0 1px 2px rgba(0, 0, 0, 0.04);
        transition: all 0.18s ease;
        cursor: pointer;
        white-space: nowrap;
      }

      .btn-agrupar-por:hover, .btn-agrupar-por:focus,
      .btn-config-frecuencias:hover, .btn-config-frecuencias:focus {
        background: #E5E5EA !important;
        color: #007AFF !important;
        border-color: rgba(0, 122, 255, 0.30) !important;
        outline: none !important;
      }

      .btn-agrupar-por.active-grouping {
        background: rgba(0, 122, 255, 0.10) !important;
        color: #007AFF !important;
        border-color: rgba(0, 122, 255, 0.35) !important;
      }

      .btn-agrupar-por i, .btn-config-frecuencias i {
        color: #636366;
        font-size: 13px;
        transition: transform 0.25s ease, color 0.18s ease;
      }

      .btn-agrupar-por.active-grouping i,
      .btn-agrupar-por:hover i,
      .btn-config-frecuencias:hover i {
        color: #007AFF;
      }

      .btn-config-frecuencias:hover i {
        transform: rotate(65deg);
      }

      .btn-agrupar-por .chevron-agrupar {
        font-size: 10px !important;
        margin-left: 1px;
      }

      .btn-agrupar-por.open .chevron-agrupar {
        transform: rotate(180deg);
      }

      .badge-agrupar-dims {
        font-size: 10.5px;
        font-weight: 700;
        padding: 1px 6px;
        border-radius: 8px;
        background: rgba(0, 0, 0, 0.07);
        color: #3A3A3C;
      }

      .btn-agrupar-por.active-grouping .badge-agrupar-dims {
        background: #007AFF;
        color: #FFFFFF;
      }

      .agrupar-dropdown-menu {
        display: none;
        position: absolute;
        top: calc(100% + 6px);
        right: 0;
        width: 245px;
        background: #FFFFFF;
        border-radius: 14px;
        border: 1px solid rgba(0, 0, 0, 0.09);
        box-shadow: 0 12px 32px rgba(0, 0, 0, 0.14);
        padding: 12px 14px;
        z-index: 2050;
        user-select: none;
      }

      .agrupar-dropdown-menu.show {
        display: block;
        animation: fadeInAgrupar 0.15s ease-out;
      }

      @keyframes fadeInAgrupar {
        from { opacity: 0; transform: translateY(-4px); }
        to { opacity: 1; transform: translateY(0); }
      }

      .agrupar-menu-header {
        display: flex;
        align-items: center;
        justify-content: space-between;
        margin-bottom: 4px;
      }

      .agrupar-menu-title {
        font-size: 13px;
        font-weight: 700;
        color: #1C1C1E;
      }

      .btn-agrupar-reset {
        background: none;
        border: none;
        padding: 2px 6px;
        font-size: 11px;
        font-weight: 600;
        color: #007AFF;
        border-radius: 6px;
        cursor: pointer;
      }

      .btn-agrupar-reset:hover {
        background: rgba(0, 122, 255, 0.08);
      }

      .agrupar-menu-subtitle {
        font-size: 11px;
        color: #8E8E93;
        margin-bottom: 10px;
        line-height: 1.3;
      }

      .agrupar-switches-list {
        display: flex;
        flex-direction: column;
        gap: 6px;
      }

      .agrupar-switch-row {
        display: flex;
        align-items: center;
        justify-content: space-between;
        padding: 6px 10px;
        border-radius: 10px;
        background: #F8F9FB;
        border: 1px solid rgba(0, 0, 0, 0.05);
        margin: 0 !important;
        cursor: pointer;
        transition: background 0.15s ease, border-color 0.15s ease;
      }

      .agrupar-switch-row:hover {
        background: #F0F2F7;
      }

      .agrupar-dim-info {
        display: flex;
        align-items: center;
        gap: 8px;
        font-size: 12.5px;
        font-weight: 600;
        color: #1C1C1E;
      }

      .agrupar-dim-info i {
        width: 15px;
        text-align: center;
        color: #007AFF;
        font-size: 12px;
      }

      .agrupar-switch-row input.chk-agrupar-item {
        display: none;
      }

      .ios-switch-toggle {
        position: relative;
        display: inline-block !important;
        width: 34px !important;
        height: 20px !important;
        background: #E5E5EA !important;
        border-radius: 20px !important;
        transition: background 0.2s ease !important;
        flex-shrink: 0 !important;
        vertical-align: middle !important;
        box-sizing: border-box !important;
        cursor: pointer !important;
      }

      .ios-switch-toggle::after {
        content: '';
        position: absolute;
        top: 2px;
        left: 2px;
        width: 16px;
        height: 16px;
        background: #FFFFFF;
        border-radius: 50%;
        box-shadow: 0 1px 3px rgba(0, 0, 0, 0.25);
        transition: transform 0.2s ease;
      }

      .agrupar-switch-row input.chk-agrupar-item:checked + .ios-switch-toggle {
        background: #34C759;
      }

      .agrupar-switch-row input.chk-agrupar-item:checked + .ios-switch-toggle::after {
        transform: translateX(14px);
      }

      .modal-content {
        border-radius: 16px !important;
        border: 1px solid rgba(0,0,0,0.08) !important;
        box-shadow: 0 18px 40px rgba(0,0,0,0.16) !important;
      }

      .modal-header {
        border-bottom: 1px solid #F2F2F7 !important;
        padding: 16px 20px 12px 20px !important;
      }

      .modal-footer {
        border-top: 1px solid #F2F2F7 !important;
        padding: 12px 20px !important;
      }

      .frec-var-card .form-group { margin-bottom: 0 !important; }

      .frec-var-card select.form-control {
        height: 30px !important;
        padding: 2px 8px !important;
        font-size: 12px !important;
        font-weight: 700 !important;
        border-radius: 8px !important;
        border: 1px solid rgba(0,0,0,0.12) !important;
        background-color: #F8F9FB !important;
        color: #1C1C1E !important;
        cursor: pointer;
      }

      .frec-var-card select.form-control:focus {
        border-color: #007AFF !important;
        background-color: #FFFFFF !important;
        box-shadow: 0 0 0 2px rgba(0,122,255,0.15) !important;
      }

      @media (max-width: 992px) {
        .tabs-top-right-bar {
          position: static !important;
          margin-bottom: 12px;
          justify-content: flex-end;
          width: 100%;
        }
      }

      /* Slicer Card iOS */
      .slicer-card {
        background: #F8F9FB;
        border-radius: 14px;
        padding: 16px;
        border: 1px solid rgba(0, 0, 0, 0.06);
      }
      
      .slicer-title {
        font-size: 13px;
        font-weight: 700;
        color: #1C1C1E;
        margin-bottom: 14px;
        display: flex;
        align-items: center;
        gap: 6px;
      }
      
      .slicer-indicator {
        display: inline-block;
        width: 4px;
        height: 14px;
        background: #007AFF;
        border-radius: 2px;
      }
      
      /* Controles iOS Form */
      select.form-control, input.form-control, .selectize-input {
        border-radius: 10px !important;
        border: 1px solid rgba(0, 0, 0, 0.12) !important;
        font-size: 13.5px !important;
        padding: 8px 12px !important;
        background-color: #F9F9FB !important;
        box-shadow: none !important;
        height: 38px !important;
      }
      
      .selectize-control.form-control {
        border: none !important;
        background: transparent !important;
        padding: 0 !important;
        height: auto !important;
        box-shadow: none !important;
      }
      
      .selectize-control.multi .selectize-input {
        height: auto !important;
        min-height: 38px !important;
        padding: 5px 32px 5px 12px !important;
        display: flex !important;
        flex-wrap: wrap !important;
        align-items: center !important;
        position: relative !important;
      }
      
      .selectize-input input, 
      .selectize-input input:focus {
        border: none !important;
        outline: none !important;
        box-shadow: none !important;
        background: transparent !important;
        padding: 0 !important;
        margin: 0 !important;
        font-family: inherit !important;
        font-size: 13.5px !important;
        color: #1C1C1E !important;
        display: inline-block !important;
        height: auto !important;
        max-width: 100% !important;
      }
      
      .selectize-input input::placeholder {
        color: #1C1C1E !important;
        opacity: 0.7 !important;
        font-size: 13.5px !important;
        font-weight: 400 !important;
      }
      
      .selectize-control.multi .selectize-input:after {
        content: ' ';
        display: block;
        position: absolute;
        top: 50%;
        right: 14px;
        margin-top: -3px;
        width: 0;
        height: 0;
        border-style: solid;
        border-width: 5px 5px 0 5px;
        border-color: #636366 transparent transparent transparent;
        pointer-events: none;
      }
      
      .selectize-control.multi .selectize-input.dropdown-active:after {
        margin-top: -4px;
        border-width: 0 5px 5px 5px;
        border-color: transparent transparent #636366 transparent;
      }
      
      .selectize-control.multi .selectize-input > div {
        background: rgba(0, 122, 255, 0.12) !important;
        color: #007AFF !important;
        border-radius: 7px !important;
        padding: 2px 8px !important;
        font-size: 12px !important;
        font-weight: 600 !important;
        margin: 2px 4px 2px 0 !important;
        border: none !important;
        display: inline-flex !important;
        align-items: center !important;
      }
      
      .selectize-control.multi .selectize-input [data-value] .remove {
        border-left: 1px solid rgba(0, 122, 255, 0.25) !important;
        color: #007AFF !important;
        margin-left: 6px !important;
        padding-left: 5px !important;
        font-weight: 700 !important;
        text-decoration: none !important;
        opacity: 0.7;
      }
      
      .selectize-control.multi .selectize-input [data-value] .remove:hover {
        opacity: 1;
        background: transparent !important;
      }
      
      div.selectize-dropdown, 
      div.selectize-dropdown.form-control,
      div.selectize-dropdown.multi {
        height: auto !important;
        min-height: auto !important;
        max-height: 280px !important;
        border-radius: 12px !important;
        box-shadow: 0 12px 32px rgba(0, 0, 0, 0.15) !important;
        border: 1px solid rgba(0, 0, 0, 0.12) !important;
        background-color: #FFFFFF !important;
        background: #FFFFFF !important;
        padding: 6px !important;
        margin-top: 4px !important;
        z-index: 99999 !important;
        overflow-y: auto !important;
        opacity: 1 !important;
      }
      
      div.selectize-dropdown .selectize-dropdown-content {
        max-height: 260px !important;
        overflow-y: auto !important;
        background: #FFFFFF !important;
        background-color: #FFFFFF !important;
      }
      
      div.selectize-dropdown .option {
        padding: 8px 12px !important;
        font-size: 13px !important;
        border-radius: 8px !important;
        color: #1C1C1E !important;
        cursor: pointer !important;
        background: transparent !important;
        transition: background 0.15s ease !important;
      }
      
      div.selectize-dropdown .option.active,
      div.selectize-dropdown .option:hover {
        background-color: rgba(0, 122, 255, 0.08) !important;
        color: #007AFF !important;
        font-weight: 600 !important;
      }
      
      div.selectize-dropdown .option.selected {
        background-color: rgba(0, 122, 255, 0.15) !important;
        color: #007AFF !important;
        font-weight: 600 !important;
      }
      
      .selectize-input.focus, .form-control:focus {
        border-color: #007AFF !important;
        box-shadow: 0 0 0 3px rgba(0, 122, 255, 0.15) !important;
        background-color: #FFFFFF !important;
      }
      
      label {
        font-size: 12px;
        font-weight: 600;
        color: #636366;
        margin-bottom: 6px;
      }
      
      table.dataTable {
        border-collapse: separate !important;
        border-spacing: 0 !important;
        font-size: 12.5px !important;
        font-variant-numeric: tabular-nums;
      }
      
      table.dataTable thead th {
        background-color: #F8F9FA !important;
        color: #48484A !important;
        font-weight: 600 !important;
        border-bottom: 1px solid rgba(0, 0, 0, 0.08) !important;
        padding: 12px 10px !important;
        font-size: 11.5px;
        letter-spacing: 0.2px;
      }
      
      table.dataTable tbody td {
        padding: 9px 10px !important;
        border-bottom: 1px solid rgba(0, 0, 0, 0.04) !important;
        color: #1C1C1E;
      }
      
      table.dataTable tbody tr:hover {
        background-color: rgba(0, 122, 255, 0.03) !important;
      }
      
      table.dataTable tfoot th {
        background-color: #F2F2F7 !important;
        color: #1C1C1E !important;
        font-weight: 700 !important;
        border-top: 2px solid rgba(0, 0, 0, 0.10) !important;
        padding: 10px 10px !important;
        font-size: 12px !important;
      }

      /* Botones de Acción y Controles estilo Emil Kowalski */
      .btn-descargar-excel {
        display: inline-flex !important;
        align-items: center !important;
        gap: 6px !important;
        height: 38px !important;
        padding: 0 14px !important;
        box-sizing: border-box !important;
        border-radius: 10px !important;
        font-size: 12px !important;
        font-weight: 600 !important;
        background: #FFFFFF !important;
        color: #1C1C1E !important;
        border: 1px solid rgba(0, 0, 0, 0.12) !important;
        box-shadow: 0 1px 2px rgba(0, 0, 0, 0.04) !important;
        text-decoration: none !important;
        transition: all 0.18s cubic-bezier(0.16, 1, 0.3, 1) !important;
        cursor: pointer !important;
        white-space: nowrap !important;
      }
      .btn-descargar-excel i {
        color: #34C759 !important;
        font-size: 13px !important;
        transition: transform 0.2s cubic-bezier(0.16, 1, 0.3, 1) !important;
      }
      .btn-descargar-excel:hover, .btn-descargar-excel:focus {
        background: #F2F2F7 !important;
        color: #1b7a36 !important;
        border-color: rgba(52, 199, 89, 0.40) !important;
        transform: translateY(-1px);
        box-shadow: 0 3px 8px rgba(52, 199, 89, 0.18) !important;
        text-decoration: none !important;
      }
      .btn-descargar-excel:active {
        transform: translateY(0) scale(0.98) !important;
        box-shadow: 0 1px 2px rgba(0, 0, 0, 0.04) !important;
      }
      .btn-descargar-excel:hover i {
        transform: scale(1.15);
      }
      
      /* Range Picker Conectado Emil Kowalski (Desde / Hasta) */
      .range-picker-connected {
        display: inline-flex;
        align-items: center;
        height: 38px;
        background: #FFFFFF;
        border: 1px solid rgba(0, 0, 0, 0.12);
        border-radius: 10px;
        padding: 0 10px 0 12px;
        box-shadow: 0 1px 2px rgba(0, 0, 0, 0.04);
        transition: all 0.2s cubic-bezier(0.16, 1, 0.3, 1);
        box-sizing: border-box;
      }
      .range-picker-connected:hover {
        border-color: rgba(0, 0, 0, 0.24);
        box-shadow: 0 2px 6px rgba(0, 0, 0, 0.06);
      }
      .range-picker-connected:focus-within {
        border-color: #007AFF;
        box-shadow: 0 0 0 3px rgba(0, 122, 255, 0.15);
      }
      .range-picker-icon {
        display: inline-flex;
        align-items: center;
        color: #8E8E93;
        font-size: 13px;
        margin-right: 8px;
        pointer-events: none;
      }
      .range-picker-segment {
        display: inline-flex;
        align-items: center;
        gap: 4px;
      }
      .range-picker-prefix {
        font-size: 11px;
        font-weight: 700;
        color: #8E8E93;
        text-transform: uppercase;
        letter-spacing: 0.4px;
        user-select: none;
        pointer-events: none;
        white-space: nowrap;
      }
      .range-picker-segment .form-group {
        margin: 0 !important;
        margin-bottom: 0 !important;
        padding: 0 !important;
        display: inline-block !important;
      }
      .range-picker-segment select.form-control {
        border: none !important;
        background-color: transparent !important;
        box-shadow: none !important;
        outline: none !important;
        height: 32px !important;
        line-height: 32px !important;
        padding: 0 18px 0 3px !important;
        font-size: 12px !important;
        font-weight: 600 !important;
        color: #1C1C1E !important;
        cursor: pointer;
        -webkit-appearance: none;
        -moz-appearance: none;
        appearance: none;
        background-image: url('data:image/svg+xml;charset=UTF-8,%3Csvg xmlns=%22http://www.w3.org/2000/svg%22 width=%2210%22 height=%226%22 viewBox=%220 0 10 6%22%3E%3Cpath fill=%22none%22 stroke=%22%238E8E93%22 stroke-linecap=%22round%22 stroke-linejoin=%22round%22 stroke-width=%221.5%22 d=%22M1 1l4 4 4-4%22/%3E%3C/svg%3E') !important;
        background-repeat: no-repeat !important;
        background-position: right 2px center !important;
        transition: color 0.15s ease;
      }
      .range-picker-segment select.form-control:hover,
      .range-picker-segment select.form-control:focus {
        color: #007AFF !important;
        background-image: url('data:image/svg+xml;charset=UTF-8,%3Csvg xmlns=%22http://www.w3.org/2000/svg%22 width=%2210%22 height=%226%22 viewBox=%220 0 10 6%22%3E%3Cpath fill=%22none%22 stroke=%22%23007AFF%22 stroke-linecap=%22round%22 stroke-linejoin=%22round%22 stroke-width=%221.5%22 d=%22M1 1l4 4 4-4%22/%3E%3C/svg%3E') !important;
      }
      .range-picker-divider {
        width: 1px;
        height: 18px;
        background: rgba(0, 0, 0, 0.10);
        margin: 0 8px;
        pointer-events: none;
      }
      
      .btn-ajustar-semana {
        height: 38px !important;
        padding: 0 15px !important;
        box-sizing: border-box !important;
        border-radius: 10px !important;
        background: linear-gradient(135deg, #FF9500 0%, #FF5E3A 100%) !important;
        color: #FFFFFF !important;
        font-weight: 600 !important;
        font-size: 12px !important;
        border: none !important;
        display: inline-flex !important;
        align-items: center !important;
        gap: 6px !important;
        box-shadow: 0 1px 2px rgba(0, 0, 0, 0.08), 0 3px 10px rgba(255, 149, 0, 0.30) !important;
        transition: all 0.18s cubic-bezier(0.16, 1, 0.3, 1) !important;
        cursor: pointer;
        white-space: nowrap !important;
      }
      .btn-ajustar-semana:hover, .btn-ajustar-semana:focus {
        transform: translateY(-1px);
        box-shadow: 0 4px 14px rgba(255, 149, 0, 0.42) !important;
        color: #FFFFFF !important;
      }
      .btn-ajustar-semana:active {
        transform: translateY(0) scale(0.98);
      }
      
      .btn-deshacer-ajuste {
        height: 38px !important;
        padding: 0 12px !important;
        box-sizing: border-box !important;
        border-radius: 10px !important;
        background: #FFFFFF !important;
        color: #8E8E93 !important;
        font-weight: 600 !important;
        font-size: 12px !important;
        border: 1px solid rgba(0, 0, 0, 0.12) !important;
        box-shadow: 0 1px 2px rgba(0, 0, 0, 0.04) !important;
        display: inline-flex !important;
        align-items: center !important;
        gap: 5px !important;
        transition: all 0.18s cubic-bezier(0.16, 1, 0.3, 1) !important;
        cursor: pointer;
        white-space: nowrap !important;
      }
      .btn-deshacer-ajuste:hover, .btn-deshacer-ajuste:focus {
        background: #F2F2F7 !important;
        color: #1C1C1E !important;
        border-color: rgba(0, 0, 0, 0.22) !important;
        transform: translateY(-1px);
      }
      .btn-deshacer-ajuste:active {
        transform: translateY(0) scale(0.98);
      }
      
      .btn-borrar-manuales:hover, .btn-borrar-manuales:focus {
        background: #FFF1F0 !important;
        color: #FF3B30 !important;
        border-color: rgba(255, 59, 48, 0.35) !important;
        transform: translateY(-1px);
      }
      
      .detalle-toolbar-legend {
        display: inline-flex;
        align-items: center;
        gap: 6px;
        flex-wrap: wrap;
      }
      .legend-pill {
        font-size: 11px;
        font-weight: 600;
        padding: 4px 9px;
        border-radius: 8px;
        white-space: nowrap;
        letter-spacing: -0.1px;
        display: inline-flex;
        align-items: center;
      }
      .legend-manual { background: rgba(88, 86, 214, 0.10); color: #5856D6; border: 1px solid rgba(88, 86, 214, 0.22); }
      .legend-ruta   { background: rgba(225, 29, 72, 0.10);  color: #E11D48; border: 1px solid rgba(225, 29, 72, 0.22); }
      .legend-auto   { background: rgba(245, 158, 11, 0.12); color: #D97706; border: 1px solid rgba(245, 158, 11, 0.25); }
      .legend-real   { background: rgba(0, 122, 255, 0.08);  color: #007AFF; border: 1px solid rgba(0, 122, 255, 0.20); }
      .legend-ajuste { background: rgba(52, 199, 89, 0.10);  color: #248A3D; border: 1px solid rgba(52, 199, 89, 0.22); }
      
      .badge-dia-mapa {
        display: inline-flex !important;
        align-items: center !important;
        gap: 4px !important;
        font-size: 10px !important;
        padding: 3px 6px !important;
        border-radius: 6px !important;
        font-weight: 600 !important;
        white-space: nowrap !important;
        flex-shrink: 0 !important;
        line-height: 1.25 !important;
        transition: transform 0.15s ease !important;
      }
      .badge-dia-mapa:hover {
        transform: translateY(-1px) !important;
      }
      .zoom-btn-mapa {
        width: 36px;
        height: 36px;
        border-radius: 8px;
        background: #FFFFFF;
        border: 1px solid rgba(0, 0, 0, 0.15);
        box-shadow: 0 2px 8px rgba(0, 0, 0, 0.12);
        font-size: 18px;
        font-weight: bold;
        color: #1C1C1E;
        cursor: pointer;
        display: flex;
        align-items: center;
        justify-content: center;
        transition: all 0.15s ease;
      }
      .zoom-btn-mapa:hover {
        background: #F2F2F7;
        transform: scale(1.05);
      }
      .zoom-btn-mapa:active {
        transform: scale(0.95);
      }
      #mapa-wrapper {
        position: relative;
        width: 100%;
        height: 620px;
        background: #F8FAFC;
        border-radius: 14px;
        overflow: hidden;
        border: 1px solid rgba(0, 0, 0, 0.08);
        box-shadow: inset 0 1px 4px rgba(0,0,0,0.03);
      }
      #mapaCanvasLectura {
        position: absolute;
        left: 0;
        top: 0;
        width: 100%;
        height: 100%;
        cursor: grab;
      }
      #mapaCanvasLectura:active {
        cursor: grabbing;
      }
      #mapa-tooltip {
        position: absolute;
        display: none;
        pointer-events: none;
        background: rgba(15, 23, 42, 0.94);
        color: #FFFFFF;
        padding: 10px 14px;
        border-radius: 8px;
        font-size: 12px;
        box-shadow: 0 4px 16px rgba(0, 0, 0, 0.3);
        z-index: 1000;
        backdrop-filter: blur(4px);
        line-height: 1.45;
        border: 1px solid rgba(255, 255, 255, 0.15);
      }

      /* =========================================================================
         ESTILOS CONCILIACIÓN CAMPO VS SAP
         ========================================================================= */
      .btn-descarga-conciliacion {
        background: #107C41 !important;
        color: #FFFFFF !important;
        border: none !important;
        border-radius: 9px !important;
        font-size: 12px !important;
        font-weight: 600 !important;
        padding: 6px 14px !important;
        box-shadow: 0 1px 3px rgba(16, 124, 65, 0.25) !important;
        transition: all 0.15s ease !important;
        display: inline-flex !important;
        align-items: center !important;
        gap: 6px !important;
      }
      .btn-descarga-conciliacion:hover {
        background: #0E6835 !important;
        transform: translateY(-1px);
        color: #FFFFFF !important;
      }
      .kpi-concil-card {
        padding: 10px 14px;
        border-radius: 10px;
        border: 1px solid rgba(0, 0, 0, 0.06);
        display: flex;
        flex-direction: column;
        gap: 2px;
      }
      .card-concil-ok {
        background: rgba(52, 199, 89, 0.08);
        border-color: rgba(52, 199, 89, 0.22);
      }
      .card-concil-ok .concil-card-val { color: #28A745; }
      .card-concil-sin-sap {
        background: rgba(255, 59, 48, 0.08);
        border-color: rgba(255, 59, 48, 0.22);
      }
      .card-concil-sin-sap .concil-card-val { color: #E02424; }
      .card-concil-sin-ruta {
        background: rgba(0, 122, 255, 0.08);
        border-color: rgba(0, 122, 255, 0.22);
      }
      .card-concil-sin-ruta .concil-card-val { color: #007AFF; }
      .card-concil-desfase {
        background: rgba(175, 82, 222, 0.08);
        border-color: rgba(175, 82, 222, 0.22);
      }
      .card-concil-desfase .concil-card-val { color: #9333EA; }
      .card-concil-proceso {
        background: rgba(142, 142, 147, 0.08);
        border-color: rgba(142, 142, 147, 0.22);
      }
      .card-concil-proceso .concil-card-val { color: #6B7280; }
      .concil-card-label {
        font-size: 11px;
        font-weight: 700;
        text-transform: uppercase;
        color: #8E8E93;
      }
      .concil-card-val {
        font-size: 20px;
        font-weight: 700;
        line-height: 1.2;
      }
      .concil-card-sub {
        font-size: 11px;
        color: #8E8E93;
      }
      .badge-concil-pill {
        display: inline-block;
        padding: 3px 8px;
        border-radius: 6px;
        font-size: 11px;
        font-weight: 700;
        white-space: nowrap;
      }
      .pill-ok { background: rgba(52, 199, 89, 0.14); color: #1E7E34; border: 1px solid rgba(52, 199, 89, 0.3); }
      .pill-sin-sap { background: rgba(255, 59, 48, 0.14); color: #D70015; border: 1px solid rgba(255, 59, 48, 0.3); }
      .pill-sin-ruta { background: rgba(0, 122, 255, 0.14); color: #0062CC; border: 1px solid rgba(0, 122, 255, 0.3); }
      .pill-desfase { background: rgba(175, 82, 222, 0.14); color: #7928CA; border: 1px solid rgba(175, 82, 222, 0.3); }
      .pill-proceso { background: rgba(142, 142, 147, 0.14); color: #4B5563; border: 1px solid rgba(142, 142, 147, 0.3); }
      .filtros-grid-container {
        display: grid;
        grid-template-columns: 105px 105px 1.3fr 1fr 1fr 1.2fr 90px;
        gap: 12px;
        align-items: flex-end;
      }
      .filtros-grid-container .form-group {
        margin-bottom: 0 !important;
      }
      .filtros-grid-container .filtro-item {
        min-width: 0;
      }
      .filtros-grid-container .btn-reset-col {
        display: flex;
        align-items: flex-end;
      }
      @media (max-width: 1200px) {
        .filtros-grid-container {
          grid-template-columns: repeat(4, 1fr);
        }
      }

      .btn-refresh-sap {
        background-color: rgba(0, 122, 255, 0.10) !important;
        color: #007AFF !important;
        border: 1px solid rgba(0, 122, 255, 0.20) !important;
        border-radius: 20px !important;
        padding: 6px 14px !important;
        font-size: 12px !important;
        font-weight: 600 !important;
        transition: all 0.2s ease !important;
      }
      .btn-refresh-sap:hover {
        background-color: rgba(0, 122, 255, 0.20) !important;
      }

      /* =========================================================================
         EMIL KOWALSKI 3-DOTS ACTION DROPDOWN
         ========================================================================= */
      .more-options-dropdown-wrapper {
        position: relative;
        display: inline-block;
      }

      .btn-more-options {
        display: inline-flex;
        align-items: center;
        justify-content: center;
        width: 38px;
        height: 38px;
        box-sizing: border-box;
        border-radius: 10px;
        background: #F2F2F7;
        color: #1C1C1E;
        border: 1px solid rgba(0, 0, 0, 0.10);
        box-shadow: 0 1px 2px rgba(0, 0, 0, 0.04);
        transition: all 0.18s cubic-bezier(0.16, 1, 0.3, 1);
        cursor: pointer;
        padding: 0;
      }

      .btn-more-options:hover, .btn-more-options:focus {
        background: #E5E5EA !important;
        color: #007AFF !important;
        border-color: rgba(0, 122, 255, 0.30) !important;
        transform: translateY(-1px);
        box-shadow: 0 3px 8px rgba(0, 0, 0, 0.08) !important;
        outline: none !important;
      }

      .btn-more-options.open {
        background: rgba(0, 122, 255, 0.10) !important;
        color: #007AFF !important;
        border-color: rgba(0, 122, 255, 0.35) !important;
      }

      .btn-more-options i {
        font-size: 15px;
        color: #636366;
        transition: transform 0.2s ease, color 0.18s ease;
      }

      .btn-more-options:hover i, .btn-more-options.open i {
        color: #007AFF;
      }

      .more-options-menu {
        display: none;
        position: absolute;
        top: calc(100% + 8px);
        right: 0;
        width: 340px;
        min-width: 320px;
        max-width: calc(100vw - 24px);
        box-sizing: border-box;
        background: rgba(255, 255, 255, 0.98);
        backdrop-filter: blur(20px);
        -webkit-backdrop-filter: blur(20px);
        border-radius: 14px;
        border: 1px solid rgba(0, 0, 0, 0.09);
        box-shadow: 0 16px 36px -8px rgba(0, 0, 0, 0.14), 0 4px 14px -2px rgba(0, 0, 0, 0.06);
        padding: 8px;
        z-index: 2100;
        user-select: none;
      }

      .more-options-menu.show {
        display: block;
        animation: fadeInMoreOptions 0.18s cubic-bezier(0.16, 1, 0.3, 1);
      }

      @keyframes fadeInMoreOptions {
        from { opacity: 0; transform: translateY(-6px) scale(0.98); }
        to { opacity: 1; transform: translateY(0) scale(1); }
      }

      .more-options-header {
        padding: 8px 10px 4px 10px;
        font-size: 10px;
        font-weight: 700;
        text-transform: uppercase;
        letter-spacing: 0.6px;
        color: #8E8E93;
      }

      .more-options-list {
        display: flex;
        flex-direction: column;
        gap: 2px;
      }

      .more-options-divider {
        height: 1px;
        background: rgba(0, 0, 0, 0.06);
        margin: 4px 6px;
      }

      .more-options-item-btn {
        display: flex !important;
        flex-direction: row !important;
        flex-wrap: nowrap !important;
        align-items: center !important;
        gap: 12px !important;
        width: 100% !important;
        text-align: left !important;
        padding: 8px 10px !important;
        background: transparent !important;
        border: none !important;
        border-radius: 9px !important;
        color: #1C1C1E !important;
        text-decoration: none !important;
        cursor: pointer !important;
        box-shadow: none !important;
        box-sizing: border-box !important;
        transition: background 0.15s ease, transform 0.1s ease !important;
      }

      .more-options-item-btn > span.action-label {
        display: flex !important;
        flex-direction: row !important;
        flex-wrap: nowrap !important;
        align-items: center !important;
        gap: 12px !important;
        width: 100% !important;
        min-width: 0 !important;
        text-align: left !important;
      }

      .more-options-item-btn > i.fa-download,
      .more-options-item-btn > i.fa {
        display: none !important;
      }

      .more-options-item-btn:hover {
        background: #F2F2F7 !important;
        color: #1C1C1E !important;
        text-decoration: none !important;
      }

      .more-options-item-btn:active {
        transform: scale(0.98) !important;
        background: #E5E5EA !important;
      }

      .more-options-item-btn:focus {
        outline: none !important;
        box-shadow: none !important;
        background: #F2F2F7 !important;
      }

      .menu-item-icon {
        width: 32px;
        height: 32px;
        border-radius: 8px;
        display: flex;
        align-items: center;
        justify-content: center;
        flex-shrink: 0;
      }

      .menu-item-icon i {
        font-size: 13.5px;
      }

      .menu-item-icon.icon-excel {
        background: rgba(52, 199, 89, 0.12);
        color: #34C759;
      }

      .menu-item-icon.icon-gear {
        background: rgba(0, 122, 255, 0.12);
        color: #007AFF;
      }

      .menu-item-icon.icon-sliders {
        background: rgba(175, 82, 222, 0.12);
        color: #AF52DE;
      }

      .menu-item-icon.icon-hourglass {
        background: rgba(255, 149, 0, 0.12);
        color: #FF9500;
      }

      .menu-item-content {
        display: flex !important;
        flex-direction: column !important;
        min-width: 0 !important;
        flex: 1 1 auto !important;
        line-height: 1.25 !important;
      }

      .menu-item-title {
        font-size: 13px !important;
        font-weight: 600 !important;
        color: #1C1C1E !important;
        white-space: nowrap !important;
      }

      .menu-item-desc {
        font-size: 10.5px !important;
        font-weight: 400 !important;
        color: #8E8E93 !important;
        line-height: 1.3 !important;
      }

      .opt-switch-row {
        display: flex !important;
        flex-direction: row !important;
        flex-wrap: nowrap !important;
        align-items: center !important;
        justify-content: space-between !important;
        gap: 12px !important;
        padding: 8px 10px !important;
        border-radius: 9px !important;
        cursor: pointer !important;
        margin: 0 !important;
        box-sizing: border-box !important;
        width: 100% !important;
        user-select: none !important;
        transition: background 0.15s ease !important;
      }

      .opt-switch-row:hover {
        background: #F2F2F7 !important;
      }

      .opt-switch-control {
        display: inline-flex !important;
        align-items: center !important;
        justify-content: center !important;
        flex-shrink: 0 !important;
        width: 34px !important;
        height: 20px !important;
        margin-left: 8px !important;
        position: relative !important;
      }

      .opt-switch-control input[type='checkbox'] {
        position: absolute !important;
        opacity: 0 !important;
        width: 0 !important;
        height: 0 !important;
        margin: 0 !important;
        pointer-events: none !important;
      }

      .opt-switch-control input[type='checkbox']:checked + .ios-switch-toggle {
        background: #34C759 !important;
      }

      .opt-switch-control input[type='checkbox']:checked + .ios-switch-toggle::after {
        transform: translateX(14px) !important;
      }

      /* =========================================================================
         FRECUENCIA TAB: CARDS & HEATMAP MATRIX STYLING
         ========================================================================= */
      .freq-chart-card {
        padding: 20px 24px;
        background: #FFFFFF;
        border-radius: 16px;
        border: 1px solid rgba(0, 0, 0, 0.08);
        box-shadow: 0 4px 16px rgba(0, 0, 0, 0.04);
      }

      .freq-card-header {
        display: flex;
        justify-content: space-between;
        align-items: flex-start;
        flex-wrap: wrap;
        gap: 12px;
        margin-bottom: 6px;
      }

      .freq-card-title {
        font-size: 16px;
        font-weight: 700;
        color: #1C1C1E;
        margin: 0 0 3px 0;
      }

      .freq-card-subtitle {
        font-size: 12px;
        color: #8E8E93;
        font-weight: 400;
      }

      .freq-badge-dias, .freq-badge-selecciona {
        font-size: 11px;
        font-weight: 600;
        padding: 4px 12px;
        border-radius: 20px;
        background: rgba(52, 199, 89, 0.12);
        color: #248A3D;
        white-space: nowrap;
      }

      .freq-card-hint {
        text-align: right;
        font-size: 11px;
        color: #8E8E93;
        margin-top: 4px;
        user-select: none;
      }

      .freq-heatmap-container {
        width: 100%;
        overflow-x: auto;
        margin-top: 14px;
        padding-bottom: 6px;
      }

      .freq-heatmap-table {
        width: 100%;
        border-collapse: separate;
        border-spacing: 4px;
        font-family: -apple-system, BlinkMacSystemFont, 'Inter', sans-serif;
      }

      .freq-heatmap-table th {
        font-size: 11px;
        font-weight: 600;
        color: #8E8E93;
        padding: 6px 4px;
        text-align: center;
        white-space: nowrap;
      }

      .freq-heatmap-table th.col-turno-header {
        text-align: left;
        padding-left: 8px;
        min-width: 130px;
      }

      .freq-heatmap-row {
        cursor: pointer;
        transition: transform 0.1s ease;
      }

      .freq-heatmap-row:hover .cell-turno-label {
        color: #007AFF;
      }

      .freq-heatmap-row.row-selected .cell-turno-label {
        color: #007AFF;
        font-weight: 700;
      }

      .freq-heatmap-row.row-selected .freq-cell {
        box-shadow: 0 0 0 2px #007AFF;
      }

      .cell-turno-label {
        font-size: 12px;
        font-weight: 600;
        color: #1C1C1E;
        padding: 6px 8px;
        white-space: nowrap;
        transition: color 0.15s ease;
      }

      .freq-cell {
        height: 28px;
        min-width: 36px;
        border-radius: 6px;
        font-size: 11px;
        font-weight: 600;
        text-align: center;
        vertical-align: middle;
        line-height: 28px;
        transition: all 0.15s ease;
      }

      .freq-cell-empty {
        background: #F4F4F6;
        color: #C7C7CC;
      }

      .freq-heatmap-footer {
        font-size: 11px;
        color: #8E8E93;
        margin-top: 12px;
      }

      .modo-frec-segmented {
        display: inline-flex;
        background: #E5E5EA;
        padding: 3px;
        border-radius: 9px;
        gap: 2px;
      }
      .btn-modo-frec-pill {
        border: none;
        background: transparent;
        color: #636366;
        font-size: 12px;
        font-weight: 600;
        padding: 5px 14px;
        border-radius: 7px;
        cursor: pointer;
        transition: all 0.15s ease;
      }
      .btn-modo-frec-pill:hover {
        color: #1C1C1E;
      }
      .btn-modo-frec-pill.active-auto {
        background: #007AFF;
        color: #FFFFFF;
        box-shadow: 0 1px 3px rgba(0, 122, 255, 0.28);
      }
      .btn-modo-frec-pill.active-manual {
        background: #34C759;
        color: #FFFFFF;
        box-shadow: 0 1px 3px rgba(52, 199, 89, 0.28);
      }
    ")),
    tags$script(HTML("
      $(document).ready(function() {
        function syncAgruparDims() {
          var selected = [];
          $('.chk-agrupar-item:checked').each(function() {
            selected.push($(this).val());
          });
          var totalDims = $('.chk-agrupar-item').length;
          $('#badge_agrupar_dims').text(selected.length + '/' + totalDims);
          if (selected.length < totalDims) {
            $('#btn_toggle_agrupar').addClass('active-grouping');
          } else {
            $('#btn_toggle_agrupar').removeClass('active-grouping');
          }
          if (window.Shiny && Shiny.setInputValue) {
            Shiny.setInputValue('dims_agrupar', selected);
          }
        }

        $(document).on('click', '#btn_toggle_agrupar', function(e) {
          e.stopPropagation();
          $('#menu_agrupar_por').toggleClass('show');
          $(this).toggleClass('open');
          $('#menu_mas_opciones').removeClass('show');
          $('#btn_toggle_mas_opciones').removeClass('open');
        });

        $(document).on('click', '#menu_agrupar_por', function(e) {
          e.stopPropagation();
        });

        $(document).on('click', '#btn_toggle_mas_opciones', function(e) {
          e.stopPropagation();
          $('#menu_mas_opciones').toggleClass('show');
          $(this).toggleClass('open');
          $('#menu_agrupar_por').removeClass('show');
          $('#btn_toggle_agrupar').removeClass('open');
        });

        $(document).on('click', '#menu_mas_opciones', function(e) {
          if ($(e.target).closest('a, button').length) {
            setTimeout(function() {
              $('#menu_mas_opciones').removeClass('show');
              $('#btn_toggle_mas_opciones').removeClass('open');
            }, 120);
          } else {
            e.stopPropagation();
          }
        });

        $(document).on('click', function() {
          $('#menu_agrupar_por').removeClass('show');
          $('#btn_toggle_agrupar').removeClass('open');
          $('#menu_mas_opciones').removeClass('show');
          $('#btn_toggle_mas_opciones').removeClass('open');
        });

        $(document).on('click', '.freq-heatmap-row', function() {
          var turnoKey = $(this).data('turno-key');
          if (window.Shiny && Shiny.setInputValue) {
            Shiny.setInputValue('frecuencia_turno_click', turnoKey, { priority: 'event' });
          }
        });

        $(document).on('change', '.chk-agrupar-item', function(e) {
          var totalChecked = $('.chk-agrupar-item:checked').length;
          if (totalChecked === 0) {
            $(this).prop('checked', true);
            if (window.Shiny && Shiny.setInputValue) {
              Shiny.setInputValue('alerta_minimo_agrupar', new Date().getTime(), { priority: 'event' });
            }
            return;
          }
          syncAgruparDims();
        });

        $(document).on('click', '#btn_agrupar_todos', function(e) {
          e.preventDefault();
          $('.chk-agrupar-item').prop('checked', true);
          syncAgruparDims();
        });

        $(document).on('change', '#switch_ajuste_cualquier_semana', function(e) {
          var val = $(this).is(':checked');
          if (window.Shiny && Shiny.setInputValue) {
            Shiny.setInputValue('switch_ajuste_cualquier_semana', val);
          }
        });

        $(document).on('shiny:connected', function() {
          syncAgruparDims();
          if (window.Shiny && Shiny.setInputValue) {
            var val = $('#switch_ajuste_cualquier_semana').length ? $('#switch_ajuste_cualquier_semana').is(':checked') : true;
            Shiny.setInputValue('switch_ajuste_cualquier_semana', val);
          }
        });
      });
    "))
  ),
  
  div(class = "ios-header",
      div(
        h1(class = "ios-title", "Control Semanal de Cosecha"),
        p(class = "ios-subtitle", "Arándano • Universo Completo (Proyectado vs Real)"),
        uiOutput("badge_origen_datos")
      ),
      div(style = "display: flex; align-items: center; gap: 10px;",
          actionButton("btn_refresh_sap", "Actualizar SAP", icon = icon("sync"), class = "btn-refresh-sap"),
          span(style = "font-size: 12px; background: #E5E5EA; padding: 6px 14px; border-radius: 20px; font-weight: 600; color: #3A3A3C;", 
               "Plataforma Proyecciones")
      )
  ),
  
  div(style = "max-width: 1450px; margin: 0 auto; padding: 0 16px;",
      div(class = "ios-card", style = "margin-bottom: 20px; padding: 18px 20px;",
          div(class = "filtros-grid-container",
              div(class = "filtro-item",
                  selectInput("filtro_campana", "Campaña", 
                              choices = sort(unique(df_cosecha_inicial$Campaña), decreasing = TRUE),
                              selected = max(df_cosecha_inicial$Campaña),
                              width = "100%")
              ),
              div(class = "filtro-item",
                  {
                    camp_max_ini <- max(df_cosecha_inicial$Campaña)
                    es_camp_act_ini <- grepl("2026|^26", as.character(camp_max_ini))
                    sem_hoy_ini     <- if (es_camp_act_ini) lubridate::isoweek(Sys.Date()) else numeric(0)
                    sems_disp_ini   <- sort(unique(df_cosecha_inicial$Semana[as.character(df_cosecha_inicial$Campaña) == as.character(camp_max_ini)]))
                    if (length(sem_hoy_ini) > 0 && !(sem_hoy_ini %in% sems_disp_ini)) {
                      sems_disp_ini <- sort(c(sems_disp_ini, sem_hoy_ini))
                    }
                    sem_def_ini <- if (length(sem_hoy_ini) > 0 && sem_hoy_ini %in% sems_disp_ini) sem_hoy_ini else tail(sems_disp_ini, 1)
                    selectInput("filtro_semana", "Semana", choices = sems_disp_ini, selected = sem_def_ini, width = "100%")
                  }
              ),
              div(class = "filtro-item",
                  selectizeInput("filtro_grupo", "Grupo", 
                                 choices = names(grupos_modulos), 
                                 selected = NULL,
                                 multiple = TRUE,
                                 width = "100%",
                                 options = list(
                                   placeholder = "Todos",
                                   plugins = list("remove_button")
                                 ))
              ),
              div(class = "filtro-item",
                  selectizeInput("filtro_modulo", "Módulo", 
                                 choices = NULL, 
                                 selected = NULL,
                                 multiple = TRUE,
                                 width = "100%",
                                 options = list(
                                   placeholder = "Todos",
                                   plugins = list("remove_button")
                                 ))
              ),
              div(class = "filtro-item",
                  selectizeInput("filtro_turno", "Turno", 
                                 choices = NULL, 
                                 selected = NULL,
                                 multiple = TRUE,
                                 width = "100%",
                                 options = list(
                                   placeholder = "Todos",
                                   plugins = list("remove_button")
                                 ))
              ),
              div(class = "filtro-item",
                  selectizeInput("filtro_variedad", "Variedad", 
                                 choices = NULL, 
                                 selected = NULL,
                                 multiple = TRUE,
                                 width = "100%",
                                 options = list(
                                   placeholder = "Todas",
                                   plugins = list("remove_button")
                                 ))
              ),
              div(class = "filtro-item btn-reset-col",
                  actionButton("btn_reset", "Limpiar", 
                               style = "width: 100%; border-radius: 10px; background: #E5E5EA; border: none; font-weight: 600; font-size: 13px; color: #1C1C1E; height: 38px; padding: 0;")
              )
          )
      ),
      
      div(class = "kpi-container",
          div(class = "kpi-card",
              div(class = "kpi-label", "Cosecha Real"),
              div(class = "kpi-value", textOutput("kpi_kilos_real")),
              uiOutput("ui_kpi_real_detalle")
          ),
          div(class = "kpi-card",
              div(class = "kpi-label", "Cosecha Ajustada"),
              div(class = "kpi-value", textOutput("kpi_kilos_cierre")),
              uiOutput("ui_kpi_ajuste_detalle")
          ),
          div(class = "kpi-card",
              div(class = "kpi-label", "Proyección Total"),
              div(class = "kpi-value", textOutput("kpi_proyeccion")),
              span(class = "kpi-badge", style = "background: rgba(255, 149, 0, 0.12); color: #FF9500;", "Kg Proyectados")
          ),
          div(class = "kpi-card",
              div(class = "kpi-label", "Área en Evaluación"),
              div(class = "kpi-value", textOutput("kpi_area")),
              span(class = "kpi-badge", textOutput("kpi_area_badge", inline = TRUE))
          ),
          div(class = "kpi-card",
              div(class = "kpi-label", "Cumplimiento General"),
              div(class = "kpi-value", textOutput("kpi_cumplimiento")),
              span(class = "kpi-badge", style = "background: rgba(52, 199, 89, 0.12); color: #34C759; border: 1px solid rgba(52, 199, 89, 0.25);", textOutput("kpi_cumplimiento_badge", inline = TRUE))
          )
      ),
      
      div(class = "ios-card", style = "position: relative;",
          # Barra de Controles en la esquina superior derecha (a la altura de los selectores de pestañas)
          div(class = "tabs-top-right-bar",
              # Agrupar por
              div(class = "agrupar-dropdown-wrapper",
                  tags$button(
                    id = "btn_toggle_agrupar",
                    type = "button",
                    class = "btn-agrupar-por",
                    title = "Agrupar tabla y evolución histórica por dimensiones",
                    icon("layer-group"),
                    tags$span("Agrupar por"),
                    tags$span(id = "badge_agrupar_dims", class = "badge-agrupar-dims", "5/6"),
                    icon("chevron-down", class = "chevron-agrupar")
                  ),
                  div(id = "menu_agrupar_por",
                      class = "agrupar-dropdown-menu",
                      div(class = "agrupar-menu-header",
                          tags$span(class = "agrupar-menu-title", "Agrupar por"),
                          tags$button(
                            id = "btn_agrupar_todos",
                            type = "button",
                            class = "btn-agrupar-reset",
                            "Marcar todos"
                          )
                      ),
                      div(class = "agrupar-menu-subtitle",
                          "Filtra primero y consolida según los checks activos:"
                      ),
                      div(class = "agrupar-switches-list",
                          tags$label(
                            class = "agrupar-switch-row",
                            div(class = "agrupar-dim-info", icon("compass"), tags$span("Sector")),
                            tags$input(type = "checkbox", class = "chk-agrupar-item", value = "Sector", checked = "checked"),
                            tags$span(class = "ios-switch-toggle")
                          ),
                          tags$label(
                            class = "agrupar-switch-row",
                            div(class = "agrupar-dim-info", icon("cubes"), tags$span("Módulo")),
                            tags$input(type = "checkbox", class = "chk-agrupar-item", value = "Módulo", checked = "checked"),
                            tags$span(class = "ios-switch-toggle")
                          ),
                          tags$label(
                            class = "agrupar-switch-row",
                            div(class = "agrupar-dim-info", icon("clock"), tags$span("Turno")),
                            tags$input(type = "checkbox", class = "chk-agrupar-item", value = "Turno", checked = "checked"),
                            tags$span(class = "ios-switch-toggle")
                          ),
                          tags$label(
                            class = "agrupar-switch-row",
                            div(class = "agrupar-dim-info", icon("faucet-drip"), tags$span("Válvula"), span(style = "font-size: 9px; font-weight: 700; color: #0E7A68; background: rgba(14, 122, 104, 0.12); padding: 2px 6px; border-radius: 6px; margin-left: 6px;", "Conteo / Frecuencia")),
                            tags$input(type = "checkbox", class = "chk-agrupar-item", value = "Válvula"),
                            tags$span(class = "ios-switch-toggle")
                          ),
                          tags$label(
                            class = "agrupar-switch-row",
                            div(class = "agrupar-dim-info", icon("seedling"), tags$span("Variedad")),
                            tags$input(type = "checkbox", class = "chk-agrupar-item", value = "Variedad", checked = "checked"),
                            tags$span(class = "ios-switch-toggle")
                          ),
                          tags$label(
                            class = "agrupar-switch-row",
                            div(class = "agrupar-dim-info", icon("leaf"), tags$span("Tipo")),
                            tags$input(type = "checkbox", class = "chk-agrupar-item", value = "Tipo", checked = "checked"),
                            tags$span(class = "ios-switch-toggle")
                          )
                      )
                  )
              ),
              # Menú Desplegable "Más Opciones" estilo Emil Kowalski
              div(class = "more-options-dropdown-wrapper",
                  tags$button(
                    id = "btn_toggle_mas_opciones",
                    type = "button",
                    class = "btn-more-options",
                    title = "Más opciones y configuraciones",
                    icon("ellipsis-vertical")
                  ),
                  div(id = "menu_mas_opciones",
                      class = "more-options-menu",
                      div(class = "more-options-header",
                          tags$span("Acciones y Configuración")
                      ),
                      div(class = "more-options-list",
                          # 1. Descargar Excel
                          downloadButton(
                            "btn_descarga_excel",
                            label = tagList(
                              div(class = "menu-item-icon icon-excel", icon("file-excel")),
                              div(class = "menu-item-content",
                                  tags$span(class = "menu-item-title", "Descargar Excel"),
                                  tags$span(class = "menu-item-desc", "Exportar tabla con detalle completo")
                              )
                            ),
                            class = "more-options-item-btn"
                          ),
                          div(class = "more-options-divider"),
                          # 2. Configurar Frecuencias
                          actionButton(
                            "btn_abrir_modal_frecuencias",
                            label = tagList(
                              div(class = "menu-item-icon icon-gear", icon("gear")),
                              div(class = "menu-item-content",
                                  tags$span(class = "menu-item-title", "Frecuencias"),
                                  tags$span(class = "menu-item-desc", "Modo Auto (por válvula) o Manual (por módulo)")
                              )
                            ),
                            class = "more-options-item-btn"
                          ),
                          # 3. % 2ª Pasada Rocío
                          actionButton(
                            "btn_abrir_modal_pct_rocio",
                            label = tagList(
                              div(class = "menu-item-icon icon-sliders", icon("sliders")),
                              div(class = "menu-item-content",
                                  tags$span(class = "menu-item-title", "% 2ª Pasada"),
                                  tags$span(class = "menu-item-desc", "Reparto Rocío por módulo (Lun/Mar)")
                              )
                            ),
                            class = "more-options-item-btn"
                          ),
                          # 4. Maduración
                          actionButton(
                            "btn_abrir_modal_maduracion",
                            label = tagList(
                              div(class = "menu-item-icon icon-hourglass", icon("hourglass-half")),
                              div(class = "menu-item-content",
                                  tags$span(class = "menu-item-title", "Maduración"),
                                  tags$span(class = "menu-item-desc", "Días de maduración por módulo y variedad")
                              )
                            ),
                            class = "more-options-item-btn"
                          ),
                          div(class = "more-options-divider"),
                          # 5. Switch para permitir ajuste en cualquier semana
                          tags$label(
                            class = "opt-switch-row",
                            title = "Permitir ajustar semanas anteriores o futuras además de la semana actual",
                            div(style = "display: flex; align-items: center; gap: 12px; min-width: 0; flex: 1 1 auto;",
                                div(class = "menu-item-icon", style = "background: rgba(255, 149, 0, 0.12); color: #FF9500;", icon("calendar-check")),
                                div(class = "menu-item-content",
                                    tags$span(class = "menu-item-title", "Ajustar cualquier semana"),
                                    tags$span(class = "menu-item-desc", "Desactivar bloqueo de semana actual")
                                )
                            ),
                            div(class = "opt-switch-control",
                                tags$input(type = "checkbox", id = "switch_ajuste_cualquier_semana", checked = "checked"),
                                tags$span(class = "ios-switch-toggle")
                            )
                          )
                      )
                  )
              )
          ),
          
          tabsetPanel(
            id = "tabs_navegacion",
            type = "pills",
            tabPanel("Detalle Semanal",
                     div(style = "margin-top: 14px;",
                         # Fila Superior: Título a la izquierda, Leyenda de Estados a la derecha
                         div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 12px; flex-wrap: wrap; gap: 10px;",
                             div(
                               div(style = "display: flex; align-items: center; gap: 8px;",
                                 h3(style = "font-size: 15px; font-weight: 700; margin: 0 0 3px 0; color: #1C1C1E;", "Detalle General por Sector, Módulo y Turno"),
                                 uiOutput("ui_detalle_badge_modo", inline = TRUE)
                               ),
                               span(style = "font-size: 11.5px; color: #8E8E93;", "Cifras en Kilogramos • Clic en celda para ver detalle, ingresar kilos o calcular por kg/ha y ha")
                             ),
                             div(class = "detalle-toolbar-legend",
                                 span(class = "legend-pill legend-manual", "🟣 Manual"),
                                 span(class = "legend-pill legend-ruta", "🔴 Ruta (Sin Kg SAP)"),
                                 span(class = "legend-pill legend-auto", "🟠 Auto (Ciclo)"),
                                 span(class = "legend-pill legend-real", "🔵 Real SAP"),
                                 span(class = "legend-pill legend-ajuste", "🟢 Ajuste / kg/ha")
                             )
                         ),
                         # Fila de Acciones: Selector de Rango Emil Kowalski (Desde / Hasta) y Botones de Ajuste
                         div(style = "display: flex; gap: 8px; align-items: center; flex-wrap: wrap; margin-bottom: 14px;",
                             div(class = "range-picker-connected",
                                 div(class = "range-picker-icon", icon("calendar-day")),
                                 div(class = "range-picker-segment",
                                     tags$span(class = "range-picker-prefix", "Desde:"),
                                     selectInput("ajustar_desde_dia", label = NULL,
                                                 choices = c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado"),
                                                 selected = {
                                                   w_num <- lubridate::wday(Sys.Date(), week_start = 1)
                                                   if (w_num > 6) "Lunes" else c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")[w_num]
                                                 },
                                                 selectize = FALSE,
                                                 width = "95px")
                                 ),
                                 div(class = "range-picker-divider"),
                                 div(class = "range-picker-segment",
                                     tags$span(class = "range-picker-prefix", "Hasta:"),
                                     selectInput("ajustar_hasta_dia", label = NULL,
                                                 choices = c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado"),
                                                 selected = "Sábado",
                                                 selectize = FALSE,
                                                 width = "95px")
                                 )
                             ),
                             actionButton("btn_ajustar_semana", "Ajustar Semana", class = "btn-ajustar-semana", icon = icon("calculator")),
                             actionButton("btn_deshacer_ajuste", "Deshacer Ajuste", class = "btn-deshacer-ajuste", icon = icon("rotate-left")),
                             actionButton("btn_borrar_manuales", "Borrar Ajuste Manual", class = "btn-deshacer-ajuste btn-borrar-manuales", icon = icon("trash-can")),
                             div(style = "display: flex; align-items: center; gap: 6px; margin-left: auto;",
                                 tags$span(style = "font-size: 11px; font-weight: 700; color: #8E8E93; text-transform: uppercase;", "Vista:"),
                                 div(class = "ios-segmented-switch",
                                     radioButtons(
                                         inputId  = "filtro_modo_detalle",
                                         label    = NULL,
                                         choices  = c("Real" = "real", "Ajustado" = "ajustado"),
                                         selected = "real",
                                         inline   = TRUE
                                     )
                                 )
                             )
                         ),
                         DTOutput("tabla_control_cosecha")
                     )
            ),
            # ------------------------------------------------------------------
            # TAB: RESUMEN (REAL VS AJUSTADO: VARIEDAD / MÓDULO / PROGRAMA)
            # ------------------------------------------------------------------
            tabPanel("Resumen",
                     div(style = "margin-top: 14px;",
                         # Toolbar de Resumen
                         div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 14px; flex-wrap: wrap; gap: 10px;",
                             div(
                               div(style = "display: flex; align-items: center; gap: 8px;",
                                 h3(style = "font-size: 15px; font-weight: 700; margin: 0 0 3px 0; color: #1C1C1E;", "Resumen de Cosecha: Real vs Ajustado"),
                                 span(class = "legend-pill legend-ajuste", "📊 Consolidado Semanal")
                               ),
                               span(style = "font-size: 11.5px; color: #8E8E93;", "Comparativo de kilos proyectados, reales y ajustados por día y acumulado • Cifras en Kilogramos")
                             ),
                             div(style = "display: flex; align-items: center; gap: 12px; flex-wrap: wrap; margin-left: auto;",
                                 # Selector de Alcance (Toda la semana vs Filtros activos)
                                 div(style = "display: flex; align-items: center; gap: 6px;",
                                     span(style = "font-size: 11px; font-weight: 700; color: #8E8E93; text-transform: uppercase;", "Alcance:"),
                                     div(class = "ios-segmented-switch",
                                         radioButtons(
                                             inputId  = "switch_alcance_resumen",
                                             label    = NULL,
                                             choices  = c("Semana Completa" = "toda", "Con Filtros" = "filtrada"),
                                             selected = "toda",
                                             inline   = TRUE
                                         )
                                     )
                                 ),
                                 # Switch de Formas (Variedad / Módulo y Variedad / Distribución Comercial)
                                 div(style = "display: flex; align-items: center; gap: 6px;",
                                     span(style = "font-size: 11px; font-weight: 700; color: #8E8E93; text-transform: uppercase;", "Forma:"),
                                     div(class = "ios-segmented-switch",
                                         radioButtons(
                                             inputId  = "switch_forma_resumen",
                                             label    = NULL,
                                             choices  = c("Por Variedad" = "variedad", 
                                                          "Por Módulo y Variedad" = "modulo_variedad", 
                                                          "Distribución Comercial" = "programa"),
                                             selected = "variedad",
                                             inline   = TRUE
                                         )
                                     )
                                 ),
                                 downloadButton("btn_descarga_resumen_excel", "Descargar Excel", class = "btn-descargar-excel")
                             )
                         ),
                         
                         # FORMA 1: Por Variedad
                         conditionalPanel(
                           condition = "input.switch_forma_resumen == 'variedad'",
                           DTOutput("tabla_resumen_variedad")
                         ),
                         
                         # FORMA 2: Por Módulo y Variedad
                         conditionalPanel(
                           condition = "input.switch_forma_resumen == 'modulo_variedad'",
                           DTOutput("tabla_resumen_modulo_variedad")
                         ),
                         
                         # FORMA 3: Distribución Comercial
                         conditionalPanel(
                           condition = "input.switch_forma_resumen == 'programa'",
                           div(style = "display: flex; flex-direction: column; gap: 18px;",
                               div(style = "background: #FFFFFF; border-radius: 14px; padding: 16px 20px; border: 1px solid rgba(0, 0, 0, 0.06); box-shadow: 0 1px 3px rgba(0,0,0,0.03);",
                                   div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 12px;",
                                       h4(style = "font-size: 13.5px; font-weight: 700; color: #1C1C1E; text-transform: uppercase; margin: 0;", 
                                          textOutput("titulo_resumen_programa_comp", inline = TRUE)),
                                       span(style = "font-size: 11.5px; color: #8E8E93; font-weight: 600;", "Comparativo Proyección vs Ajuste / Real")
                                   ),
                                   DTOutput("tabla_resumen_programa_comp"),
                                   
                                   # Sensibilidad de Escenarios
                                   div(style = "display: flex; gap: 24px; align-items: center; margin-top: 18px; padding: 12px 18px; background: rgba(0, 122, 255, 0.04); border-radius: 12px; border: 1px solid rgba(0, 122, 255, 0.12); flex-wrap: wrap;",
                                       div(
                                           span(style = "font-size: 11.5px; color: #64748B; font-weight: 600; display: block; margin-bottom: 2px;", "Escenario Pesimista (-10%)"),
                                           strong(style = "font-size: 16px; color: #DC2626; font-weight: 700;", textOutput("txt_escenario_pesimista", inline = TRUE))
                                       ),
                                       div(style = "width: 1px; height: 32px; background: rgba(0,0,0,0.08);"),
                                       div(
                                           span(style = "font-size: 11.5px; color: #64748B; font-weight: 600; display: block; margin-bottom: 2px;", "Escenario Optimista (+10%)"),
                                           strong(style = "font-size: 16px; color: #16A34A; font-weight: 700;", textOutput("txt_escenario_optimista", inline = TRUE))
                                       )
                                   )
                               ),
                               
                               div(style = "background: #FFFFFF; border-radius: 14px; padding: 16px 20px; border: 1px solid rgba(0, 0, 0, 0.06); box-shadow: 0 1px 3px rgba(0,0,0,0.03);",
                                   div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 12px;",
                                       h4(style = "font-size: 13.5px; font-weight: 700; color: #1C1C1E; text-transform: uppercase; margin: 0;", "DISTRIBUCIÓN DE KG - DETALLE SEMANAL POR DÍA"),
                                       span(style = "font-size: 11.5px; color: #8E8E93; font-weight: 600;", "Kilos por Día y Total de Semana")
                                   ),
                                   DTOutput("tabla_resumen_programa_dias")
                               )
                           )
                         )
                     )
            ),
            tabPanel("Evolución de Rendimiento",
                     div(style = "margin-top: 14px;",
                         fluidRow(
                           column(8,
                                  div(style = "background: #FFFFFF; border-radius: 14px; padding: 14px 18px; border: 1px solid rgba(0, 0, 0, 0.05);",
                                      div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px; flex-wrap: wrap; gap: 8px;",
                                          div(style = "display: flex; align-items: center; gap: 8px; flex-wrap: wrap;",
                                              h3(style = "font-size: 15px; font-weight: 700; margin: 0; color: #1C1C1E;", 
                                                 textOutput("titulo_grafico_historico", inline = TRUE)),
                                              uiOutput("ui_badge_agrupacion_hist", inline = TRUE),
                                              div(class = "unit-switch-inline", style = "margin-left: 6px;",
                                                  span(class = "unit-switch-label", "Visualización:"),
                                                  div(class = "ios-segmented-switch",
                                                      radioButtons(
                                                        inputId  = "switch_unidad",
                                                        label    = NULL,
                                                        choices  = c("Kg" = "kg", "Kg/ha" = "kgha"),
                                                        selected = "kg",
                                                        inline   = TRUE
                                                      )
                                                  )
                                              )
                                          ),
                                          div(style = "display: flex; align-items: center; gap: 14px; font-size: 12px; color: #636366; font-weight: 500;",
                                              span(HTML("<b style='color:#007AFF;'>───</b> Cosecha Real (SAP)")),
                                              span(HTML("<b style='color:#34C759;'>- - -</b> Cosecha Ajustada")),
                                              span(HTML("<b style='color:#FF9500;'>• • •</b> Proyectado Oficial"))
                                          )
                                      ),
                                      plotlyOutput("grafico_historico", height = "490px")
                                  )
                           ),
                           column(4,
                                  div(class = "slicer-card", style = "min-height: 546px; display: flex; flex-direction: column; justify-content: space-between;",
                                      div(
                                        div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 4px;",
                                            div(class = "slicer-title", style = "margin-bottom: 0;",
                                                span(class = "slicer-indicator", style = "background: #34C759;"),
                                                uiOutput("ui_titulo_dona_semana", inline = TRUE)
                                            ),
                                            span(class = "kpi-badge", style = "background: rgba(52, 199, 89, 0.12); color: #34C759; font-size: 10px; padding: 2px 7px;", "Semana Previa")
                                        ),
                                        div(style = "font-size: 11px; color: #8E8E93; margin-bottom: 8px;",
                                            textOutput("txt_subtitulo_dona")
                                        )
                                      ),
                                      div(style = "flex: 1; display: flex; align-items: center; justify-content: center;",
                                          plotlyOutput("grafico_dona_dias", height = "440px")
                                      )
                                  )
                           )
                         ),
                     )
            ),
            tabPanel("Histórico Conteo",
                     div(style = "margin-top: 14px;",
                         div(style = "background: #FFFFFF; border-radius: 14px; padding: 18px 22px; border: 1px solid rgba(0, 0, 0, 0.05); box-shadow: 0 2px 8px rgba(0,0,0,0.02);",
                             div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 14px; flex-wrap: wrap; gap: 12px;",
                                 div(style = "display: flex; align-items: center; gap: 10px; flex-wrap: wrap;",
                                     h3(style = "font-size: 16px; font-weight: 700; margin: 0; color: #1C1C1E;", 
                                        textOutput("titulo_grafico_conteo", inline = TRUE)),
                                     uiOutput("ui_badge_conteo_agrupacion", inline = TRUE)
                                 ),
                                 div(style = "display: flex; align-items: center; gap: 14px; flex-wrap: wrap;",
                                     # Switch Métrica (Por Planta por defecto)
                                     div(class = "unit-switch-inline",
                                         span(class = "unit-switch-label", "Métrica:"),
                                         div(class = "ios-segmented-switch",
                                             radioButtons(
                                               inputId  = "switch_conteo_metrica",
                                               label    = NULL,
                                               choices  = c("Por Planta (Promedio)" = "planta", "Total Órganos" = "total"),
                                               selected = "planta",
                                               inline   = TRUE
                                             )
                                         )
                                     ),
                                     # Switch Presentación (Agrupadas vs Apiladas)
                                     div(class = "unit-switch-inline",
                                         span(class = "unit-switch-label", "Diseño:"),
                                         div(class = "ios-segmented-switch",
                                             radioButtons(
                                               inputId  = "switch_conteo_barmode",
                                               label    = NULL,
                                               choices  = c("Lado a Lado" = "group", "Apilado" = "stack"),
                                               selected = "group",
                                               inline   = TRUE
                                             )
                                         )
                                     ),
                                     # Switch Órgano visible
                                     div(class = "unit-switch-inline",
                                         span(class = "unit-switch-label", "Mostrar:"),
                                         div(class = "ios-segmented-switch",
                                             radioButtons(
                                               inputId  = "switch_conteo_organo",
                                               label    = NULL,
                                               choices  = c("Botón y Flor" = "ambos", "Solo Botón" = "boton", "Solo Flor" = "flor"),
                                               selected = "ambos",
                                               inline   = TRUE
                                             )
                                         )
                                     ),
                                     # Leyenda de Colores
                                     div(style = "display: flex; align-items: center; gap: 12px; font-size: 12px; font-weight: 600;",
                                         span(HTML("<span style='display:inline-block; width:12px; height:12px; background-color:#9AC8E0; border:1px solid #7EB2CE; border-radius:3px; vertical-align:middle; margin-right:4px;'></span>Botón")),
                                         span(HTML("<span style='display:inline-block; width:12px; height:12px; background-color:#023B59; border-radius:3px; vertical-align:middle; margin-right:4px;'></span>Flor"))
                                     )
                                 )
                             ),
                             plotlyOutput("grafico_conteo_historico", height = "560px")
                         )
                     )
            ),
            tabPanel("Ruta de cosecha",
                     div(style = "margin-top: 14px;",
                         # Barra Superior de Control y KPIs
                         div(style = "background: #FFFFFF; border-radius: 14px; padding: 14px 18px; border: 1px solid rgba(0, 0, 0, 0.05); margin-bottom: 12px; box-shadow: 0 2px 8px rgba(0,0,0,0.03);",
                             div(style = "display: flex; justify-content: space-between; align-items: center; flex-wrap: wrap; gap: 12px;",
                                 # Selectores a la izquierda
                                 div(style = "display: flex; align-items: center; gap: 14px; flex-wrap: wrap;",
                                     div(style = "display: flex; flex-direction: column;",
                                         tags$label(style = "font-size: 11px; font-weight: 700; color: #8E8E93; text-transform: uppercase; margin-bottom: 3px;", "Semana del Mapa"),
                                         uiOutput("ui_filtro_semana_mapa")
                                     )
                                     ,
                                     div(style = "display: flex; flex-direction: column;",
                                         tags$label(style = "font-size: 11px; font-weight: 700; color: #8E8E93; text-transform: uppercase; margin-bottom: 3px;", "Pasada"),
                                         selectInput(
                                           inputId  = "filtro_pasada_mapa",
                                           label    = NULL,
                                           choices  = c("1ª Pasada" = "1", "2ª Pasada" = "2", "Ambas (1ª + 2ª)" = "todas"),
                                           selected = "1",
                                           width    = "135px"
                                         )
                                     )
                                      ,
                                      div(style = "display: flex; flex-direction: column;",
                                          tags$label(style = "font-size: 11px; font-weight: 700; color: #8E8E93; text-transform: uppercase; margin-bottom: 3px;", "Vista"),
                                          div(class = "ios-segmented-switch",
                                              radioButtons(
                                                  inputId  = "filtro_modo_mapa",
                                                  label    = NULL,
                                                  choices  = c("Real" = "real", "Ajustado" = "ajustado"),
                                                  selected = "real",
                                                  inline   = TRUE
                                              )
                                          )
                                      )
                                 ),
                                 # KPIs Principales a la derecha
                                 div(style = "display: flex; align-items: center; gap: 20px; flex-wrap: wrap;",
                                     div(style = "text-align: right;",
                                         div(style = "font-size: 11px; color: #8E8E93; font-weight: 600; text-transform: uppercase;", "Área Cosechada"),
                                         div(id = "mapa_kpi_ha", style = "font-size: 19px; font-weight: 700; color: #1C1C1E;", "0.00 ha")
                                     ),
                                     div(style = "text-align: right; border-left: 1px solid #E5E5EA; padding-left: 16px;",
                                         div(style = "font-size: 11px; color: #8E8E93; font-weight: 600; text-transform: uppercase;", "Kilos Cosechados"),
                                         div(id = "mapa_kpi_kg", style = "font-size: 19px; font-weight: 700; color: #007AFF;", "0 kg")
                                     ),
                                     div(style = "text-align: right; border-left: 1px solid #E5E5EA; padding-left: 16px;",
                                         div(style = "font-size: 11px; color: #8E8E93; font-weight: 600; text-transform: uppercase;", "Rendimiento"),
                                         div(id = "mapa_kpi_rend", style = "font-size: 19px; font-weight: 700; color: #34C759;", "0 kg/ha")
                                     )
                                 )
                             ),
                              # Avance Diario (Badges con totales)
                              div(style = "display: flex; align-items: center; gap: 5px; flex-wrap: nowrap; overflow-x: auto; margin-top: 10px; padding-top: 8px; border-top: 1px solid rgba(0, 0, 0, 0.05);",
                                  span(style = "font-weight: 700; color: #8E8E93; font-size: 10.5px; margin-right: 2px; flex-shrink: 0; white-space: nowrap;", "Avance Diario:"),
                                  span(id = "mapa_badge_lunes",     class = "badge-dia-mapa", style = "background: rgba(255,0,0,0.1); color: #D32F2F; border: 1px solid rgba(255,0,0,0.3);", "● Lunes: 0 kg"),
                                  span(id = "mapa_badge_martes",    class = "badge-dia-mapa", style = "background: rgba(0,162,232,0.12); color: #0288D1; border: 1px solid rgba(0,162,232,0.3);", "● Martes: 0 kg"),
                                  span(id = "mapa_badge_miercoles", class = "badge-dia-mapa", style = "background: rgba(255,127,39,0.12); color: #E65100; border: 1px solid rgba(255,127,39,0.3);", "● Miércoles: 0 kg"),
                                  span(id = "mapa_badge_jueves",    class = "badge-dia-mapa", style = "background: rgba(255,235,59,0.22); color: #F57F17; border: 1px solid rgba(251,192,45,0.4);", "● Jueves: 0 kg"),
                                  span(id = "mapa_badge_viernes",   class = "badge-dia-mapa", style = "background: rgba(0,200,83,0.12); color: #2E7D32; border: 1px solid rgba(0,200,83,0.3);", "● Viernes: 0 kg"),
                                  span(id = "mapa_badge_sabado",    class = "badge-dia-mapa", style = "background: rgba(163,73,164,0.12); color: #7B1FA2; border: 1px solid rgba(163,73,164,0.3);", "● Sábado: 0 kg")
                              )
                         ),
                         # Contenedor Canvas 2D
                         div(id = "mapa-wrapper",
                             tags$canvas(id = "mapaCanvasLectura"),
                             # Badge Superior Flotante Dinámico
                             uiOutput("ui_mapa_badge_flotante"),
                             # Botones Flotantes de Zoom
                             div(style = "position: absolute; right: 16px; bottom: 16px; display: flex; flex-direction: column; gap: 6px; z-index: 100;",
                                 tags$button(type = "button", id = "btn_mapa_zoom_in",  class = "zoom-btn-mapa", "+"),
                                 tags$button(type = "button", id = "btn_mapa_zoom_out", class = "zoom-btn-mapa", "−")
                             ),
                             # Tooltip Flotante
                             div(id = "mapa-tooltip")
                         ),
                         # Motor Canvas JavaScript incrustado de forma segura
                         tags$script(HTML(paste0("const VALVES_MAPA = ", json_valvulas_mapa, ";\n", codigo_js_mapa_archivo)))
                     )
            ),
            # Pestaña Frecuencia (Evolución por turno & Turnos por semana)
            # Pestaña Conciliación Campo vs SAP (Auditoría de Cruce)
            tabPanel("Conciliación Campo vs SAP",
                     div(style = "margin-top: 14px; display: flex; flex-direction: column; gap: 14px;",
                         # Cabecera informativa y Regla D+1
                         div(style = "background: #FFFFFF; border-radius: 14px; padding: 16px 20px; border: 1px solid rgba(0, 0, 0, 0.05); box-shadow: 0 2px 8px rgba(0,0,0,0.02);",
                             div(style = "display: flex; justify-content: space-between; align-items: flex-start; flex-wrap: wrap; gap: 12px; margin-bottom: 12px;",
                                 div(
                                   h3(style = "font-size: 16px; font-weight: 700; margin: 0 0 4px 0; color: #1C1C1E;", "Conciliación de Cosecha: Campo vs SAP"),
                                   p(style = "font-size: 12px; color: #8E8E93; margin: 0;", "Cruce de trazabilidad entre rutas reportadas en campo (Google Sheets) y recepción en balanzas oficiales (SAP)")
                                 ),
                                 downloadButton("btn_descarga_conciliacion_excel", "Descargar Inconsistencias (Excel)", class = "btn-descarga-conciliacion")
                             ),
                             uiOutput("ui_alerta_regla_d1"),
                             # Mini Cards de Resumen KPI
                             div(style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(180px, 1fr)); gap: 10px; margin-top: 12px;",
                                 div(class = "kpi-concil-card card-concil-ok",
                                     div(class = "concil-card-label", "Conciliadas OK"),
                                     div(class = "concil-card-val", textOutput("kpi_concil_ok")),
                                     div(class = "concil-card-sub", textOutput("kpi_concil_ok_sub"))
                                 ),
                                 div(class = "kpi-concil-card card-concil-sin-sap",
                                     div(class = "concil-card-label", "Ruta sin Kilos SAP"),
                                     div(class = "concil-card-val", textOutput("kpi_concil_sin_sap")),
                                     div(class = "concil-card-sub", textOutput("kpi_concil_sin_sap_sub"))
                                 ),
                                 div(class = "kpi-concil-card card-concil-sin-ruta",
                                     div(class = "concil-card-label", "Kilos SAP sin Ruta"),
                                     div(class = "concil-card-val", textOutput("kpi_concil_sin_ruta")),
                                     div(class = "concil-card-sub", textOutput("kpi_concil_sin_ruta_sub"))
                                 ),
                                 div(class = "kpi-concil-card card-concil-desfase",
                                     div(class = "concil-card-label", "Desfase de Día"),
                                     div(class = "concil-card-val", textOutput("kpi_concil_desfase")),
                                     div(class = "concil-card-sub", textOutput("kpi_concil_desfase_sub"))
                                 ),
                                 div(class = "kpi-concil-card card-concil-proceso",
                                     div(class = "concil-card-label", "Cosecha Hoy (D+1)"),
                                     div(class = "concil-card-val", textOutput("kpi_concil_proceso")),
                                     div(class = "concil-card-sub", textOutput("kpi_concil_proceso_sub"))
                                 )
                             )
                         ),
                         # Contenedor Tabla y Filtros de Estado
                         div(style = "background: #FFFFFF; border-radius: 14px; padding: 18px 20px; border: 1px solid rgba(0, 0, 0, 0.05); box-shadow: 0 2px 8px rgba(0,0,0,0.02);",
                             div(style = "display: flex; justify-content: space-between; align-items: center; flex-wrap: wrap; gap: 12px; margin-bottom: 14px;",
                                 div(style = "display: flex; align-items: center; gap: 8px; flex-wrap: wrap;",
                                     tags$span(style = "font-size: 11.5px; font-weight: 700; color: #8E8E93; text-transform: uppercase;", "Filtrar por Estado:"),
                                     div(class = "ios-segmented-switch",
                                         radioButtons(
                                           inputId  = "filtro_estado_conciliacion",
                                           label    = NULL,
                                           choices  = c("Solo Inconsistencias" = "inconsistencias",
                                                        "Ruta sin SAP" = "ruta_sin_sap",
                                                        "SAP sin Ruta" = "sap_sin_ruta",
                                                        "Desfase Día"  = "desfase",
                                                        "Hoy (D+1)"    = "proceso_hoy",
                                                        "Conciliadas"  = "conciliado",
                                                        "Todas"        = "todas"),
                                           selected = "inconsistencias",
                                           inline   = TRUE
                                         )
                                     )
                                 ),
                                 div(style = "font-size: 11.5px; color: #8E8E93;",
                                     textOutput("txt_conteo_filtro_conciliacion", inline = TRUE)
                                 )
                             ),
                             DTOutput("tabla_conciliacion_campo_sap")
                         )
                     )
            ),
            tabPanel("Frecuencia",
                     div(style = "margin-top: 14px; display: flex; flex-direction: column; gap: 20px;",
                         # TARJETA 1: Evolución por turno
                         div(class = "ios-card freq-chart-card",
                             div(class = "freq-card-header",
                                 div(
                                   h3(class = "freq-card-title", textOutput("titulo_grafico_frecuencia", inline = TRUE)),
                                   span(class = "freq-card-subtitle", textOutput("subtitulo_grafico_frecuencia", inline = TRUE))
                                 ),
                                 div(class = "freq-badge-dias", "Días entre cosechas")
                             ),
                             div(style = "position: relative; margin-top: 8px;",
                                 plotlyOutput("grafico_frecuencia_turno", height = "390px")
                             ),
                             div(class = "freq-card-hint",
                                 textOutput("ui_hint_grafico_frecuencia", inline = TRUE)
                             )
                         ),
                         
                         # TARJETA 2: Turnos por semana (Heatmap Matrix)
                         div(class = "ios-card freq-chart-card",
                             div(class = "freq-card-header",
                                 div(
                                   h3(class = "freq-card-title", textOutput("titulo_tabla_frecuencia", inline = TRUE)),
                                   span(class = "freq-card-subtitle", "Lee cada fila de izquierda a derecha para seguir el cambio durante la campaña.")
                                 ),
                                 div(class = "freq-badge-selecciona",
                                     uiOutput("ui_badge_filtro_turno", inline = TRUE)
                                 )
                             ),
                             div(class = "freq-heatmap-container",
                                 uiOutput("tabla_heatmap_frecuencia")
                             ),
                             div(class = "freq-heatmap-footer",
                                 "El color más intenso representa una frecuencia mayor. Las celdas grises no tienen frecuencia registrada."
                             )
                         )
                     )
            )
          )
      )
  )
)

# ==============================================================================
# SERVER
# ==============================================================================

server <- function(input, output, session) {
  # Reactive Values para datos de cosecha (Comercial aislado de Testblock) y ediciones manuales
  rv_cosecha           <- reactiveVal(df_cosecha_inicial)
  rv_cosecha_testblock <- reactiveVal(df_cosecha_testblock_ini)
  ediciones_manuales   <- reactiveVal(cargar_ediciones_disco())
  rv_ajuste_valvulas <- reactiveVal(cargar_ajuste_valvulas_disco())
  trigger_refresh    <- reactiveVal(0)
  
  output$badge_origen_datos <- renderUI({
    trigger_refresh()
    ts_str <- if (exists("metadata_actualizacion", inherits = TRUE) &&
                  is.list(metadata_actualizacion) &&
                  !is.null(metadata_actualizacion$Fecha_Hora_Actualizacion)) {
      formatear_hora_peru(metadata_actualizacion$Fecha_Hora_Actualizacion)
    } else if (nzchar(ruta_rdata_cargada) && file.exists(ruta_rdata_cargada)) {
      formatear_hora_peru(file.info(ruta_rdata_cargada)$mtime)
    } else {
      formatear_hora_peru(Sys.time())
    }
    
    es_nube <- origen_rdata_actual == "NUBE_DRIVE"
    es_g    <- origen_rdata_actual == "DRIVE_LOCAL_G" || dir.exists(ruta_raiz_arandano)
    
    if (es_nube) {
      color_bg <- "rgba(52, 199, 89, 0.14)"; color_txt <- "#1E7E34"; color_brd <- "rgba(52, 199, 89, 0.35)"
      etiqueta <- "☁️ Conectado a Google Drive (Nube)"
    } else if (es_g) {
      color_bg <- "rgba(52, 199, 89, 0.14)"; color_txt <- "#1E7E34"; color_brd <- "rgba(52, 199, 89, 0.35)"
      etiqueta <- "🟢 Conectado a Google Drive (G:)"
    } else {
      color_bg <- "rgba(255, 59, 48, 0.14)"; color_txt <- "#D70015"; color_brd <- "rgba(255, 59, 48, 0.35)"
      etiqueta <- "⚠️ Sin Conexión a Google Drive"
    }
    
    div(
      style = sprintf(
        "display: inline-flex; align-items: center; gap: 6px; margin-top: 5px; padding: 3px 10px; border-radius: 12px; font-size: 11px; font-weight: 600; background: %s; color: %s; border: 1px solid %s;",
        color_bg, color_txt, color_brd
      ),
      sprintf("%s • Última actualización (Perú): %s", etiqueta, ts_str)
    )
  })
  
  # Botón para recargar SAP y proyecciones en vivo sin reiniciar R (Exclusivo Google Drive)
  observeEvent(input$btn_refresh_sap, {
    if (file.exists(file.path(ruta_raiz_arandano, "datos_maestros_cosecha_2026.RData"))) {
      load(file.path(ruta_raiz_arandano, "datos_maestros_cosecha_2026.RData"), envir = .GlobalEnv)
      origen_rdata_actual <<- "DRIVE_LOCAL_G"
      df_cosecha_base <<- df_cosecha %>% mutate(Variedad = limpiar_variedad(Variedad))
    } else if (nzchar(id_drive_data_maestra)) {
      if (descargar_data_maestra_drive(id_drive_data_maestra, "datos_maestros_cosecha_2026.RData")) {
        load("datos_maestros_cosecha_2026.RData", envir = .GlobalEnv)
        origen_rdata_actual <<- "NUBE_DRIVE"
        df_cosecha_base <<- df_cosecha %>% mutate(Variedad = limpiar_variedad(Variedad))
      }
    }
    if (exists("df_hist_oficial_wide") && is.data.frame(df_hist_oficial_wide)) {
      df_hist_oficial_wide <<- limpiar_hist_wide_sin_testblock(df_hist_oficial_wide)
    }
    camp_act <- if (!is.null(input$filtro_campana)) input$filtro_campana else 2026
    nuevo_df <- integrar_sap_en_historico(df_cosecha_base, ruta_raiz_arandano, campana_actual = camp_act)
    rv_cosecha(nuevo_df %>% filter(!Es_Testblock))
    rv_cosecha_testblock(nuevo_df %>% filter(Es_Testblock))
    ediciones_manuales(cargar_ediciones_disco())
    tryCatch(rm(list = ls(cache_rutas_campo), envir = cache_rutas_campo), error = function(e) NULL)
    trigger_refresh(trigger_refresh() + 1)
    showNotification("Datos de SAP y Proyecciones actualizados correctamente desde Google Drive.", type = "message", duration = 3)
  })
  
  # Selector de Semanas Unificado (ordenado por orden cronológico de campaña)
  # Selector de Semanas Unificado (actualización dinámica)
  observeEvent(list(input$filtro_campana, trigger_refresh()), {
    req(input$filtro_campana)
    
    semanas_cosecha <- rv_cosecha() %>%
      filter(as.character(Campaña) == as.character(input$filtro_campana)) %>%
      pull(Semana) %>%
      unique()
    
    semanas_proy <- obtener_semanas_proyeccion(ruta_raiz_arandano, input$filtro_campana)
    
    es_campana_actual <- grepl("2026|^26", as.character(input$filtro_campana))
    semana_hoy <- if (es_campana_actual) lubridate::isoweek(Sys.Date()) else numeric(0)
    
    semanas_disp <- unique(na.omit(c(semanas_cosecha, semanas_proy, semana_hoy)))
    semanas_disp <- semanas_disp[order(orden_semana_campana(semanas_disp))]
    
    sel_prev <- isolate(input$filtro_semana)
    sel_default <- if (length(semana_hoy) > 0 && semana_hoy %in% semanas_disp) {
      semana_hoy
    } else if (!is.null(sel_prev) && sel_prev %in% semanas_disp) {
      sel_prev
    } else {
      tail(semanas_disp, 1)
    }
    
    updateSelectInput(
      session,
      "filtro_semana",
      choices  = semanas_disp,
      selected = sel_default
    )
  }, ignoreInit = FALSE)
  
  # Restablecer filtros
  observeEvent(input$btn_reset, {
    updateSelectizeInput(session, "filtro_grupo", selected = character(0))
    updateSelectizeInput(session, "filtro_modulo", selected = character(0))
    updateSelectizeInput(session, "filtro_turno", selected = character(0))
    updateSelectizeInput(session, "filtro_variedad", selected = character(0))
  })
  
  # Reactive 1: Carga la proyección semanal según Campaña y Semana
  proyeccion_semanal <- reactive({
    req(input$filtro_campana, input$filtro_semana)
    trigger_refresh()
    cargar_proyeccion(ruta_raiz_arandano, input$filtro_campana, input$filtro_semana)
  })
  
  # Reactive auxiliar: Catálogo de Áreas consolidado por Módulo, Turno y Variedad
  areas_por_turno <- reactive({
    req(input$filtro_campana)
    es_campana_2026 <- grepl("2026|^26", as.character(input$filtro_campana))
    
    areas_base %>%
      filter(as.character(Campaña) == as.character(input$filtro_campana)) %>%
      mutate(
        Turno_num = suppressWarnings(as.numeric(Turno)),
        Módulo = case_when(
          es_campana_2026 & Módulo %in% c("13.1", "13.2") ~ "13",
          es_campana_2026 & Módulo %in% c("14.1", "14.2") ~ "14",
          as.character(Módulo) == "9" & between(Turno_num, 1, 11)  ~ "9.2",
          as.character(Módulo) == "9" & Turno_num >= 12            ~ "9.1",
          as.character(Módulo) == "11" & between(Turno_num, 1, 12) ~ "11.1",
          as.character(Módulo) == "11" & Turno_num >= 13           ~ "11.2",
          TRUE ~ Módulo
        ),
        Var_join = limpiar_variedad(Variedad)
      ) %>%
      group_by(Módulo, Turno, Var_join) %>%
      summarise(Área_Cat = sum(Área, na.rm = TRUE), .groups = "drop")
  })
  
  # Reactive 2A: Universo Base sin ediciones manuales (100% Comercial; Testblock está separado desde origen)
  universo_base <- reactive({
    req(input$filtro_campana, input$filtro_semana)
    
    dias_semana <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Domingo")
    es_campana_2026 <- grepl("2026|^26", as.character(input$filtro_campana))
    
    # 1. Cosecha comercial registrada en la semana
    df_filtrado <- rv_cosecha() %>%
      filter(
        as.character(Campaña) == as.character(input$filtro_campana),
        Semana == input$filtro_semana,
        !coalesce(Es_Testblock, FALSE)
      )
    
    df_concil <- tryCatch(obtener_cosecha_conciliada_semana(input$filtro_semana, input$filtro_campana), error = function(e) NULL)
    
    cosecha_preparada <- if (!is.null(df_concil) && nrow(df_concil) > 0) {
      df_concil %>%
        filter(Es_Valido_Operativo) %>%
        mutate(
          Turno_num = suppressWarnings(as.numeric(Turno)),
          Sector    = case_when(as.character(Módulo) %in% c("9", "9.1", "9.2", "11", "11.1", "11.2", "12") ~ "9", TRUE ~ trimws(as.character(Sector))),
          Módulo    = trimws(as.character(Módulo)),
          Módulo    = case_when(
            es_campana_2026 & Módulo %in% c("13.1", "13.2") ~ "13",
            es_campana_2026 & Módulo %in% c("14.1", "14.2") ~ "14",
            as.character(Módulo) == "9" & between(Turno_num, 1, 11)  ~ "9.2",
            as.character(Módulo) == "9" & Turno_num >= 12            ~ "9.1",
            as.character(Módulo) == "11" & between(Turno_num, 1, 12) ~ "11.1",
            as.character(Módulo) == "11" & Turno_num >= 13           ~ "11.2",
            TRUE ~ Módulo
          ),
          Turno    = trimws(as.character(Turno)),
          Variedad = limpiar_variedad(Variedad)
        ) %>%
        group_by(Sector, Módulo, Turno, Variedad) %>%
        summarise(
          Lunes     = round(sum(Kg_Lunes_rec, na.rm = TRUE), 2),
          Martes    = round(sum(Kg_Martes_rec, na.rm = TRUE), 2),
          Miércoles = round(sum(Kg_Miercoles_rec, na.rm = TRUE), 2),
          Jueves    = round(sum(Kg_Jueves_rec, na.rm = TRUE), 2),
          Viernes   = round(sum(Kg_Viernes_rec, na.rm = TRUE), 2),
          Sábado    = round(sum(Kg_Sabado_rec, na.rm = TRUE), 2),
          Domingo   = 0,
          .groups   = "drop"
        ) %>%
        mutate(Es_Testblock = FALSE)
    } else if (nrow(df_filtrado) > 0) {
      df_filtrado %>%
        mutate(
          Dia       = factor(dias_semana[wday(Fecha, week_start = 1)], levels = dias_semana),
          Turno_num = suppressWarnings(as.numeric(Turno)),
          Sector    = case_when(as.character(Módulo) %in% c("9", "9.1", "9.2", "11", "11.1", "11.2", "12") ~ "9", TRUE ~ trimws(as.character(Sector))),
          Módulo    = trimws(as.character(Módulo)),
          Módulo    = case_when(
            es_campana_2026 & Módulo %in% c("13.1", "13.2") ~ "13",
            es_campana_2026 & Módulo %in% c("14.1", "14.2") ~ "14",
            as.character(Módulo) == "9" & between(Turno_num, 1, 11)  ~ "9.2",
            as.character(Módulo) == "9" & Turno_num >= 12            ~ "9.1",
            as.character(Módulo) == "11" & between(Turno_num, 1, 12) ~ "11.1",
            as.character(Módulo) == "11" & Turno_num >= 13           ~ "11.2",
            TRUE ~ Módulo
          ),
          Turno    = trimws(as.character(Turno)),
          Variedad = limpiar_variedad(Variedad)
        ) %>%
        group_by(Sector, Módulo, Turno, Variedad, Dia) %>%
        summarise(Kg_Dia = sum(`Kg Total`, na.rm = TRUE), .groups = "drop") %>%
        pivot_wider(names_from = Dia, values_from = Kg_Dia, values_fill = 0) %>%
        mutate(Es_Testblock = FALSE)
    } else {
      tibble(Sector = character(), Módulo = character(), Turno = character(), Variedad = character(), Es_Testblock = logical())
    }
    for (d in dias_semana) {
      if (!d %in% names(cosecha_preparada)) cosecha_preparada[[d]] <- numeric(nrow(cosecha_preparada))
    }
    
    # 2. Obtener proyecciones comerciales
    df_proy <- proyeccion_semanal()
    
    if (nrow(cosecha_preparada) == 0 && (is.null(df_proy) || nrow(df_proy) == 0)) {
      return(tibble())
    }
    
    # 3. Claves normalizadas para FULL JOIN
    cosecha_preparada <- cosecha_preparada %>%
      mutate(
        Sector_join   = as.character(trimws(Sector)),
        Módulo_join   = as.character(trimws(Módulo)),
        Turno_join    = as.character(trimws(Turno)),
        Variedad_join = as.character(limpiar_variedad(Variedad))
      )
    
    if (!is.null(df_proy) && nrow(df_proy) > 0) {
      df_proy <- df_proy %>%
        mutate(
          Sector_join   = as.character(trimws(Sector_join)),
          Módulo_join   = as.character(trimws(Módulo_join)),
          Turno_join    = as.character(trimws(Turno_join)),
          Variedad_join = as.character(limpiar_variedad(Variedad_join))
        )
      
      universo <- full_join(
        cosecha_preparada,
        df_proy,
        by = c("Sector_join", "Módulo_join", "Turno_join", "Variedad_join")
      ) %>%
        mutate(
          Sector   = coalesce(as.character(Sector), Sector_join),
          Módulo   = coalesce(as.character(Módulo), Módulo_join),
          Turno    = coalesce(as.character(Turno), Turno_join),
          Variedad = as.character(limpiar_variedad(coalesce(as.character(Variedad.x), as.character(Variedad.y), Variedad_join)))
        ) %>%
        select(-any_of(c("Variedad.x", "Variedad.y")))
    } else {
      universo <- cosecha_preparada %>% 
        mutate(
          Proyección = NA_real_,
          Variedad   = as.character(limpiar_variedad(Variedad))
        )
    }
    
    # 4. Asignar Área y Tipo desde los catálogos oficiales de la campaña seleccionada
    areas_cat <- areas_base %>%
      filter(as.character(Campaña) == as.character(input$filtro_campana)) %>%
      mutate(
        Módulo = case_when(
          es_campana_2026 & as.character(Módulo) %in% c("13.1", "13.2") ~ "13",
          es_campana_2026 & as.character(Módulo) %in% c("14.1", "14.2") ~ "14",
          TRUE ~ as.character(Módulo)
        )
      ) %>%
      group_by(
        Sector_cat = trimws(as.character(Sector)), 
        Módulo_cat = trimws(as.character(Módulo)), 
        Turno_cat  = trimws(as.character(Turno)), 
        Var_cat    = limpiar_variedad(Variedad)
      ) %>%
      summarise(Área_cat = sum(Área, na.rm = TRUE), .groups = "drop")
    
    areas_cat_mtv <- areas_por_turno()
    
    tratt_cat <- tratamientos_base %>%
      filter(as.character(Campaña) == as.character(input$filtro_campana)) %>%
      mutate(
        Módulo_unif = case_when(
          es_campana_2026 & Módulo %in% c("13.1", "13.2") ~ "13",
          es_campana_2026 & Módulo %in% c("14.1", "14.2") ~ "14",
          TRUE ~ Módulo
        )
      ) %>%
      distinct(Módulo_unif, .keep_all = TRUE)
    
    universo <- universo %>%
      left_join(areas_cat, by = c("Sector_join" = "Sector_cat", "Módulo_join" = "Módulo_cat", 
                                  "Turno_join" = "Turno_cat", "Variedad_join" = "Var_cat")) %>%
      left_join(areas_cat_mtv, by = c("Módulo_join" = "Módulo", "Turno_join" = "Turno", "Variedad_join" = "Var_join")) %>%
      left_join(tratt_cat %>% select(Módulo_unif, Tipo_cat = Tipo), by = c("Módulo_join" = "Módulo_unif")) %>%
      mutate(
        Área = coalesce(Área_cat, Área_Cat, 0),
        Tipo = coalesce(Tipo_cat, "-")
      ) %>%
      select(-any_of(c("Área_cat", "Área_Cat", "Tipo_cat")))
    
    # 4B. Calcular Ha Recorrida desde Rutas de Campo (con fallback a SAP protegido por remanente)
    # Si estamos en la semana actual en curso, Ha Recorrida toma SOLO los días previos al día de ajuste seleccionado
    dias_nombres_u   <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
    es_campana_act_u <- grepl("2026|^26", as.character(input$filtro_campana))
    sem_actual_num_u <- lubridate::isoweek(Sys.Date())
    permitir_cualquier_sem_u <- is.null(input$switch_ajuste_cualquier_semana) || isTRUE(input$switch_ajuste_cualquier_semana)
    es_semana_act_u  <- (as.numeric(input$filtro_semana) == sem_actual_num_u) || permitir_cualquier_sem_u
    
    d_ini_u <- if (es_campana_act_u && es_semana_act_u && !is.null(input$ajustar_desde_dia) && input$ajustar_desde_dia %in% dias_nombres_u) {
      input$ajustar_desde_dia
    } else {
      "Sábado"
    }
    idx_ini_u <- match(d_ini_u, dias_nombres_u)
    dias_previos_u <- if (es_campana_act_u && es_semana_act_u) {
      if (!is.na(idx_ini_u) && idx_ini_u > 1) dias_nombres_u[1:(idx_ini_u - 1)] else character(0)
    } else {
      dias_nombres_u
    }
    cols_ha_prev_u <- paste0("Ha_", dias_previos_u)
    
    rc_file_cand <- tryCatch(obtener_rutas_campo_en_vivo(input$filtro_semana), error = function(e) cargar_rutas_cosecha())
    if (is.null(rc_file_cand) || nrow(rc_file_cand) == 0) rc_file_cand <- tryCatch(cargar_rutas_cosecha(), error = function(e) NULL)
    rc_sem_u <- if (!is.null(rc_file_cand) && "Semana" %in% names(rc_file_cand)) {
      rc_file_cand %>% filter(as.numeric(Semana) == as.numeric(input$filtro_semana))
    } else tibble()
    
    rc_ha_turno <- if (nrow(rc_sem_u) > 0) {
      rc_temp_u <- rc_sem_u %>%
        filter(!("Tipo_Registro" %in% names(.)) | Tipo_Registro != "Rectificacion")
      
      # Acumular avance ejecutado previo a dia_inicio_u (solo días con kilos o Lunes/Martes en curso)
      ha_prev_vec_u <- numeric(nrow(rc_temp_u))
      for (d_prev in dias_previos_u) {
        col_h <- paste0("Ha_", d_prev)
        col_k <- paste0("Kg_", d_prev)
        v_h   <- if (col_h %in% names(rc_temp_u)) coalesce(rc_temp_u[[col_h]], 0) else 0
        v_k   <- if (col_k %in% names(rc_temp_u)) coalesce(rc_temp_u[[col_k]], 0) else 0
        es_ejec <- (d_prev %in% c("Lunes", "Martes"))
        ha_prev_vec_u <- ha_prev_vec_u + ifelse(es_ejec, v_h, 0)
      }
      rc_temp_u$Ha_Rc_Prev <- ha_prev_vec_u
      
      rc_temp_u %>%
        mutate(
          Turno_num_rc = suppressWarnings(as.numeric(Turno)),
          Módulo_rc = case_when(
            es_campana_act_u & as.character(Módulo) %in% c("13.1", "13.2") ~ "13",
            es_campana_act_u & as.character(Módulo) %in% c("14.1", "14.2") ~ "14",
            as.character(Módulo) == "9" & between(Turno_num_rc, 1, 11)  ~ "9.2",
            as.character(Módulo) == "9" & Turno_num_rc >= 12            ~ "9.1",
            as.character(Módulo) == "11" & between(Turno_num_rc, 1, 12) ~ "11.1",
            as.character(Módulo) == "11" & Turno_num_rc >= 13           ~ "11.2",
            TRUE                                                         ~ as.character(Módulo)
          ),
          Turno_rc   = as.character(Turno),
          Var_rc     = limpiar_variedad(Variedad),
          kg_tot_rc  = if ("Kg_Total_Prog" %in% names(.)) coalesce(Kg_Total_Prog, 0) else 0,
          num_pas_rc = if ("Num_Pasada" %in% names(.)) suppressWarnings(as.numeric(Num_Pasada)) else 1
        ) %>%
        group_by(Módulo_rc, Turno_rc, Var_rc) %>%
        summarise(
          Ha_Rc          = sum(coalesce(Ha_Rc_Prev, 0), na.rm = TRUE),
          Ha_Rc_2da      = sum(ifelse(coalesce(num_pas_rc, 1) == 2, coalesce(Ha_Rc_Prev, 0), 0), na.rm = TRUE),
          Kg_Real_1ra_rc = sum(ifelse(num_pas_rc == 1, kg_tot_rc, 0), na.rm = TRUE),
          Kg_Real_2da_rc = sum(ifelse(num_pas_rc == 2, kg_tot_rc, 0), na.rm = TRUE),
          .groups        = "drop"
        )
    } else tibble(Módulo_rc = character(), Turno_rc = character(), Var_rc = character(), Ha_Rc = numeric(), Ha_Rc_2da = numeric(), Kg_Real_1ra_rc = numeric(), Kg_Real_2da_rc = numeric())
    
    # Identificar válvulas con Num_Pasada == 2 por día para asignar correctamente kilos SAP de 2ª pasada
    valvs_dias_p2_u <- list()
    if (nrow(rc_sem_u) > 0) {
      rc_p2_u <- rc_sem_u %>%
        filter(!("Tipo_Registro" %in% names(.)) | Tipo_Registro != "Rectificacion") %>%
        filter(coalesce(suppressWarnings(as.numeric(Num_Pasada)), 1) == 2)
      col_cod_p2_u <- intersect(c("Codigo_Valvula", "codigo", "Codigo_Valv"), names(rc_p2_u))[1]
      if (!is.na(col_cod_p2_u) && nrow(rc_p2_u) > 0) {
        rc_p2_u <- rc_p2_u %>% mutate(cod_norm_temp = norm_codigo_valvula(.data[[col_cod_p2_u]]))
        for (d in c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")) {
          d_sin <- chartr("áéíóúÁÉÍÓÚ", "aeiouAEIOU", tolower(d))
          has_adv <- (if (d %in% names(rc_p2_u)) coalesce(rc_p2_u[[d]], 0) > 0 else FALSE) |
                     (if (paste0("Ha_", d) %in% names(rc_p2_u)) coalesce(rc_p2_u[[paste0("Ha_", d)]], 0) > 0 else FALSE) |
                     (if (paste0("Ha_", tools::toTitleCase(d_sin)) %in% names(rc_p2_u)) coalesce(rc_p2_u[[paste0("Ha_", tools::toTitleCase(d_sin))]], 0) > 0 else FALSE)
          valvs_dias_p2_u[[d]] <- unique(na.omit(rc_p2_u$cod_norm_temp[has_adv]))
        }
      }
    }
    for (d in c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")) {
      if (is.null(valvs_dias_p2_u[[d]])) valvs_dias_p2_u[[d]] <- character(0)
    }

    # 4C. Válvulas físicas oficiales de SAP (área real y desglose exacto de kilos SAP por pasada)
    sap_ha_valvs_turno <- if (exists("df_hist_oficial_wide") && !is.null(df_hist_oficial_wide) && nrow(df_hist_oficial_wide) > 0) {
      cols_ha_sap_exist_u <- intersect(cols_ha_prev_u, names(df_hist_oficial_wide))
      cols_kg_sap_exist_u <- intersect(paste0("Kg_", dias_previos_u), names(df_hist_oficial_wide))
      
      df_sap_sem_u <- df_hist_oficial_wide %>%
        filter(as.numeric(Semana) == as.numeric(input$filtro_semana)) %>%
        mutate(
          Turno_num_s = suppressWarnings(as.numeric(Turno_Norm)),
          Módulo_s = case_when(
            es_campana_2026 & Módulo_Norm %in% c("13.1", "13.2") ~ "13",
            es_campana_2026 & Módulo_Norm %in% c("14.1", "14.2") ~ "14",
            as.character(Módulo_Norm) == "9" & between(Turno_num_s, 1, 11)  ~ "9.2",
            as.character(Módulo_Norm) == "9" & Turno_num_s >= 12            ~ "9.1",
            as.character(Módulo_Norm) == "11" & between(Turno_num_s, 1, 12) ~ "11.1",
            as.character(Módulo_Norm) == "11" & Turno_num_s >= 13           ~ "11.2",
            TRUE                                                           ~ as.character(Módulo_Norm)
          ),
          Turno_s = as.character(Turno_Norm),
          Var_s   = limpiar_variedad(Variedad),
          cod_norm_s = norm_codigo_valvula(Codigo_Valv),
          Tiene_Cosecha_Prev_Sap = (if (length(cols_ha_sap_exist_u) > 0) rowSums(across(all_of(cols_ha_sap_exist_u)), na.rm = TRUE) else 0) > 0 |
            (if (length(cols_kg_sap_exist_u) > 0) rowSums(across(all_of(cols_kg_sap_exist_u)), na.rm = TRUE) else 0) > 0
        )
      
      kg_2da_vals_u <- numeric(nrow(df_sap_sem_u))
      kg_1ra_vals_u <- numeric(nrow(df_sap_sem_u))
      for (d in dias_previos_u) {
        col_k <- paste0("Kg_", d)
        if (col_k %in% names(df_sap_sem_u)) {
          val_k <- coalesce(df_sap_sem_u[[col_k]], 0)
          is_p2 <- df_sap_sem_u$cod_norm_s %in% valvs_dias_p2_u[[d]]
          kg_2da_vals_u <- kg_2da_vals_u + ifelse(is_p2, val_k, 0)
          kg_1ra_vals_u <- kg_1ra_vals_u + ifelse(is_p2, 0, val_k)
        }
      }
      df_sap_sem_u$Kg_Real_2da_sap <- kg_2da_vals_u
      df_sap_sem_u$Kg_Real_1ra_sap <- kg_1ra_vals_u
      
      df_sap_sem_u %>%
        group_by(Módulo_s, Turno_s, Var_s) %>%
        summarise(
          Ha_Valvs_SAP = sum(Area_Ha[!duplicated(Codigo_Valv) & Tiene_Cosecha_Prev_Sap], na.rm = TRUE),
          Kg_Real_2da_sap = sum(Kg_Real_2da_sap, na.rm = TRUE),
          Kg_Real_1ra_sap = sum(Kg_Real_1ra_sap, na.rm = TRUE),
          .groups = "drop"
        )
    } else {
      tibble(Módulo_s = character(), Turno_s = character(), Var_s = character(), Ha_Valvs_SAP = numeric(), Kg_Real_2da_sap = numeric(), Kg_Real_1ra_sap = numeric())
    }
    
    cols_dias_kg_u <- intersect(dias_previos_u, c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Domingo"))
    
    universo <- universo %>%
      left_join(rc_ha_turno, by = c("Módulo_join" = "Módulo_rc", "Turno_join" = "Turno_rc", "Variedad_join" = "Var_rc")) %>%
      left_join(sap_ha_valvs_turno, by = c("Módulo_join" = "Módulo_s", "Turno_join" = "Turno_s", "Variedad_join" = "Var_s")) %>%
      mutate(
        Ha_Rc        = coalesce(Ha_Rc, 0),
        Ha_Valvs_SAP = coalesce(Ha_Valvs_SAP, 0),
        kg_sap_act   = if (length(cols_dias_kg_u) > 0) {
          rowSums(across(all_of(cols_dias_kg_u)), na.rm = TRUE)
        } else {
          rowSums(across(all_of(intersect(c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Domingo"), names(.)))), na.rm = TRUE)
        },
        Kg_SAP_Real  = round(kg_sap_act, 2),
        rend_proy_u  = ifelse(!is.na(Proyección) & Proyección > 0 & Área > 0, Proyección / Área, 1),
        es_remanente_u = (Ha_Rc == 0 & Ha_Valvs_SAP == 0) & (kg_sap_act > 0) & 
          ((!is.na(Proyección) & Proyección > 0 & (kg_sap_act / Proyección) < 0.15) | (kg_sap_act / pmax(0.1, Área)) < 50),
        `Ha Recorrida` = case_when(
          Ha_Valvs_SAP > 0 ~ round(pmin(Área, Ha_Valvs_SAP), 2),
          Ha_Rc > 0        ~ round(pmin(Área, Ha_Rc), 2),
          es_remanente_u   ~ round(pmin(Área, kg_sap_act / pmax(1, rend_proy_u)), 2),
          kg_sap_act > 0   ~ round(pmin(Área, kg_sap_act / pmax(1, rend_proy_u)), 2),
          TRUE             ~ 0
        ),
        # Separacion de Ha Real por pasada (2a pasada solo ROCIO, segun rutas de campo)
        `Ha Real 2ª Pas` = ifelse(limpiar_variedad(Variedad) == "ROCIO", round(pmin(`Ha Recorrida`, coalesce(Ha_Rc_2da, 0)), 2), 0),
        `Ha Real 1ª Pas` = round(pmax(0, `Ha Recorrida` - `Ha Real 2ª Pas`), 2)
      ) %>%
      select(-any_of(c("Ha_Rc", "Ha_Rc_2da", "Ha_Valvs_SAP", "kg_sap_act", "rend_proy_u", "es_remanente_u")))
    
    # 5. Flags iniciales de bloqueo
    universo <- universo %>%
      mutate(
        Proy_bloqueado      = !is.na(Proyección) & Proyección > 0,
        Lunes_bloqueado     = !is.na(Lunes) & Lunes > 0,
        Martes_bloqueado    = !is.na(Martes) & Martes > 0,
        Miércoles_bloqueado = !is.na(Miércoles) & Miércoles > 0,
        Jueves_bloqueado    = !is.na(Jueves) & Jueves > 0,
        Viernes_bloqueado   = !is.na(Viernes) & Viernes > 0,
        Sábado_bloqueado    = !is.na(Sábado) & Sábado > 0,
        Proy_manual         = FALSE,
        Lunes_manual        = FALSE,
        Martes_manual       = FALSE,
        Miércoles_manual    = FALSE,
        Jueves_manual       = FALSE,
        Viernes_manual      = FALSE,
        Sábado_manual       = FALSE,
        Lunes_tipo          = ifelse(!is.na(Lunes) & Lunes > 0, "sap", "none"),
        Martes_tipo         = ifelse(!is.na(Martes) & Martes > 0, "sap", "none"),
        Miércoles_tipo      = ifelse(!is.na(Miércoles) & Miércoles > 0, "sap", "none"),
        Jueves_tipo         = ifelse(!is.na(Jueves) & Jueves > 0, "sap", "none"),
        Viernes_tipo        = ifelse(!is.na(Viernes) & Viernes > 0, "sap", "none"),
        Sábado_tipo         = ifelse(!is.na(Sábado) & Sábado > 0, "sap", "none")
      )
    
    return(universo)
  })
  
  # Reactive 2B: Superpone ediciones manuales ligeras sobre universo_base()
  universo_completo <- reactive({
    universo <- universo_base()
    if (is.null(universo) || nrow(universo) == 0) return(tibble())
    modo_detalle <- if (!is.null(input$filtro_modo_detalle)) as.character(input$filtro_modo_detalle) else "real"
    
    ed_todas <- ediciones_manuales()
    ha_ajustadas_ed <- tibble(
      Módulo_k = character(), Turno_k = character(), Var_k = character(),
      Ha_Ajustada_Total = numeric(), Ha_Aj_1ra_Ed = numeric(), Kg_Aj_1ra_Ed = numeric(),
      Ha_Aj_2da_Ed = numeric(), Kg_Aj_2da_Ed = numeric(), Tiene_Ha_Asig = logical(), Tiene_Detalle_Pas = logical(),
      Motivo_Guardado = character()
    )
    if (!is.null(ed_todas) && nrow(ed_todas) > 0) {
      ed_filtradas <- ed_todas %>%
        filter(
          as.character(Campaña) == as.character(input$filtro_campana),
          as.numeric(Semana)    == as.numeric(input$filtro_semana)
        )
      
      if (nrow(ed_filtradas) > 0) {
        dias_validos <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
        ha_ajustadas_ed <- ed_filtradas %>%
          filter(Campo %in% dias_validos, coalesce(Valor, 0) > 0, coalesce(Tipo_Edicion, "") != "ruta_sin_kg") %>%
          mutate(
            Módulo_k = trimws(as.character(Módulo)),
            Turno_k  = trimws(as.character(Turno)),
            Var_k    = limpiar_variedad(Variedad),
            Ha_val   = if ("Ha_Asignada" %in% names(.)) suppressWarnings(as.numeric(Ha_Asignada)) else NA_real_,
            Ha_1ra   = if ("Ha_Asignada_1ra" %in% names(.)) suppressWarnings(as.numeric(Ha_Asignada_1ra)) else NA_real_,
            Kg_1ra   = if ("Kg_Asignado_1ra" %in% names(.)) suppressWarnings(as.numeric(Kg_Asignado_1ra)) else NA_real_,
            Ha_2da   = if ("Ha_Asignada_2da" %in% names(.)) suppressWarnings(as.numeric(Ha_Asignada_2da)) else NA_real_,
            Kg_2da   = if ("Kg_Asignado_2da" %in% names(.)) suppressWarnings(as.numeric(Kg_Asignado_2da)) else NA_real_,
            Mot_val  = if ("Motivo_Ajuste" %in% names(.)) as.character(Motivo_Ajuste) else NA_character_
          ) %>%
          group_by(Módulo_k, Turno_k, Var_k) %>%
          summarise(
            Ha_Ajustada_Total = round(sum(Ha_val, na.rm = TRUE), 2),
            Ha_Aj_1ra_Ed      = round(sum(Ha_1ra, na.rm = TRUE), 2),
            Kg_Aj_1ra_Ed      = round(sum(Kg_1ra, na.rm = TRUE), 2),
            Ha_Aj_2da_Ed      = round(sum(Ha_2da, na.rm = TRUE), 2),
            Kg_Aj_2da_Ed      = round(sum(Kg_2da, na.rm = TRUE), 2),
            Tiene_Ha_Asig     = any(!is.na(Ha_val) & Ha_val > 0),
            Tiene_Detalle_Pas = any(!is.na(Ha_1ra) | !is.na(Ha_2da)),
            Motivo_Guardado   = {
              m <- na.omit(Mot_val[nzchar(Mot_val)])
              if (length(m) > 0) m[1] else NA_character_
            },
            .groups           = "drop"
          )
        
        claves_u <- paste(trimws(as.character(universo$Sector)),
                          trimws(as.character(universo$Módulo)),
                          trimws(as.character(universo$Turno)),
                          limpiar_variedad(universo$Variedad), sep = "___")
        claves_u_mtv <- paste(trimws(as.character(universo$Módulo)),
                              trimws(as.character(universo$Turno)),
                              limpiar_variedad(universo$Variedad), sep = "___")
        claves_ed <- paste(trimws(as.character(ed_filtradas$Sector)),
                           trimws(as.character(ed_filtradas$Módulo)),
                           trimws(as.character(ed_filtradas$Turno)),
                           limpiar_variedad(ed_filtradas$Variedad), sep = "___")
        claves_ed_mtv <- paste(trimws(as.character(ed_filtradas$Módulo)),
                               trimws(as.character(ed_filtradas$Turno)),
                               limpiar_variedad(ed_filtradas$Variedad), sep = "___")
        dias_validos <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
        
        for (i in seq_len(nrow(ed_filtradas))) {
          tipo_i <- if ("Tipo_Edicion" %in% names(ed_filtradas)) coalesce(as.character(ed_filtradas$Tipo_Edicion[i]), "manual") else "manual"
          
          matches <- which(claves_u == claves_ed[i])
          if (length(matches) == 0) {
            matches <- which(claves_u_mtv == claves_ed_mtv[i])
          }
          if (length(matches) == 0) next
          
          # REGLA TESTBLOCK: Testblock NO se proyecta ni se ajusta automáticamente.
          # Si la edición es automática (auto o ruta_sin_kg), nunca aplicar a filas de Testblock.
          if (tipo_i %in% c("auto", "ruta_sin_kg")) {
            matches_validos <- matches[coalesce(universo$Tipo[matches], "") != "Testblock"]
            if (length(matches_validos) == 0) next
            idx <- matches_validos[1]
          } else {
            idx <- matches[1]
          }
          
          if (!is.na(idx)) {
            campo_i <- ed_filtradas$Campo[i]
            val_i   <- suppressWarnings(as.numeric(ed_filtradas$Valor[i]))
            if (!is.na(val_i)) {
              if (campo_i == "Proyección") {
                if (!universo$Proy_bloqueado[idx]) {
                  universo$Proyección[idx]  <- val_i
                  universo$Proy_manual[idx] <- TRUE
                }
              } else if (campo_i %in% dias_validos) {
                lock_col <- paste0(campo_i, "_bloqueado")
                man_col  <- paste0(campo_i, "_manual")
                tipo_col <- paste0(campo_i, "_tipo")
                # En modo "real", el dato de SAP tiene prioridad si existe.
                # En modo "ajustado", se sobrepone la asignación proyectada guardada del ajuste.
                debe_aplicar <- if (modo_detalle == "ajustado") {
                  TRUE
                } else {
                  !universo[[lock_col]][idx]
                }
                if (debe_aplicar) {
                  universo[[campo_i]][idx]  <- val_i
                  universo[[man_col]][idx]  <- (tipo_i == "manual")
                  universo[[tipo_col]][idx] <- tipo_i
                  if (modo_detalle == "ajustado" && tipo_i %in% c("auto", "ruta_sin_kg", "manual")) {
                    universo[[lock_col]][idx] <- FALSE
                  }
                }
              }
            }
          }
        }
      }
    }
    
    # Cálculo de factores de tendencia floral como respaldo si no hay motivo guardado
    df_mad_actual <- rv_maduracion()
    if (is.null(df_mad_actual) || nrow(df_mad_actual) == 0) df_mad_actual <- cargar_maduracion_disco()
    
    df_c_hist <- if (exists("df_conteo_historico") && is.data.frame(df_conteo_historico) && nrow(df_conteo_historico) > 0) {
      df_conteo_historico
    } else if (file.exists("historico_conteo.rds")) {
      readRDS("historico_conteo.rds")
    } else {
      tibble()
    }
    
    df_factores_flor_u <- tryCatch({
      calcular_factor_tendencia_floral(
        df_conteo        = df_c_hist,
        campana_sel      = input$filtro_campana,
        sem_cosecha      = input$filtro_semana,
        df_maduracion    = df_mad_actual,
        sensibilidad_pct = rv_sensibilidad_floral()
      )
    }, error = function(e) tibble())
    
    # Cálculo de totales consolidados (incluyendo Domingo en Ajuste por si hubiera cosecha dominical)
    universo <- universo %>%
      mutate(
        Módulo_join   = trimws(as.character(Módulo)),
        Turno_join    = trimws(as.character(Turno)),
        Variedad_join = limpiar_variedad(Variedad)
      ) %>%
      left_join(ha_ajustadas_ed, by = c("Módulo_join" = "Módulo_k", "Turno_join" = "Turno_k", "Variedad_join" = "Var_k"))
    
    if (!is.null(df_factores_flor_u) && nrow(df_factores_flor_u) > 0) {
      universo <- universo %>%
        left_join(
          df_factores_flor_u %>%
            mutate(
              Módulo_f = trimws(as.character(Módulo)),
              Turno_f  = trimws(as.character(Turno)),
              Var_f    = limpiar_variedad(Variedad)
            ) %>%
            select(Módulo_f, Turno_f, Var_f, Factor_Floral_u = Factor_Floral, Tipo_Efecto_Floral_u = Tipo_Efecto_Floral),
          by = c("Módulo_join" = "Módulo_f", "Turno_join" = "Turno_f", "Variedad_join" = "Var_f")
        )
    } else {
      universo <- universo %>%
        mutate(Factor_Floral_u = 1.0, Tipo_Efecto_Floral_u = "Neutro")
    }
    
    universo <- universo %>%
      mutate(
        across(c(Lunes, Martes, Miércoles, Jueves, Viernes, Sábado, Domingo), ~ coalesce(.x, 0)),
        Ajuste           = round(Lunes + Martes + Miércoles + Jueves + Viernes + Sábado + Domingo, 2),
        Proyección       = round(Proyección, 2),
        
        # Kilos Reales SAP: suma estricta de días con dato oficial de SAP
        Kg_SAP_Real      = round(
          ifelse(Lunes_tipo == "sap", Lunes, 0) +
          ifelse(Martes_tipo == "sap", Martes, 0) +
          ifelse(Miércoles_tipo == "sap", Miércoles, 0) +
          ifelse(Jueves_tipo == "sap", Jueves, 0) +
          ifelse(Viernes_tipo == "sap", Viernes, 0) +
          ifelse(Sábado_tipo == "sap", Sábado, 0) +
          coalesce(Domingo, 0),
          2
        ),
        
        # Desglose de pasadas de la cosecha real basado en cruce exacto válvula-día de SAP con campo
        Kg_Real_2da      = case_when(
          limpiar_variedad(Variedad) == "ROCIO" & coalesce(Kg_Real_2da_sap, 0) > 0 ~ 
            round(pmin(Kg_SAP_Real, Kg_Real_2da_sap), 2),
          limpiar_variedad(Variedad) == "ROCIO" & coalesce(`Ha Real 2ª Pas`, 0) > 0 & coalesce(`Ha Real 1ª Pas` + `Ha Real 2ª Pas`, 0) > 0 ~ 
            round(pmin(Kg_SAP_Real, Kg_SAP_Real * (`Ha Real 2ª Pas` / (`Ha Real 1ª Pas` + `Ha Real 2ª Pas`))), 2),
          TRUE ~ 0
        ),
        Kg_Real_1ra      = case_when(
          limpiar_variedad(Variedad) == "ROCIO" ~ round(pmax(0, Kg_SAP_Real - Kg_Real_2da), 2),
          TRUE ~ Kg_SAP_Real
        ),
        
        `Kg/Ha Proy`     = ifelse(Área > 0 & !is.na(Proyección), round(Proyección / Área, 2), NA_real_),
        `Kg/Ha Real`     = ifelse(coalesce(`Ha Recorrida`, 0) > 0 & coalesce(Kg_SAP_Real, 0) > 0, round(Kg_SAP_Real / `Ha Recorrida`, 2), NA_real_),
        `Kg/Ha Real 1ª Pas` = case_when(
          coalesce(`Ha Real 1ª Pas`, 0) <= 0 ~ NA_real_,
          coalesce(`Ha Real 2ª Pas`, 0) <= 0 & coalesce(Kg_SAP_Real, 0) > 0 ~ round(Kg_SAP_Real / `Ha Real 1ª Pas`, 2),
          coalesce(Kg_Real_1ra, 0) > 0 ~ round(Kg_Real_1ra / `Ha Real 1ª Pas`, 2),
          TRUE ~ NA_real_
        ),
        `Kg/Ha Real 2ª Pas` = ifelse(coalesce(`Ha Real 2ª Pas`, 0) > 0 & coalesce(Kg_Real_2da, 0) > 0, round(Kg_Real_2da / `Ha Real 2ª Pas`, 2), NA_real_),
        Diferencia       = ifelse(!is.na(Proyección), round(Ajuste - Proyección, 2), NA_real_),
        
        Tiene_Dias_Ajustados = (Lunes_tipo %in% c("auto", "manual") |
                                  Martes_tipo %in% c("auto", "manual") |
                                  Miércoles_tipo %in% c("auto", "manual") |
                                  Jueves_tipo %in% c("auto", "manual") |
                                  Viernes_tipo %in% c("auto", "manual") |
                                  Sábado_tipo %in% c("auto", "manual")),
        
        # 1ª Pasada Ajustada: Faltante para completar el turno físico
        `Ha Aj. 1ª Pas` = case_when(
          Tipo == "Testblock" ~ 0,
          !Tiene_Dias_Ajustados ~ 0,
          coalesce(Tiene_Detalle_Pas, FALSE) & coalesce(Ha_Aj_1ra_Ed, 0) > 0 ~ round(pmin(Área, Ha_Aj_1ra_Ed), 2),
          coalesce(Tiene_Detalle_Pas, FALSE) & coalesce(Ha_Aj_2da_Ed, 0) > 0 ~ 0,
          coalesce(Tiene_Ha_Asig, FALSE) & coalesce(Ha_Ajustada_Total, 0) > 0 & limpiar_variedad(Variedad) != "ROCIO" ~ round(pmin(Área, Ha_Ajustada_Total), 2),
          coalesce(Tiene_Ha_Asig, FALSE) & coalesce(Ha_Ajustada_Total, 0) > 0 & limpiar_variedad(Variedad) == "ROCIO" ~ 
            round(pmin(Área, pmax(0, Área - coalesce(`Ha Recorrida`, 0))), 2),
          coalesce(`Ha Recorrida`, 0) > 0 ~ round(pmin(Área, pmax(0, Área - `Ha Recorrida`)), 2),
          Ajuste > 0 ~ round(Área, 2),
          TRUE ~ 0
        ),
        
        # 2ª Pasada Ajustada: Área que repite cosecha (exclusiva de ROCÍO)
        `Ha Aj. 2ª Pas` = case_when(
          Tipo == "Testblock" ~ 0,
          limpiar_variedad(Variedad) != "ROCIO" ~ 0,
          !Tiene_Dias_Ajustados ~ 0,
          coalesce(Tiene_Detalle_Pas, FALSE) & coalesce(Ha_Aj_2da_Ed, 0) > 0 ~ round(pmin(Área, Ha_Aj_2da_Ed), 2),
          coalesce(Tiene_Ha_Asig, FALSE) & coalesce(Ha_Ajustada_Total, 0) > `Ha Aj. 1ª Pas` ~ 
            round(pmin(Área, Ha_Ajustada_Total - `Ha Aj. 1ª Pas`), 2),
          TRUE ~ 0
        ),
        
        # Kilos ajustados netos (excluyendo lo ya cosechado en real)
        kg_aj_neto_u = pmax(0, Ajuste - coalesce(Kg_SAP_Real, 0)),
        Kg_Aj_1ra = case_when(
          Tipo == "Testblock" ~ 0,
          !Tiene_Dias_Ajustados ~ 0,
          coalesce(Tiene_Detalle_Pas, FALSE) & (coalesce(Kg_Aj_1ra_Ed, 0) + coalesce(Kg_Aj_2da_Ed, 0)) > 0 ~ 
            round(kg_aj_neto_u * (coalesce(Kg_Aj_1ra_Ed, 0) / pmax(1, coalesce(Kg_Aj_1ra_Ed, 0) + coalesce(Kg_Aj_2da_Ed, 0))), 2),
          coalesce(`Ha Aj. 1ª Pas`, 0) > 0 & coalesce(`Ha Aj. 2ª Pas`, 0) > 0 ~ 
            round(kg_aj_neto_u * (`Ha Aj. 1ª Pas` / pmax(0.01, `Ha Aj. 1ª Pas` + `Ha Aj. 2ª Pas` * 0.6)), 2),
          TRUE ~ kg_aj_neto_u
        ),
        Kg_Aj_2da = case_when(
          Tipo == "Testblock" ~ 0,
          limpiar_variedad(Variedad) != "ROCIO" ~ 0,
          !Tiene_Dias_Ajustados ~ 0,
          TRUE ~ round(pmax(0, kg_aj_neto_u - Kg_Aj_1ra), 2)
        ),
        Kg_Aj_Total = round(Kg_Aj_1ra + Kg_Aj_2da, 2),
        
        `Kg/Ha Aj. 1ª Pas` = ifelse(coalesce(`Ha Aj. 1ª Pas`, 0) > 0 & coalesce(Kg_Aj_1ra, 0) > 0, 
                                    round(Kg_Aj_1ra / `Ha Aj. 1ª Pas`, 2), NA_real_),
        `Kg/Ha Aj. 2ª Pas` = ifelse(limpiar_variedad(Variedad) == "ROCIO" & coalesce(`Ha Aj. 2ª Pas`, 0) > 0 & coalesce(Kg_Aj_2da, 0) > 0, 
                                    round(Kg_Aj_2da / `Ha Aj. 2ª Pas`, 2), NA_real_),
        
        `Ha Ajustada`    = round(`Ha Aj. 1ª Pas` + `Ha Aj. 2ª Pas`, 2),
        `Kg/Ha Ajustado` = ifelse(Área > 0, round(Ajuste / Área, 2), 0),
        
        # Motivo del Ajuste para el turno
        `Motivo del Ajuste` = case_when(
          Tipo == "Testblock" ~ "Testblock (no editable)",
          Lunes_manual | Martes_manual | Miércoles_manual | Jueves_manual | Viernes_manual | Sábado_manual ~ "Edición manual",
          !is.na(Motivo_Guardado) & nzchar(Motivo_Guardado) ~ Motivo_Guardado,
          Tiene_Dias_Ajustados & coalesce(`Ha Aj. 1ª Pas`, 0) == 0 & coalesce(`Ha Aj. 2ª Pas`, 0) > 0 ~ "2da pasada Rocío (60% sin floración)",
          Tiene_Dias_Ajustados ~ {
            pct_fl <- round((coalesce(Factor_Floral_u, 1.0) - 1) * 100, 1)
            txt_fl <- case_when(
              coalesce(Tipo_Efecto_Floral_u, "") == "Premio"  ~ sprintf("Premio floral %+0.1f%%", pct_fl),
              coalesce(Tipo_Efecto_Floral_u, "") == "Castigo" ~ sprintf("Castigo floral %+0.1f%%", pct_fl),
              TRUE                                            ~ "Floral neutro (0%)"
            )
            paste0("Ajuste automático • ", txt_fl)
          },
          coalesce(Ajuste, 0) == 0 & coalesce(Proyección, 0) == 0 ~ "-",
          coalesce(Diferencia, 0) == 0 ~ "Sin ajuste (Proy. original)",
          TRUE ~ "-"
        )
      ) %>%
      select(-any_of(c("Ha_Ajustada_Total", "Tiene_Ha_Asig", "Tiene_Detalle_Pas", "Ha_Aj_1ra_Ed", "Kg_Aj_1ra_Ed", "Ha_Aj_2da_Ed", "Kg_Aj_2da_Ed", "Tiene_Dias_Ajustados", "Módulo_join", "Turno_join", "Variedad_join", "Motivo_Guardado", "Factor_Floral_u", "Tipo_Efecto_Floral_u")))
    
    return(universo)
  })
  
  # Observador 1: Módulos (Escucha a universo_base para no re-ejecutarse al editar celdas)
  observeEvent(list(universo_base(), input$filtro_grupo), {
    df_u <- universo_base()
    if (is.null(df_u) || nrow(df_u) == 0) {
      updateSelectizeInput(session, "filtro_modulo", choices = character(0), selected = character(0))
      return()
    }
    
    if (!is.null(input$filtro_grupo) && length(input$filtro_grupo) > 0) {
      mods_sel <- unlist(grupos_modulos[input$filtro_grupo], use.names = FALSE)
      df_u <- df_u %>%
        filter(suppressWarnings(as.numeric(sub("\\..*", "", as.character(Módulo)))) %in% mods_sel)
    }
    
    mods_disp <- sort(unique(na.omit(as.character(df_u$Módulo))))
    mods_disp <- mods_disp[order(suppressWarnings(as.numeric(sub("\\..*", "", mods_disp))), mods_disp)]
    
    sel_prev <- isolate(input$filtro_modulo)
    sel_nueva <- intersect(sel_prev, mods_disp)
    
    updateSelectizeInput(
      session, 
      "filtro_modulo", 
      choices = mods_disp, 
      selected = sel_nueva,
      server = FALSE
    )
  }, ignoreInit = FALSE)
  
  # Observador 2: Turnos (Escucha a universo_base para no re-ejecutarse al editar celdas)
  observeEvent(list(universo_base(), input$filtro_grupo, input$filtro_modulo), {
    df_u <- universo_base()
    if (is.null(df_u) || nrow(df_u) == 0) {
      updateSelectizeInput(session, "filtro_turno", choices = character(0), selected = character(0))
      return()
    }
    
    if (!is.null(input$filtro_grupo) && length(input$filtro_grupo) > 0) {
      mods_sel <- unlist(grupos_modulos[input$filtro_grupo], use.names = FALSE)
      df_u <- df_u %>%
        filter(suppressWarnings(as.numeric(sub("\\..*", "", as.character(Módulo)))) %in% mods_sel)
    }
    
    if (!is.null(input$filtro_modulo) && length(input$filtro_modulo) > 0) {
      df_u <- df_u %>%
        filter(as.character(Módulo) %in% as.character(input$filtro_modulo))
    }
    
    tur_disp <- sort(unique(na.omit(as.character(df_u$Turno))))
    tur_disp <- tur_disp[order(suppressWarnings(as.numeric(tur_disp)), tur_disp)]
    
    sel_prev <- isolate(input$filtro_turno)
    sel_nueva <- intersect(sel_prev, tur_disp)
    
    updateSelectizeInput(
      session, 
      "filtro_turno", 
      choices = tur_disp, 
      selected = sel_nueva,
      server = FALSE
    )
  }, ignoreInit = FALSE)
  
  # Observador 3: Variedad (Escucha a universo_base para no re-ejecutarse al editar celdas)
  observeEvent(list(universo_base(), input$filtro_grupo, input$filtro_modulo, input$filtro_turno), {
    df_u <- universo_base()
    if (is.null(df_u) || nrow(df_u) == 0) {
      updateSelectizeInput(session, "filtro_variedad", choices = character(0), selected = character(0))
      return()
    }
    
    if (!is.null(input$filtro_grupo) && length(input$filtro_grupo) > 0) {
      mods_sel <- unlist(grupos_modulos[input$filtro_grupo], use.names = FALSE)
      df_u <- df_u %>%
        filter(suppressWarnings(as.numeric(sub("\\..*", "", as.character(Módulo)))) %in% mods_sel)
    }
    
    if (!is.null(input$filtro_modulo) && length(input$filtro_modulo) > 0) {
      df_u <- df_u %>%
        filter(as.character(Módulo) %in% as.character(input$filtro_modulo))
    }
    
    if (!is.null(input$filtro_turno) && length(input$filtro_turno) > 0) {
      df_u <- df_u %>%
        filter(as.character(Turno) %in% as.character(input$filtro_turno))
    }
    
    vars_disponibles <- sort(unique(na.omit(limpiar_variedad(df_u$Variedad))))
    vars_disponibles <- vars_disponibles[vars_disponibles != ""]
    
    sel_prev <- isolate(input$filtro_variedad)
    sel_nueva <- intersect(sel_prev, vars_disponibles)
    
    updateSelectizeInput(
      session, 
      "filtro_variedad", 
      choices = vars_disponibles, 
      selected = sel_nueva,
      server = FALSE
    )
  }, ignoreInit = FALSE)
  
  # Dimensiones activas en 'Agrupar por' (Orden canónico)
  # Dimensiones activas en "Agrupar por" (Orden canónico)
  dims_todas_agrupar  <- c("Sector", "Módulo", "Turno", "Válvula", "Variedad", "Tipo")
  dims_tabla_agrupar  <- c("Sector", "Módulo", "Turno", "Variedad", "Tipo")
  dims_conteo_agrupar <- c("Sector", "Módulo", "Turno", "Válvula", "Variedad")
  
  cols_agrupar <- reactive({
    sel <- input$dims_agrupar
    if (is.null(sel) || length(sel) == 0) {
      return(dims_tabla_agrupar)
    }
    activas <- intersect(dims_todas_agrupar, as.character(sel))
    if (length(activas) == 0) dims_tabla_agrupar else activas
  })
  
  observeEvent(input$alerta_minimo_agrupar, {
    showNotification(
      "Debe mantener al menos 1 dimensión activa en 'Agrupar por'.",
      type = "warning",
      duration = 3
    )
  })
  
  # Identifica si hay algún filtro específico de grupo, módulo, turno o variedad activo
  hay_filtro_tabla_activo <- reactive({
    (!is.null(input$filtro_grupo) && length(input$filtro_grupo) > 0) ||
    (!is.null(input$filtro_modulo) && length(input$filtro_modulo) > 0) ||
    (!is.null(input$filtro_turno) && length(input$filtro_turno) > 0) ||
    (!is.null(input$filtro_variedad) && length(input$filtro_variedad) > 0)
  })
  
  # Conjunto de turnos activos/visibles según los filtros actuales
  turnos_activos_filtrados <- reactive({
    u <- universo_completo()
    if (is.null(u) || nrow(u) == 0) return(tibble())
    
    if (!is.null(input$filtro_grupo) && length(input$filtro_grupo) > 0) {
      mods_permitidos <- unlist(grupos_modulos[input$filtro_grupo], use.names = FALSE)
      u <- u %>%
        filter(suppressWarnings(as.numeric(sub("\\..*", "", as.character(Módulo)))) %in% mods_permitidos)
    }
    if (!is.null(input$filtro_modulo) && length(input$filtro_modulo) > 0) {
      u <- u %>% filter(as.character(Módulo) %in% as.character(input$filtro_modulo))
    }
    if (!is.null(input$filtro_turno) && length(input$filtro_turno) > 0) {
      u <- u %>% filter(as.character(Turno) %in% as.character(input$filtro_turno))
    }
    if (!is.null(input$filtro_variedad) && length(input$filtro_variedad) > 0) {
      u <- u %>% filter(limpiar_variedad(Variedad) %in% limpiar_variedad(input$filtro_variedad))
    }
    
    u %>%
      distinct(
        Sector   = trimws(as.character(Sector)),
        Módulo   = trimws(as.character(Módulo)),
        Turno    = trimws(as.character(Turno)),
        Variedad = limpiar_variedad(Variedad)
      )
  })

  # Datos filtrados según las selecciones interactivas y agrupados según 'Agrupar por'
  datos_reactivos <- reactive({
    universo <- universo_completo()
    if (is.null(universo) || nrow(universo) == 0) return(tibble())
    
    # 1. Aplicar primero los filtros globales
    if (!is.null(input$filtro_grupo) && length(input$filtro_grupo) > 0) {
      mods_permitidos <- unlist(grupos_modulos[input$filtro_grupo], use.names = FALSE)
      universo <- universo %>%
        filter(suppressWarnings(as.numeric(sub("\\..*", "", as.character(Módulo)))) %in% mods_permitidos)
    }
    
    if (!is.null(input$filtro_modulo) && length(input$filtro_modulo) > 0) {
      universo <- universo %>%
        filter(as.character(Módulo) %in% as.character(input$filtro_modulo))
    }
    
    if (!is.null(input$filtro_turno) && length(input$filtro_turno) > 0) {
      universo <- universo %>%
        filter(as.character(Turno) %in% as.character(input$filtro_turno))
    }
    
    if (!is.null(input$filtro_variedad) && length(input$filtro_variedad) > 0) {
      universo <- universo %>% 
        filter(limpiar_variedad(Variedad) %in% limpiar_variedad(input$filtro_variedad))
    }
    
    if (nrow(universo) == 0) return(tibble())
    
    universo <- universo %>%
      mutate(
        Sector   = trimws(as.character(Sector)),
        Módulo   = trimws(as.character(Módulo)),
        Turno    = trimws(as.character(Turno)),
        Variedad = limpiar_variedad(Variedad),
        Tipo     = case_when(
          grepl("test|tb", Tipo, ignore.case = TRUE)  ~ "Testblock",
          grepl("org[aá]n", Tipo, ignore.case = TRUE) ~ "Orgánico",
          grepl("conv", Tipo, ignore.case = TRUE)     ~ "Convencional",
          TRUE                                        ~ tools::toTitleCase(tolower(trimws(Tipo)))
        )
      )
    
    # 2. Agrupar según los checks activos en 'Agrupar por'
    dims_activas <- intersect(dims_tabla_agrupar, cols_agrupar())
    if (length(dims_activas) == 0) dims_activas <- dims_tabla_agrupar
    
    if (length(dims_activas) < 5) {
      tipo_dia_agrupado <- function(vals_kg, vals_tipo) {
        kg_pos <- coalesce(vals_kg, 0) > 0
        if (!any(kg_pos)) return("none")
        tipos_pos <- vals_tipo[kg_pos]
        if (any(tipos_pos == "manual", na.rm = TRUE))      return("manual")
        if (any(tipos_pos == "ruta_sin_kg", na.rm = TRUE)) return("ruta_sin_kg")
        if (any(tipos_pos == "auto", na.rm = TRUE))        return("auto")
        if (any(tipos_pos == "sap", na.rm = TRUE))         return("sap")
        "none"
      }
      
      universo <- universo %>%
        group_by(across(all_of(dims_activas))) %>%
        summarise(
          Proyección          = round(sum(Proyección, na.rm = TRUE), 2),
          Área                = round(sum(Área, na.rm = TRUE), 2),
          `Ha Recorrida`      = round(sum(`Ha Recorrida`, na.rm = TRUE), 2),
          `Ha Real 1ª Pas`    = round(sum(`Ha Real 1ª Pas`, na.rm = TRUE), 2),
          `Ha Real 2ª Pas`    = round(sum(`Ha Real 2ª Pas`, na.rm = TRUE), 2),
          `Ha Aj. 1ª Pas`     = round(sum(`Ha Aj. 1ª Pas`, na.rm = TRUE), 2),
          `Ha Aj. 2ª Pas`     = round(sum(`Ha Aj. 2ª Pas`, na.rm = TRUE), 2),
          `Ha Ajustada`       = round(sum(`Ha Ajustada`, na.rm = TRUE), 2),
          Kg_Aj_1ra_tot       = sum(coalesce(Kg_Aj_1ra, 0), na.rm = TRUE),
          Kg_Aj_2da_tot       = sum(coalesce(Kg_Aj_2da, 0), na.rm = TRUE),
          Kg_SAP_Real         = round(sum(coalesce(Kg_SAP_Real, 0), na.rm = TRUE), 2),
          Kg_Real_1ra         = round(sum(coalesce(Kg_Real_1ra, 0), na.rm = TRUE), 2),
          Kg_Real_2da         = round(sum(coalesce(Kg_Real_2da, 0), na.rm = TRUE), 2),
          Proy_manual         = any(Proy_manual, na.rm = TRUE),
          Lunes_manual        = any(Lunes_manual & coalesce(Lunes, 0) > 0, na.rm = TRUE),
          Martes_manual       = any(Martes_manual & coalesce(Martes, 0) > 0, na.rm = TRUE),
          Miércoles_manual    = any(Miércoles_manual & coalesce(Miércoles, 0) > 0, na.rm = TRUE),
          Jueves_manual       = any(Jueves_manual & coalesce(Jueves, 0) > 0, na.rm = TRUE),
          Viernes_manual      = any(Viernes_manual & coalesce(Viernes, 0) > 0, na.rm = TRUE),
          Sábado_manual       = any(Sábado_manual & coalesce(Sábado, 0) > 0, na.rm = TRUE),
          Lunes_tipo          = tipo_dia_agrupado(Lunes, Lunes_tipo),
          Martes_tipo         = tipo_dia_agrupado(Martes, Martes_tipo),
          Miércoles_tipo      = tipo_dia_agrupado(Miércoles, Miércoles_tipo),
          Jueves_tipo         = tipo_dia_agrupado(Jueves, Jueves_tipo),
          Viernes_tipo        = tipo_dia_agrupado(Viernes, Viernes_tipo),
          Sábado_tipo         = tipo_dia_agrupado(Sábado, Sábado_tipo),
          Lunes               = sum(coalesce(Lunes, 0), na.rm = TRUE),
          Martes              = sum(coalesce(Martes, 0), na.rm = TRUE),
          Miércoles           = sum(coalesce(Miércoles, 0), na.rm = TRUE),
          Jueves              = sum(coalesce(Jueves, 0), na.rm = TRUE),
          Viernes             = sum(coalesce(Viernes, 0), na.rm = TRUE),
          Sábado              = sum(coalesce(Sábado, 0), na.rm = TRUE),
          Ajuste              = round(sum(Ajuste, na.rm = TRUE), 2),
          `Motivo del Ajuste` = {
            mots <- na.omit(`Motivo del Ajuste`[`Motivo del Ajuste` != "-" & `Motivo del Ajuste` != "" & !is.na(`Motivo del Ajuste`)])
            if (length(mots) == 0) "-" else paste(unique(mots), collapse = " | ")
          },
          Proy_bloqueado      = TRUE,
          Lunes_bloqueado     = TRUE,
          Martes_bloqueado    = TRUE,
          Miércoles_bloqueado = TRUE,
          Jueves_bloqueado    = TRUE,
          Viernes_bloqueado   = TRUE,
          Sábado_bloqueado    = TRUE,
          .groups             = "drop"
        ) %>%
        mutate(
          `Kg/Ha Proy`       = ifelse(Área > 0 & Proyección > 0, round(Proyección / Área, 2), NA_real_),
          `Kg/Ha Real`       = ifelse(`Ha Recorrida` > 0 & Kg_SAP_Real > 0, round(Kg_SAP_Real / `Ha Recorrida`, 2), NA_real_),
          `Kg/Ha Real 1ª Pas` = case_when(`Ha Real 1ª Pas` <= 0 ~ NA_real_, `Ha Real 2ª Pas` <= 0 & Kg_SAP_Real > 0 ~ round(Kg_SAP_Real / `Ha Real 1ª Pas`, 2), Kg_Real_1ra > 0 ~ round(Kg_Real_1ra / `Ha Real 1ª Pas`, 2), TRUE ~ NA_real_),
          `Kg/Ha Real 2ª Pas` = ifelse(`Ha Real 2ª Pas` > 0 & Kg_Real_2da > 0, round(Kg_Real_2da / `Ha Real 2ª Pas`, 2), NA_real_),
          `Kg/Ha Aj. 1ª Pas` = ifelse(`Ha Aj. 1ª Pas` > 0 & Kg_Aj_1ra_tot > 0, round(Kg_Aj_1ra_tot / `Ha Aj. 1ª Pas`, 2), NA_real_),
          `Kg/Ha Aj. 2ª Pas` = ifelse(`Ha Aj. 2ª Pas` > 0 & Kg_Aj_2da_tot > 0, round(Kg_Aj_2da_tot / `Ha Aj. 2ª Pas`, 2), NA_real_),
          `Kg/Ha Ajustado`   = ifelse(Área > 0, round(Ajuste / Área, 2), 0),
          Diferencia         = round(Ajuste - Proyección, 2),
          Kg_Aj_1ra          = round(Kg_Aj_1ra_tot, 2),
          Kg_Aj_2da          = round(Kg_Aj_2da_tot, 2),
          Kg_Aj_Total        = round(Kg_Aj_1ra_tot + Kg_Aj_2da_tot, 2)
        )
      
      for (d_col in setdiff(dims_tabla_agrupar, dims_activas)) {
        universo[[d_col]] <- "Todos"
      }
    }
    
    resultado <- universo %>%
      mutate(
        across(c(Lunes, Martes, Miércoles, Jueves, Viernes, Sábado), ~ ifelse(.x == 0, NA_real_, round(.x, 2)))
      ) %>%
      select(
        Sector, Módulo, Turno, Variedad, Tipo,
        Proyección, Área, `Ha Real 1ª Pas`, `Ha Real 2ª Pas`, `Ha Aj. 1ª Pas`, `Ha Aj. 2ª Pas`,
        `Kg/Ha Proy`, `Kg/Ha Real 1ª Pas`, `Kg/Ha Real 2ª Pas`, `Kg/Ha Aj. 1ª Pas`, `Kg/Ha Aj. 2ª Pas`,
        Lunes, Martes, Miércoles, Jueves, Viernes, Sábado,
        Ajuste, Diferencia, `Motivo del Ajuste`,
        Proy_manual, Lunes_manual, Martes_manual, Miércoles_manual, Jueves_manual, Viernes_manual, Sábado_manual,
        Proy_bloqueado, Lunes_bloqueado, Martes_bloqueado, Miércoles_bloqueado, Jueves_bloqueado, Viernes_bloqueado, Sábado_bloqueado,
        Lunes_tipo, Martes_tipo, Miércoles_tipo, Jueves_tipo, Viernes_tipo, Sábado_tipo,
        Kg_SAP_Real, Kg_Real_1ra, Kg_Real_2da, Kg_Aj_1ra, Kg_Aj_2da, Kg_Aj_Total,
        `Ha Recorrida`, `Kg/Ha Real`
      ) %>%
      arrange(
        suppressWarnings(as.numeric(Sector)), Sector,
        suppressWarnings(as.numeric(Módulo)), Módulo,
        suppressWarnings(as.numeric(Turno)), Turno,
        Variedad, Tipo
      )
    
    return(resultado)
  })
  
  # KPIs
  # Kilos de Testblock de la semana seleccionada (aislados de la tabla, mapas, frecuencias y rendimientos)
  kg_testblock_reactivo <- reactive({
    req(input$filtro_campana, input$filtro_semana)
    # Si el usuario filtra por un Grupo, Módulo o Turno comercial específico, Testblock (Sector 4) es 0
    if ((!is.null(input$filtro_grupo) && length(input$filtro_grupo) > 0) ||
        (!is.null(input$filtro_modulo) && length(input$filtro_modulo) > 0) ||
        (!is.null(input$filtro_turno) && length(input$filtro_turno) > 0)) {
      return(0)
    }
    df_tb <- rv_cosecha_testblock()
    if (is.null(df_tb) || !is.data.frame(df_tb) || nrow(df_tb) == 0) return(0)
    df_tb <- df_tb %>%
      filter(
        as.character(Campaña) == as.character(input$filtro_campana),
        as.numeric(Semana) == as.numeric(input$filtro_semana)
      )
    if (!is.null(input$filtro_variedad) && length(input$filtro_variedad) > 0) {
      df_tb <- df_tb %>%
        filter(limpiar_variedad(Variedad) %in% limpiar_variedad(input$filtro_variedad))
    }
    sum(coalesce(df_tb$`Kg Total`, 0), na.rm = TRUE)
  })

  # 1. Cosecha Real (Total = Comercial + Testblock)
  output$kpi_kilos_real <- renderText({
    df <- datos_reactivos()
    kg_com <- if (nrow(df) > 0) sum(coalesce(df$Kg_SAP_Real, 0), na.rm = TRUE) else 0
    kg_tb  <- kg_testblock_reactivo()
    kg_tot <- kg_com + kg_tb
    if (kg_tot <= 0) return("0")
    format(round(kg_tot, 1), big.mark = ",")
  })

  output$ui_kpi_real_detalle <- renderUI({
    df <- datos_reactivos()
    kg_com      <- if (nrow(df) > 0) sum(coalesce(df$Kg_SAP_Real, 0), na.rm = TRUE) else 0
    kg_tb       <- kg_testblock_reactivo()
    kg_tot      <- kg_com + kg_tb
    kg_real_1ra <- if (nrow(df) > 0) sum(coalesce(df$Kg_Real_1ra, 0), na.rm = TRUE) else 0
    kg_real_2da <- if (nrow(df) > 0) sum(coalesce(df$Kg_Real_2da, 0), na.rm = TRUE) else 0
    
    sem_sel  <- as.numeric(input$filtro_semana)
    camp_sel <- as.character(input$filtro_campana)
    
    df_concil <- tryCatch(obtener_cosecha_conciliada_semana(sem_sel, camp_sel), error = function(e) NULL)
    kg_desfase_reasig <- 0
    kg_sin_ruta_excl  <- 0
    
    if (!is.null(df_concil) && nrow(df_concil) > 0) {
      if ("Kg_Desfase" %in% names(df_concil)) {
        kg_desfase_reasig <- sum(coalesce(df_concil$Kg_Desfase, 0), na.rm = TRUE)
      }
      sin_r_mask <- !df_concil$Tiene_Campo & df_concil$Tiene_SAP
      if (any(sin_r_mask)) {
        kg_sin_ruta_excl <- sum(coalesce(df_concil$Kg_Total_sap[sin_r_mask], 0), na.rm = TRUE)
      }
    }
    
    if (kg_tot <= 0 && kg_sin_ruta_excl <= 0) {
      return(tags$span(
        class = "kpi-badge",
        style = "background: rgba(142, 142, 147, 0.12); color: #8E8E93; border: 1px solid rgba(142, 142, 147, 0.25); font-size: 11px;",
        "Sin cosecha SAP"
      ))
    }
    
    tagList(
      tags$div(
        style = "display: flex; flex-direction: column; gap: 4px; margin-top: 5px;",
        tags$div(
          style = "display: flex; align-items: center; gap: 6px; flex-wrap: wrap;",
          # 1. AZUL: Comercial Operativo (Kilos que SÍ usaremos con desfase reasignado)
          tags$span(
            class = "kpi-badge",
            style = "background: rgba(0, 122, 255, 0.10); color: #007AFF; border: 1px solid rgba(0, 122, 255, 0.28); font-size: 11px; padding: 2px 7px; border-radius: 6px; font-weight: 600;",
            title = "Kilos comerciales oficiales que SÍ entran a cálculo de ajuste y proyecciones",
            sprintf("Comercial: %s kg", format(round(kg_com, 0), big.mark = ","))
          ),
          # 2. AZUL: Testblock (Entra al KPI pero NO a ajustes)
          if (kg_tb > 0) {
            tags$span(
              class = "kpi-badge",
              style = "background: rgba(0, 122, 255, 0.08); color: #007AFF; border: 1px solid rgba(0, 122, 255, 0.22); font-size: 11px; padding: 2px 7px; border-radius: 6px; font-weight: 600;",
              title = "Kilos de Testblock cosechados. Entran al KPI total, pero NO al ajuste",
              sprintf("Testblock: %s kg", format(round(kg_tb, 0), big.mark = ","))
            )
          },
          # 3. AZUL: Reasig. Desfase (Kilos de desfase unificados que SÍ se usan)
          if (kg_desfase_reasig > 0) {
            tags$span(
              class = "kpi-badge",
              style = "background: rgba(0, 122, 255, 0.10); color: #007AFF; border: 1px solid rgba(0, 122, 255, 0.28); font-size: 11px; padding: 2px 7px; border-radius: 6px; font-weight: 600;",
              title = "Kilos desfasados de días contiguos reasignados a la fecha de campo (SÍ se usan en cálculo y ajuste)",
              sprintf("Reasig. Desfase: %s kg", format(round(kg_desfase_reasig, 0), big.mark = ","))
            )
          },
          # 4. NARANJA: Sin Ruta (Posiblemente sí / Por corregir con campo)
          if (kg_sin_ruta_excl > 0) {
            tags$span(
              class = "kpi-badge",
              style = "background: rgba(255, 149, 0, 0.12); color: #D97706; border: 1px solid rgba(255, 149, 0, 0.32); font-size: 11px; padding: 2px 7px; border-radius: 6px; font-weight: 600;",
              title = "Kilos en balanza SAP sin ruta en campo. Pendiente de corregir (debería ser 0 kg)",
              sprintf("Sin Ruta: %s kg", format(round(kg_sin_ruta_excl, 0), big.mark = ","))
            )
          }
        ),
        if (kg_real_2da > 0) {
          tags$span(
            class = "kpi-badge",
            style = "background: rgba(142, 142, 147, 0.08); color: #636366; border: 1px solid rgba(142, 142, 147, 0.20); font-size: 10.5px; padding: 1px 6px; border-radius: 5px; width: fit-content;",
            sprintf("Pasadas Com.: 1ª: %s kg | 2ª: %s kg", format(round(kg_real_1ra, 0), big.mark = ","), format(round(kg_real_2da, 0), big.mark = ","))
          )
        }
      )
    )
  })
  output$kpi_kilos_cierre <- renderText({
    df <- datos_reactivos()
    if (nrow(df) == 0) return("0")
    format(round(sum(df$Ajuste, na.rm = TRUE), 1), big.mark = ",")
  })

  output$ui_kpi_ajuste_detalle <- renderUI({
    df <- datos_reactivos()
    if (nrow(df) == 0) {
      return(tags$span(class = "kpi-badge", style = "background: rgba(245, 158, 11, 0.12); color: #D97706; border: 1px solid rgba(245, 158, 11, 0.22); font-size: 11px;", "Sin datos"))
    }
    
    dias_semana_nombres <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
    dias_abrev <- c(Lunes = "Lun", Martes = "Mar", Miércoles = "Mié", Jueves = "Jue", Viernes = "Vie", Sábado = "Sáb")
    
    # Días que tienen ajuste activo (auto o manual con kilos > 0)
    dias_con_ajuste <- dias_semana_nombres[sapply(dias_semana_nombres, function(d) {
      tipo_col <- paste0(d, "_tipo")
      if (tipo_col %in% names(df)) {
        any(df[[tipo_col]] %in% c("auto", "manual") & coalesce(df[[d]], 0) > 0, na.rm = TRUE)
      } else {
        FALSE
      }
    })]
    
    # Kilos reales base tomados directamente de la columna Kg_SAP_Real de la tabla (Comercial oficial con reconciliaciones)
    kg_real_base <- if (nrow(df) > 0) sum(coalesce(df$Kg_SAP_Real, 0), na.rm = TRUE) else 0
    kg_cierre_tot <- if (nrow(df) > 0) sum(coalesce(df$Ajuste, 0), na.rm = TRUE) else 0
    kg_aj_tot     <- pmax(0, kg_cierre_tot - kg_real_base)
    
    kg_aj_2da <- if (nrow(df) > 0) sum(coalesce(df$Kg_Aj_2da, 0), na.rm = TRUE) else 0
    kg_aj_1ra <- pmax(0, kg_aj_tot - kg_aj_2da)
    
    # Si no hay ajuste activo en la semana
    if (length(dias_con_ajuste) == 0 || kg_aj_tot <= 0) {
      return(tags$span(
        class = "kpi-badge",
        style = "background: rgba(142, 142, 147, 0.12); color: #8E8E93; border: 1px solid rgba(142, 142, 147, 0.25); font-size: 11px;",
        "Sin ajuste activo (100% Real)"
      ))
    }
    
    # Días previos reales tomados como base hasta el inicio del ajuste
    idx_primer_aj <- min(match(dias_con_ajuste, dias_semana_nombres))
    dias_reales_prev <- if (idx_primer_aj > 1) dias_semana_nombres[1:(idx_primer_aj - 1)] else character(0)
    
    # Etiquetas de rango de días
    lbl_real_dias <- if (length(dias_reales_prev) == 1) {
      dias_abrev[dias_reales_prev[1]]
    } else if (length(dias_reales_prev) > 1) {
      paste0(dias_abrev[dias_reales_prev[1]], "-", dias_abrev[dias_reales_prev[length(dias_reales_prev)]])
    } else {
      ""
    }
    
    lbl_aj_dias <- if (length(dias_con_ajuste) == 1) {
      dias_abrev[dias_con_ajuste[1]]
    } else if (length(dias_con_ajuste) > 1) {
      paste0(dias_abrev[dias_con_ajuste[1]], "-", dias_abrev[dias_con_ajuste[length(dias_con_ajuste)]])
    } else {
      ""
    }
    
    # Texto para ajuste con desglose de 1ra y 2da pasada
    txt_ajuste <- if (kg_aj_2da > 0) {
      sprintf("Ajus (%s): +%s kg (1ª: %s | 2ª: %s)", 
              lbl_aj_dias, 
              format(round(kg_aj_tot, 0), big.mark = ","), 
              format(round(kg_aj_1ra, 0), big.mark = ","), 
              format(round(kg_aj_2da, 0), big.mark = ","))
    } else {
      sprintf("Ajus (%s): +%s kg (1ª Pasada)", 
              lbl_aj_dias, 
              format(round(kg_aj_tot, 0), big.mark = ","))
    }
    
    tagList(
      tags$div(
        style = "display: flex; flex-direction: column; gap: 3px; margin-top: 5px;",
        if (kg_real_base > 0) {
          tags$span(
            class = "kpi-badge",
            style = "background: rgba(0, 122, 255, 0.10); color: #007AFF; border: 1px solid rgba(0, 122, 255, 0.22); font-size: 11px; padding: 2px 8px; width: fit-content;",
            sprintf("Real (%s): %s kg", lbl_real_dias, format(round(kg_real_base, 0), big.mark = ","))
          )
        },
        tags$span(
          class = "kpi-badge",
          style = "background: rgba(245, 158, 11, 0.12); color: #D97706; border: 1px solid rgba(245, 158, 11, 0.22); font-size: 11px; padding: 2px 8px; width: fit-content;",
          txt_ajuste
        )
      )
    )
  })
  
  # 3. Proyección Total
  output$kpi_proyeccion <- renderText({
    df <- datos_reactivos()
    if (nrow(df) == 0) return("0")
    format(round(sum(df$Proyección, na.rm = TRUE), 1), big.mark = ",")
  })
  
  # 4. Área en Evaluación
  output$kpi_area <- renderText({
    df <- datos_reactivos()
    if (nrow(df) == 0) return("0")
    format(round(sum(df$Área, na.rm = TRUE), 2), big.mark = ",")
  })
  
  output$kpi_area_badge <- renderText({
    df <- datos_reactivos()
    if (nrow(df) == 0) return("Hectáreas")
    rec <- sum(df$`Ha Recorrida`, na.rm = TRUE)
    ajus <- sum(coalesce(df$`Ha Aj. 1ª Pas`, 0) + coalesce(df$`Ha Aj. 2ª Pas`, 0), na.rm = TRUE)
    sprintf("Rec: %s ha | Ajus: %s ha", format(round(rec, 1), big.mark = ","), format(round(ajus, 1), big.mark = ","))
  })
  
  # 5. Cumplimiento General (con Real y con Ajuste respecto a la Proyección)
  output$kpi_cumplimiento <- renderText({
    df <- datos_reactivos()
    if (nrow(df) == 0) return("0%")
    tot_sem  <- sum(df$Ajuste, na.rm = TRUE)
    tot_proy <- sum(df$Proyección, na.rm = TRUE)
    if (tot_proy > 0) {
      paste0(round((tot_sem / tot_proy) * 100, 1), "%")
    } else {
      "-"
    }
  })

  output$kpi_cumplimiento_badge <- renderText({
    df <- datos_reactivos()
    if (nrow(df) == 0) return("% Meta")
    tot_proy  <- sum(coalesce(df$Proyección, 0), na.rm = TRUE)
    if (tot_proy <= 0) return("% Meta")
    
    kg_real   <- sum(coalesce(df$Kg_SAP_Real, 0), na.rm = TRUE)
    tot_sem   <- sum(coalesce(df$Ajuste, 0), na.rm = TRUE)
    kg_aj_tot <- sum(coalesce(df$Kg_Aj_Total, 0), na.rm = TRUE)
    
    cump_real <- round((kg_real / tot_proy) * 100, 1)
    cump_sem  <- round((tot_sem / tot_proy) * 100, 1)
    
    if (kg_aj_tot > 0) {
      sprintf("Real: %s%% | Ajuste: %s%%", cump_real, cump_sem)
    } else {
      sprintf("Real vs Proy (%s%%)", cump_real)
    }
  })
  
  # ==============================================================================
  # DESCARGA DE DATOS A EXCEL (.XLSX)
  # ==============================================================================
  output$btn_descarga_excel <- downloadHandler(
    filename = function() {
      paste0("Control_Cosecha_", input$filtro_campana, "_Semana", input$filtro_semana, "_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      df <- datos_reactivos()
      req(nrow(df) > 0)
      
      dims_no_activas <- setdiff(dims_tabla_agrupar, cols_agrupar())
      columnas_ocultas <- c(
        dims_no_activas,
        "Proy_manual", "Lunes_manual", "Martes_manual", "Miércoles_manual", 
        "Jueves_manual", "Viernes_manual", "Sábado_manual",
        "Proy_bloqueado", "Lunes_bloqueado", "Martes_bloqueado", "Miércoles_bloqueado", 
        "Jueves_bloqueado", "Viernes_bloqueado", "Sábado_bloqueado",
        "Lunes_tipo", "Martes_tipo", "Miércoles_tipo", "Jueves_tipo", "Viernes_tipo", "Sábado_tipo",
        "Kg_SAP_Real", "Kg_Real_1ra", "Kg_Real_2da", "Kg_Aj_1ra", "Kg_Aj_2da", "Kg_Aj_Total",
        "Ha Recorrida", "Kg/Ha Real"
      )
      
      df_export <- df %>%
        select(-any_of(columnas_ocultas))
      
      writexl::write_xlsx(df_export, path = file)
    }
  )
  
  # Render DataTable con edición celda a celda y fila inferior de Totales
  output$tabla_control_cosecha <- renderDT({
    df <- datos_reactivos()
    req(nrow(df) > 0)
    
    dims_activas <- cols_agrupar()
    esta_agrupado <- (length(dims_activas) < 5)
    idx_dims_ocultas <- which(!dims_tabla_agrupar %in% dims_activas) - 1L
    targets_ocultos <- c(idx_dims_ocultas, 25:(ncol(df)-1))
    
    es_campana_act <- grepl("2026|^26", as.character(input$filtro_campana))
    sem_actual_num <- lubridate::isoweek(Sys.Date())
    permitir_cualquier_sem <- is.null(input$switch_ajuste_cualquier_semana) || isTRUE(input$switch_ajuste_cualquier_semana)
    es_semana_act  <- (as.numeric(input$filtro_semana) == sem_actual_num) || permitir_cualquier_sem
    
    # Edición celda a celda delegada al modal interactivo (clic en celda)
    config_editable <- FALSE
    
    etiqueta_filas <- if (esta_agrupado) "registros agrupados" else "turnos"
    
    # Contenedor con tfoot para mostrar fila de totales al pie de la tabla
    sketch_tabla <- htmltools::withTags(table(
      class = "display",
      thead(
        tr(lapply(names(df), th))
      ),
      tfoot(
        tr(lapply(seq_along(names(df)), function(i) th()))
      )
    ))
    
    datatable(
      df,
      container = sketch_tabla,
      rownames = FALSE,
      editable = config_editable,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        stateSave = FALSE,
        dom = 't<"bottom"ip>',
        language = list(
          search = "Buscar:",
          lengthMenu = paste0("Mostrar _MENU_ ", etiqueta_filas),
          info = paste0("Mostrando _START_ a _END_ de _TOTAL_ ", etiqueta_filas),
          paginate = list(previous = "←", `next` = "→")
        ),
        columnDefs = list(
          list(visible = FALSE, targets = targets_ocultos),
          list(defaultContent = "-", targets = "_all"),
          list(className = 'dt-center', targets = 0:3),
          list(
            targets = 4,
            className = 'dt-center',
            render = JS("
              function(data, type, row) {
                if (!data || data === '-') return '-';
                if (data === 'Testblock') {
                  return '<span style=\"display: inline-block; padding: 2px 8px; border-radius: 6px; font-weight: 700; font-size: 11px; background: rgba(147, 51, 234, 0.12); color: #7E22CE; border: 1px solid rgba(147, 51, 234, 0.25);\">🧪 Testblock</span>';
                }
                if (data === 'Orgánico') {
                  return '<span style=\"display: inline-block; padding: 2px 8px; border-radius: 6px; font-weight: 600; font-size: 11px; background: rgba(52, 199, 89, 0.12); color: #28a745; border: 1px solid rgba(52, 199, 89, 0.25);\">🌱 Orgánico</span>';
                }
                return '<span style=\"font-size: 11.5px; color: #48484A;\">' + data + '</span>';
              }
            ")
          ),
          list(className = 'dt-right', targets = 5:23),
          list(
            targets = 24,
            className = 'dt-left',
            render = JS("
              function(data, type, row) {
                if (!data || data === '-' || data === '') return '<span style=\"color:#8E8E93;\">-</span>';
                var formatted = data;
                formatted = formatted.replace(/(Premio floral [^•\\)]+)/g, '<b style=\"color:#16A34A; background:rgba(22,163,74,0.12); padding:2px 7px; border-radius:6px;\">▲ $1</b>');
                formatted = formatted.replace(/(Castigo floral [^•\\)]+)/g, '<b style=\"color:#DC2626; background:rgba(220,38,38,0.12); padding:2px 7px; border-radius:6px;\">▼ $1</b>');
                formatted = formatted.replace(/(Floral neutro[^\\)]*\\))/g, '<span style=\"color:#6B7280; font-weight:600;\">$1</span>');
                formatted = formatted.replace(/(Edición manual)/g, '<span style=\"color:#7C3AED; font-weight:700;\">✍️ $1</span>');
                return '<span style=\"font-size:11px; color:#1C1C1E; line-height:1.3;\">' + formatted + '</span>';
              }
            ")
          )
        ),
        footerCallback = JS("
          function(tfoot, data, start, end, display) {
            var api = this.api();
            var numVal = function(i) {
              if (typeof i === 'number') return i;
              if (typeof i === 'string') {
                var v = parseFloat(i.replace(/[^0-9.-]+/g, ''));
                return isNaN(v) ? 0 : v;
              }
              return 0;
            };
            var fmt = function(n) {
              return n.toLocaleString('en-US', { minimumFractionDigits: 1, maximumFractionDigits: 1 });
            };
            
            var firstVis = -1;
            for (var c = 0; c <= 4; c++) {
              if (api.column(c).visible()) {
                if (firstVis === -1) {
                  firstVis = c;
                  $(api.column(c).footer()).html('TOTAL');
                } else {
                  $(api.column(c).footer()).html('');
                }
              }
            }
            
            var sumCols = [5, 6, 7, 8, 9, 10, 16, 17, 18, 19, 20, 21, 22, 23];
            var sums = {};
            sumCols.forEach(function(colIdx) {
              var total = api.column(colIdx, { search: 'applied' }).data().reduce(function(a, b) {
                return numVal(a) + numVal(b);
              }, 0);
              sums[colIdx] = total;
              $(api.column(colIdx).footer()).html(fmt(total));
            });
            
            var areaTot = sums[6] || 0;
            var kghaProy = areaTot > 0 ? (sums[5] / areaTot) : 0;
            
            // Promedio ponderado kg/ha: pares [colHa, colKgHa, colDestino]
            var wavg = function(colHa, colKgHa) {
              var kg = 0, ha = 0;
              api.rows({ search: 'applied' }).every(function() {
                var d = this.data();
                var h = numVal(d[colHa]);
                var k = numVal(d[colKgHa]);
                if (h > 0 && k > 0) { kg += h * k; ha += h; }
              });
              return ha > 0 ? (kg / ha) : 0;
            };
            var kghaR1 = wavg(7, 12);  // Ha Real 1ª / Kg/Ha Real 1ª
            var kghaR2 = wavg(8, 13);  // Ha Real 2ª / Kg/Ha Real 2ª
            var kgha1ra = wavg(9, 14); // Ha Aj. 1ª / Kg/Ha Aj. 1ª
            var kgha2da = wavg(10, 15); // Ha Aj. 2ª / Kg/Ha Aj. 2ª
            
            $(api.column(11).footer()).html(fmt(kghaProy));
            $(api.column(12).footer()).html(kghaR1 > 0 ? fmt(kghaR1) : '-');
            $(api.column(13).footer()).html(kghaR2 > 0 ? fmt(kghaR2) : '-');
            $(api.column(14).footer()).html(kgha1ra > 0 ? fmt(kgha1ra) : '-');
            $(api.column(15).footer()).html(kgha2da > 0 ? fmt(kgha2da) : '-');
            $(api.column(24).footer()).html('');
          }
        ")
      ),
      callback = JS("
        // 1. Mostrar cursor pointer sobre celdas interactivas de días, rendimiento ajustado y proyección
        table.on('mouseenter', 'tbody td', function() {
          var cell = table.cell(this);
          if (!cell.index()) return;
          var colIdx = cell.index().column;
          var colDias = [5, 14, 15, 16, 17, 18, 19, 20, 21];
          if (colDias.indexOf(colIdx) !== -1) {
            $(this).css('cursor', 'pointer');
          }
        });

        // 2. Clic en celda para abrir modal interactivo de detalle / cálculo / edición
        table.on('click', 'tbody td', function(e) {
          var cell = table.cell(this);
          if (!cell.index()) return;
          var colIdx = cell.index().column;
          var rowIdx = cell.index().row;
          var rowData = table.row(rowIdx).data();
          if (!rowData) return;
          
          var colDiasMap = { 5: 'Proyección', 14: 'Kg/Ha Aj. 1ª Pas', 15: 'Kg/Ha Aj. 2ª Pas', 16: 'Lunes', 17: 'Martes', 18: 'Miércoles', 19: 'Jueves', 20: 'Viernes', 21: 'Sábado' };
          if (colDiasMap.hasOwnProperty(colIdx)) {
            var lockMap = { 5: 32, 16: 33, 17: 34, 18: 35, 19: 36, 20: 37, 21: 38 };
            if (lockMap.hasOwnProperty(colIdx)) {
              var isLocked = rowData[lockMap[colIdx]];
              if (isLocked === true || isLocked === 1 || isLocked === 'TRUE') {
                Shiny.setInputValue('alerta_celda_bloqueada', { time: new Date().getTime() }, { priority: 'event' });
                return;
              }
            }
            
            Shiny.setInputValue('abrir_modal_editar_celda', {
              row: rowIdx + 1,
              sector: rowData[0],
              modulo: rowData[1],
              turno: rowData[2],
              variedad: rowData[3],
              tipo: rowData[4],
              colName: colDiasMap[colIdx],
              time: new Date().getTime()
            }, { priority: 'event' });
          }
        });
      "),
    ) %>%
      formatRound(columns = c("Proyección", "Área", "Ha Real 1ª Pas", "Ha Real 2ª Pas", "Ha Aj. 1ª Pas", "Ha Aj. 2ª Pas", "Kg/Ha Proy", "Kg/Ha Real 1ª Pas", "Kg/Ha Real 2ª Pas", "Kg/Ha Aj. 1ª Pas", "Kg/Ha Aj. 2ª Pas", "Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Ajuste", "Diferencia"), digits = 2) %>%
      formatStyle('Ha Real 1ª Pas', color = '#007AFF', fontWeight = '600') %>%
      formatStyle('Ha Real 2ª Pas', color = '#007AFF', fontWeight = '600') %>%
      formatStyle('Ha Aj. 1ª Pas', color = '#34C759', fontWeight = '600') %>%
      formatStyle('Ha Aj. 2ª Pas', color = '#34C759', fontWeight = '600') %>%
      formatStyle('Kg/Ha Real 1ª Pas', color = '#007AFF', fontWeight = '600') %>%
      formatStyle('Kg/Ha Real 2ª Pas', color = '#007AFF', fontWeight = '600') %>%
      formatStyle('Kg/Ha Aj. 1ª Pas', fontWeight = 'bold', color = '#34C759') %>%
      formatStyle('Kg/Ha Aj. 2ª Pas', fontWeight = 'bold', color = '#34C759') %>%
      formatStyle(
        'Proyección',
        'Proy_manual',
        color = styleEqual(c(TRUE, FALSE), c('#5856D6', '#FF9500')),
        backgroundColor = styleEqual(c(TRUE, FALSE), c('rgba(88, 86, 214, 0.12)', 'transparent')),
        fontWeight = '600'
      ) %>%
      formatStyle('Ajuste', fontWeight = 'bold', color = '#007AFF') %>%
      formatStyle(
        'Diferencia',
        color = styleInterval(0, c('#FF3B30', '#34C759')),
        fontWeight = '600'
      ) %>%
      formatStyle(
        'Lunes',
        'Lunes_tipo',
        color = styleEqual(c('manual', 'ruta_sin_kg', 'auto', 'sap', 'none'), c('#5856D6', '#E11D48', '#D97706', '#007AFF', '#1C1C1E')),
        backgroundColor = styleEqual(c('manual', 'ruta_sin_kg', 'auto', 'sap', 'none'), c('rgba(88, 86, 214, 0.12)', 'rgba(225, 29, 72, 0.12)', 'rgba(245, 158, 11, 0.14)', 'transparent', 'transparent')),
        fontWeight = styleEqual(c('manual', 'ruta_sin_kg', 'auto', 'sap', 'none'), c('bold', 'bold', 'bold', 'normal', 'normal'))
      ) %>%
      formatStyle(
        'Martes',
        'Martes_tipo',
        color = styleEqual(c('manual', 'ruta_sin_kg', 'auto', 'sap', 'none'), c('#5856D6', '#E11D48', '#D97706', '#007AFF', '#1C1C1E')),
        backgroundColor = styleEqual(c('manual', 'ruta_sin_kg', 'auto', 'sap', 'none'), c('rgba(88, 86, 214, 0.12)', 'rgba(225, 29, 72, 0.12)', 'rgba(245, 158, 11, 0.14)', 'transparent', 'transparent')),
        fontWeight = styleEqual(c('manual', 'ruta_sin_kg', 'auto', 'sap', 'none'), c('bold', 'bold', 'bold', 'normal', 'normal'))
      ) %>%
      formatStyle(
        'Miércoles',
        'Miércoles_tipo',
        color = styleEqual(c('manual', 'ruta_sin_kg', 'auto', 'sap', 'none'), c('#5856D6', '#E11D48', '#D97706', '#007AFF', '#1C1C1E')),
        backgroundColor = styleEqual(c('manual', 'ruta_sin_kg', 'auto', 'sap', 'none'), c('rgba(88, 86, 214, 0.12)', 'rgba(225, 29, 72, 0.12)', 'rgba(245, 158, 11, 0.14)', 'transparent', 'transparent')),
        fontWeight = styleEqual(c('manual', 'ruta_sin_kg', 'auto', 'sap', 'none'), c('bold', 'bold', 'bold', 'normal', 'normal'))
      ) %>%
      formatStyle(
        'Jueves',
        'Jueves_tipo',
        color = styleEqual(c('manual', 'ruta_sin_kg', 'auto', 'sap', 'none'), c('#5856D6', '#E11D48', '#D97706', '#007AFF', '#1C1C1E')),
        backgroundColor = styleEqual(c('manual', 'ruta_sin_kg', 'auto', 'sap', 'none'), c('rgba(88, 86, 214, 0.12)', 'rgba(225, 29, 72, 0.12)', 'rgba(245, 158, 11, 0.14)', 'transparent', 'transparent')),
        fontWeight = styleEqual(c('manual', 'ruta_sin_kg', 'auto', 'sap', 'none'), c('bold', 'bold', 'bold', 'normal', 'normal'))
      ) %>%
      formatStyle(
        'Viernes',
        'Viernes_tipo',
        color = styleEqual(c('manual', 'ruta_sin_kg', 'auto', 'sap', 'none'), c('#5856D6', '#E11D48', '#D97706', '#007AFF', '#1C1C1E')),
        backgroundColor = styleEqual(c('manual', 'ruta_sin_kg', 'auto', 'sap', 'none'), c('rgba(88, 86, 214, 0.12)', 'rgba(225, 29, 72, 0.12)', 'rgba(245, 158, 11, 0.14)', 'transparent', 'transparent')),
        fontWeight = styleEqual(c('manual', 'ruta_sin_kg', 'auto', 'sap', 'none'), c('bold', 'bold', 'bold', 'normal', 'normal'))
      ) %>%
      formatStyle(
        'Sábado',
        'Sábado_tipo',
        color = styleEqual(c('manual', 'ruta_sin_kg', 'auto', 'sap', 'none'), c('#5856D6', '#E11D48', '#D97706', '#007AFF', '#1C1C1E')),
        backgroundColor = styleEqual(c('manual', 'ruta_sin_kg', 'auto', 'sap', 'none'), c('rgba(88, 86, 214, 0.12)', 'rgba(225, 29, 72, 0.12)', 'rgba(245, 158, 11, 0.14)', 'transparent', 'transparent')),
        fontWeight = styleEqual(c('manual', 'ruta_sin_kg', 'auto', 'sap', 'none'), c('bold', 'bold', 'bold', 'normal', 'normal'))
      )
  })
  
  # Observador: Alerta celda bloqueada
  observeEvent(input$alerta_celda_bloqueada, {
    if (length(cols_agrupar()) < 5) {
      showNotification(
        "La tabla está agrupada. Para editar celdas manualmente, activa las 5 dimensiones en 'Agrupar por' (vista detallada por Turno).",
        type = "message",
        duration = 4
      )
    } else {
      showNotification(
        "Esta celda contiene registros oficiales (Cosecha SAP / Proyección Oficial) y está bloqueada contra edición.",
        type = "warning",
        duration = 4
      )
    }
  })
  
  # ==============================================================================
  # MODAL INTERACTIVO: REGISTRO, DETALLE AUTOMÁTICO Y CÁLCULO (KG O KG/HA Y HA)
  # ==============================================================================
  modal_celda_activa <- reactiveVal(NULL)
  modal_kgha_activo  <- reactiveVal(NULL)
  
  observeEvent(input$abrir_modal_editar_celda, {
    info <- input$abrir_modal_editar_celda
    req(info)
    
    if (length(cols_agrupar()) < 5) {
      showNotification(
        "La tabla está agrupada. Para editar celdas, desglosa la tabla activando las 5 dimensiones en 'Agrupar por' (vista por Turno).",
        type = "message",
        duration = 4
      )
      return()
    }
    
    permitir_cualquier_sem <- is.null(input$switch_ajuste_cualquier_semana) || isTRUE(input$switch_ajuste_cualquier_semana)
    es_campana_act <- grepl("2026|^26", as.character(input$filtro_campana))
    sem_actual_num <- lubridate::isoweek(Sys.Date())
    es_semana_act  <- (as.numeric(input$filtro_semana) == sem_actual_num) || permitir_cualquier_sem
    
    if (!es_campana_act || !es_semana_act) {
      showNotification("Solo se permite editar celdas en la semana actual en curso.", type = "error", duration = 4)
      return()
    }
    
    df_actual <- datos_reactivos()
    req(nrow(df_actual) > 0)
    
    # Localizar la fila exacta por Módulo, Turno y Variedad (inmune a ordenamiento/paginación)
    fila_sub <- df_actual %>%
      filter(
        as.character(Módulo) == as.character(info$modulo),
        as.character(Turno)  == as.character(info$turno),
        limpiar_variedad(Variedad) == limpiar_variedad(info$variedad)
      )
    
    fila <- if (nrow(fila_sub) > 0) {
      fila_sub[1, ]
    } else if (!is.null(info$row) && info$row >= 1 && info$row <= nrow(df_actual)) {
      df_actual[info$row, ]
    } else {
      return()
    }
    
    col_name <- as.character(info$colName)
    
    # Intercepción para edición directa de rendimiento ajustado (1ª o 2ª Pasada)
    if (col_name %in% c("Kg/Ha Aj. 1ª Pas", "Kg/Ha Aj. 2ª Pas")) {
      abrir_modal_kgha_ajustado(fila, col_name)
      return()
    }
    
    lock_col <- if (col_name == "Proyección") "Proy_bloqueado" else paste0(col_name, "_bloqueado")
    if (isTRUE(fila[[lock_col]])) {
      showNotification(
        sprintf("La celda [%s] del Turno %s contiene datos oficiales de SAP/Proyección y está bloqueada contra edición.", col_name, fila$Turno),
        type = "warning",
        duration = 4
      )
      return()
    }
    
    area_turno <- coalesce(as.numeric(fila$Área), 0)
    valor_actual <- coalesce(as.numeric(fila[[col_name]]), 0)
    tipo_col <- if (col_name == "Proyección") "Proy_manual" else paste0(col_name, "_tipo")
    tipo_celda <- coalesce(as.character(fila[[tipo_col]]), "none")
    
    # 1. Buscar si tiene edición manual previa en ediciones_manuales()
    ed_actuales <- ediciones_manuales()
    ed_celda <- if (!is.null(ed_actuales) && nrow(ed_actuales) > 0) {
      ed_actuales %>%
        filter(
          as.character(Campaña) == as.character(input$filtro_campana),
          as.numeric(Semana)    == as.numeric(input$filtro_semana),
          as.character(Módulo)   == as.character(fila$Módulo),
          as.character(Turno)    == as.character(fila$Turno),
          limpiar_variedad(Variedad) == limpiar_variedad(fila$Variedad),
          as.character(Campo)    == as.character(col_name)
        )
    } else tibble()
    
    # 2. Buscar si tiene válvulas asignadas en ajuste automático (rv_ajuste_valvulas)
    valvs_aj <- rv_ajuste_valvulas()
    valvs_celda <- if (!is.null(valvs_aj) && nrow(valvs_aj) > 0) {
      valvs_aj %>%
        filter(
          as.character(Campaña) == as.character(input$filtro_campana),
          as.numeric(Semana)    == as.numeric(input$filtro_semana),
          as.character(Módulo)   == as.character(fila$Módulo),
          as.character(Turno)    == as.character(fila$Turno),
          limpiar_variedad(Variedad) == limpiar_variedad(fila$Variedad),
          as.character(Dia_Asignado) == as.character(col_name)
        )
    } else tibble()
    
    # 3. Calcular hectáreas consumidas en otros días y hectáreas disponibles para este turno
    ha_real_1ra <- coalesce(as.numeric(fila[['Ha Real 1ª Pas']]), 0)
    ha_real_2da <- coalesce(as.numeric(fila[['Ha Real 2ª Pas']]), 0)
    
    ed_otros_dias <- if (!is.null(ed_actuales) && nrow(ed_actuales) > 0) {
      ed_actuales %>%
        filter(
          as.character(Campaña) == as.character(input$filtro_campana),
          as.numeric(Semana)    == as.numeric(input$filtro_semana),
          as.character(Módulo)   == as.character(fila$Módulo),
          as.character(Turno)    == as.character(fila$Turno),
          limpiar_variedad(Variedad) == limpiar_variedad(fila$Variedad),
          as.character(Campo)    != as.character(col_name),
          coalesce(Tipo_Edicion, "") == "manual"
        )
    } else tibble()
    
    ha_man_otros_1ra <- sum(coalesce(as.numeric(ed_otros_dias$Ha_Asignada_1ra), 0), na.rm = TRUE)
    ha_man_otros_2da <- sum(coalesce(as.numeric(ed_otros_dias$Ha_Asignada_2da), 0), na.rm = TRUE)
    
    ha_disponible_1ra <- max(0, round(area_turno - ha_real_1ra - ha_man_otros_1ra, 2))
    ha_disponible_2da <- max(0, round(area_turno - ha_real_2da - ha_man_otros_2da, 2))
    
    pasada_sugerida <- if (nrow(valvs_celda) > 0 && any(valvs_celda$Num_Pasada == 2)) {
      2L
    } else if (nrow(ed_celda) > 0 && coalesce(ed_celda$Ha_Asignada_2da[1], 0) > 0) {
      2L
    } else if (limpiar_variedad(fila$Variedad) == "ROCIO" && ha_real_1ra >= (area_turno - 0.05)) {
      2L
    } else {
      1L
    }
    
    ha_disponible_sug <- if (pasada_sugerida == 2L) ha_disponible_2da else ha_disponible_1ra
    
    # Hectáreas y rendimiento sugeridos para precargar
    ha_preset <- if (nrow(ed_celda) > 0 && !is.na(ed_celda$Ha_Asignada[1]) && ed_celda$Ha_Asignada[1] > 0) {
      round(pmin(ha_disponible_sug, ed_celda$Ha_Asignada[1]), 2)
    } else if (nrow(valvs_celda) > 0 && sum(valvs_celda$Ha_Asignada, na.rm = TRUE) > 0) {
      round(pmin(ha_disponible_sug, sum(valvs_celda$Ha_Asignada, na.rm = TRUE)), 2)
    } else {
      ha_disponible_sug
    }
    
    kg_preset <- round(valor_actual, 2)
    kgha_preset <- if (ha_preset > 0 && kg_preset > 0) {
      round(kg_preset / ha_preset, 2)
    } else if (!is.na(fila$`Kg/Ha Proy`) && as.numeric(fila$`Kg/Ha Proy`) > 0) {
      round(as.numeric(fila$`Kg/Ha Proy`), 2)
    } else {
      100.0
    }
    
    ctx <- list(
      fila               = fila,
      campo              = col_name,
      area_turno         = area_turno,
      valor_actual       = valor_actual,
      tipo_celda         = tipo_celda,
      ed_celda           = ed_celda,
      valvs_celda        = valvs_celda,
      ha_preset          = ha_preset,
      kg_preset          = kg_preset,
      kgha_preset        = kgha_preset,
      pasada_sug         = pasada_sugerida,
      ha_disponible_1ra  = ha_disponible_1ra,
      ha_disponible_2da  = ha_disponible_2da,
      ha_disponible_sug  = ha_disponible_sug,
      ha_real_1ra        = ha_real_1ra,
      ha_real_2da        = ha_real_2da,
      ha_man_otros_1ra   = ha_man_otros_1ra,
      ha_man_otros_2da   = ha_man_otros_2da
    )
    modal_celda_activa(ctx)
    
    modal_header <- tagList(
      tags$style(HTML("
        #shiny-modal .modal-content {
          border-radius: 14px !important;
          border: 1px solid #e4e4e7 !important;
          box-shadow: 0 20px 25px -5px rgba(0, 0, 0, 0.08), 0 8px 10px -6px rgba(0, 0, 0, 0.04) !important;
          font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif !important;
          overflow: hidden !important;
        }
        #shiny-modal .modal-header {
          padding: 16px 20px !important;
          border-bottom: 1px solid #f4f4f5 !important;
          background: #ffffff !important;
        }
        #shiny-modal .modal-title {
          width: 100% !important;
          margin: 0 !important;
        }
        #shiny-modal .modal-body {
          padding: 20px !important;
          background: #ffffff !important;
        }
        #shiny-modal .modal-footer {
          padding: 14px 20px !important;
          border-top: 1px solid #f4f4f5 !important;
          background: #fafafa !important;
          display: flex !important;
          align-items: center !important;
          justify-content: flex-end !important;
          gap: 8px !important;
        }
        #shiny-modal .shiny-input-container {
          width: 100% !important;
          margin-bottom: 0 !important;
        }
        #shiny-modal .form-control {
          height: 38px !important;
          border-radius: 8px !important;
          border: 1px solid #e4e4e7 !important;
          box-shadow: 0 1px 2px rgba(0,0,0,0.02) !important;
          font-size: 13.5px !important;
          color: #09090b !important;
          background-color: #ffffff !important;
          padding: 6px 12px !important;
          box-sizing: border-box !important;
          font-variant-numeric: tabular-nums !important;
          transition: border-color 0.15s ease, box-shadow 0.15s ease !important;
        }
        #shiny-modal .form-control:focus {
          border-color: #18181b !important;
          box-shadow: 0 0 0 1px #18181b !important;
          outline: none !important;
        }
        .ek-segmented {
          width: 100% !important;
        }
        .ek-segmented .shiny-options-group {
          display: flex !important;
          background: #f4f4f5 !important;
          padding: 3px !important;
          border-radius: 9px !important;
          border: 1px solid #e4e4e7 !important;
          gap: 3px !important;
          width: 100% !important;
          box-sizing: border-box !important;
        }
        .ek-segmented .radio-inline {
          flex: 1 !important;
          text-align: center !important;
          margin: 0 !important;
          padding: 0 !important;
          position: relative !important;
          display: flex !important;
        }
        .ek-segmented .radio-inline input[type='radio'] {
          position: absolute !important;
          opacity: 0 !important;
          width: 0 !important;
          height: 0 !important;
          pointer-events: none !important;
        }
        .ek-segmented .radio-inline span {
          display: flex !important;
          flex: 1 !important;
          align-items: center !important;
          justify-content: center !important;
          padding: 7px 12px !important;
          font-size: 12.5px !important;
          font-weight: 500 !important;
          color: #71717a !important;
          border-radius: 6px !important;
          transition: all 0.15s cubic-bezier(0.16, 1, 0.3, 1) !important;
          cursor: pointer !important;
          user-select: none !important;
          background: transparent !important;
        }
        .ek-segmented .radio-inline input[type='radio']:checked + span {
          background: #ffffff !important;
          color: #09090b !important;
          font-weight: 600 !important;
          box-shadow: 0 1px 3px rgba(0, 0, 0, 0.08), 0 1px 2px rgba(0, 0, 0, 0.04) !important;
        }
        .ek-segmented .radio-inline:hover input[type='radio']:not(:checked) + span {
          color: #18181b !important;
        }
        #shiny-modal .ek-btn-primary {
          background: #18181b !important;
          color: #ffffff !important;
          border: 1px solid #18181b !important;
          border-radius: 8px !important;
          font-size: 13px !important;
          font-weight: 500 !important;
          padding: 8px 16px !important;
          transition: background-color 0.15s ease !important;
          box-shadow: 0 1px 2px rgba(0,0,0,0.05) !important;
        }
        #shiny-modal .ek-btn-primary:hover, #shiny-modal .ek-btn-primary:focus {
          background: #27272a !important;
          color: #ffffff !important;
          border-color: #27272a !important;
        }
        #shiny-modal .modal-footer .btn-default,
        #shiny-modal .modal-footer button[data-dismiss='modal'],
        #shiny-modal .modal-footer button[data-bs-dismiss='modal'] {
          background: #ffffff !important;
          color: #52525b !important;
          border: 1px solid #e4e4e7 !important;
          border-radius: 8px !important;
          font-size: 13px !important;
          font-weight: 500 !important;
          padding: 8px 14px !important;
          transition: background-color 0.15s ease !important;
        }
        #shiny-modal .modal-footer .btn-default:hover,
        #shiny-modal .modal-footer button[data-dismiss='modal']:hover,
        #shiny-modal .modal-footer button[data-bs-dismiss='modal']:hover {
          background: #f4f4f5 !important;
          color: #18181b !important;
        }
        #shiny-modal .ek-btn-danger {
          background: transparent !important;
          color: #e11d48 !important;
          border: 1px solid #fecdd3 !important;
          border-radius: 8px !important;
          font-size: 12.5px !important;
          font-weight: 500 !important;
          padding: 8px 12px !important;
          transition: all 0.15s ease !important;
        }
        #shiny-modal .ek-btn-danger:hover, #shiny-modal .ek-btn-danger:focus {
          background: #fff1f2 !important;
          color: #be123c !important;
          border-color: #fda4af !important;
        }
      ")),
      div(
        style = "display: flex; align-items: flex-start; justify-content: space-between; width: 100%;",
        div(
          tags$div(style = "font-size: 15.5px; font-weight: 600; color: #09090b; letter-spacing: -0.01em; line-height: 1.3;", 
                   sprintf("Registrar Cosecha • %s", col_name)),
          tags$div(style = "font-size: 12px; font-weight: 400; color: #71717a; margin-top: 3px;",
                   sprintf("Sector %s • Módulo %s • Turno %s • %s (%s) • %.2f ha",
                           fila$Sector, fila$Módulo, fila$Turno, fila$Variedad, fila$Tipo, area_turno))
        ),
        div(
          if (tipo_celda == "auto") {
            tags$span(style = "display: inline-flex; align-items: center; padding: 2px 8px; border-radius: 9999px; font-size: 11px; font-weight: 500; background: #fef3c7; color: #92400e; border: 1px solid #fde68a;", "Ajuste automático")
          } else if (tipo_celda == "manual") {
            tags$span(style = "display: inline-flex; align-items: center; padding: 2px 8px; border-radius: 9999px; font-size: 11px; font-weight: 500; background: #ede9fe; color: #5b21b6; border: 1px solid #ddd6fe;", "Edición manual")
          } else if (tipo_celda == "ruta_sin_kg") {
            tags$span(style = "display: inline-flex; align-items: center; padding: 2px 8px; border-radius: 9999px; font-size: 11px; font-weight: 500; background: #ffe4e6; color: #9f1239; border: 1px solid #fecdd3;", "Ruta sin SAP")
          } else {
            tags$span(style = "display: inline-flex; align-items: center; padding: 2px 8px; border-radius: 9999px; font-size: 11px; font-weight: 500; background: #f4f4f5; color: #71717a; border: 1px solid #e4e4e7;", "Sin registro")
          }
        )
      )
    )
    
    showModal(modalDialog(
      title = modal_header,
      size = "m",
      easyClose = TRUE,
      uiOutput("ui_modal_celda_cuerpo"),
      footer = tagList(
        if (tipo_celda == "manual") {
          actionButton("modal_btn_restablecer", "Restablecer automático", class = "ek-btn-danger", style = "margin-right: auto;")
        },
        modalButton("Cancelar"),
        actionButton("modal_btn_guardar", "Guardar registro", class = "ek-btn-primary")
      )
    ))
  })
  
  output$ui_modal_celda_cuerpo <- renderUI({
    ctx <- modal_celda_activa()
    req(ctx)
    
    fila <- ctx$fila
    col_name <- ctx$campo
    tipo_celda <- ctx$tipo_celda
    valvs_celda <- ctx$valvs_celda
    ed_celda <- ctx$ed_celda
    area_turno <- ctx$area_turno
    valor_actual <- ctx$valor_actual
    ha_preset <- ctx$ha_preset
    kgha_preset <- ctx$kgha_preset
    ha_disponible_sug <- ctx$ha_disponible_sug
    
    tagList(
      # Tarjeta con el detalle si fue ajustado automáticamente
      if (tipo_celda %in% c("auto", "ruta_sin_kg") || nrow(valvs_celda) > 0) {
        criterio_txt <- if (nrow(valvs_celda) > 0) {
          first(na.omit(valvs_celda$Motivo_Ajuste))
        } else if (nrow(ed_celda) > 0) {
          first(na.omit(ed_celda$Motivo_Ajuste))
        } else {
          fila$`Motivo del Ajuste`
        }
        pasada_str <- if (nrow(valvs_celda) > 0 && any(valvs_celda$Num_Pasada == 2)) "2ª Pasada" else "1ª Pasada"
        
        div(
          style = "background: #fafaf9; border: 1px solid #e7e5e4; border-radius: 8px; padding: 12px 14px; margin-bottom: 16px;",
          div(style = "display: flex; align-items: center; justify-content: space-between; margin-bottom: 8px;",
            tags$span(style = "font-size: 12px; font-weight: 600; color: #44403c;", "Detalle del ajuste asignado"),
            tags$span(style = "font-size: 11px; font-weight: 500; color: #78716c; background: #f5f5f4; border: 1px solid #e7e5e4; padding: 1px 7px; border-radius: 6px;", pasada_str)
          ),
          div(style = "display: grid; grid-template-columns: repeat(3, 1fr); gap: 8px; padding: 8px 10px; background: #ffffff; border: 1px solid #e7e5e4; border-radius: 6px;",
            div(tags$div(style = "font-size: 11px; color: #78716c;", "Kilos"), tags$div(style = "font-size: 13px; font-weight: 600; color: #1c1917; font-variant-numeric: tabular-nums;", sprintf("%s kg", format(round(valor_actual, 1), big.mark = ",")))),
            div(tags$div(style = "font-size: 11px; color: #78716c;", "Hectáreas"), tags$div(style = "font-size: 13px; font-weight: 600; color: #1c1917; font-variant-numeric: tabular-nums;", sprintf("%.2f ha", ha_preset))),
            div(tags$div(style = "font-size: 11px; color: #78716c;", "Rendimiento"), tags$div(style = "font-size: 13px; font-weight: 600; color: #1c1917; font-variant-numeric: tabular-nums;", sprintf("%.1f kg/ha", ifelse(ha_preset > 0, valor_actual / ha_preset, 0))))
          ),
          if (!is.na(criterio_txt) && nzchar(criterio_txt) && criterio_txt != "-") {
            div(style = "font-size: 11.5px; color: #57534e; margin-top: 8px; line-height: 1.35;",
              tags$span(style = "font-weight: 500; color: #292524;", "Criterio: "), criterio_txt
            )
          },
          if (nrow(valvs_celda) > 0) {
            div(style = "font-size: 11px; color: #78716c; margin-top: 6px; line-height: 1.35;",
              tags$span(style = "font-weight: 500; color: #44403c;", "Válvulas: "),
              paste(sprintf("%s (%.2f ha, %.1f kg)", valvs_celda$codigo, valvs_celda$Ha_Asignada, valvs_celda$Kg_Asignado), collapse = " • ")
            )
          }
        )
      } else if (tipo_celda == "manual") {
        div(
          style = "background: #fbfbfe; border: 1px solid #e4e4e7; border-left: 3px solid #6366f1; border-radius: 8px; padding: 10px 14px; margin-bottom: 16px; display: flex; align-items: center; justify-content: space-between;",
          div(
            tags$span(style = "font-size: 12px; color: #52525b;", "Valor registrado previamente: "),
            tags$span(style = "font-size: 13.5px; font-weight: 600; color: #09090b; font-variant-numeric: tabular-nums;", sprintf("%s kg", format(round(valor_actual, 1), big.mark = ",")))
          ),
          if (nrow(ed_celda) > 0 && nzchar(coalesce(ed_celda$Motivo_Ajuste[1], ""))) {
            tags$span(style = "font-size: 11.5px; color: #71717a;", sprintf("%s", ed_celda$Motivo_Ajuste[1]))
          }
        )
      },
      
      # Selector de método de ingreso (Segmented Control)
      div(
        style = "margin-bottom: 14px;",
        tags$label(style = "display: block; font-size: 12px; font-weight: 500; color: #3f3f46; margin-bottom: 6px;", "Forma de ingreso"),
        div(
          class = "ek-segmented",
          radioButtons(
            "modal_modo_ingreso",
            label = NULL,
            choices = c(
              "Kilos directos" = "kilos",
              "Rendimiento y hectáreas" = "kgha"
            ),
            selected = "kilos",
            inline = TRUE
          )
        )
      ),
      
      # Inputs condicionales
      conditionalPanel(
        condition = "input.modal_modo_ingreso == 'kilos'",
        div(
          style = "margin-bottom: 14px;",
          tags$label(style = "display: block; font-size: 12px; font-weight: 500; color: #3f3f46; margin-bottom: 6px;", "Kilos totales para el día (kg)"),
          numericInput(
            "modal_input_kilos",
            label = NULL,
            value = round(valor_actual, 2),
            min = 0,
            step = 10,
            width = "100%"
          )
        )
      ),
      
      conditionalPanel(
        condition = "input.modal_modo_ingreso == 'kgha'",
        div(
          style = "display: grid; grid-template-columns: 1fr 1fr; gap: 12px; width: 100%; box-sizing: border-box; margin-bottom: 14px;",
          div(
            style = "min-width: 0;",
            tags$label(style = "display: block; font-size: 12px; font-weight: 500; color: #3f3f46; margin-bottom: 6px;", "Rendimiento (kg/ha)"),
            numericInput(
              "modal_input_kgha",
              label = NULL,
              value = round(kgha_preset, 2),
              min = 0,
              step = 5,
              width = "100%"
            )
          ),
          div(
            style = "min-width: 0;",
            tags$label(
              style = "display: flex; align-items: baseline; justify-content: space-between; font-size: 12px; font-weight: 500; color: #3f3f46; margin-bottom: 6px;",
              tags$span("Hectáreas a cosechar (ha)"),
              tags$span(style = "font-size: 11px; font-weight: 400; color: #71717a;", sprintf("Disp: %.2f ha", ha_disponible_sug))
            ),
            numericInput(
              "modal_input_ha",
              label = NULL,
              value = round(ha_preset, 2),
              min = 0,
              max = round(area_turno, 2),
              step = 0.05,
              width = "100%"
            )
          )
        )
      ),
      
      # Cálculo en vivo / Rendimiento resultante
      uiOutput("ui_modal_calculo_vivo"),
      
      # Selector de pasada si la variedad es Rocío
      if (limpiar_variedad(fila$Variedad) == "ROCIO") {
        div(
          style = "margin-top: 14px; padding-top: 12px; border-top: 1px solid #f4f4f5; margin-bottom: 14px;",
          tags$label(style = "display: block; font-size: 12px; font-weight: 500; color: #3f3f46; margin-bottom: 6px;", "Pasada a registrar"),
          div(
            class = "ek-segmented",
            radioButtons(
              "modal_input_pasada",
              label = NULL,
              choices = c("1ª Pasada" = 1, "2ª Pasada" = 2),
              selected = ctx$pasada_sug,
              inline = TRUE
            )
          )
        )
      },
      
      # Nota o motivo de edición
      div(
        style = "margin-top: 4px;",
        tags$label(style = "display: block; font-size: 12px; font-weight: 500; color: #3f3f46; margin-bottom: 6px;", "Nota / motivo (opcional)"),
        textInput(
          "modal_input_motivo",
          label = NULL,
          value = "",
          placeholder = "Ej: Ajuste por cuadrilla, proyección validada",
          width = "100%"
        )
      )
    )
  })
  
  output$ui_modal_calculo_vivo <- renderUI({
    modo <- input$modal_modo_ingreso
    ctx <- modal_celda_activa()
    req(ctx)
    
    pasada_activa <- if (!is.null(input$modal_input_pasada)) as.integer(input$modal_input_pasada) else ctx$pasada_sug
    ha_real_pasada <- if (pasada_activa == 2L) ctx$ha_real_2da else ctx$ha_real_1ra
    ha_pool_pasada <- max(0, round(ctx$area_turno - ha_real_pasada, 2))
    
    ed_act <- ediciones_manuales()
    if (!is.null(ed_act) && nrow(ed_act) > 0) {
      if (!("Modo_Ingreso" %in% names(ed_act))) {
        ed_act$Modo_Ingreso <- ifelse(grepl("kg/ha en", coalesce(ed_act$Motivo_Ajuste, "")), "kgha", "kilos")
      }
    }
    
    ed_otros_pasada <- if (!is.null(ed_act) && nrow(ed_act) > 0) {
      ed_act %>%
        filter(
          as.character(Campaña) == as.character(input$filtro_campana),
          as.numeric(Semana)    == as.numeric(input$filtro_semana),
          as.character(Módulo)   == as.character(ctx$fila$Módulo),
          as.character(Turno)    == as.character(ctx$fila$Turno),
          limpiar_variedad(Variedad) == limpiar_variedad(ctx$fila$Variedad),
          as.character(Campo)    != as.character(ctx$campo),
          coalesce(Tipo_Edicion, "") == "manual",
          if (pasada_activa == 2L) coalesce(Ha_Asignada_2da, 0) > 0 | coalesce(Kg_Asignado_2da, 0) > 0
          else coalesce(Ha_Asignada_1ra, 0) > 0 | coalesce(Kg_Asignado_1ra, 0) > 0
        )
    } else tibble()
    
    if (nrow(ed_otros_pasada) > 0) {
      if (!("Modo_Ingreso" %in% names(ed_otros_pasada))) {
        ed_otros_pasada$Modo_Ingreso <- ifelse(grepl("kg/ha en", coalesce(ed_otros_pasada$Motivo_Ajuste, "")), "kgha", "kilos")
      }
      ed_fijas <- ed_otros_pasada %>%
        filter(coalesce(Modo_Ingreso, "") == "kgha" | grepl("kg/ha en", coalesce(Motivo_Ajuste, "")))
      ed_prorr <- ed_otros_pasada %>%
        filter(!(coalesce(Modo_Ingreso, "") == "kgha" | grepl("kg/ha en", coalesce(Motivo_Ajuste, ""))))
    } else {
      ed_fijas <- tibble()
      ed_prorr <- tibble()
    }
    ha_fijas <- if (nrow(ed_fijas) > 0) sum(if (pasada_activa == 2L) coalesce(ed_fijas$Ha_Asignada_2da, 0) else coalesce(ed_fijas$Ha_Asignada_1ra, 0), na.rm = TRUE) else 0
    ha_pool_prorr <- max(0, round(ha_pool_pasada - ha_fijas, 2))
    
    if (!is.null(modo) && modo == "kgha") {
      kgha <- suppressWarnings(as.numeric(input$modal_input_kgha))
      ha   <- suppressWarnings(as.numeric(input$modal_input_ha))
      kgha <- if (length(kgha) == 0 || is.na(kgha) || kgha < 0) 0 else kgha
      ha   <- if (length(ha) == 0 || is.na(ha) || ha < 0) 0 else ha
      kilos_calc <- round(kgha * ha, 2)
      ha_restante <- max(0, round(ha_pool_prorr - ha, 2))
      
      div(
        style = "background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 8px; padding: 12px 14px; display: flex; align-items: baseline; justify-content: space-between; margin-bottom: 14px;",
        div(
          tags$div(style = "font-size: 12px; font-weight: 500; color: #475569;", "Total estimado"),
          tags$div(
            style = "font-size: 11.5px; color: #94a3b8; margin-top: 2px;",
            sprintf("%.2f kg/ha × %.2f ha%s", kgha, ha,
                    if (nrow(ed_prorr) > 0) sprintf(" • Restan %.2f ha para otros días", ha_restante) else "")
          )
        ),
        tags$div(
          style = "font-size: 20px; font-weight: 600; color: #0f172a; font-variant-numeric: tabular-nums;",
          sprintf("%s kg", format(kilos_calc, big.mark = ","))
        )
      )
    } else {
      kg_hoy <- suppressWarnings(as.numeric(input$modal_input_kilos))
      kg_hoy <- if (length(kg_hoy) == 0 || is.na(kg_hoy) || kg_hoy < 0) 0 else kg_hoy
      
      if (nrow(ed_prorr) > 0) {
        kg_otros_sum <- sum(coalesce(as.numeric(ed_prorr$Valor), 0), na.rm = TRUE)
        dias_otros_str <- paste(unique(ed_prorr$Campo), collapse = ", ")
        kg_total <- kg_otros_sum + kg_hoy
        
        ha_hoy_prorr <- if (kg_total > 0) round(ha_pool_prorr * (kg_hoy / kg_total), 2) else 0
        ha_otros_prorr <- max(0, round(ha_pool_prorr - ha_hoy_prorr, 2))
        kgha_conjunto <- if (ha_pool_prorr > 0 && kg_total > 0) round(kg_total / ha_pool_prorr, 1) else 0
        
        div(
          style = "background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 8px; padding: 11px 14px; margin-bottom: 14px;",
          div(
            style = "display: flex; align-items: baseline; justify-content: space-between;",
            div(
              tags$div(
                style = "font-size: 12px; font-weight: 600; color: #334155;",
                sprintf("Prorrateo proporcional con %s (%s kg)", dias_otros_str, format(round(kg_otros_sum, 1), big.mark = ","))
              ),
              tags$div(
                style = "font-size: 11.5px; color: #64748b; margin-top: 3px;",
                sprintf("Asignación: %s %.2f ha • %s %.2f ha (Total: %.2f ha)",
                        ctx$campo, ha_hoy_prorr, dias_otros_str, ha_otros_prorr, ha_pool_prorr)
              )
            ),
            tags$div(
              style = "text-align: right;",
              tags$div(
                style = "font-size: 14.5px; font-weight: 600; color: #0f172a; font-variant-numeric: tabular-nums;",
                sprintf("%s kg/ha", format(kgha_conjunto, big.mark = ","))
              ),
              tags$div(style = "font-size: 10.5px; color: #94a3b8;", "rendimiento uniforme")
            )
          )
        )
      } else {
        kgha_res <- if (ha_pool_prorr > 0 && kg_hoy > 0) round(kg_hoy / ha_pool_prorr, 1) else 0
        
        div(
          style = "background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 8px; padding: 10px 14px; display: flex; align-items: baseline; justify-content: space-between; margin-bottom: 14px;",
          div(
            tags$div(style = "font-size: 12px; font-weight: 500; color: #475569;", sprintf("Rendimiento implícito (%.2f ha disponibles)", ha_pool_prorr)),
            tags$div(style = "font-size: 11px; color: #94a3b8; margin-top: 2px;", "Se asume el consumo total de las hectáreas faltantes")
          ),
          tags$div(
            style = "font-size: 14.5px; font-weight: 600; color: #0f172a; font-variant-numeric: tabular-nums;",
            sprintf("%s kg/ha", format(kgha_res, big.mark = ","))
          )
        )
      }
    }
  })
  
  observeEvent(input$modal_btn_guardar, {
    ctx <- modal_celda_activa()
    req(ctx)
    
    modo <- input$modal_modo_ingreso
    pasada_activa <- if (!is.null(input$modal_input_pasada)) as.integer(input$modal_input_pasada) else ctx$pasada_sug
    ha_real_pasada <- if (pasada_activa == 2L) ctx$ha_real_2da else ctx$ha_real_1ra
    ha_pool_pasada <- max(0, round(ctx$area_turno - ha_real_pasada, 2))
    
    val_final <- if (!is.null(modo) && modo == "kgha") {
      kgha <- suppressWarnings(as.numeric(input$modal_input_kgha))
      ha   <- suppressWarnings(as.numeric(input$modal_input_ha))
      kgha <- if (length(kgha) == 0 || is.na(kgha)) 0 else kgha
      ha   <- if (length(ha) == 0 || is.na(ha)) 0 else ha
      round(kgha * ha, 2)
    } else {
      kg <- suppressWarnings(as.numeric(input$modal_input_kilos))
      kg <- if (length(kg) == 0 || is.na(kg)) 0 else kg
      round(kg, 2)
    }
    
    if (val_final < 0) {
      showNotification("El valor debe ser un número positivo (>= 0).", type = "warning", duration = 4)
      return()
    }
    
    ed_actuales <- ediciones_manuales()
    if (!is.null(ed_actuales) && nrow(ed_actuales) > 0) {
      ed_actuales <- ed_actuales %>% mutate(Campaña = as.character(Campaña), Semana = as.numeric(Semana))
      if (!("Modo_Ingreso" %in% names(ed_actuales))) {
        ed_actuales$Modo_Ingreso <- ifelse(grepl("kg/ha en", coalesce(ed_actuales$Motivo_Ajuste, "")), "kgha", "kilos")
      }
    }
    
    ed_otros_pasada <- if (!is.null(ed_actuales) && nrow(ed_actuales) > 0) {
      ed_actuales %>%
        filter(
          as.character(Campaña) == as.character(input$filtro_campana),
          as.numeric(Semana)    == as.numeric(input$filtro_semana),
          as.character(Módulo)   == as.character(ctx$fila$Módulo),
          as.character(Turno)    == as.character(ctx$fila$Turno),
          limpiar_variedad(Variedad) == limpiar_variedad(ctx$fila$Variedad),
          as.character(Campo)    != as.character(ctx$campo),
          coalesce(Tipo_Edicion, "") == "manual",
          if (pasada_activa == 2L) coalesce(Ha_Asignada_2da, 0) > 0 | coalesce(Kg_Asignado_2da, 0) > 0
          else coalesce(Ha_Asignada_1ra, 0) > 0 | coalesce(Kg_Asignado_1ra, 0) > 0
        )
    } else tibble()
    
    if (nrow(ed_otros_pasada) > 0) {
      if (!("Modo_Ingreso" %in% names(ed_otros_pasada))) {
        ed_otros_pasada$Modo_Ingreso <- ifelse(grepl("kg/ha en", coalesce(ed_otros_pasada$Motivo_Ajuste, "")), "kgha", "kilos")
      }
      ed_fijas <- ed_otros_pasada %>%
        filter(coalesce(Modo_Ingreso, "") == "kgha" | grepl("kg/ha en", coalesce(Motivo_Ajuste, "")))
      ed_prorr <- ed_otros_pasada %>%
        filter(!(coalesce(Modo_Ingreso, "") == "kgha" | grepl("kg/ha en", coalesce(Motivo_Ajuste, ""))))
    } else {
      ed_fijas <- tibble()
      ed_prorr <- tibble()
    }
    ha_fijas <- if (nrow(ed_fijas) > 0) sum(if (pasada_activa == 2L) coalesce(ed_fijas$Ha_Asignada_2da, 0) else coalesce(ed_fijas$Ha_Asignada_1ra, 0), na.rm = TRUE) else 0
    ha_pool_prorr <- max(0, round(ha_pool_pasada - ha_fijas, 2))
    
    # Si el valor ingresado es 0, restablecer celda y recalcular días prorrateables restantes
    if (val_final == 0) {
      ed_filtradas <- ed_actuales %>%
        filter(!(
          as.character(Campaña) == as.character(input$filtro_campana) &
            as.numeric(Semana)    == as.numeric(input$filtro_semana) &
            as.character(Sector)   == as.character(ctx$fila$Sector) &
            as.character(Módulo)   == as.character(ctx$fila$Módulo) &
            as.character(Turno)    == as.character(ctx$fila$Turno) &
            limpiar_variedad(Variedad) == limpiar_variedad(ctx$fila$Variedad) &
            as.character(Campo)    == as.character(ctx$campo)
        ))
      
      if (nrow(ed_prorr) > 0) {
        kg_tot_rest <- sum(coalesce(as.numeric(ed_prorr$Valor), 0), na.rm = TRUE)
        ha_acum <- 0
        for (i in seq_len(nrow(ed_prorr))) {
          d_campo <- ed_prorr$Campo[i]
          ha_nueva_i <- if (i == nrow(ed_prorr)) {
            max(0, round(ha_pool_prorr - ha_acum, 2))
          } else {
            if (kg_tot_rest > 0) round(ha_pool_prorr * (as.numeric(ed_prorr$Valor[i]) / kg_tot_rest), 2) else 0
          }
          ha_acum <- ha_acum + ha_nueva_i
          
          idx <- which(
            as.character(ed_filtradas$Campaña) == as.character(input$filtro_campana) &
            as.numeric(ed_filtradas$Semana)    == as.numeric(input$filtro_semana) &
            as.character(ed_filtradas$Módulo)   == as.character(ctx$fila$Módulo) &
            as.character(ed_filtradas$Turno)    == as.character(ctx$fila$Turno) &
            limpiar_variedad(ed_filtradas$Variedad) == limpiar_variedad(ctx$fila$Variedad) &
            as.character(ed_filtradas$Campo)    == as.character(d_campo)
          )
          if (length(idx) > 0) {
            ed_filtradas$Ha_Asignada[idx] <- ha_nueva_i
            if (pasada_activa == 2L) {
              ed_filtradas$Ha_Asignada_2da[idx] <- ha_nueva_i
            } else {
              ed_filtradas$Ha_Asignada_1ra[idx] <- ha_nueva_i
            }
            ed_filtradas$Motivo_Ajuste[idx] <- if (nrow(ed_prorr) > 1) {
              sprintf("Manual prorrateado (%s kg en %.2f ha)", format(round(as.numeric(ed_prorr$Valor[i]), 1)), ha_nueva_i)
            } else {
              sprintf("Manual (%s kg en %.2f ha faltantes)", format(round(as.numeric(ed_prorr$Valor[i]), 1)), ha_nueva_i)
            }
          }
        }
      }
      
      ediciones_manuales(ed_filtradas)
      guardar_ediciones_disco(ed_filtradas)
      removeModal()
      showNotification(sprintf("Se restableció la celda [%s] del Turno %s.", ctx$campo, ctx$fila$Turno), type = "message", duration = 3)
      return()
    }
    
    # Calcular hectáreas finales según modo
    ha_final <- 0
    if (!is.null(modo) && modo == "kgha") {
      ha_in <- suppressWarnings(as.numeric(input$modal_input_ha))
      ha_final <- if (length(ha_in) == 0 || is.na(ha_in)) 0 else round(ha_in, 2)
      
      if (nrow(ed_prorr) > 0) {
        ha_restante <- max(0, round(ha_pool_prorr - ha_final, 2))
        kg_tot_otros <- sum(coalesce(as.numeric(ed_prorr$Valor), 0), na.rm = TRUE)
        ha_acum <- 0
        for (i in seq_len(nrow(ed_prorr))) {
          d_campo <- ed_prorr$Campo[i]
          ha_nueva_i <- if (i == nrow(ed_prorr)) {
            max(0, round(ha_restante - ha_acum, 2))
          } else {
            if (kg_tot_otros > 0) round(ha_restante * (as.numeric(ed_prorr$Valor[i]) / kg_tot_otros), 2) else 0
          }
          ha_acum <- ha_acum + ha_nueva_i
          
          idx <- which(
            as.character(ed_actuales$Campaña) == as.character(input$filtro_campana) &
            as.numeric(ed_actuales$Semana)    == as.numeric(input$filtro_semana) &
            as.character(ed_actuales$Módulo)   == as.character(ctx$fila$Módulo) &
            as.character(ed_actuales$Turno)    == as.character(ctx$fila$Turno) &
            limpiar_variedad(ed_actuales$Variedad) == limpiar_variedad(ctx$fila$Variedad) &
            as.character(ed_actuales$Campo)    == as.character(d_campo)
          )
          if (length(idx) > 0) {
            ed_actuales$Ha_Asignada[idx] <- ha_nueva_i
            if (pasada_activa == 2L) {
              ed_actuales$Ha_Asignada_2da[idx] <- ha_nueva_i
            } else {
              ed_actuales$Ha_Asignada_1ra[idx] <- ha_nueva_i
            }
          }
        }
      }
    } else {
      # Modo kilos directos con prorrateo proporcional
      if (nrow(ed_prorr) > 0) {
        kg_tot_otros <- sum(coalesce(as.numeric(ed_prorr$Valor), 0), na.rm = TRUE)
        kg_total_conjunto <- kg_tot_otros + val_final
        ha_acum <- 0
        for (i in seq_len(nrow(ed_prorr))) {
          d_campo <- ed_prorr$Campo[i]
          ha_nueva_i <- if (kg_total_conjunto > 0) round(ha_pool_prorr * (as.numeric(ed_prorr$Valor[i]) / kg_total_conjunto), 2) else 0
          ha_acum <- ha_acum + ha_nueva_i
          
          idx <- which(
            as.character(ed_actuales$Campaña) == as.character(input$filtro_campana) &
            as.numeric(ed_actuales$Semana)    == as.numeric(input$filtro_semana) &
            as.character(ed_actuales$Módulo)   == as.character(ctx$fila$Módulo) &
            as.character(ed_actuales$Turno)    == as.character(ctx$fila$Turno) &
            limpiar_variedad(ed_actuales$Variedad) == limpiar_variedad(ctx$fila$Variedad) &
            as.character(ed_actuales$Campo)    == as.character(d_campo)
          )
          if (length(idx) > 0) {
            ed_actuales$Ha_Asignada[idx] <- ha_nueva_i
            if (pasada_activa == 2L) {
              ed_actuales$Ha_Asignada_2da[idx] <- ha_nueva_i
            } else {
              ed_actuales$Ha_Asignada_1ra[idx] <- ha_nueva_i
            }
            ed_actuales$Motivo_Ajuste[idx] <- sprintf("Manual prorrateado (%s kg en %.2f ha)", format(round(as.numeric(ed_prorr$Valor[i]), 1)), ha_nueva_i)
          }
        }
        ha_final <- max(0, round(ha_pool_prorr - ha_acum, 2))
      } else {
        ha_final <- round(ha_pool_prorr, 2)
      }
    }
    
    num_pasada_sel <- pasada_activa
    motivo_txt <- if (!is.null(input$modal_input_motivo)) trimws(as.character(input$modal_input_motivo)) else ""
    motivo_final <- if (length(motivo_txt) > 0 && nzchar(motivo_txt[1])) {
      motivo_txt[1]
    } else if (!is.null(modo) && modo == "kgha") {
      sprintf("Manual (%s kg/ha en %.2f ha)", format(round(coalesce(as.numeric(input$modal_input_kgha), 0), 1)), ha_final)
    } else if (nrow(ed_prorr) > 0) {
      sprintf("Manual prorrateado (%s kg en %.2f ha)", format(round(val_final, 1)), ha_final)
    } else {
      sprintf("Manual (%s kg en %.2f ha faltantes)", format(round(val_final, 1), big.mark = ","), ha_final)
    }
    
    nueva_edicion <- tibble(
      Campaña         = as.character(input$filtro_campana),
      Semana          = as.numeric(input$filtro_semana),
      Sector          = as.character(ctx$fila$Sector),
      Módulo          = as.character(ctx$fila$Módulo),
      Turno           = as.character(ctx$fila$Turno),
      Variedad        = limpiar_variedad(ctx$fila$Variedad),
      Campo           = as.character(ctx$campo),
      Valor           = val_final,
      Ha_Asignada     = ha_final,
      Ha_Asignada_1ra = ifelse(num_pasada_sel == 1L, ha_final, 0),
      Kg_Asignado_1ra = ifelse(num_pasada_sel == 1L, val_final, 0),
      Ha_Asignada_2da = ifelse(num_pasada_sel == 2L, ha_final, 0),
      Kg_Asignado_2da = ifelse(num_pasada_sel == 2L, val_final, 0),
      Tipo_Edicion    = "manual",
      Modo_Ingreso    = if (!is.null(modo)) modo else "kilos",
      Motivo_Ajuste   = motivo_final,
      Fecha_Edicion   = Sys.time()
    )
    
    ed_actuales <- ed_actuales %>%
      mutate(Campaña = as.character(Campaña), Semana = as.numeric(Semana)) %>%
      filter(!(
        as.character(Campaña) == as.character(input$filtro_campana) &
          as.numeric(Semana)    == as.numeric(input$filtro_semana) &
          as.character(Sector)   == as.character(ctx$fila$Sector) &
          as.character(Módulo)   == as.character(ctx$fila$Módulo) &
          as.character(Turno)    == as.character(ctx$fila$Turno) &
          limpiar_variedad(Variedad) == limpiar_variedad(ctx$fila$Variedad) &
          as.character(Campo)    == as.character(ctx$campo)
      )) %>%
      bind_rows(nueva_edicion)
    
    ediciones_manuales(ed_actuales)
    ok_disco <- guardar_ediciones_disco(ed_actuales)
    removeModal()
    
    msg_extra <- if (nrow(ed_prorr) > 0) sprintf(" (Prorrateado con %s)", paste(unique(ed_prorr$Campo), collapse = ", ")) else ""
    if (ok_disco) {
      showNotification(
        sprintf("Dato guardado como manual: [%s] = %s Kg, %.2f ha (Turno %s)%s", ctx$campo, format(val_final, big.mark = ","), ha_final, ctx$fila$Turno, msg_extra),
        type = "message",
        duration = 3
      )
    } else {
      showNotification(
        sprintf("Dato registrado en sesión: [%s] = %s Kg, %.2f ha (Turno %s)%s", ctx$campo, format(val_final, big.mark = ","), ha_final, ctx$fila$Turno, msg_extra),
        type = "warning",
        duration = 4
      )
    }
  })
  
  observeEvent(input$modal_btn_restablecer, {
    ctx <- modal_celda_activa()
    req(ctx)
    
    pasada_activa <- if (nrow(ctx$ed_celda) > 0 && coalesce(ctx$ed_celda$Ha_Asignada_2da[1], 0) > 0) 2L else ctx$pasada_sug
    ha_real_pasada <- if (pasada_activa == 2L) ctx$ha_real_2da else ctx$ha_real_1ra
    ha_pool_pasada <- max(0, round(ctx$area_turno - ha_real_pasada, 2))
    
    valvs_aj <- rv_ajuste_valvulas()
    valvs_celda <- if (!is.null(valvs_aj) && nrow(valvs_aj) > 0) {
      valvs_aj %>%
        filter(
          as.character(Campaña) == as.character(input$filtro_campana),
          as.numeric(Semana)    == as.numeric(input$filtro_semana),
          as.character(Módulo)   == as.character(ctx$fila$Módulo),
          as.character(Turno)    == as.character(ctx$fila$Turno),
          limpiar_variedad(Variedad) == limpiar_variedad(ctx$fila$Variedad),
          as.character(Dia_Asignado) == as.character(ctx$campo)
        )
    } else tibble()
    
    ed_actuales <- ediciones_manuales()
    ed_filtradas <- ed_actuales %>%
      mutate(Campaña = as.character(Campaña), Semana = as.numeric(Semana)) %>%
      filter(!(
        as.character(Campaña) == as.character(input$filtro_campana) &
          as.numeric(Semana)    == as.numeric(input$filtro_semana) &
          as.character(Sector)   == as.character(ctx$fila$Sector) &
          as.character(Módulo)   == as.character(ctx$fila$Módulo) &
          as.character(Turno)    == as.character(ctx$fila$Turno) &
          limpiar_variedad(Variedad) == limpiar_variedad(ctx$fila$Variedad) &
          as.character(Campo)    == as.character(ctx$campo)
      ))
    
    ed_otros_pasada <- ed_filtradas %>%
      filter(
        as.character(Campaña) == as.character(input$filtro_campana),
        as.numeric(Semana)    == as.numeric(input$filtro_semana),
        as.character(Módulo)   == as.character(ctx$fila$Módulo),
        as.character(Turno)    == as.character(ctx$fila$Turno),
        limpiar_variedad(Variedad) == limpiar_variedad(ctx$fila$Variedad),
        coalesce(Tipo_Edicion, "") == "manual",
        if (pasada_activa == 2L) coalesce(Ha_Asignada_2da, 0) > 0 | coalesce(Kg_Asignado_2da, 0) > 0
        else coalesce(Ha_Asignada_1ra, 0) > 0 | coalesce(Kg_Asignado_1ra, 0) > 0
      )
    
    if (nrow(ed_otros_pasada) > 0) {
      if (!("Modo_Ingreso" %in% names(ed_otros_pasada))) {
        ed_otros_pasada$Modo_Ingreso <- ifelse(grepl("kg/ha en", coalesce(ed_otros_pasada$Motivo_Ajuste, "")), "kgha", "kilos")
      }
      ed_fijas <- ed_otros_pasada %>%
        filter(coalesce(Modo_Ingreso, "") == "kgha" | grepl("kg/ha en", coalesce(Motivo_Ajuste, "")))
      ed_prorr <- ed_otros_pasada %>%
        filter(!(coalesce(Modo_Ingreso, "") == "kgha" | grepl("kg/ha en", coalesce(Motivo_Ajuste, ""))))
    } else {
      ed_fijas <- tibble()
      ed_prorr <- tibble()
    }
    ha_fijas <- if (nrow(ed_fijas) > 0) sum(if (pasada_activa == 2L) coalesce(ed_fijas$Ha_Asignada_2da, 0) else coalesce(ed_fijas$Ha_Asignada_1ra, 0), na.rm = TRUE) else 0
    ha_pool_prorr <- max(0, round(ha_pool_pasada - ha_fijas, 2))
    
    if (nrow(ed_prorr) > 0) {
      kg_tot_rest <- sum(coalesce(as.numeric(ed_prorr$Valor), 0), na.rm = TRUE)
      ha_acum <- 0
      for (i in seq_len(nrow(ed_prorr))) {
        d_campo <- ed_prorr$Campo[i]
        ha_nueva_i <- if (i == nrow(ed_prorr)) {
          max(0, round(ha_pool_prorr - ha_acum, 2))
        } else {
          if (kg_tot_rest > 0) round(ha_pool_prorr * (as.numeric(ed_prorr$Valor[i]) / kg_tot_rest), 2) else 0
        }
        ha_acum <- ha_acum + ha_nueva_i
        
        idx <- which(
          as.character(ed_filtradas$Campaña) == as.character(input$filtro_campana) &
          as.numeric(ed_filtradas$Semana)    == as.numeric(input$filtro_semana) &
          as.character(ed_filtradas$Módulo)   == as.character(ctx$fila$Módulo) &
          as.character(ed_filtradas$Turno)    == as.character(ctx$fila$Turno) &
          limpiar_variedad(ed_filtradas$Variedad) == limpiar_variedad(ctx$fila$Variedad) &
          as.character(ed_filtradas$Campo)    == as.character(d_campo)
        )
        if (length(idx) > 0) {
          ed_filtradas$Ha_Asignada[idx] <- ha_nueva_i
          if (pasada_activa == 2L) {
            ed_filtradas$Ha_Asignada_2da[idx] <- ha_nueva_i
          } else {
            ed_filtradas$Ha_Asignada_1ra[idx] <- ha_nueva_i
          }
          ed_filtradas$Motivo_Ajuste[idx] <- if (nrow(ed_prorr) > 1) {
            sprintf("Manual prorrateado (%s kg en %.2f ha)", format(round(as.numeric(ed_prorr$Valor[i]), 1)), ha_nueva_i)
          } else {
            sprintf("Manual (%s kg en %.2f ha faltantes)", format(round(as.numeric(ed_prorr$Valor[i]), 1)), ha_nueva_i)
          }
        }
      }
    }
    
    if (nrow(valvs_celda) > 0) {
      reg_restaurado <- tibble(
        Campaña         = as.character(input$filtro_campana),
        Semana          = as.numeric(input$filtro_semana),
        Sector          = as.character(ctx$fila$Sector),
        Módulo          = as.character(ctx$fila$Módulo),
        Turno           = as.character(ctx$fila$Turno),
        Variedad        = limpiar_variedad(ctx$fila$Variedad),
        Campo           = as.character(ctx$campo),
        Valor           = round(sum(valvs_celda$Kg_Asignado, na.rm = TRUE), 2),
        Ha_Asignada     = round(sum(valvs_celda$Ha_Asignada, na.rm = TRUE), 2),
        Ha_Asignada_1ra = round(sum(ifelse(coalesce(valvs_celda$Num_Pasada, 1L) == 1L, valvs_celda$Ha_Asignada, 0), na.rm = TRUE), 2),
        Kg_Asignado_1ra = round(sum(ifelse(coalesce(valvs_celda$Num_Pasada, 1L) == 1L, valvs_celda$Kg_Asignado, 0), na.rm = TRUE), 2),
        Ha_Asignada_2da = round(sum(ifelse(coalesce(valvs_celda$Num_Pasada, 1L) == 2L, valvs_celda$Ha_Asignada, 0), na.rm = TRUE), 2),
        Kg_Asignado_2da = round(sum(ifelse(coalesce(valvs_celda$Num_Pasada, 1L) == 2L, valvs_celda$Kg_Asignado, 0), na.rm = TRUE), 2),
        Tipo_Edicion    = "auto",
        Motivo_Ajuste   = first(na.omit(valvs_celda$Motivo_Ajuste)),
        Fecha_Edicion   = Sys.time()
      )
      ed_filtradas <- bind_rows(ed_filtradas, reg_restaurado)
    }
    
    ediciones_manuales(ed_filtradas)
    guardar_ediciones_disco(ed_filtradas)
    removeModal()
    showNotification(sprintf("Se restableció la celda [%s] del Turno %s a su estado automático.", ctx$campo, ctx$fila$Turno), type = "message", duration = 3)
  })
  
  # ==============================================================================
  # MODAL INTERACTIVO: AJUSTE DIRECTO DE RENDIMIENTO (KG/HA) Y REPARTO PROPORCIONAL
  # ==============================================================================
  abrir_modal_kgha_ajustado <- function(fila, col_name) {
    pasada_sel <- if (col_name == "Kg/Ha Aj. 2ª Pas") 2L else 1L
    col_ha <- if (pasada_sel == 2L) "Ha Aj. 2ª Pas" else "Ha Aj. 1ª Pas"
    col_kgha <- if (pasada_sel == 2L) "Kg/Ha Aj. 2ª Pas" else "Kg/Ha Aj. 1ª Pas"
    col_kg <- if (pasada_sel == 2L) "Kg_Aj_2da" else "Kg_Aj_1ra"
    
    ha_pasada <- coalesce(as.numeric(fila[[col_ha]]), 0)
    kgha_actual <- coalesce(as.numeric(fila[[col_kgha]]), 0)
    kg_pasada <- coalesce(as.numeric(fila[[col_kg]]), 0)
    kgha_proy <- coalesce(as.numeric(fila[['Kg/Ha Proy']]), 0)
    area_turno <- coalesce(as.numeric(fila$Área), 0)
    
    if (ha_pasada <= 0) {
      showNotification(
        sprintf("El Turno %s no tiene hectáreas asignadas en la %s para ajustar.", 
                fila$Turno, if (pasada_sel == 2L) "2ª Pasada" else "1ª Pasada"),
        type = "warning",
        duration = 4
      )
      return()
    }
    
    # Identificar días ajustados de este turno para esta pasada
    dias_semana <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
    ed_act <- ediciones_manuales()
    valvs_act <- rv_ajuste_valvulas()
    
    # Días con asignación específica de pasada en ediciones previas o válvulas
    dias_pasada_valvs <- if (!is.null(valvs_act) && nrow(valvs_act) > 0) {
      valvs_act %>%
        filter(
          as.character(Campaña) == as.character(input$filtro_campana),
          as.numeric(Semana)    == as.numeric(input$filtro_semana),
          as.character(Módulo)   == as.character(fila$Módulo),
          as.character(Turno)    == as.character(fila$Turno),
          limpiar_variedad(Variedad) == limpiar_variedad(fila$Variedad),
          coalesce(Num_Pasada, 1L) == pasada_sel
        ) %>%
        pull(Dia_Asignado) %>% unique()
    } else character(0)
    
    dias_pasada_ed <- if (!is.null(ed_act) && nrow(ed_act) > 0) {
      ed_act %>%
        filter(
          as.character(Campaña) == as.character(input$filtro_campana),
          as.numeric(Semana)    == as.numeric(input$filtro_semana),
          as.character(Módulo)   == as.character(fila$Módulo),
          as.character(Turno)    == as.character(fila$Turno),
          limpiar_variedad(Variedad) == limpiar_variedad(fila$Variedad),
          if (pasada_sel == 2L) coalesce(Ha_Asignada_2da, 0) > 0 | coalesce(Kg_Asignado_2da, 0) > 0
          else coalesce(Ha_Asignada_1ra, 0) > 0 | coalesce(Kg_Asignado_1ra, 0) > 0
        ) %>%
        pull(Campo) %>% unique()
    } else character(0)
    
    # Días con datos de ajuste en la fila
    dias_con_ajuste_fila <- dias_semana[sapply(dias_semana, function(d) {
      val <- coalesce(as.numeric(fila[[d]]), 0)
      tip <- coalesce(as.character(fila[[paste0(d, "_tipo")]]), "none")
      bloq <- isTRUE(fila[[paste0(d, "_bloqueado")]])
      val > 0 && tip %in% c("auto", "manual") && !bloq
    })]
    
    dias_ajustados <- if (pasada_sel == 2L) {
      cands <- unique(c(dias_pasada_valvs, dias_pasada_ed))
      intersect(dias_con_ajuste_fila, cands)
    } else {
      # 1ra pasada
      if (limpiar_variedad(fila$Variedad) == "ROCIO" && coalesce(as.numeric(fila[['Ha Aj. 2ª Pas']]), 0) > 0) {
        dias_2da_cands <- if (!is.null(valvs_act) && nrow(valvs_act) > 0) {
          valvs_act %>%
            filter(
              as.character(Campaña) == as.character(input$filtro_campana),
              as.numeric(Semana)    == as.numeric(input$filtro_semana),
              as.character(Módulo)   == as.character(fila$Módulo),
              as.character(Turno)    == as.character(fila$Turno),
              limpiar_variedad(Variedad) == limpiar_variedad(fila$Variedad),
              coalesce(Num_Pasada, 1L) == 2L
            ) %>% pull(Dia_Asignado) %>% unique()
        } else character(0)
        setdiff(dias_con_ajuste_fila, dias_2da_cands)
      } else {
        dias_con_ajuste_fila
      }
    }
    
    if (length(dias_ajustados) == 0) {
      showNotification(
        sprintf("No se encontraron días con ajuste activo para la %s del Turno %s.", 
                if (pasada_sel == 2L) "2ª Pasada" else "1ª Pasada", fila$Turno),
        type = "warning",
        duration = 4
      )
      return()
    }
    
    # Kilos actuales de cada día ajustado
    kg_dias_actuales <- sapply(dias_ajustados, function(d) coalesce(as.numeric(fila[[d]]), 0))
    
    # Si algún día tiene edición manual previa
    tiene_manuales <- any(sapply(dias_ajustados, function(d) {
      coalesce(as.character(fila[[paste0(d, "_tipo")]]), "") == "manual"
    }))
    
    ctx_kgha <- list(
      fila             = fila,
      col_name         = col_name,
      pasada_sel       = pasada_sel,
      ha_pasada        = ha_pasada,
      kgha_actual      = kgha_actual,
      kg_pasada        = kg_pasada,
      kgha_proy        = kgha_proy,
      area_turno       = area_turno,
      dias_ajustados   = dias_ajustados,
      kg_dias_actuales = kg_dias_actuales,
      tiene_manuales   = tiene_manuales
    )
    modal_kgha_activo(ctx_kgha)
    
    showModal(modalDialog(
      title = tagList(
        div(
          style = "display: flex; align-items: flex-start; justify-content: space-between; width: 100%;",
          div(
            tags$div(style = "font-size: 15.5px; font-weight: 600; color: #09090b; letter-spacing: -0.01em; line-height: 1.3;", 
                     sprintf("Ajustar Rendimiento • %s", if (pasada_sel == 2L) "2ª Pasada" else "1ª Pasada")),
            tags$div(style = "font-size: 12px; font-weight: 400; color: #71717a; margin-top: 3px;",
                     sprintf("Sector %s • Módulo %s • Turno %s • %s (%s) • %.2f ha",
                             fila$Sector, fila$Módulo, fila$Turno, fila$Variedad, fila$Tipo, area_turno))
          ),
          div(
            if (tiene_manuales) {
              tags$span(style = "display: inline-flex; align-items: center; padding: 2px 8px; border-radius: 9999px; font-size: 11px; font-weight: 500; background: #ede9fe; color: #5b21b6; border: 1px solid #ddd6fe;", "Edición manual")
            } else {
              tags$span(style = "display: inline-flex; align-items: center; padding: 2px 8px; border-radius: 9999px; font-size: 11px; font-weight: 500; background: #fef3c7; color: #92400e; border: 1px solid #fde68a;", "Ajuste automático")
            }
          )
        )
      ),
      size = "m",
      easyClose = TRUE,
      uiOutput("ui_modal_kgha_cuerpo"),
      footer = tagList(
        if (tiene_manuales) {
          actionButton("modal_kgha_btn_restablecer", "Restablecer a automático", class = "ek-btn-danger", style = "margin-right: auto;")
        },
        modalButton("Cancelar"),
        actionButton("modal_kgha_btn_guardar", "Aplicar y Guardar como Manual", class = "ek-btn-primary")
      )
    ))
  }
  
  output$ui_modal_kgha_cuerpo <- renderUI({
    ctx <- modal_kgha_activo()
    req(ctx)
    
    fila <- ctx$fila
    ha_pasada <- ctx$ha_pasada
    kgha_actual <- ctx$kgha_actual
    kg_pasada <- ctx$kg_pasada
    kgha_proy <- ctx$kgha_proy
    dias <- ctx$dias_ajustados
    kg_orig <- ctx$kg_dias_actuales
    tot_orig <- sum(kg_orig)
    
    # Proporciones porcentuales iniciales
    props_pct <- if (tot_orig > 0) round((kg_orig / tot_orig) * 100, 1) else rep(round(100 / length(dias), 1), length(dias))
    props_pct[length(dias)] <- round(100 - sum(props_pct[seq_len(length(dias)-1)]), 1)
    
    tagList(
      # Datos ocultos para cálculo JS
      tags$input(type = "hidden", id = "modal_kgha_hidden_ha", value = sprintf("%.4f", ha_pasada)),
      tags$input(type = "hidden", id = "modal_kgha_hidden_kg_orig", value = sprintf("%.4f", kg_pasada)),
      
      # 3 Tarjetas de métricas superiores
      div(
        style = "display: grid; grid-template-columns: repeat(3, 1fr); gap: 10px; margin-bottom: 18px;",
        div(
          style = "background: #fbfbfa; border: 1px solid #e7e5e4; border-radius: 9px; padding: 10px 12px;",
          tags$div(style = "font-size: 11px; font-weight: 500; color: #78716c;", "Área asignada"),
          tags$div(style = "font-size: 15px; font-weight: 700; color: #1c1917; margin-top: 3px; font-variant-numeric: tabular-nums;", 
                   sprintf("%.2f ha", ha_pasada))
        ),
        div(
          style = "background: #fbfbfa; border: 1px solid #e7e5e4; border-radius: 9px; padding: 10px 12px;",
          tags$div(style = "font-size: 11px; font-weight: 500; color: #78716c;", "Rendimiento actual"),
          tags$div(style = "font-size: 15px; font-weight: 700; color: #0284c7; margin-top: 3px; font-variant-numeric: tabular-nums;", 
                   sprintf("%.2f kg/ha", kgha_actual)),
          tags$div(style = "font-size: 10.5px; color: #a8a29e; margin-top: 1px;", 
                   sprintf("Total: %s kg", format(round(kg_pasada, 1), big.mark = ",")))
        ),
        div(
          style = "background: #fbfbfa; border: 1px solid #e7e5e4; border-radius: 9px; padding: 10px 12px;",
          tags$div(style = "font-size: 11px; font-weight: 500; color: #78716c;", "Rendimiento proyección"),
          tags$div(style = "font-size: 15px; font-weight: 700; color: #16a34a; margin-top: 3px; font-variant-numeric: tabular-nums;", 
                   if (kgha_proy > 0) sprintf("%.2f kg/ha", kgha_proy) else "-"),
          tags$div(style = "font-size: 10.5px; color: #a8a29e; margin-top: 1px;", 
                   if (kgha_proy > 0) sprintf("Total: %s kg", format(round(ha_pasada * kgha_proy, 1), big.mark = ",")) else "")
        )
      ),
      
      # Input para nuevo rendimiento
      div(
        style = "margin-bottom: 14px;",
        div(
          style = "display: flex; align-items: center; justify-content: space-between; margin-bottom: 6px;",
          tags$label(style = "font-size: 12.5px; font-weight: 600; color: #18181b; margin: 0;", "Nuevo rendimiento objetivo (kg/ha)"),
          if (kgha_proy > 0) {
            actionLink(
              "modal_kgha_btn_copiar_proy",
              label = tagList(
                tags$span(style = "font-size: 11px; color: #2563eb; font-weight: 500;", 
                          sprintf("Usar proyección (%.2f)", kgha_proy))
              )
            )
          }
        ),
        numericInput(
          "modal_kgha_input_valor",
          label = NULL,
          value = round(kgha_actual, 2),
          min = 1,
          step = 10,
          width = "100%"
        )
      ),
      
      # Card de distribución editable (porcentajes interactivos por día)
      div(
        style = "background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 9px; padding: 12px 14px; margin-top: 6px;",
        div(
          style = "display: flex; align-items: baseline; justify-content: space-between; margin-bottom: 10px;",
          div(
            style = "display: flex; align-items: center; gap: 8px;",
            tags$div(style = "font-size: 12px; font-weight: 600; color: #334155;", "Distribución proporcional estimada"),
            tags$span(id = "modal_kgha_suma_pct_badge", style = "font-size: 10.5px; font-weight: 600; padding: 1px 7px; border-radius: 6px; background: #ecfdf5; color: #059669; border: 1px solid #a7f3d0;", "100.0%")
          ),
          tags$div(
            style = "font-size: 13.5px; font-weight: 700; color: #0f172a; font-variant-numeric: tabular-nums;",
            "Total: ", tags$span(id = "modal_kgha_tot_kg", sprintf("%s kg", format(round(kg_pasada, 2), big.mark = ","))),
            " (", tags$span(id = "modal_kgha_tot_diff", style = "color: #64748b;", "+0.0 kg"), ")"
          )
        ),
        div(
          style = "display: flex; flex-direction: column; gap: 6px;",
          lapply(seq_along(dias), function(i) {
            d <- dias[i]
            d_orig <- kg_orig[i]
            p_ini <- props_pct[i]
            
            div(
              class = "modal-kgha-day-row",
              `data-day` = d,
              `data-orig-kg` = sprintf("%.4f", d_orig),
              style = "display: flex; align-items: center; justify-content: space-between; padding: 6px 10px; background: #ffffff; border: 1px solid #e2e8f0; border-radius: 6px;",
              div(
                style = "display: flex; align-items: center; gap: 8px;",
                tags$span(style = "font-size: 12px; font-weight: 600; color: #1e293b; min-width: 65px;", d),
                div(
                  class = "modal-kgha-pct-container",
                  style = "display: inline-flex; align-items: center; background: #ffffff; border: 1px solid #d4d4d8; border-radius: 6px; padding: 2px 6px; box-shadow: 0 1px 2px rgba(0,0,0,0.03);",
                  tags$input(
                    type = "number",
                    class = "modal-kgha-pct-input",
                    `data-day` = d,
                    min = "0",
                    max = "100",
                    step = "0.5",
                    value = sprintf("%.1f", p_ini),
                    style = "width: 48px; border: none; background: transparent; font-size: 11.5px; font-weight: 600; color: #09090b; text-align: right; outline: none; padding: 0; font-variant-numeric: tabular-nums;"
                  ),
                  tags$span(style = "font-size: 10.5px; font-weight: 600; color: #71717a; margin-left: 2px;", "%")
                )
              ),
              div(
                style = "display: flex; align-items: center; gap: 10px; font-variant-numeric: tabular-nums;",
                tags$span(style = "font-size: 11.5px; color: #94a3b8; text-decoration: line-through;", sprintf("%s kg", format(round(d_orig, 1), big.mark = ","))),
                tags$span(class = "modal-kgha-day-kilos", `data-day` = d, style = "font-size: 12.5px; font-weight: 600; color: #0f172a;", sprintf("%s kg", format(round(d_orig, 2), big.mark = ","))),
                tags$span(class = "modal-kgha-day-diff", `data-day` = d, style = "font-size: 11px; font-weight: 500; min-width: 55px; text-align: right; color: #16a34a;", "+0.0 kg")
              )
            )
          })
        ),
        div(
          style = "font-size: 11px; color: #64748b; margin-top: 10px; line-height: 1.35;",
          "Nota: Los ", tags$b("porcentajes son editables"), ". Al guardar, los valores de estos días se convertirán en ",
          tags$b(style = "color: #7c3aed;", "ediciones manuales"),
          " con la proporción indicada y el rendimiento ajustado del turno se actualizará al nuevo objetivo."
        )
      ),
      
      # Script interactivo para recálculo instantáneo en cliente sin pérdida de foco
      tags$script(HTML("
        (function() {
          function recalcularKghaModal(changedDay) {
            var areaPasada = parseFloat($('#modal_kgha_hidden_ha').val()) || 0;
            var kgOrigTotal = parseFloat($('#modal_kgha_hidden_kg_orig').val()) || 0;
            var targetKgha = parseFloat($('#modal_kgha_input_valor').val()) || 0;
            var totalKgNuevo = Math.round(areaPasada * targetKgha * 100) / 100;
            
            var rows = $('.modal-kgha-day-row');
            var nDays = rows.length;
            if (nDays === 0) return;
            
            // Auto-balance automático si son exactamente 2 días
            if (nDays === 2 && changedDay) {
              var otherRow = rows.filter(function() { return $(this).attr('data-day') !== changedDay; });
              var changedRow = rows.filter(function() { return $(this).attr('data-day') === changedDay; });
              var changedInput = changedRow.find('.modal-kgha-pct-input');
              var changedVal = parseFloat(changedInput.val());
              if (isNaN(changedVal)) changedVal = 0;
              if (changedVal < 0) changedVal = 0;
              if (changedVal > 100) changedVal = 100;
              var otherVal = Math.round((100 - changedVal) * 10) / 10;
              otherRow.find('.modal-kgha-pct-input').val(otherVal);
            }
            
            var sumPct = 0;
            rows.each(function() {
              var p = parseFloat($(this).find('.modal-kgha-pct-input').val()) || 0;
              sumPct += p;
            });
            sumPct = Math.round(sumPct * 10) / 10;
            
            var badge = $('#modal_kgha_suma_pct_badge');
            if (Math.abs(sumPct - 100) < 0.1) {
              badge.text('100.0%').css({ background: '#ecfdf5', color: '#059669', borderColor: '#a7f3d0' });
            } else {
              badge.text('Suma: ' + sumPct + '%').css({ background: '#fef2f2', color: '#dc2626', borderColor: '#fecaca' });
            }
            
            var kgAcum = 0;
            var haAcum = 0;
            var diasData = [];
            
            rows.each(function(idx) {
              var row = $(this);
              var day = row.attr('data-day');
              var origKg = parseFloat(row.attr('data-orig-kg')) || 0;
              var pct = parseFloat(row.find('.modal-kgha-pct-input').val()) || 0;
              var prop = sumPct > 0 ? (pct / sumPct) : (1 / nDays);
              
              var dayKg = 0;
              var dayHa = 0;
              if (idx === nDays - 1) {
                dayKg = Math.max(0, Math.round((totalKgNuevo - kgAcum) * 100) / 100);
                dayHa = Math.max(0, Math.round((areaPasada - haAcum) * 100) / 100);
              } else {
                dayKg = Math.round(totalKgNuevo * prop * 100) / 100;
                dayHa = Math.round(areaPasada * prop * 100) / 100;
                kgAcum += dayKg;
                haAcum += dayHa;
              }
              
              var dayDiff = Math.round((dayKg - origKg) * 10) / 10;
              
              row.find('.modal-kgha-day-kilos').text(dayKg.toLocaleString('en-US', { minimumFractionDigits: 2, maximumFractionDigits: 2 }) + ' kg');
              var diffSpan = row.find('.modal-kgha-day-diff');
              diffSpan.text((dayDiff >= 0 ? '+' : '') + dayDiff.toFixed(1) + ' kg');
              diffSpan.css('color', dayDiff >= 0 ? '#16a34a' : '#dc2626');
              
              diasData.push({
                dia: day,
                pct: pct,
                kg: dayKg,
                ha: dayHa
              });
            });
            
            var totDiff = Math.round((totalKgNuevo - kgOrigTotal) * 10) / 10;
            $('#modal_kgha_tot_kg').text(totalKgNuevo.toLocaleString('en-US', { minimumFractionDigits: 2, maximumFractionDigits: 2 }) + ' kg');
            var totDiffSpan = $('#modal_kgha_tot_diff');
            totDiffSpan.text((totDiff >= 0 ? '+' : '') + totDiff.toFixed(1) + ' kg');
            totDiffSpan.css('color', totDiff >= 0 ? '#16a34a' : '#dc2626');
            
            Shiny.setInputValue('modal_kgha_distribucion_actualizada', {
              nuevo_kgha: targetKgha,
              total_kg: totalKgNuevo,
              area_pasada: areaPasada,
              dias: diasData
            }, { priority: 'event' });
          }
          
          $(document).off('input change', '.modal-kgha-pct-input').on('input change', '.modal-kgha-pct-input', function() {
            var day = $(this).attr('data-day');
            recalcularKghaModal(day);
          });
          
          $(document).off('input change', '#modal_kgha_input_valor').on('input change', '#modal_kgha_input_valor', function() {
            recalcularKghaModal(null);
          });
          
          setTimeout(function() { recalcularKghaModal(null); }, 60);
        })();
      "))
    )
  })
  
  observeEvent(input$modal_kgha_btn_copiar_proy, {
    ctx <- modal_kgha_activo()
    req(ctx)
    if (ctx$kgha_proy > 0) {
      updateNumericInput(session, "modal_kgha_input_valor", value = round(ctx$kgha_proy, 2))
    }
  })
  
  observeEvent(input$modal_kgha_btn_guardar, {
    ctx <- modal_kgha_activo()
    req(ctx)
    
    nuevo_kgha <- suppressWarnings(as.numeric(input$modal_kgha_input_valor))
    if (length(nuevo_kgha) == 0 || is.na(nuevo_kgha) || nuevo_kgha <= 0) {
      showNotification("Por favor ingresa un rendimiento válido (mayor a 0 kg/ha).", type = "warning", duration = 4)
      return()
    }
    
    nuevo_kgha <- round(nuevo_kgha, 2)
    fila <- ctx$fila
    pasada_sel <- ctx$pasada_sel
    ha_pasada <- ctx$ha_pasada
    dias <- ctx$dias_ajustados
    
    # Obtener distribución enviada desde el cliente (con porcentajes editados)
    dist_info <- input$modal_kgha_distribucion_actualizada
    
    nuevas_filas <- if (!is.null(dist_info) && length(dist_info$dias) > 0) {
      df_dias_cli <- bind_rows(lapply(dist_info$dias, as_tibble))
      lapply(seq_len(nrow(df_dias_cli)), function(i) {
        d_nom <- as.character(df_dias_cli$dia[i])
        k_val <- round(as.numeric(df_dias_cli$kg[i]), 2)
        h_val <- round(as.numeric(df_dias_cli$ha[i]), 2)
        p_val <- round(as.numeric(df_dias_cli$pct[i]), 1)
        tibble(
          Campaña         = as.character(input$filtro_campana),
          Semana          = as.numeric(input$filtro_semana),
          Sector          = as.character(fila$Sector),
          Módulo          = as.character(fila$Módulo),
          Turno           = as.character(fila$Turno),
          Variedad        = limpiar_variedad(fila$Variedad),
          Campo           = d_nom,
          Valor           = k_val,
          Ha_Asignada     = h_val,
          Ha_Asignada_1ra = if (pasada_sel == 1L) h_val else 0,
          Kg_Asignado_1ra = if (pasada_sel == 1L) k_val else 0,
          Ha_Asignada_2da = if (pasada_sel == 2L) h_val else 0,
          Kg_Asignado_2da = if (pasada_sel == 2L) k_val else 0,
          Tipo_Edicion    = "manual",
          Modo_Ingreso    = "kgha",
          Motivo_Ajuste   = sprintf("Manual (%s kg/ha en %.2f ha, %.1f%%)", format(round(nuevo_kgha, 2)), h_val, p_val),
          Fecha_Edicion   = Sys.time()
        )
      }) %>% bind_rows()
    } else {
      # Fallback proporcional por servidor
      kg_orig <- ctx$kg_dias_actuales
      tot_orig <- sum(kg_orig)
      kg_tot_nuevo <- round(ha_pasada * nuevo_kgha, 2)
      props <- if (tot_orig > 0) kg_orig / tot_orig else rep(1 / length(dias), length(dias))
      kg_dist <- numeric(length(dias))
      ha_dist <- numeric(length(dias))
      kg_acum <- 0
      ha_acum <- 0
      for (i in seq_along(dias)) {
        if (i == length(dias)) {
          kg_dist[i] <- max(0, round(kg_tot_nuevo - kg_acum, 2))
          ha_dist[i] <- max(0, round(ha_pasada - ha_acum, 2))
        } else {
          kg_dist[i] <- round(kg_tot_nuevo * props[i], 2)
          ha_dist[i] <- round(ha_pasada * props[i], 2)
          kg_acum <- kg_acum + kg_dist[i]
          ha_acum <- ha_acum + ha_dist[i]
        }
      }
      lapply(seq_along(dias), function(i) {
        d_nom <- dias[i]
        k_val <- kg_dist[i]
        h_val <- ha_dist[i]
        tibble(
          Campaña         = as.character(input$filtro_campana),
          Semana          = as.numeric(input$filtro_semana),
          Sector          = as.character(fila$Sector),
          Módulo          = as.character(fila$Módulo),
          Turno           = as.character(fila$Turno),
          Variedad        = limpiar_variedad(fila$Variedad),
          Campo           = d_nom,
          Valor           = k_val,
          Ha_Asignada     = h_val,
          Ha_Asignada_1ra = if (pasada_sel == 1L) h_val else 0,
          Kg_Asignado_1ra = if (pasada_sel == 1L) k_val else 0,
          Ha_Asignada_2da = if (pasada_sel == 2L) h_val else 0,
          Kg_Asignado_2da = if (pasada_sel == 2L) k_val else 0,
          Tipo_Edicion    = "manual",
          Modo_Ingreso    = "kgha",
          Motivo_Ajuste   = sprintf("Manual (%s kg/ha en %.2f ha)", format(round(nuevo_kgha, 2)), h_val),
          Fecha_Edicion   = Sys.time()
        )
      }) %>% bind_rows()
    }
    
    ed_actuales <- ediciones_manuales()
    if (!is.null(ed_actuales) && nrow(ed_actuales) > 0) {
      ed_actuales <- ed_actuales %>% mutate(Campaña = as.character(Campaña), Semana = as.numeric(Semana))
    } else {
      ed_actuales <- tibble()
    }
    
    ed_actualizadas <- ed_actuales %>%
      filter(!(
        as.character(Campaña) == as.character(input$filtro_campana) &
        as.numeric(Semana)    == as.numeric(input$filtro_semana) &
        as.character(Sector)  == as.character(fila$Sector) &
        as.character(Módulo)  == as.character(fila$Módulo) &
        as.character(Turno)   == as.character(fila$Turno) &
        limpiar_variedad(Variedad) == limpiar_variedad(fila$Variedad) &
        as.character(Campo)   %in% as.character(dias)
      )) %>%
      bind_rows(nuevas_filas)
    
    ediciones_manuales(ed_actualizadas)
    ok_disco <- guardar_ediciones_disco(ed_actualizadas)
    removeModal()
    
    if (ok_disco) {
      showNotification(
        sprintf("Rendimiento ajustado a %.2f kg/ha para Turno %s. Los días %s se guardaron como manuales.",
                nuevo_kgha, fila$Turno, paste(dias, collapse = ", ")),
        type = "message",
        duration = 4
      )
    } else {
      showNotification(
        sprintf("Rendimiento actualizado en sesión a %.2f kg/ha para Turno %s (%s).",
                nuevo_kgha, fila$Turno, paste(dias, collapse = ", ")),
        type = "warning",
        duration = 4
      )
    }
  })
  
  observeEvent(input$modal_kgha_btn_restablecer, {
    ctx <- modal_kgha_activo()
    req(ctx)
    
    fila <- ctx$fila
    dias <- ctx$dias_ajustados
    pasada_sel <- ctx$pasada_sel
    
    valvs_aj <- rv_ajuste_valvulas()
    valvs_t <- if (!is.null(valvs_aj) && nrow(valvs_aj) > 0) {
      valvs_aj %>%
        filter(
          as.character(Campaña) == as.character(input$filtro_campana),
          as.numeric(Semana)    == as.numeric(input$filtro_semana),
          as.character(Módulo)   == as.character(fila$Módulo),
          as.character(Turno)    == as.character(fila$Turno),
          limpiar_variedad(Variedad) == limpiar_variedad(fila$Variedad),
          as.character(Dia_Asignado) %in% as.character(dias)
        )
    } else tibble()
    
    ed_actuales <- ediciones_manuales()
    if (!is.null(ed_actuales) && nrow(ed_actuales) > 0) {
      ed_filtradas <- ed_actuales %>%
        mutate(Campaña = as.character(Campaña), Semana = as.numeric(Semana)) %>%
        filter(!(
          as.character(Campaña) == as.character(input$filtro_campana) &
          as.numeric(Semana)    == as.numeric(input$filtro_semana) &
          as.character(Sector)  == as.character(fila$Sector) &
          as.character(Módulo)  == as.character(fila$Módulo) &
          as.character(Turno)   == as.character(fila$Turno) &
          limpiar_variedad(Variedad) == limpiar_variedad(fila$Variedad) &
          as.character(Campo)   %in% as.character(dias)
        ))
    } else {
      ed_filtradas <- tibble()
    }
    
    if (nrow(valvs_t) > 0) {
      ed_auto_restauradas <- valvs_t %>%
        group_by(Campaña, Semana, Sector, Módulo, Turno, Variedad, Dia_Asignado) %>%
        summarise(
          Valor           = round(sum(Kg_Asignado, na.rm = TRUE), 2),
          Ha_Asignada     = round(sum(Ha_Asignada, na.rm = TRUE), 2),
          Ha_Asignada_1ra = round(sum(ifelse(coalesce(Num_Pasada, 1L) == 1L, Ha_Asignada, 0), na.rm = TRUE), 2),
          Kg_Asignado_1ra = round(sum(ifelse(coalesce(Num_Pasada, 1L) == 1L, Kg_Asignado, 0), na.rm = TRUE), 2),
          Ha_Asignada_2da = round(sum(ifelse(coalesce(Num_Pasada, 1L) == 2L, Ha_Asignada, 0), na.rm = TRUE), 2),
          Kg_Asignado_2da = round(sum(ifelse(coalesce(Num_Pasada, 1L) == 2L, Kg_Asignado, 0), na.rm = TRUE), 2),
          Motivo_Ajuste   = first(na.omit(Motivo_Ajuste)),
          .groups         = "drop"
        ) %>%
        rename(Campo = Dia_Asignado) %>%
        mutate(
          Fecha_Edicion = Sys.time(),
          Tipo_Edicion  = "auto",
          Modo_Ingreso  = NA_character_
        )
      ed_filtradas <- bind_rows(ed_filtradas, ed_auto_restauradas)
    }
    
    ediciones_manuales(ed_filtradas)
    guardar_ediciones_disco(ed_filtradas)
    removeModal()
    
    showNotification(
      sprintf("Se restableció el cálculo automático para los días %s del Turno %s.", paste(dias, collapse = ", "), fila$Turno),
      type = "message",
      duration = 4
    )
  })
  
  # ==============================================================================
  # SERVER: TAB RESUMEN (REAL VS AJUSTADO EN 3 FORMAS)
  # ==============================================================================
  
  # Dataset base para el resumen (Semana Completa o Con Filtros según switch)
  datos_resumen_base <- reactive({
    df <- if (isTRUE(input$switch_alcance_resumen == "filtrada")) {
      datos_reactivos()
    } else {
      universo_completo()
    }
    req(!is.null(df) && nrow(df) > 0)
    
    es_campana_act <- if (!is.null(input$filtro_campana)) grepl("2026|^26", as.character(input$filtro_campana)) else TRUE
    
    df %>%
      mutate(
        Turno_num_res = suppressWarnings(as.numeric(Turno)),
        Módulo_Str = case_when(
          es_campana_act & Módulo %in% c("13.1", "13.2") ~ "13",
          es_campana_act & Módulo %in% c("14.1", "14.2") ~ "14",
          as.character(Módulo) == "9" & between(Turno_num_res, 1, 11)  ~ "9.2",
          as.character(Módulo) == "9" & Turno_num_res >= 12            ~ "9.1",
          as.character(Módulo) == "11" & between(Turno_num_res, 1, 12) ~ "11.1",
          as.character(Módulo) == "11" & Turno_num_res >= 13           ~ "11.2",
          TRUE ~ trimws(as.character(Módulo))
        ),
        Variedad_Limpia = case_when(
          grepl("EB\\s*9", Variedad, ignore.case = TRUE) ~ "MAGICA",
          toupper(trimws(Variedad)) %in% c("MILAGROS", "VARIEDADES") ~ "VARIEDADES",
          TRUE ~ toupper(trimws(Variedad))
        ),
        Tipo_Norm = case_when(
          grepl("test|tb", Tipo, ignore.case = TRUE)  ~ "Testblock",
          grepl("org[aá]n", Tipo, ignore.case = TRUE) ~ "Orgánico",
          grepl("conv", Tipo, ignore.case = TRUE)     ~ "Convencional",
          TRUE                                        ~ "Convencional"
        ),
        Variedad_Resumen = mapply(formatear_var_resumen, Variedad_Limpia, Tipo_Norm),
        Programa = mapply(clasificar_programa_comercial, Variedad_Limpia, Tipo_Norm, Módulo_Str)
      )
  })
  
  # Textos dinámicos para encabezados y escenarios
  output$titulo_resumen_programa_comp <- renderText({
    d_aj <- if (!is.null(input$ajustar_desde_dia)) toupper(input$ajustar_desde_dia) else "JUEVES"
    paste0("AJUSTE ", d_aj, " - DISTRIBUCIÓN DE KG (SEMANA ", input$filtro_semana, ")")
  })
  
  output$txt_escenario_pesimista <- renderText({
    df <- datos_resumen_base()
    req(!is.null(df) && nrow(df) > 0)
    tot_p <- sum(coalesce(df$Proyección, 0), na.rm = TRUE)
    paste0(format(round(tot_p * 0.9, 0), big.mark = ","), " kg")
  })
  
  output$txt_escenario_optimista <- renderText({
    df <- datos_resumen_base()
    req(!is.null(df) && nrow(df) > 0)
    tot_p <- sum(coalesce(df$Proyección, 0), na.rm = TRUE)
    paste0(format(round(tot_p * 1.1, 0), big.mark = ","), " kg")
  })
  
  # ----------------------------------------------------------------------------
  # FORMA 1: TABLA RESUMEN POR VARIEDAD (Imagen 1)
  # ----------------------------------------------------------------------------
  output$tabla_resumen_variedad <- renderDT({
    df <- datos_resumen_base()
    req(!is.null(df) && nrow(df) > 0)
    
    t_var <- df %>%
      group_by(Variedad = Variedad_Resumen) %>%
      summarise(
        Lunes      = sum(coalesce(Lunes, 0), na.rm = TRUE),
        Martes     = sum(coalesce(Martes, 0), na.rm = TRUE),
        Miércoles  = sum(coalesce(Miércoles, 0), na.rm = TRUE),
        Jueves     = sum(coalesce(Jueves, 0), na.rm = TRUE),
        Viernes    = sum(coalesce(Viernes, 0), na.rm = TRUE),
        Sábado     = sum(coalesce(Sábado, 0), na.rm = TRUE),
        Acumulado  = sum(coalesce(Ajuste, 0), na.rm = TRUE),
        Proyectado = sum(coalesce(Proyección, 0), na.rm = TRUE),
        Diferencia = Acumulado - Proyectado,
        .groups    = "drop"
      ) %>%
      arrange(match(Variedad, orden_variedades_resumen), Variedad)
    
    sketch_var <- htmltools::withTags(table(
      class = "display",
      thead(
        tr(
          th("Variedad"), th("Lunes"), th("Martes"), th("Miércoles"),
          th("Jueves"), th("Viernes"), th("Sábado"), th("Acumulado"),
          th("Proyectado"), th("Diferencia")
        )
      ),
      tfoot(
        tr(lapply(1:10, function(i) th()))
      )
    ))
    
    datatable(
      t_var,
      container = sketch_var,
      rownames  = FALSE,
      options   = list(
        dom        = 't',
        paging     = FALSE,
        scrollX    = TRUE,
        ordering   = FALSE,
        columnDefs = list(
          list(className = 'dt-left', targets = 0),
          list(
            className = 'dt-right', 
            targets = 1:9,
            render = JS("
              function(data, type, row, meta) {
                if (type !== 'display') return data;
                if (data === null || data === undefined || data === 0 || data === '0') return '-';
                return Math.round(data).toLocaleString('en-US');
              }
            ")
          )
        ),
        footerCallback = JS("
          function(tfoot, data, start, end, display) {
            var api = this.api();
            var numVal = function(i) {
              if (typeof i === 'number') return i;
              if (typeof i === 'string') {
                var v = parseFloat(i.replace(/[^0-9.-]+/g, ''));
                return isNaN(v) ? 0 : v;
              }
              return 0;
            };
            var fmt = function(n) {
              return Math.round(n).toLocaleString('en-US');
            };
            $(api.column(0).footer()).html('Total general');
            for (var c = 1; c <= 9; c++) {
              var total = api.column(c).data().reduce(function(a, b) {
                return numVal(a) + numVal(b);
              }, 0);
              var html = fmt(total);
              if (c === 9) {
                var col = total >= 0 ? '#16A34A' : '#DC2626';
                html = '<span style=\"color:' + col + '; font-weight:700;\">' + html + '</span>';
              }
              $(api.column(c).footer()).html(html);
            }
            $(tfoot).find('th').css({'font-weight': '700', 'font-size': '13px', 'border-top': '2px solid rgba(0,0,0,0.15)'});
          }
        ")
      )
    ) %>%
      formatStyle('Acumulado', fontWeight = '700', color = '#007AFF') %>%
      formatStyle('Diferencia', color = styleInterval(0, c('#DC2626', '#16A34A')), fontWeight = '700')
  })
  
  # ----------------------------------------------------------------------------
  # FORMA 2: TABLA RESUMEN POR MÓDULO Y VARIEDAD (Imagen 2)
  # ----------------------------------------------------------------------------
  output$tabla_resumen_modulo_variedad <- renderDT({
    df <- datos_resumen_base()
    req(!is.null(df) && nrow(df) > 0)
    
    t_mod <- df %>%
      group_by(Módulo = Módulo_Str, Variedad = Variedad_Resumen) %>%
      summarise(
        Lunes      = sum(coalesce(Lunes, 0), na.rm = TRUE),
        Martes     = sum(coalesce(Martes, 0), na.rm = TRUE),
        Miércoles  = sum(coalesce(Miércoles, 0), na.rm = TRUE),
        Jueves     = sum(coalesce(Jueves, 0), na.rm = TRUE),
        Viernes    = sum(coalesce(Viernes, 0), na.rm = TRUE),
        Sábado     = sum(coalesce(Sábado, 0), na.rm = TRUE),
        Ajuste     = sum(coalesce(Ajuste, 0), na.rm = TRUE),
        Proyección = sum(coalesce(Proyección, 0), na.rm = TRUE),
        Diferencia = Ajuste - Proyección,
        .groups    = "drop"
      ) %>%
      arrange(suppressWarnings(as.numeric(Módulo)), Módulo, match(Variedad, orden_variedades_resumen), Variedad)
    
    sketch_mod <- htmltools::withTags(table(
      class = "display",
      thead(
        tr(
          th("Módulo"), th("Variedad"), th("Lunes"), th("Martes"), th("Miércoles"),
          th("Jueves"), th("Viernes"), th("Sábado"), th("Ajuste"),
          th("Proyección"), th("Diferencia")
        )
      ),
      tfoot(
        tr(lapply(1:11, function(i) th()))
      )
    ))
    
    datatable(
      t_mod,
      container = sketch_mod,
      rownames  = FALSE,
      options   = list(
        dom        = 't',
        paging     = FALSE,
        scrollX    = TRUE,
        ordering   = FALSE,
        columnDefs = list(
          list(className = 'dt-center', targets = 0),
          list(className = 'dt-left', targets = 1),
          list(
            className = 'dt-right', 
            targets = 2:10,
            render = JS("
              function(data, type, row, meta) {
                if (type !== 'display') return data;
                if (data === null || data === undefined || data === 0 || data === '0') return '-';
                return Math.round(data).toLocaleString('en-US');
              }
            ")
          )
        ),
        footerCallback = JS("
          function(tfoot, data, start, end, display) {
            var api = this.api();
            var numVal = function(i) {
              if (typeof i === 'number') return i;
              if (typeof i === 'string') {
                var v = parseFloat(i.replace(/[^0-9.-]+/g, ''));
                return isNaN(v) ? 0 : v;
              }
              return 0;
            };
            var fmt = function(n) {
              return Math.round(n).toLocaleString('en-US');
            };
            $(api.column(0).footer()).html('Total general');
            $(api.column(1).footer()).html('');
            for (var c = 2; c <= 10; c++) {
              var total = api.column(c).data().reduce(function(a, b) {
                return numVal(a) + numVal(b);
              }, 0);
              var html = fmt(total);
              if (c === 10) {
                var col = total >= 0 ? '#16A34A' : '#DC2626';
                html = '<span style=\"color:' + col + '; font-weight:700;\">' + html + '</span>';
              }
              $(api.column(c).footer()).html(html);
            }
            $(tfoot).find('th').css({'font-weight': '700', 'font-size': '13px', 'border-top': '2px solid rgba(0,0,0,0.15)'});
          }
        ")
      )
    ) %>%
      formatStyle('Ajuste', fontWeight = '700', color = '#007AFF') %>%
      formatStyle('Diferencia', color = styleInterval(0, c('#DC2626', '#16A34A')), fontWeight = '700')
  })
  
  # ----------------------------------------------------------------------------
  # FORMA 3A: DISTRIBUCIÓN COMERCIAL - COMPARATIVO (Imagen 3 Arriba)
  # ----------------------------------------------------------------------------
  output$tabla_resumen_programa_comp <- renderDT({
    df <- datos_resumen_base()
    req(!is.null(df) && nrow(df) > 0)
    
    col_proy_nombre <- paste0("Proyectado S", input$filtro_semana)
    
    t_prog_comp <- df %>%
      group_by(`DISTRIBUCION DE KG` = Programa) %>%
      summarise(
        Proyectado   = round(sum(coalesce(Proyección, 0), na.rm = TRUE)),
        Ajuste       = round(sum(coalesce(Ajuste, 0), na.rm = TRUE)),
        `Dif.`       = Ajuste - Proyectado,
        Cumplimiento = ifelse(Proyectado > 0, round((Ajuste / Proyectado) * 100), 0),
        .groups      = "drop"
      ) %>%
      arrange(match(`DISTRIBUCION DE KG`, orden_programas_comercial))
    
    names(t_prog_comp)[names(t_prog_comp) == "Proyectado"] <- col_proy_nombre
    
    sketch_pcomp <- htmltools::withTags(table(
      class = "display",
      thead(
        tr(
          th("DISTRIBUCION DE KG"), th(col_proy_nombre), th("Ajuste"), th("Dif."), th("Cumplimiento")
        )
      ),
      tfoot(
        tr(lapply(1:5, function(i) th()))
      )
    ))
    
    datatable(
      t_prog_comp,
      container = sketch_pcomp,
      rownames  = FALSE,
      options   = list(
        dom        = 't',
        paging     = FALSE,
        scrollX    = TRUE,
        ordering   = FALSE,
        columnDefs = list(
          list(className = 'dt-left', targets = 0),
          list(
            className = 'dt-right', 
            targets = 1:3,
            render = JS("
              function(data, type, row) {
                if (type !== 'display') return data;
                if (data === null || data === undefined) return '-';
                return Math.round(data).toLocaleString('en-US');
              }
            ")
          ),
          list(
            className = 'dt-right',
            targets = 4,
            render = JS("
              function(data, type, row) {
                if (type !== 'display') return data;
                return Math.round(data) + '%';
              }
            ")
          )
        ),
        footerCallback = JS("
          function(tfoot, data, start, end, display) {
            var api = this.api();
            var numVal = function(i) {
              if (typeof i === 'number') return i;
              if (typeof i === 'string') {
                var v = parseFloat(i.replace(/[^0-9.-]+/g, ''));
                return isNaN(v) ? 0 : v;
              }
              return 0;
            };
            var fmt = function(n) {
              return Math.round(n).toLocaleString('en-US');
            };
            var totP = api.column(1).data().reduce(function(a, b) { return numVal(a) + numVal(b); }, 0);
            var totA = api.column(2).data().reduce(function(a, b) { return numVal(a) + numVal(b); }, 0);
            var totD = totA - totP;
            var totC = totP > 0 ? Math.round((totA / totP) * 100) : 0;
            
            $(api.column(0).footer()).html('Total');
            $(api.column(1).footer()).html(fmt(totP));
            $(api.column(2).footer()).html(fmt(totA));
            $(api.column(3).footer()).html(fmt(totD));
            $(api.column(4).footer()).html(totC + '%');
            
            $(tfoot).find('tr').css({
              'background-color': '#16A34A',
              'color': '#FFFFFF',
              'font-weight': '700',
              'font-size': '13.5px'
            });
            $(tfoot).find('th').css({'color': '#FFFFFF', 'border': 'none'});
          }
        ")
      )
    ) %>%
      formatStyle('Ajuste', fontWeight = '700', color = '#007AFF') %>%
      formatStyle('Dif.', color = styleInterval(0, c('#DC2626', '#16A34A')), fontWeight = '700') %>%
      formatStyle('Cumplimiento', fontWeight = '700')
  })
  
  # ----------------------------------------------------------------------------
  # FORMA 3B: DISTRIBUCIÓN COMERCIAL - POR DÍA (Imagen 3 Abajo)
  # ----------------------------------------------------------------------------
  output$tabla_resumen_programa_dias <- renderDT({
    df <- datos_resumen_base()
    req(!is.null(df) && nrow(df) > 0)
    
    col_sem_nombre <- paste0("Sem ", input$filtro_semana)
    
    t_prog_dias <- df %>%
      group_by(`DISTRIBUCION DE KG` = Programa) %>%
      summarise(
        Lunes     = sum(coalesce(Lunes, 0), na.rm = TRUE),
        Martes    = sum(coalesce(Martes, 0), na.rm = TRUE),
        Miércoles = sum(coalesce(Miércoles, 0), na.rm = TRUE),
        Jueves    = sum(coalesce(Jueves, 0), na.rm = TRUE),
        Viernes   = sum(coalesce(Viernes, 0), na.rm = TRUE),
        Sábado    = sum(coalesce(Sábado, 0), na.rm = TRUE),
        Total_Sem = sum(coalesce(Ajuste, 0), na.rm = TRUE),
        .groups   = "drop"
      ) %>%
      arrange(match(`DISTRIBUCION DE KG`, orden_programas_comercial))
    
    names(t_prog_dias)[names(t_prog_dias) == "Total_Sem"] <- col_sem_nombre
    
    sketch_pdias <- htmltools::withTags(table(
      class = "display",
      thead(
        tr(
          th("DISTRIBUCION DE KG"), th("Lunes"), th("Martes"), th("Miércoles"),
          th("Jueves"), th("Viernes"), th("Sábado"), th(col_sem_nombre)
        )
      ),
      tfoot(
        tr(lapply(1:8, function(i) th()))
      )
    ))
    
    datatable(
      t_prog_dias,
      container = sketch_pdias,
      rownames  = FALSE,
      options   = list(
        dom        = 't',
        paging     = FALSE,
        scrollX    = TRUE,
        ordering   = FALSE,
        columnDefs = list(
          list(className = 'dt-left', targets = 0),
          list(
            className = 'dt-right', 
            targets = 1:7,
            render = JS("
              function(data, type, row) {
                if (type !== 'display') return data;
                if (data === null || data === undefined || data === 0 || data === '0') return '-';
                return Math.round(data).toLocaleString('en-US');
              }
            ")
          )
        ),
        footerCallback = JS("
          function(tfoot, data, start, end, display) {
            var api = this.api();
            var numVal = function(i) {
              if (typeof i === 'number') return i;
              if (typeof i === 'string') {
                var v = parseFloat(i.replace(/[^0-9.-]+/g, ''));
                return isNaN(v) ? 0 : v;
              }
              return 0;
            };
            var fmt = function(n) {
              return Math.round(n).toLocaleString('en-US');
            };
            $(api.column(0).footer()).html('Total');
            for (var c = 1; c <= 7; c++) {
              var total = api.column(c).data().reduce(function(a, b) {
                return numVal(a) + numVal(b);
              }, 0);
              $(api.column(c).footer()).html(fmt(total));
            }
            $(tfoot).find('tr').css({
              'background-color': '#16A34A',
              'color': '#FFFFFF',
              'font-weight': '700',
              'font-size': '13.5px'
            });
            $(tfoot).find('th').css({'color': '#FFFFFF', 'border': 'none'});
          }
        ")
      )
    ) %>%
      formatStyle(col_sem_nombre, fontWeight = '700', color = '#007AFF')
  })
  
  # ----------------------------------------------------------------------------
  # DESCARGA EXCEL MULTI-HOJA DEL RESUMEN
  # ----------------------------------------------------------------------------
  output$btn_descarga_resumen_excel <- downloadHandler(
    filename = function() {
      paste0("Resumen_Cosecha_", input$filtro_campana, "_Semana", input$filtro_semana, "_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      df <- datos_resumen_base()
      req(!is.null(df) && nrow(df) > 0)
      
      # 1. Por Variedad
      h1 <- df %>%
        group_by(Variedad = Variedad_Resumen) %>%
        summarise(
          Lunes      = sum(coalesce(Lunes, 0), na.rm = TRUE),
          Martes     = sum(coalesce(Martes, 0), na.rm = TRUE),
          Miércoles  = sum(coalesce(Miércoles, 0), na.rm = TRUE),
          Jueves     = sum(coalesce(Jueves, 0), na.rm = TRUE),
          Viernes    = sum(coalesce(Viernes, 0), na.rm = TRUE),
          Sábado     = sum(coalesce(Sábado, 0), na.rm = TRUE),
          Acumulado  = sum(coalesce(Ajuste, 0), na.rm = TRUE),
          Proyectado = sum(coalesce(Proyección, 0), na.rm = TRUE),
          Diferencia = Acumulado - Proyectado,
          .groups    = "drop"
        ) %>%
        arrange(match(Variedad, orden_variedades_resumen), Variedad)
      
      # 2. Por Módulo y Variedad
      h2 <- df %>%
        group_by(Módulo = Módulo_Str, Variedad = Variedad_Resumen) %>%
        summarise(
          Lunes      = sum(coalesce(Lunes, 0), na.rm = TRUE),
          Martes     = sum(coalesce(Martes, 0), na.rm = TRUE),
          Miércoles  = sum(coalesce(Miércoles, 0), na.rm = TRUE),
          Jueves     = sum(coalesce(Jueves, 0), na.rm = TRUE),
          Viernes    = sum(coalesce(Viernes, 0), na.rm = TRUE),
          Sábado     = sum(coalesce(Sábado, 0), na.rm = TRUE),
          Ajuste     = sum(coalesce(Ajuste, 0), na.rm = TRUE),
          Proyección = sum(coalesce(Proyección, 0), na.rm = TRUE),
          Diferencia = Ajuste - Proyección,
          .groups    = "drop"
        ) %>%
        arrange(suppressWarnings(as.numeric(Módulo)), Módulo, match(Variedad, orden_variedades_resumen), Variedad)
      
      # 3. Distribución Comercial Comparativo
      col_p <- paste0("Proyectado S", input$filtro_semana)
      h3 <- df %>%
        group_by(`DISTRIBUCION DE KG` = Programa) %>%
        summarise(
          Proyectado   = round(sum(coalesce(Proyección, 0), na.rm = TRUE)),
          Ajuste       = round(sum(coalesce(Ajuste, 0), na.rm = TRUE)),
          `Dif.`       = Ajuste - Proyectado,
          `Cumplimiento (%)` = ifelse(Proyectado > 0, round((Ajuste / Proyectado) * 100, 1), 0),
          .groups      = "drop"
        ) %>%
        arrange(match(`DISTRIBUCION DE KG`, orden_programas_comercial))
      names(h3)[names(h3) == "Proyectado"] <- col_p
      
      # 4. Distribución Comercial por Día
      col_s <- paste0("Sem ", input$filtro_semana)
      h4 <- df %>%
        group_by(`DISTRIBUCION DE KG` = Programa) %>%
        summarise(
          Lunes     = sum(coalesce(Lunes, 0), na.rm = TRUE),
          Martes    = sum(coalesce(Martes, 0), na.rm = TRUE),
          Miércoles = sum(coalesce(Miércoles, 0), na.rm = TRUE),
          Jueves    = sum(coalesce(Jueves, 0), na.rm = TRUE),
          Viernes   = sum(coalesce(Viernes, 0), na.rm = TRUE),
          Sábado    = sum(coalesce(Sábado, 0), na.rm = TRUE),
          Total_Sem = sum(coalesce(Ajuste, 0), na.rm = TRUE),
          .groups   = "drop"
        ) %>%
        arrange(match(`DISTRIBUCION DE KG`, orden_programas_comercial))
      names(h4)[names(h4) == "Total_Sem"] <- col_s
      
      hojas <- list(
        "Por Variedad"          = h1,
        "Por Modulo y Variedad" = h2,
        "Distribucion Comp"     = h3,
        "Distribucion Dias"     = h4
      )
      
      writexl::write_xlsx(hojas, path = file)
    }
  )

  # ============================================================================== =
  # MODAL DE CONFIGURACIÓN DE FRECUENCIAS DE COSECHA POR MÓDULO Y VARIEDAD (DÍAS)
  # ============================================================================== =
  rv_modo_frecuencia <- reactiveVal(cargar_modo_frecuencia_disco())
  rv_frecuencias     <- reactiveVal(cargar_frecuencias_disco())
  
  combinaciones_frecuencias_activas <- reactive({
    es_campana_act <- if (!is.null(input$filtro_campana)) grepl("2026|^26", as.character(input$filtro_campana)) else TRUE
    u <- universo_base()
    combos <- if (!is.null(u) && nrow(u) > 0 && all(c("Módulo", "Variedad") %in% names(u))) {
      u %>%
        mutate(
          Turno_num = suppressWarnings(as.numeric(Turno)),
          Módulo_c = case_when(
            es_campana_act & Módulo %in% c("13.1", "13.2") ~ "13",
            es_campana_act & Módulo %in% c("14.1", "14.2") ~ "14",
            Módulo == "9" & between(Turno_num, 1, 11)  ~ "9.2",
            Módulo == "9" & Turno_num >= 12            ~ "9.1",
            Módulo == "11" & between(Turno_num, 1, 12) ~ "11.1",
            Módulo == "11" & Turno_num >= 13           ~ "11.2",
            TRUE                                       ~ trimws(as.character(Módulo))
          ),
          Variedad_c = limpiar_variedad(Variedad)
        ) %>%
        filter(!is.na(Módulo_c), nzchar(Módulo_c), !is.na(Variedad_c), nzchar(Variedad_c)) %>%
        distinct(Módulo = Módulo_c, Variedad = Variedad_c)
    } else if (exists("df_valvulas") && is.data.frame(df_valvulas) && all(c("Módulo", "Variedad") %in% names(df_valvulas))) {
      df_valvulas %>%
        mutate(
          Turno_num = suppressWarnings(as.numeric(Turno)),
          Módulo_c = case_when(
            es_campana_act & Módulo %in% c("13.1", "13.2") ~ "13",
            es_campana_act & Módulo %in% c("14.1", "14.2") ~ "14",
            Módulo == "9" & between(Turno_num, 1, 11)  ~ "9.2",
            Módulo == "9" & Turno_num >= 12            ~ "9.1",
            Módulo == "11" & between(Turno_num, 1, 12) ~ "11.1",
            Módulo == "11" & Turno_num >= 13           ~ "11.2",
            TRUE                                       ~ trimws(as.character(Módulo))
          ),
          Variedad_c = limpiar_variedad(Variedad)
        ) %>%
        filter(!is.na(Módulo_c), nzchar(Módulo_c), !is.na(Variedad_c), nzchar(Variedad_c)) %>%
        distinct(Módulo = Módulo_c, Variedad = Variedad_c)
    } else {
      tibble(Módulo = c("13", "14", "6", "8", "10"), Variedad = c("ROCIO", "ROCIO", "MAGICA", "BILOXI", "MADEIRA"))
    }
    
    combos %>%
      arrange(suppressWarnings(as.numeric(sub("^M", "", Módulo))), Módulo, Variedad)
  })
  
  output$ui_modal_frecuencias_contenido <- renderUI({
    combos <- combinaciones_frecuencias_activas()
    req(nrow(combos) > 0)
    df_frec <- rv_frecuencias()
    if (is.null(df_frec) || nrow(df_frec) == 0) {
      df_frec <- cargar_frecuencias_disco(combos)
    }
    
    es_auto <- identical(rv_modo_frecuencia(), "automatico")
    
    cards_list <- lapply(seq_len(nrow(combos)), function(i) {
      m <- combos$Módulo[i]
      v <- combos$Variedad[i]
      id_in <- id_input_frec_mod(m, v)
      
      idx_m <- which(df_frec$Módulo == m & df_frec$Variedad == v)
      val_actual <- if (length(idx_m) > 0 && !is.na(df_frec$Frecuencia_Dias[idx_m[1]])) {
        as.integer(df_frec$Frecuencia_Dias[idx_m[1]])
      } else if (v == "ROCIO") {
        3L
      } else {
        7L
      }
      
      if (!is.null(input[[id_in]])) {
        val_in <- suppressWarnings(as.integer(input[[id_in]]))
        if (!is.na(val_in) && val_in >= 1L && val_in <= 15L) {
          val_actual <- val_in
        }
      }
      
      es_2da   <- (val_actual <= 4L)
      col_m    <- if (v == "ROCIO") "#007AFF" else "#5856D6"
      bg_badge <- if (v == "ROCIO") "rgba(0, 122, 255, 0.10)" else "rgba(88, 86, 214, 0.10)"
      
      badge_tx <- if (es_2da) "2ª Pasada (≤ 4d)" else "Ciclo 1 Pasada"
      badge_cl <- if (es_2da) "#007AFF" else "#636366"
      badge_bg <- if (es_2da) "rgba(0, 122, 255, 0.10)" else "rgba(142, 142, 147, 0.12)"
      
      card_style <- paste0(
        "background: #FFFFFF; border: 1.5px solid ",
        if (es_auto) "rgba(0,0,0,0.06)" else if (es_2da) "rgba(0, 122, 255, 0.30)" else "rgba(0,0,0,0.10)",
        "; border-radius: 12px; padding: 10px 14px; box-shadow: 0 1px 3px rgba(0,0,0,0.02); display: flex; align-items: center; justify-content: space-between; gap: 10px;",
        if (es_auto) " opacity: 0.60; pointer-events: none;" else ""
      )
      
      div(
        class = "frec-modvar-card",
        style = card_style,
        div(
          style = "display: flex; flex-direction: column; gap: 3px; min-width: 0;",
          div(
            style = "display: flex; align-items: center; gap: 6px; flex-wrap: wrap;",
            tags$span(style = sprintf("font-size: 11px; font-weight: 700; color: %s; background: %s; padding: 2px 7px; border-radius: 6px;", col_m, bg_badge), paste("Módulo", m)),
            tags$span(style = "font-size: 12.5px; font-weight: 700; color: #1C1C1E; white-space: nowrap; overflow: hidden; text-overflow: ellipsis;", v)
          ),
          tags$span(style = sprintf("font-size: 10px; font-weight: 600; color: %s; background: %s; padding: 1px 6px; border-radius: 6px; width: fit-content;", badge_cl, badge_bg), badge_tx)
        ),
        div(
          style = "width: 78px; flex-shrink: 0;",
          selectInput(
            inputId   = id_in,
            label     = NULL,
            choices   = setNames(1:15, paste0(1:15, " d")),
            selected  = as.character(val_actual),
            selectize = FALSE,
            width     = "78px"
          )
        )
      )
    })
    
    tagList(
      # Switch / Selector de modo Automático vs Manual
      div(
        style = "background: #F2F2F7; border: 1px solid rgba(0,0,0,0.06); border-radius: 12px; padding: 12px 16px; margin-bottom: 12px; display: flex; align-items: center; justify-content: space-between; flex-wrap: wrap; gap: 12px;",
        div(
          style = "display: flex; align-items: center; gap: 10px;",
          div(
            style = sprintf("width: 34px; height: 34px; border-radius: 9px; background: %s; display: flex; align-items: center; justify-content: center; font-size: 16px; color: #FFFFFF;",
                            if (es_auto) "#007AFF" else "#34C759"),
            icon(if (es_auto) "bolt" else "sliders")
          ),
          div(
            tags$div(style = "font-size: 13px; font-weight: 700; color: #1C1C1E;", 
                     paste("Modo de Frecuencia:", if (es_auto) "Automático (Por Válvula)" else "Manual (Por Módulo y Variedad)")),
            tags$div(
              style = "font-size: 11px; color: #636366; margin-top: 2px;",
              if (es_auto) "Calcula automáticamente el intervalo real de cada válvula (S-2 vs S-1). Botones e inputs deshabilitados."
              else "Usa estrictamente las frecuencias configuradas por Módulo y Variedad en este panel."
            )
          )
        ),
        div(
          class = "modo-frec-segmented",
          actionButton(
            "btn_switch_modo_auto",
            label = tagList(icon("bolt"), "Automático"),
            class = paste0("btn-modo-frec-pill", if (es_auto) " active-auto" else "")
          ),
          actionButton(
            "btn_switch_modo_manual",
            label = tagList(icon("sliders"), "Manual"),
            class = paste0("btn-modo-frec-pill", if (!es_auto) " active-manual" else "")
          )
        )
      ),
      
      # Banner contextual
      if (es_auto) {
        div(
          style = "background: rgba(0, 122, 255, 0.06); border: 1px solid rgba(0, 122, 255, 0.20); border-radius: 10px; padding: 9px 14px; margin-bottom: 12px; font-size: 11.5px; color: #004085; display: flex; align-items: center; gap: 8px;",
          icon("info-circle"),
          tags$span("En modo ", tags$b("Automático"), ", el motor de ajuste calcula en tiempo real la frecuencia de cada válvula. Para fijar valores personalizados o cargar del histórico, cambia el switch a ", tags$b("Manual"), ".")
        )
      } else {
        div(
          style = "background: rgba(52, 199, 89, 0.08); border: 1px solid rgba(52, 199, 89, 0.25); border-radius: 10px; padding: 9px 14px; margin-bottom: 12px; font-size: 11.5px; color: #1B5E20; display: flex; align-items: center; justify-content: space-between; flex-wrap: wrap; gap: 8px;",
          div(
            style = "display: flex; align-items: center; gap: 8px;",
            icon("check-circle"),
            tags$span("En modo ", tags$b("Manual"), ", el motor aplicará exactamente los días configurados. Puedes usar el botón inferior para ", tags$b("Actualizar en base al histórico"), ".")
          )
        )
      },
      
      # Rejilla de tarjetas por Módulo y Variedad
      div(
        style = "display: grid; grid-template-columns: repeat(auto-fill, minmax(225px, 1fr)); gap: 10px; max-height: 480px; overflow-y: auto; padding-right: 4px;",
        cards_list
      )
    )
  })
  
  output$ui_modal_frecuencias_footer <- renderUI({
    es_auto <- identical(rv_modo_frecuencia(), "automatico")
    
    div(
      style = "display: flex; align-items: center; justify-content: space-between; width: 100%; flex-wrap: wrap; gap: 8px;",
      div(
        style = "display: flex; align-items: center; gap: 8px; flex-wrap: wrap;",
        actionButton(
          "btn_actualizar_frec_hist",
          label = tagList(icon("sync-alt"), "Actualizar frecuencias en base al histórico"),
          title = if (es_auto) "Deshabilitado en modo Automático (cambia el switch a Manual)" else "Calcular mediana real observada en las últimas semanas para cada Módulo y Variedad",
          disabled = if (es_auto) "disabled" else NULL,
          style = if (es_auto) {
            "font-size: 12px; font-weight: 600; padding: 6px 14px; border-radius: 8px; background: #E5E5EA; color: #8E8E93; border: 1px solid rgba(0,0,0,0.06); cursor: not-allowed; opacity: 0.5;"
          } else {
            "font-size: 12px; font-weight: 600; padding: 6px 14px; border-radius: 8px; background: rgba(0, 122, 255, 0.10); color: #007AFF; border: 1px solid rgba(0, 122, 255, 0.30); cursor: pointer;"
          }
        ),
        actionButton(
          "btn_reset_frecuencias",
          label = tagList(icon("rotate-left"), "Restablecer (Rocío 3d / Resto 7d)"),
          disabled = if (es_auto) "disabled" else NULL,
          style = if (es_auto) {
            "font-size: 12px; font-weight: 600; padding: 6px 12px; border-radius: 8px; background: #F2F2F7; color: #AEAEB2; border: 1px solid rgba(0,0,0,0.05); cursor: not-allowed; opacity: 0.5;"
          } else {
            "font-size: 12px; font-weight: 600; padding: 6px 12px; border-radius: 8px; background: #F2F2F7; color: #3A3A3C; border: 1px solid rgba(0,0,0,0.08); cursor: pointer;"
          }
        )
      ),
      div(
        style = "display: flex; align-items: center; gap: 8px;",
        modalButton("Cerrar"),
        actionButton(
          "btn_guardar_frecuencias",
          label = tagList(icon("check"), "Guardar Frecuencias"),
          disabled = if (es_auto) "disabled" else NULL,
          style = if (es_auto) {
            "font-size: 12px; font-weight: 600; padding: 6px 16px; border-radius: 8px; background: #E5E5EA; color: #8E8E93; border: none; cursor: not-allowed; opacity: 0.5;"
          } else {
            "font-size: 12px; font-weight: 600; padding: 6px 16px; border-radius: 8px; background: #34C759; color: #FFFFFF; border: none; box-shadow: 0 2px 6px rgba(52, 199, 89, 0.28); cursor: pointer;"
          }
        )
      )
    )
  })
  
  observeEvent(input$btn_abrir_modal_frecuencias, {
    showModal(
      modalDialog(
        title = div(
          style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
          div(
            style = "display: flex; align-items: center; gap: 10px;",
            div(
              style = "width: 36px; height: 36px; border-radius: 10px; background: rgba(0, 122, 255, 0.10); color: #007AFF; display: flex; align-items: center; justify-content: center; font-size: 16px;",
              icon("sliders")
            ),
            div(
              tags$div(style = "font-size: 15px; font-weight: 700; color: #1C1C1E; line-height: 1.2;", "Frecuencias de Cosecha por Módulo y Variedad"),
              tags$div(style = "font-size: 11.5px; font-weight: 400; color: #8E8E93; margin-top: 2px;", "Configuración del motor para la distribución de días en Ajustar Semana")
            )
          )
        ),
        size = "l",
        easyClose = TRUE,
        uiOutput("ui_modal_frecuencias_contenido"),
        footer = uiOutput("ui_modal_frecuencias_footer")
      )
    )
  })
  
  observeEvent(input$btn_switch_modo_auto, {
    rv_modo_frecuencia("automatico")
    guardar_modo_frecuencia_disco("automatico")
  })
  
  observeEvent(input$btn_switch_modo_manual, {
    rv_modo_frecuencia("manual")
    guardar_modo_frecuencia_disco("manual")
  })
  
  observeEvent(input$btn_actualizar_frec_hist, {
    if (identical(rv_modo_frecuencia(), "automatico")) return()
    combos <- combinaciones_frecuencias_activas()
    req(nrow(combos) > 0)
    
    withProgress(message = "Analizando histórico de cosecha...", detail = "Calculando medianas por Módulo y Variedad", value = 0.4, {
      df_sug <- tryCatch({
        calcular_frecuencias_historicas_mv(
          df_w_data        = if (exists("df_hist_oficial_wide")) df_hist_oficial_wide else NULL,
          df_c_data        = rv_cosecha(),
          combos_mv_target = combos,
          camp_sel         = input$filtro_campana
        )
      }, error = function(e) {
        cat("[ERROR FREC HISTORICO]", e$message, "\n")
        NULL
      })
      
      if (!is.null(df_sug) && nrow(df_sug) > 0) {
        for (i in seq_len(nrow(df_sug))) {
          m <- df_sug$Módulo[i]
          v <- df_sug$Variedad[i]
          f_val <- df_sug$Frecuencia_Dias[i]
          id_in <- id_input_frec_mod(m, v)
          updateSelectInput(session, id_in, selected = as.character(f_val))
        }
        showNotification("🔄 Frecuencias autocompletadas en base al histórico por Módulo y Variedad. Revisa y pulsa 'Guardar Frecuencias'.", type = "message", duration = 4)
      } else {
        showNotification("No se pudieron extraer datos históricos suficientes para calcular frecuencias.", type = "warning", duration = 4)
      }
    })
  })
  
  observeEvent(input$btn_guardar_frecuencias, {
    if (identical(rv_modo_frecuencia(), "automatico")) return()
    combos <- combinaciones_frecuencias_activas()
    req(nrow(combos) > 0)
    df_frec_prev <- rv_frecuencias()
    
    df_nuevas <- combos %>%
      mutate(
        Frecuencia_Dias = vapply(seq_len(n()), function(i) {
          m <- Módulo[i]
          v <- Variedad[i]
          id_in <- id_input_frec_mod(m, v)
          val_ui <- input[[id_in]]
          val_int <- suppressWarnings(as.integer(val_ui))
          if (!is.null(val_ui) && length(val_int) == 1 && !is.na(val_int) && val_int >= 1L && val_int <= 15L) {
            return(val_int)
          }
          idx_p <- which(df_frec_prev$Módulo == m & df_frec_prev$Variedad == v)
          if (length(idx_p) > 0 && !is.na(df_frec_prev$Frecuencia_Dias[idx_p[1]])) {
            return(as.integer(df_frec_prev$Frecuencia_Dias[idx_p[1]]))
          }
          if (v == "ROCIO") 3L else 7L
        }, integer(1))
      )
    
    rv_frecuencias(df_nuevas)
    guardar_frecuencias_disco(df_nuevas)
    rv_modo_frecuencia("manual")
    guardar_modo_frecuencia_disco("manual")
    removeModal()
    showNotification("⚙️ Frecuencias guardadas en Modo Manual por Módulo y Variedad.", type = "message", duration = 3)
  })
  
  observeEvent(input$btn_reset_frecuencias, {
    if (identical(rv_modo_frecuencia(), "automatico")) return()
    combos <- combinaciones_frecuencias_activas()
    req(nrow(combos) > 0)
    for (i in seq_len(nrow(combos))) {
      m <- combos$Módulo[i]
      v <- combos$Variedad[i]
      val_def <- if (v == "ROCIO") 3L else 7L
      id_in <- id_input_frec_mod(m, v)
      updateSelectInput(session, id_in, selected = as.character(val_def))
    }
    showNotification("Valores restablecidos a defecto (Rocío = 3d, resto = 7d). Pulsa 'Guardar Frecuencias' para confirmar.", type = "message", duration = 3)
  })
  
  # ============================================================================== =
  # MODAL DE CONFIGURACIÓN DE % 2ª PASADA DE ROCÍO POR MÓDULO (LUNES Y MARTES)
  # ============================================================================== =
  rv_pct_rocio <- reactiveVal(cargar_pct_rocio_disco())
  
  modulos_rocio_activos <- reactive({
    u <- universo_base()
    mods_u <- character(0)
    if (!is.null(u) && nrow(u) > 0 && "Variedad" %in% names(u) && "Módulo" %in% names(u)) {
      mods_u <- u %>% filter(limpiar_variedad(Variedad) == "ROCIO") %>% pull(Módulo) %>% unique()
    }
    if (length(mods_u) == 0 && exists("df_valvulas") && is.data.frame(df_valvulas)) {
      mods_u <- df_valvulas %>% filter(limpiar_variedad(Variedad) == "ROCIO") %>% pull(Módulo) %>% unique()
    }
    if (length(mods_u) == 0 && exists("df_cosecha") && is.data.frame(df_cosecha)) {
      mods_u <- df_cosecha %>% filter(limpiar_variedad(Variedad) == "ROCIO") %>% pull(Módulo) %>% unique()
    }
    mods_u <- trimws(as.character(mods_u))
    mods_u <- sort(unique(mods_u[!is.na(mods_u) & nzchar(mods_u)]))
    if (length(mods_u) == 0) mods_u <- c("8", "9", "10", "11", "12", "13", "14")
    mods_u[order(suppressWarnings(as.numeric(mods_u)), mods_u)]
  })
  
  observeEvent(input$btn_abrir_modal_pct_rocio, {
    mods_mostrar <- modulos_rocio_activos()
    req(length(mods_mostrar) > 0)
    df_pct <- rv_pct_rocio()
    
    cards_list <- lapply(mods_mostrar, function(m) {
      idx_m <- match(m, df_pct$Módulo)
      val_lun <- if (!is.na(idx_m) && !is.na(df_pct$Pct_Pasada_Lun[idx_m])) df_pct$Pct_Pasada_Lun[idx_m] else 60
      val_mar <- if (!is.na(idx_m) && !is.na(df_pct$Pct_Pasada_Mar[idx_m])) df_pct$Pct_Pasada_Mar[idx_m] else 60
      
      div(
        class = "frec-var-card",
        style = "background: #FFFFFF; border: 1.5px solid rgba(0, 122, 255, 0.22); border-radius: 12px; padding: 10px 14px; box-shadow: 0 1px 4px rgba(0,0,0,0.03); display: flex; align-items: center; justify-content: space-between; gap: 12px;",
        div(
          style = "display: flex; flex-direction: column; gap: 2px;",
          tags$span(style = "font-size: 13.5px; font-weight: 700; color: #1C1C1E;", paste("Módulo", m)),
          tags$span(style = "font-size: 10.5px; font-weight: 600; color: #007AFF; background: rgba(0, 122, 255, 0.12); padding: 1px 7px; border-radius: 6px; width: fit-content;", "Variedad ROCÍO")
        ),
        div(
          style = "display: flex; align-items: center; gap: 10px;",
          div(
            style = "width: 86px;",
            tags$label(style = "font-size: 10.5px; font-weight: 600; color: #0056B3; margin-bottom: 2px; display: block;", "% Pasada Lun:"),
            numericInput(
              inputId = id_input_pct_rocio_lun(m),
              label   = NULL,
              value   = as.numeric(val_lun),
              min     = 0,
              max     = 150,
              step    = 5,
              width   = "86px"
            )
          ),
          div(
            style = "width: 86px;",
            tags$label(style = "font-size: 10.5px; font-weight: 600; color: #0056B3; margin-bottom: 2px; display: block;", "% Pasada Mar:"),
            numericInput(
              inputId = id_input_pct_rocio_mar(m),
              label   = NULL,
              value   = as.numeric(val_mar),
              min     = 0,
              max     = 150,
              step    = 5,
              width   = "86px"
            )
          )
        )
      )
    })
    
    showModal(
      modalDialog(
        title = div(
          style = "display: flex; align-items: center; gap: 10px;",
          div(
            style = "width: 34px; height: 34px; border-radius: 9px; background: rgba(0, 122, 255, 0.10); color: #007AFF; display: flex; align-items: center; justify-content: center; font-size: 16px;",
            icon("sliders")
          ),
          div(
            tags$div(style = "font-size: 15px; font-weight: 700; color: #1C1C1E; line-height: 1.2;", "Porcentaje de 2ª Pasada de Rocío por Módulo"),
            tags$div(style = "font-size: 11.5px; font-weight: 400; color: #8E8E93; margin-top: 2px;", "Ajusta independientemente el % de rendimiento de 2ª pasada (Lunes y Martes) para cada módulo")
          )
        ),
        size = "l",
        easyClose = TRUE,
        div(
          style = "padding: 4px 2px;",
          div(
            style = "background: #F2F2F7; border-radius: 10px; padding: 10px 14px; margin-bottom: 12px; display: flex; flex-wrap: wrap; gap: 12px; align-items: center; font-size: 11.5px; color: #3A3A3C;",
            span(HTML("<b style='color:#007AFF;'>● Lunes → Jueves:</b> Las Ha físicas del Lunes se cosechan al 100%, calculando los kilos con: Rendimiento × (% Pasada Lun / 100).")),
            span(HTML("<b style='color:#34C759;'>● Martes → Viernes:</b> Las Ha físicas del Martes se cosechan al 100%, calculando los kilos con: Rendimiento × (% Pasada Mar / 100)."))
          ),
          div(
            style = "background: rgba(0, 122, 255, 0.05); border: 1px solid rgba(0, 122, 255, 0.18); border-radius: 10px; padding: 8px 14px; margin-bottom: 14px; display: flex; align-items: center; justify-content: space-between; flex-wrap: wrap; gap: 10px;",
            div(
              tags$span(style = "font-size: 12px; font-weight: 700; color: #0056B3;", "Rellenar todos los módulos con el mismo valor:"),
              tags$span(style = "font-size: 11px; color: #636366; display: block;", "Ingresa el porcentaje deseado y haz clic en 'Aplicar a todos'")
            ),
            div(
              style = "display: flex; align-items: center; gap: 8px;",
              tags$div(style = "width: 80px;",
                       numericInput("input_pct_rocio_todos", label = NULL, value = 60, min = 0, max = 150, step = 5, width = "80px")
              ),
              actionButton(
                "btn_aplicar_pct_rocio_todos",
                label = tagList(icon("check-double"), "Aplicar a todos"),
                style = "font-size: 11.5px; font-weight: 600; padding: 6px 12px; border-radius: 8px; background: #007AFF; color: #FFFFFF; border: none;"
              )
            )
          ),
          div(
            style = "display: grid; grid-template-columns: repeat(auto-fill, minmax(290px, 1fr)); gap: 10px;",
            cards_list
          )
        ),
        footer = div(
          style = "display: flex; align-items: center; justify-content: space-between; width: 100%; flex-wrap: wrap; gap: 8px;",
          actionButton(
            "btn_reset_pct_rocio",
            label = tagList(icon("rotate-left"), "Restablecer todos al 60%"),
            style = "font-size: 12px; font-weight: 600; padding: 6px 12px; border-radius: 8px; background: #F2F2F7; color: #3A3A3C; border: 1px solid rgba(0,0,0,0.08);"
          ),
          div(
            style = "display: flex; align-items: center; gap: 8px;",
            modalButton("Cancelar"),
            actionButton(
              "btn_guardar_pct_rocio",
              label = tagList(icon("check"), "Guardar Porcentajes"),
              style = "font-size: 12px; font-weight: 600; padding: 6px 16px; border-radius: 8px; background: #007AFF; color: #FFFFFF; border: none; box-shadow: 0 2px 6px rgba(0, 122, 255, 0.25);"
            )
          )
        )
      )
    )
  })
  
  observeEvent(input$btn_aplicar_pct_rocio_todos, {
    val_todos <- suppressWarnings(as.numeric(input$input_pct_rocio_todos))
    if (is.na(val_todos) || length(val_todos) == 0 || val_todos < 0) val_todos <- 60
    mods_mostrar <- modulos_rocio_activos()
    for (m in mods_mostrar) {
      updateNumericInput(session, id_input_pct_rocio_lun(m), value = val_todos)
      updateNumericInput(session, id_input_pct_rocio_mar(m), value = val_todos)
    }
    showNotification(sprintf("Se aplicó %s%% a Lunes y Martes en todos los módulos.", val_todos), type = "message", duration = 3)
  })
  
  observeEvent(input$btn_guardar_pct_rocio, {
    mods_mostrar <- modulos_rocio_activos()
    df_pct_prev <- rv_pct_rocio()
    nuevos_pct <- tibble(Módulo = mods_mostrar) %>%
      mutate(
        Pct_Pasada_Lun = vapply(Módulo, function(m) {
          v <- suppressWarnings(as.numeric(input[[id_input_pct_rocio_lun(m)]]))
          if (!is.null(v) && length(v) == 1 && !is.na(v) && v >= 0) return(v)
          idx_p <- match(m, df_pct_prev$Módulo)
          if (!is.na(idx_p) && !is.na(df_pct_prev$Pct_Pasada_Lun[idx_p])) return(df_pct_prev$Pct_Pasada_Lun[idx_p])
          60
        }, numeric(1)),
        Pct_Pasada_Mar = vapply(Módulo, function(m) {
          v <- suppressWarnings(as.numeric(input[[id_input_pct_rocio_mar(m)]]))
          if (!is.null(v) && length(v) == 1 && !is.na(v) && v >= 0) return(v)
          idx_p <- match(m, df_pct_prev$Módulo)
          if (!is.na(idx_p) && !is.na(df_pct_prev$Pct_Pasada_Mar[idx_p])) return(df_pct_prev$Pct_Pasada_Mar[idx_p])
          60
        }, numeric(1))
      )
    rv_pct_rocio(nuevos_pct)
    guardar_pct_rocio_disco(nuevos_pct)
    removeModal()
    showNotification("⚙️ Porcentajes de 2ª pasada de Rocío guardados correctamente por módulo.", type = "message", duration = 3)
  })
  
  observeEvent(input$btn_reset_pct_rocio, {
    mods_mostrar <- modulos_rocio_activos()
    for (m in mods_mostrar) {
      updateNumericInput(session, id_input_pct_rocio_lun(m), value = 60)
      updateNumericInput(session, id_input_pct_rocio_mar(m), value = 60)
    }
    df_reset <- tibble(Módulo = mods_mostrar, Pct_Pasada_Lun = 60, Pct_Pasada_Mar = 60)
    rv_pct_rocio(df_reset)
    guardar_pct_rocio_disco(df_reset)
    showNotification("Todos los módulos de Rocío restablecidos a 60% para Lunes y Martes.", type = "message", duration = 3)
  })
  
  # ============================================================================== =
  # MODAL DE CONFIGURACIÓN DE DÍAS DE MADURACIÓN POR MÓDULO Y VARIEDAD
  # ============================================================================== =
  rv_maduracion <- reactiveVal(cargar_maduracion_disco())
  rv_sensibilidad_floral <- reactiveVal(cargar_sensibilidad_floral())
  
  combinaciones_maduracion_activas <- reactive({
    u <- universo_base()
    combos <- if (!is.null(u) && nrow(u) > 0 && all(c("Módulo", "Variedad") %in% names(u))) {
      u %>%
        filter(!is.na(Módulo), nzchar(trimws(as.character(Módulo))),
               !is.na(Variedad), nzchar(trimws(as.character(Variedad)))) %>%
        distinct(Módulo = trimws(as.character(Módulo)), Variedad = trimws(as.character(Variedad)))
    } else {
      tibble(Módulo = character(), Variedad = character())
    }
    
    if (nrow(combos) == 0 && exists("df_valvulas") && is.data.frame(df_valvulas) && all(c("Módulo", "Variedad") %in% names(df_valvulas))) {
      combos <- df_valvulas %>%
        filter(!is.na(Módulo), nzchar(trimws(as.character(Módulo))),
               !is.na(Variedad), nzchar(trimws(as.character(Variedad)))) %>%
        distinct(Módulo = trimws(as.character(Módulo)), Variedad = trimws(as.character(Variedad)))
    }
    if (nrow(combos) == 0 && exists("df_cosecha") && is.data.frame(df_cosecha) && all(c("Módulo", "Variedad") %in% names(df_cosecha))) {
      combos <- df_cosecha %>%
        filter(!is.na(Módulo), nzchar(trimws(as.character(Módulo))),
               !is.na(Variedad), nzchar(trimws(as.character(Variedad)))) %>%
        distinct(Módulo = trimws(as.character(Módulo)), Variedad = trimws(as.character(Variedad)))
    }
    
    combos %>%
      arrange(suppressWarnings(as.numeric(Módulo)), Módulo, Variedad)
  })
  
  observeEvent(input$btn_abrir_modal_maduracion, {
    combos <- combinaciones_maduracion_activas()
    req(nrow(combos) > 0)
    df_mad <- rv_maduracion()
    if (is.null(df_mad) || nrow(df_mad) == 0) {
      df_mad <- cargar_maduracion_disco(combos)
    }
    
    cards_list <- lapply(seq_len(nrow(combos)), function(i) {
      m <- combos$Módulo[i]
      v <- combos$Variedad[i]
      id_in <- id_input_maduracion(m, v)
      
      idx_m <- which(df_mad$Módulo == m & df_mad$Variedad == v)
      val_actual <- if (length(idx_m) > 0 && !is.na(df_mad$Dias_Maduracion[idx_m[1]])) {
        as.numeric(df_mad$Dias_Maduracion[idx_m[1]])
      } else {
        45
      }
      
      div(
        class = "frec-var-card",
        style = "background: #FFFFFF; border: 1.5px solid rgba(175, 82, 222, 0.20); border-radius: 12px; padding: 10px 14px; box-shadow: 0 1px 4px rgba(0,0,0,0.03); display: flex; align-items: center; justify-content: space-between; gap: 12px;",
        div(
          style = "display: flex; flex-direction: column; gap: 3px; min-width: 0;",
          div(style = "display: flex; align-items: center; gap: 6px; flex-wrap: wrap;",
              tags$span(style = "font-size: 11px; font-weight: 700; color: #AF52DE; background: rgba(175, 82, 222, 0.12); padding: 2px 7px; border-radius: 6px;", paste("Módulo", m)),
              tags$span(style = "font-size: 13px; font-weight: 700; color: #1C1C1E; white-space: nowrap; overflow: hidden; text-overflow: ellipsis;", v)
          ),
          tags$span(style = "font-size: 10.5px; color: #8E8E93;", "Tiempo a cosecha")
        ),
        div(
          style = "display: flex; align-items: center; gap: 6px; flex-shrink: 0;",
          numericInput(
            inputId = id_in,
            label   = NULL,
            value   = val_actual,
            min     = 1,
            max     = 200,
            step    = 1,
            width   = "72px"
          ),
          tags$span(style = "font-size: 11.5px; font-weight: 600; color: #636366;", "días")
        )
      )
    })
    
    showModal(
      modalDialog(
        title = div(
          style = "display: flex; align-items: center; gap: 10px;",
          div(
            style = "width: 36px; height: 36px; border-radius: 10px; background: rgba(175, 82, 222, 0.12); color: #AF52DE; display: flex; align-items: center; justify-content: center; font-size: 17px;",
            icon("hourglass-half")
          ),
          div(
            tags$div(style = "font-size: 15px; font-weight: 700; color: #1C1C1E; line-height: 1.2;", "Días de Maduración por Módulo y Variedad"),
            tags$div(style = "font-size: 11.5px; font-weight: 400; color: #8E8E93; margin-top: 2px;", "Configura los días promedio desde el conteo floral hasta la cosecha para cada combinación")
          )
        ),
        size = "l",
        easyClose = TRUE,
        div(
          style = "padding: 4px 2px;",
          # Banner informativo
          div(
            style = "background: #F2F2F7; border-radius: 10px; padding: 10px 14px; margin-bottom: 12px; display: flex; flex-wrap: wrap; gap: 12px; align-items: center; font-size: 11.5px; color: #3A3A3C;",
            span(HTML("<b style='color:#AF52DE;'>● Días de Maduración:</b> Periodo estimado en días desde la fase de conteo floral (botón/flor) hasta la recolección óptima en campo.")),
            span(HTML("<b style='color:#007AFF;'>● Calibración:</b> Permite modelar el desfase cronológico entre la carga floral y los picos de cosecha."))
          ),
          # Barra de configuración de consideración de tendencia floral (Premio / Castigo)
          div(
            style = "background: rgba(0, 122, 255, 0.05); border: 1.5px solid rgba(0, 122, 255, 0.22); border-radius: 10px; padding: 10px 14px; margin-bottom: 12px; display: flex; align-items: center; justify-content: space-between; flex-wrap: wrap; gap: 12px;",
            div(
              style = "display: flex; flex-direction: column; gap: 3px; max-width: 580px;",
              div(
                style = "display: flex; align-items: center; gap: 8px;",
                tags$span(style = "font-size: 12.5px; font-weight: 700; color: #007AFF;", icon("chart-line"), " Consideración de Tendencia Floral (Premio / Castigo):"),
                tags$span(style = "font-size: 10.5px; font-weight: 600; color: #16A34A; background: rgba(22, 163, 74, 0.12); padding: 1px 7px; border-radius: 6px;", "Recomendado: 50% - 60%")
              ),
              tags$span(
                style = "font-size: 11px; color: #636366; line-height: 1.3;",
                "Porcentaje de la pendiente floral (entre la semana origen y la siguiente) transferido como premio o castigo al rendimiento. Amortigua la fluctuación natural de los conteos."
              )
            ),
            div(
              style = "display: flex; align-items: center; gap: 6px; flex-shrink: 0;",
              tags$div(style = "width: 80px;",
                       numericInput("input_sensibilidad_floral", label = NULL, value = rv_sensibilidad_floral(), min = 0, max = 100, step = 5, width = "80px")
              ),
              tags$span(style = "font-size: 12.5px; font-weight: 700; color: #1C1C1E;", "%")
            )
          ),
          # Barra de relleno masivo
          div(
            style = "background: rgba(175, 82, 222, 0.05); border: 1px solid rgba(175, 82, 222, 0.20); border-radius: 10px; padding: 8px 14px; margin-bottom: 14px; display: flex; align-items: center; justify-content: space-between; flex-wrap: wrap; gap: 10px;",
            div(
              tags$span(style = "font-size: 12px; font-weight: 700; color: #7B1FA2;", "Rellenar todas las combinaciones con el mismo valor:"),
              tags$span(style = "font-size: 11px; color: #636366; display: block;", "Ingresa el número de días y haz clic en 'Aplicar a todos'")
            ),
            div(
              style = "display: flex; align-items: center; gap: 8px;",
              tags$div(style = "width: 80px;",
                       numericInput("input_maduracion_todos", label = NULL, value = 45, min = 1, max = 200, step = 1, width = "80px")
              ),
              actionButton(
                "btn_aplicar_maduracion_todos",
                label = tagList(icon("check-double"), "Aplicar a todos"),
                style = "font-size: 11.5px; font-weight: 600; padding: 6px 12px; border-radius: 8px; background: #AF52DE; color: #FFFFFF; border: none; cursor: pointer;"
              )
            )
          ),
          # Rejilla responsive de tarjetas
          div(
            style = "display: grid; grid-template-columns: repeat(auto-fill, minmax(270px, 1fr)); gap: 10px; max-height: 520px; overflow-y: auto; padding-right: 4px;",
            cards_list
          )
        ),
        footer = div(
          style = "display: flex; align-items: center; justify-content: space-between; width: 100%; flex-wrap: wrap; gap: 8px;",
          div(
            style = "display: flex; align-items: center; gap: 8px; flex-wrap: wrap;",
            actionButton(
              "btn_recalcular_maduracion",
              label = tagList(icon("sync-alt"), "Actualizar desde Curvas"),
              title = "Recalcular días a partir de los datos de Curvas de Maduración de campo",
              style = "font-size: 12px; font-weight: 600; padding: 6px 14px; border-radius: 8px; background: rgba(0, 122, 255, 0.10); color: #007AFF; border: 1px solid rgba(0, 122, 255, 0.30); cursor: pointer;"
            ),
            actionButton(
              "btn_reset_maduracion",
              label = tagList(icon("rotate-left"), "Restablecer a 45 días"),
              style = "font-size: 12px; font-weight: 600; padding: 6px 12px; border-radius: 8px; background: #F2F2F7; color: #3A3A3C; border: 1px solid rgba(0,0,0,0.08);"
            )
          ),
          div(
            style = "display: flex; align-items: center; gap: 8px;",
            modalButton("Cancelar"),
            actionButton(
              "btn_guardar_maduracion",
              label = tagList(icon("check"), "Guardar Días de Maduración"),
              style = "font-size: 12px; font-weight: 600; padding: 6px 16px; border-radius: 8px; background: #AF52DE; color: #FFFFFF; border: none; box-shadow: 0 2px 6px rgba(175, 82, 222, 0.28);"
            )
          )
        )
      )
    )
  })
  
  observeEvent(input$btn_recalcular_maduracion, {
    camp_sel <- if (!is.null(input$filtro_campana)) input$filtro_campana else 2026
    df_calc <- calcular_dias_maduracion_curvas(campana_target = camp_sel)
    
    if (is.null(df_calc) || nrow(df_calc) == 0) {
      showNotification("No se encontraron curvas de maduración para recalcular.", type = "warning", duration = 4)
      return()
    }
    
    combos <- combinaciones_maduracion_activas()
    actualizados <- 0
    for (i in seq_len(nrow(combos))) {
      m <- combos$Módulo[i]
      v <- combos$Variedad[i]
      idx_c <- which(df_calc$Módulo == m & df_calc$Variedad == v)
      if (length(idx_c) > 0) {
        val_nuevo <- as.integer(df_calc$Dias_Maduracion[idx_c[1]])
        updateNumericInput(session, id_input_maduracion(m, v), value = val_nuevo)
        actualizados <- actualizados + 1
      }
    }
    
    showNotification(
      sprintf("✔ Días actualizados desde curvas de campo (%d combinaciones calculadas para Campaña %s).", actualizados, camp_sel),
      type = "message",
      duration = 4
    )
  })
  
  observeEvent(input$btn_aplicar_maduracion_todos, {
    val_todos <- suppressWarnings(as.integer(input$input_maduracion_todos))
    if (is.na(val_todos) || length(val_todos) == 0 || val_todos <= 0) val_todos <- 45L
    combos <- combinaciones_maduracion_activas()
    for (i in seq_len(nrow(combos))) {
      updateNumericInput(session, id_input_maduracion(combos$Módulo[i], combos$Variedad[i]), value = val_todos)
    }
    showNotification(sprintf("Se aplicó %d días de maduración a todas las combinaciones.", val_todos), type = "message", duration = 3)
  })
  
  observeEvent(input$btn_guardar_maduracion, {
    combos <- combinaciones_maduracion_activas()
    df_mad_prev <- rv_maduracion()
    nuevos_dias <- combos %>%
      mutate(
        Dias_Maduracion = vapply(seq_len(nrow(combos)), function(i) {
          m <- combos$Módulo[i]
          v <- combos$Variedad[i]
          val <- suppressWarnings(as.integer(input[[id_input_maduracion(m, v)]]))
          if (!is.null(val) && length(val) == 1 && !is.na(val) && val >= 0) return(val)
          idx_p <- which(df_mad_prev$Módulo == m & df_mad_prev$Variedad == v)
          if (length(idx_p) > 0 && !is.na(df_mad_prev$Dias_Maduracion[idx_p[1]])) return(df_mad_prev$Dias_Maduracion[idx_p[1]])
          45L
        }, integer(1))
      )
    
    rv_maduracion(nuevos_dias)
    guardar_maduracion_disco(nuevos_dias)
    
    # Guardar porcentaje de consideración de tendencia floral
    sens_val <- suppressWarnings(as.numeric(input$input_sensibilidad_floral))
    if (!is.na(sens_val) && length(sens_val) == 1 && sens_val >= 0 && sens_val <= 100) {
      rv_sensibilidad_floral(sens_val)
      guardar_sensibilidad_floral(sens_val)
    }
    
    removeModal()
    showNotification(
      sprintf("✔ Configuración guardada: Días de maduración y %d%% de consideración floral.", as.integer(rv_sensibilidad_floral())),
      type = "message",
      duration = 4
    )
  })
  
  observeEvent(input$btn_reset_maduracion, {
    combos <- combinaciones_maduracion_activas()
    for (i in seq_len(nrow(combos))) {
      updateNumericInput(session, id_input_maduracion(combos$Módulo[i], combos$Variedad[i]), value = 45L)
    }
    updateNumericInput(session, "input_sensibilidad_floral", value = 50)
    df_reset <- combos %>% mutate(Dias_Maduracion = 45L)
    rv_maduracion(df_reset)
    guardar_maduracion_disco(df_reset)
    rv_sensibilidad_floral(50)
    guardar_sensibilidad_floral(50)
    showNotification("Días restablecidos a 45 días y consideración floral restablecida a 50%.", type = "message", duration = 3)
  })
  
  # Sincronizar dinámicamente el selector 'Hasta' para que no sea anterior a 'Desde'
  observeEvent(input$ajustar_desde_dia, {
    dias_nombres <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
    d_ini <- input$ajustar_desde_dia
    idx_ini <- match(d_ini, dias_nombres)
    if (!is.na(idx_ini)) {
      choices_hasta <- dias_nombres[idx_ini:6]
      cur_hasta <- input$ajustar_hasta_dia
      sel_hasta <- if (!is.null(cur_hasta) && cur_hasta %in% choices_hasta) cur_hasta else tail(choices_hasta, 1)
      updateSelectInput(session, "ajustar_hasta_dia", choices = choices_hasta, selected = sel_hasta)
    }
  })
  
  # ============================================================================== =
  # MOTOR DE AJUSTE AUTOMÁTICO (COMBINATORIA RENDIMIENTO PONDERADO + FRECUENCIAS)
  # ============================================================================== =
  ejecutar_ajuste_semana <- function(solo_filtrado = FALSE) {
    permitir_cualquier_sem <- is.null(input$switch_ajuste_cualquier_semana) || isTRUE(input$switch_ajuste_cualquier_semana)
    es_campana_act <- grepl("2026|^26", as.character(input$filtro_campana))
    sem_actual_num <- lubridate::isoweek(Sys.Date())
    es_semana_act  <- (as.numeric(input$filtro_semana) == sem_actual_num) || permitir_cualquier_sem
    
    if (!es_campana_act) {
      showNotification("El ajuste automático solo está habilitado para la campaña 2026.", type = "warning", duration = 4)
      return()
    }
    
    if (!es_semana_act) {
      showNotification("El ajuste automático está restringido a la semana actual en curso. Activa 'Ajustar cualquier semana' en el menú (⋮) para habilitarlo.", type = "warning", duration = 5)
      return()
    }
    
    sem_sel <- as.numeric(input$filtro_semana)
    sem_ant <- sem_sel - 1
    
    dias_nombres <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
    dias_cols_kg <- c("Kg_Lunes", "Kg_Martes", "Kg_Miércoles", "Kg_Jueves", "Kg_Viernes", "Kg_Sábado")
    
    dia_inicio_nom <- if (!is.null(input$ajustar_desde_dia) && input$ajustar_desde_dia %in% dias_nombres) {
      input$ajustar_desde_dia
    } else {
      w_num <- lubridate::wday(Sys.Date(), week_start = 1)
      if (w_num > 6) "Lunes" else dias_nombres[w_num]
    }
    
    dia_inicio_idx <- which(dias_nombres == dia_inicio_nom)[1]
    if (is.na(dia_inicio_idx)) dia_inicio_idx <- 1
    
    dia_fin_nom <- if (!is.null(input$ajustar_hasta_dia) && input$ajustar_hasta_dia %in% dias_nombres) {
      input$ajustar_hasta_dia
    } else {
      "Sábado"
    }
    
    dia_fin_idx <- which(dias_nombres == dia_fin_nom)[1]
    if (is.na(dia_fin_idx)) dia_fin_idx <- 6
    if (dia_fin_idx < dia_inicio_idx) {
      dia_fin_idx <- dia_inicio_idx
      dia_fin_nom <- dias_nombres[dia_fin_idx]
    }
    
    dias_ajustables_nom <- dias_nombres[dia_inicio_idx:dia_fin_idx]
    
    # Modo de frecuencia configurado (Automático vs Manual) y tabla de frecuencias
    modo_frec <- rv_modo_frecuencia()
    df_frec_mv <- rv_frecuencias()
    if (is.null(df_frec_mv) || nrow(df_frec_mv) == 0) {
      df_frec_mv <- cargar_frecuencias_disco()
    }
    
    # Cargar porcentajes de 2da pasada de Rocío configurados por módulo
    df_pct_rocio <- rv_pct_rocio()
    if (is.null(df_pct_rocio) || nrow(df_pct_rocio) == 0) {
      df_pct_rocio <- cargar_pct_rocio_disco()
    }
    
    msg_detail_frec <- if (modo_frec == "manual") {
      "Aplicando frecuencias manuales por Módulo y Variedad"
    } else {
      "Calculando frecuencias reales de 2 semanas y combinatoria de rendimientos"
    }
    
    withProgress(message = sprintf("Ajustando semana (%s a %s)...", dia_inicio_nom, dia_fin_nom), detail = msg_detail_frec, value = 0.2, {
      # ====================================================================
      # FRECUENCIAS AUTOMÁTICAS POR VÁLVULA (HISTÓRICO 2 SEMANAS: S-2 vs S-1)
      # ====================================================================
      df_frec_valvs_2sem <- tryCatch({
        calcular_frecuencias_valvula_2semanas(
          df_cosecha  = rv_cosecha(),
          campana_sel = input$filtro_campana,
          sem_actual  = sem_sel
        )
      }, error = function(e) {
        cat("[AVISO FRECUENCIAS] Error calculando frecuencias 2 semanas:", e$message, "\n")
        tibble()
      })
      
      valv_7d_map <- tibble(codigo_norm = character(), Dia_7d = character(), Dia_Rect_Ant = character(), Kg_Total_Ant = numeric(), Ha_Total_Ant = numeric())
      turno_dia_habitual <- tibble(Módulo = character(), Turno = character(), Dia_Habitual = character())
      rend_ant_turno <- tibble(Módulo = character(), Turno = character(), Variedad = character(), Kg_Ant_Turno = numeric(), Ha_Ant_Turno = numeric(), Rend_Ant_Turno = numeric(), Ultimo_Dia_Cosecha_Ant = character())
      
      if (exists("df_hist_oficial_wide") && !is.null(df_hist_oficial_wide) && nrow(df_hist_oficial_wide) > 0) {
        df_ant_valvs <- df_hist_oficial_wide %>%
          filter(Semana == sem_ant) %>%
          mutate(codigo_norm = norm_codigo_valvula(Codigo_Valv))
        
        if (nrow(df_ant_valvs) > 0) {
          cols_presentes <- intersect(dias_cols_kg, names(df_ant_valvs))
          if (length(cols_presentes) > 0) {
            mat_kg <- as.matrix(df_ant_valvs[, cols_presentes])
            max_col_idx <- max.col(mat_kg, ties.method = "first")
            has_kg <- rowSums(mat_kg, na.rm = TRUE) > 0
            nombres_cols_map <- sub("^Kg_", "", cols_presentes)
            df_ant_valvs$Dia_7d <- ifelse(has_kg, nombres_cols_map[max_col_idx], NA_character_)
            
            valv_7d_map <- df_ant_valvs %>%
              filter(!is.na(Dia_7d)) %>%
              arrange(desc(Kg_Total_Sem)) %>%
              group_by(codigo_norm) %>%
              slice(1) %>%
              ungroup() %>%
              mutate(Dia_Rect_Ant = NA_character_) %>%
              select(codigo_norm, Dia_7d, Dia_Rect_Ant, Kg_Total_Ant = Kg_Total_Sem, Ha_Total_Ant = Ha_Total_Sem)
            
            turno_dia_habitual <- df_ant_valvs %>%
              filter(!is.na(Dia_7d)) %>%
              mutate(
                Turno_num = suppressWarnings(as.numeric(Turno_Norm)),
                Módulo = case_when(
                  es_campana_act & Módulo_Norm %in% c("13.1", "13.2") ~ "13",
                  es_campana_act & Módulo_Norm %in% c("14.1", "14.2") ~ "14",
                  Módulo_Norm == "9" & between(Turno_num, 1, 11)  ~ "9.2",
                  Módulo_Norm == "9" & Turno_num >= 12            ~ "9.1",
                  Módulo_Norm == "11" & between(Turno_num, 1, 12) ~ "11.1",
                  Módulo_Norm == "11" & Turno_num >= 13           ~ "11.2",
                  TRUE                                            ~ as.character(Módulo_Norm)
                ),
                Turno = as.character(Turno_Norm)
              ) %>%
              group_by(Módulo, Turno, Dia_7d) %>%
              summarise(Total_Kg = sum(Kg_Total_Sem, na.rm = TRUE), .groups = "drop") %>%
              arrange(desc(Total_Kg)) %>%
              group_by(Módulo, Turno) %>%
              slice(1) %>%
              ungroup() %>%
              select(Módulo, Turno, Dia_Habitual = Dia_7d)
            
            rend_ant_turno <- df_ant_valvs %>%
              mutate(
                Turno_num = suppressWarnings(as.numeric(Turno_Norm)),
                Módulo = case_when(
                  es_campana_act & Módulo_Norm %in% c("13.1", "13.2") ~ "13",
                  es_campana_act & Módulo_Norm %in% c("14.1", "14.2") ~ "14",
                  Módulo_Norm == "9" & between(Turno_num, 1, 11)  ~ "9.2",
                  Módulo_Norm == "9" & Turno_num >= 12            ~ "9.1",
                  Módulo_Norm == "11" & between(Turno_num, 1, 12) ~ "11.1",
                  Módulo_Norm == "11" & Turno_num >= 13           ~ "11.2",
                  TRUE                                            ~ as.character(Módulo_Norm)
                ),
                Turno    = as.character(Turno_Norm),
                Variedad = limpiar_variedad(Variedad)
              ) %>%
              group_by(Módulo, Turno, Variedad) %>%
              summarise(
                Kg_Ant_Turno   = sum(Kg_Total_Sem, na.rm = TRUE),
                Ha_Ant_Turno   = sum(Ha_Total_Sem, na.rm = TRUE),
                Rend_Ant_Turno = ifelse(Ha_Ant_Turno > 0, Kg_Ant_Turno / Ha_Ant_Turno, 0),
                Ultimo_Dia_Cosecha_Ant = {
                  sub_mat <- mat_kg[cur_group_rows(), , drop = FALSE]
                  c_sums <- colSums(sub_mat, na.rm = TRUE)
                  d_con <- names(which(c_sums > 0))
                  if (length(d_con) > 0) sub("^Kg_", "", tail(d_con, 1)) else NA_character_
                },
                .groups = "drop"
              )
          }
        }
      }
      
      if (nrow(rend_ant_turno) == 0) {
        rend_ant_turno <- rv_cosecha() %>%
          filter(as.character(Campaña) == as.character(input$filtro_campana), Semana == sem_ant) %>%
          mutate(
            Turno_num = suppressWarnings(as.numeric(Turno)),
            Módulo = case_when(
              es_campana_act & Módulo %in% c("13.1", "13.2") ~ "13",
              es_campana_act & Módulo %in% c("14.1", "14.2") ~ "14",
              Módulo == "9" & between(Turno_num, 1, 11)  ~ "9.2",
              Módulo == "9" & Turno_num >= 12            ~ "9.1",
              Módulo == "11" & between(Turno_num, 1, 12) ~ "11.1",
              Módulo == "11" & Turno_num >= 13           ~ "11.2",
              TRUE                                       ~ trimws(as.character(Módulo))
            ),
            Turno    = trimws(as.character(Turno)),
            Variedad = limpiar_variedad(Variedad)
          ) %>%
          group_by(Módulo, Turno, Variedad) %>%
          summarise(
            Kg_Ant_Turno   = sum(`Kg Total`, na.rm = TRUE),
            Ha_Ant_Turno   = sum(coalesce(Área, 0), na.rm = TRUE),
            Rend_Ant_Turno = ifelse(Ha_Ant_Turno > 0, Kg_Ant_Turno / Ha_Ant_Turno, 0),
            Ultimo_Dia_Cosecha_Ant = NA_character_,
            .groups        = "drop"
          )
      }
      
      incProgress(0.3, detail = "Cruzando avance de válvulas y rendimientos...")
      
      rc <- tryCatch(obtener_rutas_campo_en_vivo(sem_sel), error = function(e) cargar_rutas_cosecha())
      if (is.null(rc) || nrow(rc) == 0) rc <- tryCatch(cargar_rutas_cosecha(), error = function(e) NULL)
      rc_all_disco <- tryCatch(cargar_rutas_cosecha(), error = function(e) NULL)
      rc_sem_all <- if (!is.null(rc) && "Semana" %in% names(rc)) rc %>% filter(Semana == sem_sel) else tibble()
      rc_sem <- if (nrow(rc_sem_all) > 0 && "Tipo_Registro" %in% names(rc_sem_all)) {
        rc_sem_all %>% filter(Tipo_Registro != "Rectificacion")
      } else rc_sem_all
      rc_rectif_sem <- if (nrow(rc_sem_all) > 0 && "Tipo_Registro" %in% names(rc_sem_all)) {
        rc_sem_all %>% filter(Tipo_Registro == "Rectificacion")
      } else tibble()
      
      # Si en la semana anterior (sem_ant) hubo válvulas rectificadas, la frecuencia hacia sem_sel se cuenta desde el último día de rectificación
      rc_ant_all <- tryCatch(obtener_rutas_campo_en_vivo(sem_ant), error = function(e) NULL)
      if (is.null(rc_ant_all) || nrow(rc_ant_all) == 0) {
        rc_ant_all <- if (!is.null(rc_all_disco) && "Semana" %in% names(rc_all_disco)) rc_all_disco %>% filter(Semana == sem_ant) else tibble()
      }
      if (!is.null(rc_ant_all) && nrow(rc_ant_all) > 0 && "Tipo_Registro" %in% names(rc_ant_all)) {
        rc_rect_ant <- rc_ant_all %>% filter(Tipo_Registro == "Rectificacion")
        if (nrow(rc_rect_ant) > 0 && "codigo" %in% names(rc_rect_ant)) {
          dias_r_cols <- intersect(c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado"), names(rc_rect_ant))
          if (length(dias_r_cols) > 0) {
            mat_r_ant <- as.matrix(rc_rect_ant[, dias_r_cols, drop = FALSE])
            mat_r_ant[is.na(mat_r_ant)] <- 0
            ultimo_dia_rect <- apply(mat_r_ant, 1, function(fila) {
              idx_pos <- which(fila > 0)
              if (length(idx_pos) > 0) dias_r_cols[max(idx_pos)] else NA_character_
            })
            df_rect_ant_map <- tibble(
              codigo_norm = norm_codigo_valvula(rc_rect_ant$codigo),
              Dia_Rect_Ant = ultimo_dia_rect
            ) %>%
              filter(!is.na(Dia_Rect_Ant)) %>%
              group_by(codigo_norm) %>%
              slice(n()) %>%
              ungroup()
            if (nrow(df_rect_ant_map) > 0) {
              valv_7d_map <- valv_7d_map %>%
                select(-any_of("Dia_Rect_Ant")) %>%
                full_join(df_rect_ant_map, by = "codigo_norm") %>%
                mutate(
                  Dia_7d       = coalesce(Dia_Rect_Ant, Dia_7d),
                  Kg_Total_Ant = coalesce(Kg_Total_Ant, 0),
                  Ha_Total_Ant = coalesce(Ha_Total_Ant, 0)
                ) %>%
                select(codigo_norm, Dia_7d, Dia_Rect_Ant, Kg_Total_Ant, Ha_Total_Ant)
            }
          }
        }
      }
      
      dias_previos_nom <- if (dia_inicio_idx > 1) dias_nombres[1:(dia_inicio_idx - 1)] else character(0)
      cols_ha_prev <- paste0("Ha_", dias_previos_nom)
      cols_kg_prev <- paste0("Kg_", dias_previos_nom)
      cols_ha_rc_exist <- intersect(cols_ha_prev, names(rc_sem))
      cols_kg_rc_exist <- intersect(cols_kg_prev, names(rc_sem))
      
      rc_valv_avance <- if (nrow(rc_sem) > 0 && "codigo" %in% names(rc_sem)) {
        rc_temp <- rc_sem %>% mutate(codigo_norm = norm_codigo_valvula(codigo))
        
        # Identificar día planificado en la ruta de la semana actual
        dias_cols_all <- paste0("Ha_", dias_nombres)
        dias_cols_ex  <- intersect(dias_cols_all, names(rc_temp))
        if (length(dias_cols_ex) > 0) {
          mat_ha <- as.matrix(rc_temp[, dias_cols_ex])
          has_any <- rowSums(mat_ha > 0, na.rm = TRUE) > 0
          max_idx <- max.col(mat_ha, ties.method = "first")
          dias_noms_sub <- sub("^Ha_", "", dias_cols_ex)
          rc_temp$Dia_Ruta_Sem <- ifelse(has_any, dias_noms_sub[max_idx], NA_character_)
        } else {
          rc_temp$Dia_Ruta_Sem <- NA_character_
        }
        
        # Acumular avance ejecutado previo a dia_inicio_nom (solo días con kilos o Lunes/Martes en curso)
        ha_prev_vec <- numeric(nrow(rc_temp))
        kg_prev_vec <- numeric(nrow(rc_temp))
        for (d in dias_previos_nom) {
          col_h <- paste0("Ha_", d)
          col_k <- paste0("Kg_", d)
          val_h <- if (col_h %in% names(rc_temp)) coalesce(rc_temp[[col_h]], 0) else 0
          val_k <- if (col_k %in% names(rc_temp)) coalesce(rc_temp[[col_k]], 0) else 0
          es_ejecutado <- (d %in% c("Lunes", "Martes"))
          ha_prev_vec <- ha_prev_vec + ifelse(es_ejecutado, val_h, 0)
          kg_prev_vec <- kg_prev_vec + ifelse(es_ejecutado, val_k, 0)
        }
        rc_temp$Ha_Prev_Valv <- ha_prev_vec
        rc_temp$Kg_Prev_Valv <- kg_prev_vec
        
        rc_temp %>%
          group_by(codigo_norm) %>%
          summarise(
            Ha_Prog      = sum(coalesce(Ha_Prev_Valv, 0), na.rm = TRUE),
            Kg_Prog      = sum(coalesce(Kg_Prev_Valv, 0), na.rm = TRUE),
            Ha_Lunes_Rc  = sum(coalesce(Ha_Lunes, 0), na.rm = TRUE),
            Kg_Lunes_Rc  = if ("Kg_Lunes" %in% names(.)) sum(coalesce(Kg_Lunes, 0), na.rm = TRUE) else 0,
            Ha_Martes_Rc = sum(coalesce(Ha_Martes, 0), na.rm = TRUE),
            Kg_Martes_Rc = if ("Kg_Martes" %in% names(.)) sum(coalesce(Kg_Martes, 0), na.rm = TRUE) else 0,
            Dia_Ruta_Sem = first(na.omit(Dia_Ruta_Sem)),
            .groups      = "drop"
          )
      } else tibble(codigo_norm = character(), Ha_Prog = numeric(), Kg_Prog = numeric(), Ha_Lunes_Rc = numeric(), Kg_Lunes_Rc = numeric(), Ha_Martes_Rc = numeric(), Kg_Martes_Rc = numeric(), Dia_Ruta_Sem = character())
      
      # Válvulas rectificadas el Martes en la semana actual (si se cosechó Lunes y se rectificó Martes, su frecuencia cuenta desde Martes)
      rectif_martes_ids <- if (nrow(rc_rectif_sem) > 0 && "codigo" %in% names(rc_rectif_sem) && "Martes" %in% names(rc_rectif_sem)) {
        unique(norm_codigo_valvula(rc_rectif_sem$codigo[coalesce(as.numeric(rc_rectif_sem$Martes), 0) > 0]))
      } else character(0)
      
      # Válvulas con avance oficial en SAP para la semana actual (solo días previos a dia_inicio_nom)
      cols_ha_sap_exist <- intersect(cols_ha_prev, names(df_hist_oficial_wide))
      cols_kg_sap_exist <- intersect(cols_kg_prev, names(df_hist_oficial_wide))
      
      sap_valv_avance <- if (exists("df_hist_oficial_wide") && !is.null(df_hist_oficial_wide) && nrow(df_hist_oficial_wide) > 0) {
        df_hist_oficial_wide %>%
          filter(as.numeric(Semana) == sem_sel) %>%
          mutate(
            codigo_norm = norm_codigo_valvula(Codigo_Valv),
            Ha_Sap_Prev = if (length(cols_ha_sap_exist) > 0) rowSums(across(all_of(cols_ha_sap_exist)), na.rm = TRUE) else 0,
            Kg_Sap_Prev = if (length(cols_kg_sap_exist) > 0) rowSums(across(all_of(cols_kg_sap_exist)), na.rm = TRUE) else 0
          ) %>%
          group_by(codigo_norm) %>%
          summarise(
            Ha_Prog_Sap      = sum(coalesce(Ha_Sap_Prev, 0), na.rm = TRUE),
            Kg_Prog_Sap      = sum(coalesce(Kg_Sap_Prev, 0), na.rm = TRUE),
            Ha_Lunes_Sap     = sum(coalesce(Ha_Lunes, 0), na.rm = TRUE),
            Kg_Lunes_Sap     = if ("Kg_Lunes" %in% names(.)) sum(coalesce(Kg_Lunes, 0), na.rm = TRUE) else 0,
            Ha_Martes_Sap    = sum(coalesce(Ha_Martes, 0), na.rm = TRUE),
            Kg_Martes_Sap    = if ("Kg_Martes" %in% names(.)) sum(coalesce(Kg_Martes, 0), na.rm = TRUE) else 0,
            .groups          = "drop"
          )
      } else {
        tibble(codigo_norm = character(), Ha_Prog_Sap = numeric(), Kg_Prog_Sap = numeric(), Ha_Lunes_Sap = numeric(), Kg_Lunes_Sap = numeric(), Ha_Martes_Sap = numeric(), Kg_Martes_Sap = numeric())
      }
      
      df_cosecha_sem <- rv_cosecha() %>%
        filter(as.character(Campaña) == as.character(input$filtro_campana), Semana == sem_sel, !coalesce(Es_Testblock, FALSE))
      
      df_cosecha_sem <- df_cosecha_sem %>%
        mutate(
          Turno_num = suppressWarnings(as.numeric(Turno)),
          Módulo = case_when(
            es_campana_act & Módulo %in% c("13.1", "13.2") ~ "13",
            es_campana_act & Módulo %in% c("14.1", "14.2") ~ "14",
            Módulo == "9" & between(Turno_num, 1, 11)  ~ "9.2",
            Módulo == "9" & Turno_num >= 12            ~ "9.1",
            Módulo == "11" & between(Turno_num, 1, 12) ~ "11.1",
            Módulo == "11" & Turno_num >= 13           ~ "11.2",
            TRUE                                       ~ trimws(as.character(Módulo))
          ),
          Turno    = trimws(as.character(Turno)),
          Variedad = limpiar_variedad(Variedad),
          wday_num = lubridate::wday(Fecha, week_start = 1)
        )
      
      kg_real_turno <- df_cosecha_sem %>%
        group_by(Módulo, Turno, Variedad) %>%
        summarise(
          Kg_Real_Sem             = sum(`Kg Total`[wday_num < dia_inicio_idx], na.rm = TRUE),
          Kg_Total_Completo_Sem   = sum(`Kg Total`, na.rm = TRUE),
          Kg_Lunes_Real           = sum(`Kg Total`[wday_num == 1], na.rm = TRUE),
          Kg_Martes_Real          = sum(`Kg Total`[wday_num == 2], na.rm = TRUE),
          Sap_Cosecho_Previo      = any(wday_num < dia_inicio_idx & `Kg Total` > 0),
          Sap_Cosecho_Lunes       = any(wday_num == 1 & `Kg Total` > 0),
          Sap_Cosecho_Martes      = any(wday_num == 2 & `Kg Total` > 0),
          Valvulas_Cosechadas_Sap = n_distinct(Válvula[wday_num < dia_inicio_idx & `Kg Total` > 0]),
          .groups                 = "drop"
        )
      
      proy_sem <- proyeccion_semanal()
      proy_turno <- if (!is.null(proy_sem) && nrow(proy_sem) > 0) {
        proy_sem %>%
          mutate(
            Módulo = case_when(
              es_campana_act & Módulo_join %in% c("13.1", "13.2") ~ "13",
              es_campana_act & Módulo_join %in% c("14.1", "14.2") ~ "14",
              TRUE ~ trimws(as.character(Módulo_join))
            ),
            Turno    = trimws(as.character(Turno_join)),
            Variedad = limpiar_variedad(Variedad_join)
          ) %>%
          group_by(Módulo, Turno, Variedad) %>%
          summarise(Proy_Kg = sum(Proyección, na.rm = TRUE), .groups = "drop")
      } else tibble(Módulo = character(), Turno = character(), Variedad = character(), Proy_Kg = numeric())
      
      if (!exists("df_valvulas") || is.null(df_valvulas) || nrow(df_valvulas) == 0) {
        showNotification("No se encontró el catálogo maestro de válvulas para proyectar.", type = "error", duration = 4)
        return()
      }
      
      valvulas_calc <- df_valvulas %>%
        mutate(
          codigo_norm = norm_codigo_valvula(coalesce(codigo_norm, codigo)),
          Turno_num   = suppressWarnings(as.numeric(Turno)),
          Sector      = case_when(as.character(Módulo) %in% c("9", "9.1", "9.2", "11", "11.1", "11.2", "12") ~ "9", TRUE ~ trimws(as.character(Sector))),
          Módulo      = case_when(
            es_campana_act & Módulo %in% c("13.1", "13.2") ~ "13",
            es_campana_act & Módulo %in% c("14.1", "14.2") ~ "14",
            as.character(Módulo) == "9" & between(Turno_num, 1, 11)  ~ "9.2",
            as.character(Módulo) == "9" & Turno_num >= 12            ~ "9.1",
            as.character(Módulo) == "11" & between(Turno_num, 1, 12) ~ "11.1",
            as.character(Módulo) == "11" & Turno_num >= 13           ~ "11.2",
            TRUE                                                     ~ trimws(as.character(Módulo))
          ),
          Turno       = trimws(as.character(Turno)),
          Variedad    = limpiar_variedad(Variedad),
          Área        = suppressWarnings(as.numeric(Área))
        ) %>%
        left_join(
          df_frec_mv %>% select(Módulo, Variedad, Frec_Manual_Dias = Frecuencia_Dias),
          by = c("Módulo", "Variedad")
        ) %>%
        left_join(
          if (!is.null(df_frec_valvs_2sem) && nrow(df_frec_valvs_2sem) > 0) {
            df_frec_valvs_2sem %>% select(codigo_norm, Frec_Auto_Dias = Dias_Frecuencia, Dia_Auto_Valv = Dia_Proyectado_Nom)
          } else tibble(codigo_norm = character(), Frec_Auto_Dias = integer(), Dia_Auto_Valv = character()),
          by = "codigo_norm"
        ) %>%
        mutate(
          Frecuencia_Dias = if (modo_frec == "manual") {
            coalesce(as.integer(Frec_Manual_Dias), ifelse(Variedad == "ROCIO", 3L, 7L))
          } else {
            coalesce(as.integer(Frec_Auto_Dias), as.integer(Frec_Manual_Dias), ifelse(Variedad == "ROCIO", 3L, 7L))
          },
          Dia_Auto_Valv = if (modo_frec == "manual") NA_character_ else Dia_Auto_Valv
        ) %>%
        left_join(
          df_pct_rocio %>%
            mutate(Módulo = trimws(as.character(Módulo))) %>%
            select(Módulo, Pct_Pasada_Lun, Pct_Pasada_Mar),
          by = "Módulo"
        ) %>%
        mutate(
          Pct_Pasada_Lun = coalesce(Pct_Pasada_Lun, 60),
          Pct_Pasada_Mar = coalesce(Pct_Pasada_Mar, 60)
        ) %>%
        left_join(rc_valv_avance, by = "codigo_norm") %>%
        left_join(sap_valv_avance, by = "codigo_norm") %>%
        mutate(
          Ha_Prog      = case_when(
            coalesce(Ha_Prog_Sap, 0) > 0                        ~ pmin(Área, Ha_Prog_Sap),
            coalesce(Kg_Prog_Sap, 0) > 0                        ~ Área,
            (dia_inicio_idx > 1 & coalesce(Ha_Lunes_Rc, 0) > 0) ~ pmin(Área, Ha_Lunes_Rc),
            (dia_inicio_idx > 2 & coalesce(Ha_Martes_Rc, 0) > 0) ~ pmin(Área, Ha_Martes_Rc),
            TRUE                                                ~ 0
          ),
          Kg_Prog      = case_when(
            coalesce(Kg_Prog_Sap, 0) > 0                        ~ Kg_Prog_Sap,
            (dia_inicio_idx > 1 & coalesce(Kg_Lunes_Rc, 0) > 0) ~ Kg_Lunes_Rc,
            (dia_inicio_idx > 2 & coalesce(Kg_Martes_Rc, 0) > 0) ~ Kg_Martes_Rc,
            TRUE                                                ~ 0
          ),
          Ha_Lunes_Rc   = pmax(coalesce(Ha_Lunes_Rc, 0), coalesce(Ha_Lunes_Sap, 0)),
          Ha_Martes_Rc  = pmax(coalesce(Ha_Martes_Rc, 0), coalesce(Ha_Martes_Sap, 0)),
          Kg_Lunes_Valv = coalesce(Kg_Lunes_Sap, 0) + ifelse(coalesce(Kg_Lunes_Sap, 0) == 0, coalesce(Kg_Lunes_Rc, 0), 0),
          Kg_Martes_Valv = coalesce(Kg_Martes_Sap, 0) + ifelse(coalesce(Kg_Martes_Sap, 0) == 0, coalesce(Kg_Martes_Rc, 0), 0),
          Ha_Faltante   = pmax(0, Área - Ha_Prog)
        ) %>%
        left_join(valv_7d_map, by = "codigo_norm") %>%
        left_join(turno_dia_habitual, by = c("Módulo", "Turno")) %>%
        mutate(
          Kg_Total_Ant            = coalesce(Kg_Total_Ant, 0),
          Ha_Total_Ant            = coalesce(Ha_Total_Ant, 0),
          Dia_Base_Ant            = coalesce(Dia_7d, Dia_Habitual),
          Dia_Natural_Valv        = case_when(
            !is.na(Dia_Ruta_Sem) & nzchar(Dia_Ruta_Sem)   ~ Dia_Ruta_Sem,
            !is.na(Dia_Rect_Ant) & nzchar(Dia_Rect_Ant)   ~ desplazar_dia_frecuencia(Dia_Rect_Ant, Frecuencia_Dias, dias_nombres, 1L, "Lunes", 6L, "Sábado"),
            !is.na(Dia_Auto_Valv) & nzchar(Dia_Auto_Valv) ~ Dia_Auto_Valv,
            !is.na(Dia_Base_Ant) & nzchar(Dia_Base_Ant)   ~ desplazar_dia_frecuencia(Dia_Base_Ant, Frecuencia_Dias, dias_nombres, 1L, "Lunes", 6L, "Sábado"),
            TRUE                                          ~ dia_inicio_nom
          ),
          idx_natural             = match(Dia_Natural_Valv, dias_nombres),
          idx_natural             = ifelse(is.na(idx_natural), dia_inicio_idx, idx_natural),
          # REGLA DE POSICIONAMIENTO EN RANGO [DESDE, HASTA]:
          # Si el día natural está dentro del rango disponible, se respeta.
          # Si cae antes (ej. Martes cuando rango empieza Miércoles), se posiciona en dia_inicio (fecha más próxima).
          # Si cae después (ej. Sábado cuando rango termina Viernes), se posiciona en dia_fin (fecha más próxima).
          idx_asig_rango          = pmin(dia_fin_idx, pmax(dia_inicio_idx, idx_natural)),
          Dia_Habitual_Calc       = dias_nombres[idx_asig_rango],
          Tiene_Ciclo_Previo_Valv = !is.na(Dia_7d) | (!is.na(Dia_Auto_Valv) & nzchar(Dia_Auto_Valv)) | (!is.na(Dia_Ruta_Sem) & nzchar(Dia_Ruta_Sem))
        )
      
      if (solo_filtrado) {
        turnos_filtro <- turnos_activos_filtrados()
        if (is.null(turnos_filtro) || nrow(turnos_filtro) == 0) {
          showNotification("No hay turnos visibles en el filtro actual para ajustar.", type = "warning", duration = 4)
          return()
        }
        claves_filtro <- paste(
          trimws(as.character(turnos_filtro$Módulo)),
          trimws(as.character(turnos_filtro$Turno)),
          limpiar_variedad(turnos_filtro$Variedad),
          sep = "___"
        )
        valvulas_calc <- valvulas_calc %>%
          filter(paste(trimws(as.character(Módulo)), trimws(as.character(Turno)), limpiar_variedad(Variedad), sep = "___") %in% claves_filtro)
        
        if (nrow(valvulas_calc) == 0) {
          showNotification("No se encontraron válvulas en el catálogo para los turnos filtrados.", type = "warning", duration = 4)
          return()
        }
      }
      
      # Cargar ediciones manuales existentes de la semana para descontar hectáreas ya asignadas manualmente
      ed_act_sem <- ediciones_manuales()
      ed_man_sem <- if (!is.null(ed_act_sem) && nrow(ed_act_sem) > 0) {
        ed_act_sem %>%
          filter(
            as.character(Campaña) == as.character(input$filtro_campana),
            as.numeric(Semana)    == as.numeric(sem_sel),
            coalesce(Tipo_Edicion, "") == "manual",
            coalesce(Valor, 0) > 0
          )
      } else tibble()

      resumen_man_turno <- if (nrow(ed_man_sem) > 0) {
        ed_man_sem %>%
          group_by(
            Módulo   = trimws(as.character(Módulo)),
            Turno    = trimws(as.character(Turno)),
            Variedad = limpiar_variedad(Variedad)
          ) %>%
          summarise(
            Ha_Manual_1ra = sum(coalesce(as.numeric(Ha_Asignada_1ra), 0), na.rm = TRUE),
            Ha_Manual_2da = sum(coalesce(as.numeric(Ha_Asignada_2da), 0), na.rm = TRUE),
            Kg_Manual_Tot = sum(coalesce(as.numeric(Valor), 0), na.rm = TRUE),
            .groups       = "drop"
          )
      } else {
        tibble(Módulo = character(), Turno = character(), Variedad = character(), Ha_Manual_1ra = numeric(), Ha_Manual_2da = numeric(), Kg_Manual_Tot = numeric())
      }
      
      turno_avance_ha <- valvulas_calc %>%
        group_by(Módulo, Turno, Variedad) %>%
        summarise(
          Ha_Prog_Turno     = sum(Ha_Prog, na.rm = TRUE),
          Kg_Prog_Turno     = sum(Kg_Prog, na.rm = TRUE),
          Ha_Lunes_Turno    = sum(Ha_Lunes_Rc, na.rm = TRUE),
          Ha_Martes_Turno   = sum(Ha_Martes_Rc, na.rm = TRUE),
          Ha_Faltante_Turno = sum(Ha_Faltante, na.rm = TRUE),
          Area_Total_Turno  = sum(Área, na.rm = TRUE),
          Valvulas_Total    = n(),
          Frecuencia_Dias   = first(Frecuencia_Dias),
          .groups           = "drop"
        ) %>%
        left_join(resumen_man_turno, by = c("Módulo", "Turno", "Variedad")) %>%
        mutate(
          Ha_Manual_1ra          = coalesce(Ha_Manual_1ra, 0),
          Ha_Manual_2da          = coalesce(Ha_Manual_2da, 0),
          Ha_Faltante_Turno_Neto = pmax(0, Area_Total_Turno - Ha_Prog_Turno - Ha_Manual_1ra),
          Turno_Completo_1ra     = (Ha_Faltante_Turno_Neto <= 0.05)
        )
      
      
      # ====================================================================
      # FACTORES DE TENDENCIA FLORAL SEGÚN MADURACIÓN (PREMIO / CASTIGO)
      # ====================================================================
      df_c_hist <- if (exists("df_conteo_historico") && is.data.frame(df_conteo_historico) && nrow(df_conteo_historico) > 0) {
        df_conteo_historico
      } else if (file.exists("historico_conteo.rds")) {
        readRDS("historico_conteo.rds")
      } else if (file.exists("D:/ACP/Actualizacion Semanal/historico_conteo.rds")) {
        readRDS("D:/ACP/Actualizacion Semanal/historico_conteo.rds")
      } else {
        tibble()
      }
      
      sens_floral_pct <- rv_sensibilidad_floral()
      df_mad_actual <- rv_maduracion()
      if (is.null(df_mad_actual) || nrow(df_mad_actual) == 0) {
        df_mad_actual <- cargar_maduracion_disco()
      }
      
      df_factores_flor <- calcular_factor_tendencia_floral(
        df_conteo        = df_c_hist,
        campana_sel      = input$filtro_campana,
        sem_cosecha      = sem_sel,
        df_maduracion    = df_mad_actual,
        sensibilidad_pct = sens_floral_pct
      )
      
      rend_turno <- turno_avance_ha %>%
        left_join(kg_real_turno, by = c("Módulo", "Turno", "Variedad")) %>%
        left_join(proy_turno, by = c("Módulo", "Turno", "Variedad")) %>%
        left_join(rend_ant_turno, by = c("Módulo", "Turno", "Variedad"))
      
      if (!is.null(df_factores_flor) && nrow(df_factores_flor) > 0) {
        rend_turno <- rend_turno %>%
          left_join(
            df_factores_flor %>%
              mutate(
                Módulo   = trimws(as.character(Módulo)),
                Turno    = trimws(as.character(Turno)),
                Variedad = limpiar_variedad(Variedad)
              ) %>%
              select(Módulo, Turno, Variedad, Factor_Floral, Tipo_Efecto_Floral, Delta_Floral_Pct),
            by = c("Módulo", "Turno", "Variedad")
          ) %>%
          mutate(
            Factor_Floral      = coalesce(Factor_Floral, 1.0),
            Tipo_Efecto_Floral = coalesce(Tipo_Efecto_Floral, "Neutro"),
            Delta_Floral_Pct   = coalesce(Delta_Floral_Pct, 0)
          )
      } else {
        rend_turno <- rend_turno %>%
          mutate(
            Factor_Floral      = 1.0,
            Tipo_Efecto_Floral = "Neutro",
            Delta_Floral_Pct   = 0
          )
      }
      
      rend_turno <- rend_turno %>%
        mutate(
          Kg_Real_Sem        = coalesce(Kg_Real_Sem, 0),
          Kg_Lunes_Real      = coalesce(Kg_Lunes_Real, 0),
          Kg_Martes_Real     = coalesce(Kg_Martes_Real, 0),
          Proy_Kg            = coalesce(Proy_Kg, 0),
          Esta_Proyectado    = (Proy_Kg > 0),
          Ha_Ant_Turno       = coalesce(Ha_Ant_Turno, 0),
          Kg_Ant_Turno       = coalesce(Kg_Ant_Turno, 0),
          Rend_Ant_Turno     = coalesce(Rend_Ant_Turno, 0),
          Sap_Cosecho_Previo = coalesce(Sap_Cosecho_Previo, FALSE),
          Sap_Cosecho_Lunes  = coalesce(Sap_Cosecho_Lunes, FALSE),
          Sap_Cosecho_Martes = coalesce(Sap_Cosecho_Martes, FALSE),
          Rend_Proy          = ifelse(Area_Total_Turno > 0 & Proy_Kg > 0, Proy_Kg / Area_Total_Turno, 0),
          
          # Detección de remanente mínimo (<15% proyección o <50 kg/ha sin reporte de rutas)
          es_remanente_minimo = (Ha_Prog_Turno == 0) & (Kg_Real_Sem > 0) & 
            ((Kg_Real_Sem / pmax(1, Proy_Kg)) < 0.15 | (Kg_Real_Sem / pmax(0.1, Area_Total_Turno)) < 50),
          
          # Detección de cosecha previa al día de inicio de ajuste
          Cosecho_Lunes     = Sap_Cosecho_Lunes | (Ha_Lunes_Turno > 0) | (Kg_Lunes_Real > 0),
          Cosecho_Martes    = Sap_Cosecho_Martes | (Ha_Martes_Turno > 0) | (Kg_Martes_Real > 0),
          Tiene_Cosecha_Act = !es_remanente_minimo & (Sap_Cosecho_Previo | Cosecho_Lunes | Cosecho_Martes | Kg_Real_Sem > 0 | Ha_Prog_Turno > 0),
          
          Ha_Recorrida_Turno = case_when(
            Ha_Prog_Turno > 0   ~ Ha_Prog_Turno,
            es_remanente_minimo ~ round(pmin(Area_Total_Turno, Kg_Real_Sem / pmax(1, Rend_Proy)), 2),
            Kg_Real_Sem > 0     ~ round(pmin(Area_Total_Turno, Kg_Real_Sem / pmax(1, Rend_Proy)), 2),
            TRUE                ~ 0
          ),
          
          Prop_Avance_Turno = ifelse(Area_Total_Turno > 0, pmin(1, Ha_Recorrida_Turno / Area_Total_Turno), 0),
          
          # ====================================================================
          # NODO 1: ¿Finalizó la Semana Anterior (S-1)?
          # No finalizó S-1 si tuvo cosecha en S-1 incompleta (< 95% área),
          # su último día en S-1 fue fin de semana (Viernes/Sábado) y el Lunes/Martes
          # actual continuó cerrando esa campaña previa.
          # ====================================================================
          Semana_Ant_No_Finalizo = (Ha_Ant_Turno > 0) &
            (Ha_Ant_Turno < 0.95 * Area_Total_Turno) &
            (coalesce(Ultimo_Dia_Cosecha_Ant, "") %in% c("Sábado", "Viernes")) &
            (Cosecho_Lunes | Cosecho_Martes),
          
          # ====================================================================
          # NODO 2 y 3 (Rama Derecha): ¿Se cosechó TODO el turno antes de dia_inicio?
          # Un turno solo se considera completo si su saldo pendiente es nulo o insignificante (<= 0.05 ha)
          # ====================================================================
          Turno_Completo_Lun_Mar = !Semana_Ant_No_Finalizo & Tiene_Cosecha_Act &
            (Ha_Faltante_Turno <= 0.05 | Area_Total_Turno - Ha_Recorrida_Turno <= 0.05),
          Es_Ciclo_Corto         = (Variedad == "ROCIO") | (Frecuencia_Dias <= 4L),
          
          # ====================================================================
          # Rendimiento Real de la Semana Actual (dividiendo Kg SAP solo entre las Ha de los días que ya tienen Kg SAP)
          # ====================================================================
          Kg_Lun_Mar_Sap = coalesce(Kg_Lunes_Real, 0) + coalesce(Kg_Martes_Real, 0),
          Ha_Con_Kg_Sap  = ifelse(Kg_Lunes_Real > 0, ifelse(Ha_Lunes_Turno > 0, Ha_Lunes_Turno, Area_Total_Turno), 0) +
            ifelse(Kg_Martes_Real > 0, ifelse(Ha_Martes_Turno > 0, Ha_Martes_Turno, Area_Total_Turno), 0),
          Rend_Sem_Actual_Bruto = case_when(
            Kg_Real_Sem > 0 & Ha_Recorrida_Turno > 0 ~ Kg_Real_Sem / Ha_Recorrida_Turno,
            Kg_Lun_Mar_Sap > 0 & Ha_Con_Kg_Sap > 0   ~ Kg_Lun_Mar_Sap / Ha_Con_Kg_Sap,
            TRUE                                     ~ 0
          ),
          Rend_Sem_Actual = case_when(
            Rend_Sem_Actual_Bruto > 0 ~ Rend_Sem_Actual_Bruto,
            Rend_Ant_Turno > 0        ~ Rend_Ant_Turno,
            TRUE                      ~ Rend_Proy
          ),
          
          Rend_Lunes_Turno = case_when(
            coalesce(Kg_Lunes_Real, 0) > 0 & coalesce(Ha_Lunes_Turno, 0) > 0 ~ Kg_Lunes_Real / Ha_Lunes_Turno,
            coalesce(Kg_Lunes_Real, 0) > 0 & Area_Total_Turno > 0              ~ Kg_Lunes_Real / Area_Total_Turno,
            TRUE                                                              ~ NA_real_
          ),
          Rend_Martes_Turno = case_when(
            coalesce(Kg_Martes_Real, 0) > 0 & coalesce(Ha_Martes_Turno, 0) > 0 ~ Kg_Martes_Real / Ha_Martes_Turno,
            coalesce(Kg_Martes_Real, 0) > 0 & Area_Total_Turno > 0              ~ Kg_Martes_Real / Area_Total_Turno,
            TRUE                                                               ~ NA_real_
          ),
          
          # ====================================================================
          # NODOS 4, 5, 6 y 7: Árbol de Decisión de Rendimiento por Rama
          # ====================================================================
          Rend_Base_Turno = case_when(
            # RAMA 2A (Derecha -> 100% Lunes/Martes -> Es Rocío): 2da pasada con Rend Sem Actual
            Turno_Completo_Lun_Mar & Es_Ciclo_Corto ~ Rend_Sem_Actual,
            
            # Cierre S-1 incompleta con avance > 30% en semana actual -> Usar rendimiento real de la semana actual
            Semana_Ant_No_Finalizo & Tiene_Cosecha_Act & Prop_Avance_Turno > 0.30 ~ Rend_Sem_Actual,
            
            # Cierre S-1 incompleta sin kilos o con avance <= 30% -> Rend S-1 (o Proy si S-1=0) con factor floral
            Semana_Ant_No_Finalizo ~ ifelse(Rend_Ant_Turno > 0, Rend_Ant_Turno, Rend_Proy),
            
            # RAMA 3A (Derecha -> No Tiene Kilos / Avance en Sem Actual):
            # Usar rendimiento de la semana anterior S-1 (o Proy si S-1=0) ajustado con factor floral
            !Tiene_Cosecha_Act ~ ifelse(Rend_Ant_Turno > 0, Rend_Ant_Turno, Rend_Proy),
            
            # RAMA 3B-1 (Derecha -> Tiene Avance -> Cosechó > 30% del turno):
            # Usar directo Rend_Sem_Actual (rendimiento real observado en campo)
            Tiene_Cosecha_Act & Prop_Avance_Turno > 0.30 ~ Rend_Sem_Actual,
            
            # RAMA 3B-2 (Derecha -> Tiene Avance -> Cosechó <= 30% del turno):
            # Muestra pequeña (<= 30%): Usar rendimiento de la semana anterior S-1 (o Proy si S-1=0) con factor floral
            Tiene_Cosecha_Act & Prop_Avance_Turno <= 0.30 ~ ifelse(Rend_Ant_Turno > 0, Rend_Ant_Turno, Rend_Proy),
            
            TRUE ~ ifelse(Rend_Ant_Turno > 0, Rend_Ant_Turno, Rend_Proy)
          ),
          
          # Rendimiento ajustado con Premio o Castigo por tendencia floral de maduración
          # REGLA: Cuando el avance es > 30% del turno, NO se aplica factor floral
          # porque el rendimiento de la semana actual ya viene afectado por ese factor.
          Rend_Base_Con_Floral = case_when(
            Tiene_Cosecha_Act & Prop_Avance_Turno > 0.30 ~ round(Rend_Base_Turno, 2),
            TRUE                                         ~ round(Rend_Base_Turno * Factor_Floral, 2)
          ),
          
          # Si el turno NO está proyectado en la semana o si se cosechó 100% entre Lunes/Martes y NO es Rocío -> No ajustar (0)
          Rend_Turno_Aplicar = ifelse(!Esta_Proyectado | (Turno_Completo_Lun_Mar & !Es_Ciclo_Corto), 0, Rend_Base_Con_Floral),
          
          Motivo_Base = case_when(
            !Esta_Proyectado ~ "No proyectado",
            Turno_Completo_Lun_Mar & !Es_Ciclo_Corto ~ "Cerró 100% en Lun/Mar (0 kg pend.)",
            Turno_Completo_Lun_Mar & Es_Ciclo_Corto ~ sprintf("2da pasada Rocío (Rend Lunes: %s kg/ha • 60%% sin floración)", trimws(format(round(coalesce(Rend_Lunes_Turno, Rend_Sem_Actual)), big.mark = ","))),
            Tiene_Cosecha_Act & Prop_Avance_Turno > 0.30 ~ sprintf("Avance >30%% (Rend act sem: %s kg/ha)", trimws(format(round(Rend_Sem_Actual), big.mark = ","))),
            Semana_Ant_No_Finalizo & Rend_Ant_Turno > 0 ~ sprintf("Cierre S-1 incompleta (Rend S-1: %s kg/ha)", trimws(format(round(Rend_Ant_Turno), big.mark = ","))),
            Semana_Ant_No_Finalizo                      ~ sprintf("Cierre S-1 incompleta (Rend proy: %s kg/ha)", trimws(format(round(Rend_Proy), big.mark = ","))),
            !Tiene_Cosecha_Act & Rend_Ant_Turno > 0 ~ sprintf("Sin avance previo (Rend S-1: %s kg/ha)", trimws(format(round(Rend_Ant_Turno), big.mark = ","))),
            !Tiene_Cosecha_Act                      ~ sprintf("Sin avance previo (Rend proy: %s kg/ha)", trimws(format(round(Rend_Proy), big.mark = ","))),
            Tiene_Cosecha_Act & Prop_Avance_Turno <= 0.30 & Rend_Ant_Turno > 0 ~ sprintf("Avance <=30%% (Rend S-1: %s kg/ha)", trimws(format(round(Rend_Ant_Turno), big.mark = ","))),
            Tiene_Cosecha_Act & Prop_Avance_Turno <= 0.30                      ~ sprintf("Avance <=30%% (Rend proy: %s kg/ha)", trimws(format(round(Rend_Proy), big.mark = ","))),
            TRUE ~ ifelse(Rend_Ant_Turno > 0, sprintf("Rend S-1 (%s kg/ha)", trimws(format(round(Rend_Ant_Turno), big.mark = ","))), sprintf("Rend proy (%s kg/ha)", trimws(format(round(Rend_Proy), big.mark = ","))))
          ),
          
          Pct_Ajuste_Flor = round((Factor_Floral - 1) * 100, 1),
          Detalle_Floral = case_when(
            !Esta_Proyectado | (Turno_Completo_Lun_Mar & !Es_Ciclo_Corto) | (Turno_Completo_Lun_Mar & Es_Ciclo_Corto) ~ "",
            Tiene_Cosecha_Act & Prop_Avance_Turno > 0.30 ~ "",
            Tipo_Efecto_Floral == "Premio"  ~ sprintf("Premio floral %+0.1f%%", Pct_Ajuste_Flor),
            Tipo_Efecto_Floral == "Castigo" ~ sprintf("Castigo floral %+0.1f%%", Pct_Ajuste_Flor),
            TRUE                            ~ "Floral neutro (0%)"
          ),
          
          Motivo_Ajuste = case_when(
            !Esta_Proyectado ~ "No proyectado",
            Turno_Completo_Lun_Mar & !Es_Ciclo_Corto ~ "Cerró 100% en Lun/Mar",
            Turno_Completo_Lun_Mar & Es_Ciclo_Corto ~ Motivo_Base,
            nzchar(Detalle_Floral) ~ paste0(Motivo_Base, " • ", Detalle_Floral),
            TRUE ~ Motivo_Base
          )
        )
      
      valvulas_calc <- valvulas_calc %>%
        left_join(
          rend_turno %>% select(
            Módulo, Turno, Variedad, Rend_Turno_Aplicar, Rend_Base_Turno, Rend_Base_Con_Floral, Rend_Ant_Turno, Rend_Sem_Actual, Rend_Proy,
            Rend_Lunes_Turno, Rend_Martes_Turno,
            Esta_Proyectado, Tiene_Cosecha_Act, Semana_Ant_No_Finalizo, Turno_Completo_Lun_Mar, Es_Ciclo_Corto,
            Cosecho_Lunes, Cosecho_Martes, Kg_Lunes_Real, Kg_Martes_Real, Kg_Real_Sem, Ha_Prog_Turno,
            Area_Total_Turno, Ha_Manual_1ra, Ha_Manual_2da, Ha_Faltante_Turno_Neto, Turno_Completo_1ra,
            Ultimo_Dia_Cosecha_Ant, Prop_Avance_Turno, Factor_Floral, Tipo_Efecto_Floral, Delta_Floral_Pct, Motivo_Ajuste
          ),
          by = c("Módulo", "Turno", "Variedad")
        ) %>%
        mutate(
          Ha_Prog_Turno = coalesce(Ha_Prog_Turno, 0),
          Kg_Real_Sem   = coalesce(Kg_Real_Sem, 0),
          Valvula_Cosecho_Lunes  = (Ha_Lunes_Rc > 0) | (Semana_Ant_No_Finalizo & Cosecho_Lunes & is.na(Dia_7d) & Ha_Prog == 0),
          Valvula_Cosecho_Martes = (Ha_Martes_Rc > 0) | (Semana_Ant_No_Finalizo & !Cosecho_Lunes & Cosecho_Martes & is.na(Dia_7d) & Ha_Prog == 0),
          Valvula_Cerro_En_Lun_Mar = Semana_Ant_No_Finalizo & (Valvula_Cosecho_Lunes | Valvula_Cosecho_Martes),
          
          Ha_Cosechada_Lunes = case_when(
            Ha_Lunes_Rc > 0                                                                          ~ Ha_Lunes_Rc,
            Valvula_Cosecho_Lunes                                                                    ~ Área,
            !Semana_Ant_No_Finalizo & Cosecho_Lunes & !Cosecho_Martes & Ha_Prog_Turno == 0            ~ Área,
            !Semana_Ant_No_Finalizo & Cosecho_Lunes & Cosecho_Martes & Ha_Prog_Turno == 0            ~ round(Área * (coalesce(Kg_Lunes_Real, 0) / pmax(1, Kg_Real_Sem)), 2),
            !Semana_Ant_No_Finalizo & Turno_Completo_Lun_Mar & !Cosecho_Martes & Ha_Prog_Turno == 0   ~ Área,
            TRUE                                                                                     ~ 0
          ),
          Ha_Cosechada_Martes = case_when(
            Ha_Martes_Rc > 0                                                                         ~ Ha_Martes_Rc,
            Valvula_Cosecho_Martes                                                                   ~ Área,
            !Semana_Ant_No_Finalizo & !Cosecho_Lunes & Cosecho_Martes & Ha_Prog_Turno == 0            ~ Área,
            !Semana_Ant_No_Finalizo & Cosecho_Lunes & Cosecho_Martes & Ha_Prog_Turno == 0            ~ round(Área * (coalesce(Kg_Martes_Real, 0) / pmax(1, Kg_Real_Sem)), 2),
            TRUE                                                                                     ~ 0
          ),
          Ha_Cerrada_Lun_Mar = pmin(Área, Ha_Cosechada_Lunes + Ha_Cosechada_Martes),
          
          # Rendimientos base observados por válvula en Lunes y Martes (SIN FACTOR FLORAL)
          Rend_Valv_Lunes = case_when(
            coalesce(Kg_Lunes_Valv, 0) > 0 & coalesce(Ha_Cosechada_Lunes, 0) > 0 ~ round(Kg_Lunes_Valv / Ha_Cosechada_Lunes, 2),
            !is.na(Rend_Lunes_Turno) & Rend_Lunes_Turno > 0                      ~ round(Rend_Lunes_Turno, 2),
            coalesce(Rend_Sem_Actual, 0) > 0                                     ~ round(Rend_Sem_Actual, 2),
            TRUE                                                                 ~ round(coalesce(Rend_Proy, 0), 2)
          ),
          Rend_Valv_Martes = case_when(
            coalesce(Kg_Martes_Valv, 0) > 0 & coalesce(Ha_Cosechada_Martes, 0) > 0 ~ round(Kg_Martes_Valv / Ha_Cosechada_Martes, 2),
            !is.na(Rend_Martes_Turno) & Rend_Martes_Turno > 0                      ~ round(Rend_Martes_Turno, 2),
            coalesce(Rend_Sem_Actual, 0) > 0                                       ~ round(Rend_Sem_Actual, 2),
            TRUE                                                                   ~ round(coalesce(Rend_Proy, 0), 2)
          )
        )
      
      # Sector homologado para el universo comercial/visual
      u_base <- universo_base()
      sectores_u <- if (!is.null(u_base) && nrow(u_base) > 0) {
        u_base %>%
          group_by(
            Módulo_norm = trimws(as.character(Módulo)),
            Turno_norm  = trimws(as.character(Turno)),
            Var_norm    = limpiar_variedad(Variedad)
          ) %>%
          summarise(
            Sector_Visual = first(na.omit(trimws(as.character(Sector)))),
            .groups = "drop"
          )
      } else {
        data.frame(Módulo_norm = character(0), Turno_norm = character(0), Var_norm = character(0), Sector_Visual = character(0))
      }
      
      # ------------------------------------------------------------------------
      # COMPONENTES DE AJUSTE AUTOMÁTICO SEGÚN FLUJOGRAMA:
      # ------------------------------------------------------------------------
      
      # Componente 0: HECTÁREAS QUE YA TIENEN RUTA PERO AÚN NO TIENEN KILOS EN SAP (ej. Martes con ruta y 0 kg en SAP)
      # Nota: Solo se calculan si el turno está proyectado. Como son Ha Recorrida, se registra Ha_Asignada = 0
      # para no inflar 'Ha Ajustada' y permitir identificar los kilos en rojo sin duplicar área recorrida.
      reg_ruta_lun <- valvulas_calc %>%
        filter(coalesce(Esta_Proyectado, FALSE), Ha_Lunes_Rc > 0, Kg_Lunes_Real == 0) %>%
        mutate(
          Dia_Asignado   = "Lunes",
          Kg_Asignado    = round(Ha_Lunes_Rc * coalesce(Rend_Base_Turno, 0), 2),
          Ha_Asignada    = 0,
          Num_Pasada     = 1L,
          Es_Ruta_Sin_Kg = TRUE
        ) %>%
        filter(Kg_Asignado > 0)
      
      reg_ruta_mar <- valvulas_calc %>%
        filter(coalesce(Esta_Proyectado, FALSE), Ha_Martes_Rc > 0, Kg_Martes_Real == 0) %>%
        mutate(
          Dia_Asignado   = "Martes",
          Kg_Asignado    = round(Ha_Martes_Rc * coalesce(Rend_Base_Turno, 0), 2),
          Ha_Asignada    = 0,
          Num_Pasada     = 1L,
          Es_Ruta_Sin_Kg = TRUE
        ) %>%
        filter(Kg_Asignado > 0)
      
      # Componente 1: PASADA REGULAR (Válvulas de S-1 en Rama Izquierda + Pasada Regular en Rama Derecha)
      # Nota: Se filtra si Turno_Completo_1ra == TRUE (si entre SAP, Rutas y Ajustes Manuales ya completó 100% de 1ª pasada)
      reg_regular <- valvulas_calc %>%
        filter(coalesce(Esta_Proyectado, FALSE), !Turno_Completo_Lun_Mar, !coalesce(Turno_Completo_1ra, FALSE)) %>%
        mutate(
          Ha_Regular = case_when(
            # RAMA IZQUIERDA (S-1 NO finalizó):
            Semana_Ant_No_Finalizo & Valvula_Cerro_En_Lun_Mar & Ha_Cerrada_Lun_Mar >= 0.90 * Área ~ 0,
            Semana_Ant_No_Finalizo & Valvula_Cerro_En_Lun_Mar ~ pmax(0, Área - Ha_Cerrada_Lun_Mar),
            Semana_Ant_No_Finalizo ~ Área,
            
            # RAMA DERECHA (S-1 SÍ finalizó, turno no completado al 100% en Lunes/Martes):
            Tiene_Cosecha_Act ~ Ha_Faltante,
            TRUE ~ Área
          ),
          
          Dia_Asignado = case_when(
            # REGLA EXCLUSIVA ROCÍO: El saldo restante de la 1ª pasada se programa de inmediato en el día de reinicio (Desde:)
            limpiar_variedad(Variedad) == "ROCIO" ~ dia_inicio_nom,
            
            # RAMA IZQUIERDA:
            Semana_Ant_No_Finalizo & Tiene_Ciclo_Previo_Valv ~ Dia_Habitual_Calc,
            Semana_Ant_No_Finalizo & !Tiene_Ciclo_Previo_Valv & !Valvula_Cerro_En_Lun_Mar ~ dia_inicio_nom,
            Semana_Ant_No_Finalizo ~ dia_inicio_nom,
            
            # RAMA DERECHA:
            Tiene_Cosecha_Act & Ha_Prog > 0 & Ha_Faltante > 0.02 ~ dia_inicio_nom,
            TRUE ~ Dia_Habitual_Calc
          ),
          
          idx_asig = match(Dia_Asignado, dias_nombres),
          Dia_Asignado = case_when(
            !is.na(idx_asig) & idx_asig > dia_fin_idx    ~ dia_fin_nom,
            !is.na(idx_asig) & idx_asig < dia_inicio_idx ~ dia_inicio_nom,
            TRUE                                         ~ Dia_Asignado
          )
        ) %>%
        select(-idx_asig) %>%
        # Descontar asignaciones manuales previas para que la suma total por turno coincida exactamente con Ha_Faltante_Turno_Neto
        group_by(Módulo, Turno, Variedad) %>%
        mutate(
          tot_ha_reg = sum(Ha_Regular, na.rm = TRUE),
          Ha_Regular = if_else(
            tot_ha_reg > coalesce(Ha_Faltante_Turno_Neto, tot_ha_reg) & tot_ha_reg > 0,
            round(Ha_Regular * (coalesce(Ha_Faltante_Turno_Neto, tot_ha_reg) / tot_ha_reg), 2),
            Ha_Regular
          ),
          Rend_Valv_Regular = case_when(
            # Si una válvula era parte del cierre de S-1 y le quedó remanente por cosechar:
            Semana_Ant_No_Finalizo & Valvula_Cerro_En_Lun_Mar ~ round(ifelse(Rend_Ant_Turno > 0, Rend_Ant_Turno, Rend_Proy) * Factor_Floral, 2),
            # Para válvulas pendientes que NO eran parte del cierre de S-1, o cualquier otra válvula regular:
            TRUE ~ coalesce(Rend_Turno_Aplicar, 0)
          ),
          Kg_Asignado    = round(Ha_Regular * Rend_Valv_Regular, 2),
          Ha_Asignada    = Ha_Regular,
          Num_Pasada     = 1L,
          Es_Ruta_Sin_Kg = FALSE,
          Motivo_Ajuste  = case_when(
            Semana_Ant_No_Finalizo & Valvula_Cerro_En_Lun_Mar ~ paste0(
              ifelse(
                coalesce(Rend_Ant_Turno, 0) > 0,
                sprintf("Cierre S-1 incompleta (Rend S-1: %s kg/ha)", trimws(format(round(coalesce(Rend_Ant_Turno, 0)), big.mark = ","))),
                sprintf("Cierre S-1 incompleta (Rend proy: %s kg/ha)", trimws(format(round(coalesce(Rend_Proy, 0)), big.mark = ",")))
              ),
              " • ",
              case_when(
                Tipo_Efecto_Floral == "Premio"  ~ sprintf("Premio floral %+0.1f%%", round((Factor_Floral - 1) * 100, 1)),
                Tipo_Efecto_Floral == "Castigo" ~ sprintf("Castigo floral %+0.1f%%", round((Factor_Floral - 1) * 100, 1)),
                TRUE                            ~ "Floral neutro (0%)"
              )
            ),
            TRUE ~ Motivo_Ajuste
          )
        ) %>%
        ungroup() %>%
        filter(Kg_Asignado > 0, Ha_Regular > 0, Dia_Asignado %in% dias_ajustables_nom)
      
      # Componente 2: REPROGRAMACIÓN CON CASTIGO POR DÍAS DE CRECIMIENTO (Rama Izquierda: Ha cosechadas en Lunes/Martes)
      # NOTA: Solo aplica si la variedad es de ciclo corto (ej. Rocío, Frecuencia <= 4 días).
      # En variedades de frecuencia semanal (7 días como Mágica), la cosecha del lunes ya cubrió esa área para la semana y NO se repite el viernes.
      reg_2da_remanente <- valvulas_calc %>%
        filter(coalesce(Esta_Proyectado, FALSE), Semana_Ant_No_Finalizo, Es_Ciclo_Corto, Valvula_Cerro_En_Lun_Mar, Ha_Cerrada_Lun_Mar > 0) %>%
        mutate(
          Ha_2da = Ha_Cerrada_Lun_Mar,
          Dia_Cierre_Idx = ifelse(Valvula_Cosecho_Lunes, 1L, 2L),
          Dia_Asignado = case_when(
            Valvula_Cosecho_Lunes  ~ ifelse(dia_fin_idx >= 5L, "Viernes", dia_fin_nom), # Lunes (1) -> Viernes (5)
            Valvula_Cosecho_Martes ~ ifelse(dia_fin_idx >= 6L, "Sábado", dia_fin_nom),  # Martes (2) -> Sábado (6)
            TRUE                   ~ ifelse(dia_fin_idx >= 5L, "Viernes", dia_fin_nom)
          ),
          Dia_Nuevo_Idx = match(Dia_Asignado, dias_nombres),
          Dias_Crecimiento = pmax(1L, Dia_Nuevo_Idx - Dia_Cierre_Idx),
          Factor_Crecimiento = case_when(
            limpiar_variedad(Variedad) == "ROCIO" & Valvula_Cosecho_Lunes  ~ coalesce(Pct_Pasada_Lun, 60) / 100,
            limpiar_variedad(Variedad) == "ROCIO" & Valvula_Cosecho_Martes ~ coalesce(Pct_Pasada_Mar, 60) / 100,
            limpiar_variedad(Variedad) == "ROCIO"                          ~ coalesce(Pct_Pasada_Lun, 60) / 100,
            TRUE                                                           ~ pmin(1, Dias_Crecimiento / pmax(1, Frecuencia_Dias))
          ),
          Rend_Base_Dia  = ifelse(Valvula_Cosecho_Lunes, Rend_Valv_Lunes, Rend_Valv_Martes),
          Kg_Asignado    = round(Ha_2da * coalesce(Rend_Base_Dia, 0) * Factor_Crecimiento, 2),
          Ha_Asignada    = Ha_2da,
          Num_Pasada     = 2L,
          Es_Ruta_Sin_Kg = FALSE,
          Motivo_Ajuste  = sprintf("2da pasada Cierre S-1 (%s • %d%% sin floración)", 
                                   ifelse(Valvula_Cosecho_Lunes, "Lunes", "Martes"), round(Factor_Crecimiento * 100))
        ) %>%
        filter(Kg_Asignado > 0, Dia_Asignado %in% dias_ajustables_nom)
      
      # Componente 3A: SEGUNDA PASADA PARA COSECHA DEL LUNES (Rama Derecha -> Lun a Jue, ej. Rocío 3 días)
      reg_2da_lun <- valvulas_calc %>%
        filter(
          coalesce(Esta_Proyectado, FALSE),
          !Semana_Ant_No_Finalizo,
          Es_Ciclo_Corto,
          Ha_Cosechada_Lunes > 0
        ) %>%
        mutate(
          Ha_2da_Corta   = Ha_Cosechada_Lunes,
          Dia_Base_Frec  = ifelse(codigo_norm %in% rectif_martes_ids, "Martes", "Lunes"),
          Dia_Asignado   = as.character(desplazar_dia_frecuencia(Dia_Base_Frec, Frecuencia_Dias, dias_nombres, dia_inicio_idx, dia_inicio_nom, dia_fin_idx, dia_fin_nom)),
          Pct_Rend       = if_else(limpiar_variedad(Variedad) == "ROCIO", coalesce(Pct_Pasada_Lun, 60) / 100, 1.0),
          # REGLA: Rendimiento de las válvulas cosechadas en Lunes x 60% SIN PREMIO/CASTIGO FLORAL
          Rend_2da_Aplicar = round(Rend_Valv_Lunes * Pct_Rend, 2),
          Kg_Asignado    = round(Ha_2da_Corta * Rend_2da_Aplicar, 2),
          Ha_Asignada    = Ha_2da_Corta,
          Num_Pasada     = 2L,
          Es_Ruta_Sin_Kg = FALSE,
          Motivo_Ajuste  = sprintf("2da pasada Rocío (Rend Lunes: %s kg/ha • %d%% sin floración)", 
                                   format(round(Rend_Valv_Lunes), big.mark = ","), round(Pct_Rend * 100))
        ) %>%
        filter(Kg_Asignado > 0, Dia_Asignado %in% dias_ajustables_nom)
      
      # Componente 3B: SEGUNDA PASADA PARA COSECHA DEL MARTES (Rama Derecha -> Mar a Vie, ej. Rocío 3 días)
      reg_2da_mar <- valvulas_calc %>%
        filter(
          coalesce(Esta_Proyectado, FALSE),
          !Semana_Ant_No_Finalizo,
          Es_Ciclo_Corto,
          Ha_Cosechada_Martes > 0
        ) %>%
        mutate(
          Ha_2da_Corta   = Ha_Cosechada_Martes,
          Dia_Asignado   = as.character(desplazar_dia_frecuencia("Martes", Frecuencia_Dias, dias_nombres, dia_inicio_idx, dia_inicio_nom, dia_fin_idx, dia_fin_nom)),
          Pct_Rend       = if_else(limpiar_variedad(Variedad) == "ROCIO", coalesce(Pct_Pasada_Mar, 60) / 100, 1.0),
          # REGLA: Rendimiento de las válvulas cosechadas en Martes x 60% SIN PREMIO/CASTIGO FLORAL
          Rend_2da_Aplicar = round(Rend_Valv_Martes * Pct_Rend, 2),
          Kg_Asignado    = round(Ha_2da_Corta * Rend_2da_Aplicar, 2),
          Ha_Asignada    = Ha_2da_Corta,
          Num_Pasada     = 2L,
          Es_Ruta_Sin_Kg = FALSE,
          Motivo_Ajuste  = sprintf("2da pasada Rocío (Rend Martes: %s kg/ha • %d%% sin floración)", 
                                   format(round(Rend_Valv_Martes), big.mark = ","), round(Pct_Rend * 100))
        ) %>%
        filter(Kg_Asignado > 0, Dia_Asignado %in% dias_ajustables_nom)
      
      # Componente 3C: SEGUNDA PASADA PARA COSECHA MANUAL DE MIÉRCOLES (Rocío -> Sábado)
      reg_2da_mie <- if (nrow(ed_man_sem) > 0 && "Miércoles" %in% ed_man_sem$Campo && dia_fin_idx >= 6L) {
        valvulas_calc %>%
          filter(
            coalesce(Esta_Proyectado, FALSE),
            limpiar_variedad(Variedad) == "ROCIO",
            Módulo %in% ed_man_sem$Módulo[ed_man_sem$Campo == "Miércoles"],
            Turno %in% ed_man_sem$Turno[ed_man_sem$Campo == "Miércoles"]
          ) %>%
          left_join(
            ed_man_sem %>%
              filter(Campo == "Miércoles", coalesce(Ha_Asignada_1ra, 0) > 0) %>%
              group_by(Módulo = trimws(as.character(Módulo)), Turno = trimws(as.character(Turno)), Variedad = limpiar_variedad(Variedad)) %>%
              summarise(Ha_Mie_Man = sum(Ha_Asignada_1ra, na.rm = TRUE), Rend_Mie_Man = sum(Valor, na.rm = TRUE) / pmax(0.1, sum(Ha_Asignada_1ra, na.rm = TRUE)), .groups = "drop"),
            by = c("Módulo", "Turno", "Variedad")
          ) %>%
          filter(coalesce(Ha_Mie_Man, 0) > 0) %>%
          group_by(Módulo, Turno, Variedad) %>%
          mutate(
            Ha_2da_Corta     = round(pmin(Área, Ha_Mie_Man / n()), 2),
            Dia_Asignado     = "Sábado",
            Pct_Rend         = coalesce(Pct_Pasada_Mar, 60) / 100,
            Rend_2da_Aplicar = round(Rend_Mie_Man * Pct_Rend, 2),
            Kg_Asignado      = round(Ha_2da_Corta * Rend_2da_Aplicar, 2),
            Ha_Asignada      = Ha_2da_Corta,
            Num_Pasada       = 2L,
            Es_Ruta_Sin_Kg   = FALSE,
            Motivo_Ajuste    = sprintf("2da pasada Rocío (Rend Miércoles Manual: %s kg/ha • %d%% sin floración)",
                                       format(round(Rend_Mie_Man), big.mark = ","), round(Pct_Rend * 100))
          ) %>%
          ungroup() %>%
          filter(Kg_Asignado > 0, Dia_Asignado %in% dias_ajustables_nom)
      } else {
        tibble(id = character(), codigo = character(), Sector = character(), Módulo = character(), Turno = character(), Variedad = character(), Dia_Asignado = character(), Kg_Asignado = numeric(), Ha_Asignada = numeric(), Num_Pasada = integer(), Es_Ruta_Sin_Kg = logical(), Motivo_Ajuste = character())
      }

      # Unificar todas las pasadas, asegurar tope [dia_inicio_nom, dia_fin_nom] y homologar sector
      registros_todos <- bind_rows(
        reg_ruta_lun %>% select(id, codigo, Sector, Módulo, Turno, Variedad, Dia_Asignado, Kg_Asignado, Ha_Asignada, Num_Pasada, Es_Ruta_Sin_Kg, Motivo_Ajuste),
        reg_ruta_mar %>% select(id, codigo, Sector, Módulo, Turno, Variedad, Dia_Asignado, Kg_Asignado, Ha_Asignada, Num_Pasada, Es_Ruta_Sin_Kg, Motivo_Ajuste),
        reg_regular %>% select(id, codigo, Sector, Módulo, Turno, Variedad, Dia_Asignado, Kg_Asignado, Ha_Asignada, Num_Pasada, Es_Ruta_Sin_Kg, Motivo_Ajuste),
        reg_2da_remanente %>% select(id, codigo, Sector, Módulo, Turno, Variedad, Dia_Asignado, Kg_Asignado, Ha_Asignada, Num_Pasada, Es_Ruta_Sin_Kg, Motivo_Ajuste),
        reg_2da_lun %>% select(id, codigo, Sector, Módulo, Turno, Variedad, Dia_Asignado, Kg_Asignado, Ha_Asignada, Num_Pasada, Es_Ruta_Sin_Kg, Motivo_Ajuste),
        reg_2da_mar %>% select(id, codigo, Sector, Módulo, Turno, Variedad, Dia_Asignado, Kg_Asignado, Ha_Asignada, Num_Pasada, Es_Ruta_Sin_Kg, Motivo_Ajuste),
        if (nrow(reg_2da_mie) > 0) reg_2da_mie %>% select(id, codigo, Sector, Módulo, Turno, Variedad, Dia_Asignado, Kg_Asignado, Ha_Asignada, Num_Pasada, Es_Ruta_Sin_Kg, Motivo_Ajuste) else tibble()
      ) %>%
        mutate(
          idx_d = match(Dia_Asignado, dias_nombres),
          Dia_Asignado = case_when(
            !Es_Ruta_Sin_Kg & !is.na(idx_d) & idx_d > dia_fin_idx    ~ dia_fin_nom,
            !Es_Ruta_Sin_Kg & !is.na(idx_d) & idx_d < dia_inicio_idx ~ dia_inicio_nom,
            TRUE                                                     ~ Dia_Asignado
          )
        ) %>%
        select(-idx_d)

      # Guardar el detalle de válvulas asignadas para la vista de mapa de ruta proyectada
      df_valvs_ajuste_nueva <- registros_todos %>%
        filter(Kg_Asignado > 0, Es_Ruta_Sin_Kg | Dia_Asignado %in% dias_ajustables_nom) %>%
        mutate(
          Campaña         = as.character(input$filtro_campana),
          Semana          = as.numeric(sem_sel),
          Dia_Desde       = as.character(dia_inicio_nom),
          Dia_Hasta       = as.character(dia_fin_nom),
          Tipo_Asignacion = as.character(case_when(
            coalesce(Es_Ruta_Sin_Kg, FALSE) ~ "ruta_sin_kg",
            coalesce(Num_Pasada, 1L) == 2L  ~ "2da_pasada",
            TRUE                            ~ "regular"
          )),
          Motivo_Ajuste   = as.character(Motivo_Ajuste),
          Fecha_Ajuste    = Sys.time()
        ) %>%
        select(Campaña, Semana, id, codigo, Sector, Módulo, Turno, Variedad, Dia_Asignado, Kg_Asignado, Ha_Asignada, Num_Pasada, Tipo_Asignacion, Dia_Desde, Dia_Hasta, Motivo_Ajuste, Fecha_Ajuste)

      valvs_anteriores <- rv_ajuste_valvulas()
      valvs_filtradas <- if (!is.null(valvs_anteriores) && is.data.frame(valvs_anteriores) && nrow(valvs_anteriores) > 0) {
        if (solo_filtrado) {
          claves_valvs_ant <- paste(
            trimws(as.character(valvs_anteriores$Módulo)),
            trimws(as.character(valvs_anteriores$Turno)),
            limpiar_variedad(valvs_anteriores$Variedad),
            sep = "___"
          )
          valvs_anteriores %>%
            filter(!(as.character(Campaña) == as.character(input$filtro_campana) &
                     as.numeric(Semana)    == as.numeric(sem_sel) &
                     claves_valvs_ant %in% claves_filtro))
        } else {
          valvs_anteriores %>%
            filter(!(as.character(Campaña) == as.character(input$filtro_campana) & as.numeric(Semana) == as.numeric(sem_sel)))
        }
      } else {
        tibble()
      }
      valvs_guardar <- if (nrow(df_valvs_ajuste_nueva) > 0 && nrow(valvs_filtradas) > 0) {
        bind_rows(valvs_filtradas, df_valvs_ajuste_nueva)
      } else if (nrow(df_valvs_ajuste_nueva) > 0) {
        df_valvs_ajuste_nueva
      } else {
        valvs_filtradas
      }
      rv_ajuste_valvulas(valvs_guardar)
      guardar_ajuste_valvulas_disco(valvs_guardar)
      
      registros_homologados <- registros_todos %>%
        mutate(
          Módulo_norm = trimws(as.character(Módulo)),
          Turno_norm  = trimws(as.character(Turno)),
          Var_norm    = limpiar_variedad(Variedad)
        ) %>%
        left_join(sectores_u, by = c("Módulo_norm", "Turno_norm", "Var_norm")) %>%
        mutate(Sector = coalesce(Sector_Visual, trimws(as.character(Sector)))) %>%
        select(-Módulo_norm, -Turno_norm, -Var_norm, -Sector_Visual)
      
      auto_ediciones <- registros_homologados %>%
        filter(Kg_Asignado > 0, Es_Ruta_Sin_Kg | Dia_Asignado %in% dias_ajustables_nom) %>%
        group_by(Sector, Módulo, Turno, Variedad, Campo = Dia_Asignado) %>%
        summarise(
          Valor           = round(sum(Kg_Asignado, na.rm = TRUE), 2),
          Ha_Asignada_1ra = round(sum(ifelse(coalesce(Num_Pasada, 1L) == 1L, Ha_Asignada, 0), na.rm = TRUE), 2),
          Kg_Asignado_1ra = round(sum(ifelse(coalesce(Num_Pasada, 1L) == 1L, Kg_Asignado, 0), na.rm = TRUE), 2),
          Ha_Asignada_2da = round(sum(ifelse(coalesce(Num_Pasada, 1L) == 2L, Ha_Asignada, 0), na.rm = TRUE), 2),
          Kg_Asignado_2da = round(sum(ifelse(coalesce(Num_Pasada, 1L) == 2L, Kg_Asignado, 0), na.rm = TRUE), 2),
          Ha_Asignada     = round(sum(Ha_Asignada, na.rm = TRUE), 2),
          Tipo_Edicion    = as.character(ifelse(any(Es_Ruta_Sin_Kg, na.rm = TRUE), "ruta_sin_kg", "auto")),
          Motivo_Ajuste   = as.character(first(na.omit(Motivo_Ajuste))),
          .groups         = "drop"
        ) %>%
        mutate(
          Campaña       = as.character(input$filtro_campana),
          Semana        = sem_sel,
          Fecha_Edicion = Sys.time()
        )
      
      incProgress(0.3, detail = "Guardando ediciones y actualizando tabla...")
      
      ed_actuales <- ediciones_manuales()
      
      ed_manuales_existentes <- ed_actuales %>%
        filter(
          as.character(Campaña) == as.character(input$filtro_campana),
          as.numeric(Semana)    == sem_sel,
          Tipo_Edicion          == "manual"
        )
      
      claves_manuales <- paste(
        trimws(as.character(ed_manuales_existentes$Sector)),
        trimws(as.character(ed_manuales_existentes$Módulo)),
        trimws(as.character(ed_manuales_existentes$Turno)),
        limpiar_variedad(ed_manuales_existentes$Variedad),
        trimws(as.character(ed_manuales_existentes$Campo)),
        sep = "___"
      )
      claves_manuales_mtv <- paste(
        trimws(as.character(ed_manuales_existentes$Módulo)),
        trimws(as.character(ed_manuales_existentes$Turno)),
        limpiar_variedad(ed_manuales_existentes$Variedad),
        trimws(as.character(ed_manuales_existentes$Campo)),
        sep = "___"
      )
      
      claves_sap_bloqueadas <- character(0)
      claves_sap_bloqueadas_mtv <- character(0)
      if (!is.null(u_base) && nrow(u_base) > 0) {
        for (dia_col in dias_nombres) {
          lock_col <- paste0(dia_col, "_bloqueado")
          if (lock_col %in% names(u_base)) {
            bloq_idx <- which(coalesce(u_base[[lock_col]], FALSE) == TRUE)
            if (length(bloq_idx) > 0) {
              claves_sap_bloqueadas <- c(
                claves_sap_bloqueadas,
                paste(
                  trimws(as.character(u_base$Sector[bloq_idx])),
                  trimws(as.character(u_base$Módulo[bloq_idx])),
                  trimws(as.character(u_base$Turno[bloq_idx])),
                  limpiar_variedad(u_base$Variedad[bloq_idx]),
                  dia_col,
                  sep = "___"
                )
              )
              claves_sap_bloqueadas_mtv <- c(
                claves_sap_bloqueadas_mtv,
                paste(
                  trimws(as.character(u_base$Módulo[bloq_idx])),
                  trimws(as.character(u_base$Turno[bloq_idx])),
                  limpiar_variedad(u_base$Variedad[bloq_idx]),
                  dia_col,
                  sep = "___"
                )
              )
            }
          }
        }
      }
      
      claves_auto <- paste(
        trimws(as.character(auto_ediciones$Sector)),
        trimws(as.character(auto_ediciones$Módulo)),
        trimws(as.character(auto_ediciones$Turno)),
        limpiar_variedad(auto_ediciones$Variedad),
        trimws(as.character(auto_ediciones$Campo)),
        sep = "___"
      )
      claves_auto_mtv <- paste(
        trimws(as.character(auto_ediciones$Módulo)),
        trimws(as.character(auto_ediciones$Turno)),
        limpiar_variedad(auto_ediciones$Variedad),
        trimws(as.character(auto_ediciones$Campo)),
        sep = "___"
      )
      
      auto_validas <- auto_ediciones[!(
        claves_auto %in% claves_manuales |
          claves_auto_mtv %in% claves_manuales_mtv |
          claves_auto %in% claves_sap_bloqueadas |
          claves_auto_mtv %in% claves_sap_bloqueadas_mtv
      ), ]
      
      if (nrow(auto_validas) == 0) {
        msg_v <- if (solo_filtrado) "No hay válvulas o días pendientes para autocompletar en la selección filtrada." else "No hay válvulas o días pendientes para autocompletar en esta semana."
        showNotification(msg_v, type = "warning", duration = 4)
        return()
      }
      
      if (solo_filtrado) {
        claves_ed_act <- paste(
          trimws(as.character(ed_actuales$Módulo)),
          trimws(as.character(ed_actuales$Turno)),
          limpiar_variedad(ed_actuales$Variedad),
          sep = "___"
        )
        ed_conservar <- ed_actuales %>%
          filter(!(
            as.character(Campaña) == as.character(input$filtro_campana) &
              as.numeric(Semana)    == sem_sel &
              (is.na(Tipo_Edicion) | Tipo_Edicion %in% c("auto", "ruta_sin_kg")) &
              claves_ed_act %in% claves_filtro
          ))
      } else {
        ed_conservar <- ed_actuales %>%
          filter(!(
            as.character(Campaña) == as.character(input$filtro_campana) &
              as.numeric(Semana)    == sem_sel &
              (is.na(Tipo_Edicion) | Tipo_Edicion %in% c("auto", "ruta_sin_kg"))
          ))
      }
      
      ed_nuevas_totales <- if (nrow(auto_validas) > 0 && nrow(ed_conservar) > 0) {
        bind_rows(ed_conservar, auto_validas)
      } else if (nrow(auto_validas) > 0) {
        auto_validas
      } else {
        ed_conservar
      }
      
      ediciones_manuales(ed_nuevas_totales)
      guardar_ediciones_disco(ed_nuevas_totales)
      
      n_premios  <- sum(rend_turno$Tipo_Efecto_Floral == "Premio" & rend_turno$Esta_Proyectado & !(rend_turno$Tiene_Cosecha_Act & rend_turno$Prop_Avance_Turno > 0.30), na.rm = TRUE)
      n_castigos <- sum(rend_turno$Tipo_Efecto_Floral == "Castigo" & rend_turno$Esta_Proyectado & !(rend_turno$Tiene_Cosecha_Act & rend_turno$Prop_Avance_Turno > 0.30), na.rm = TRUE)
      txt_floral_info <- if (n_premios > 0 || n_castigos > 0) {
        sprintf(" • Tendencia floral (%d%% consideración): %d premios, %d castigos", as.integer(sens_floral_pct), n_premios, n_castigos)
      } else {
        ""
      }
      
      n_frec_2sem <- if (exists("df_frec_valvs_2sem") && !is.null(df_frec_valvs_2sem) && nrow(df_frec_valvs_2sem) > 0) {
        sum(df_frec_valvs_2sem$Tiene_Intervalo_2S, na.rm = TRUE)
      } else 0
      txt_frec_info <- if (n_frec_2sem > 0) sprintf(" • %d válvulas con frecuencia real 2S (S-2/S-1)", n_frec_2sem) else ""
      
      txt_alcance <- if (solo_filtrado) "en la selección filtrada" else "en toda la semana"
      showNotification(
        sprintf("Se completaron automáticamente %d turnos proyectados %s de %s a %s (%s Kg)%s%s.", 
                nrow(auto_validas), txt_alcance, dia_inicio_nom, dia_fin_nom, format(round(sum(auto_validas$Valor), 1), big.mark = ","), txt_floral_info, txt_frec_info),
        type = "message",
        duration = 6
      )
    })
  }

  # BOTÓN AJUSTAR SEMANA
  observeEvent(input$btn_ajustar_semana, {
    permitir_cualquier_sem <- is.null(input$switch_ajuste_cualquier_semana) || isTRUE(input$switch_ajuste_cualquier_semana)
    es_campana_act <- grepl("2026|^26", as.character(input$filtro_campana))
    sem_actual_num <- lubridate::isoweek(Sys.Date())
    es_semana_act  <- (as.numeric(input$filtro_semana) == sem_actual_num) || permitir_cualquier_sem
    
    if (!es_campana_act) {
      showNotification("El ajuste automático solo está habilitado para la campaña 2026.", type = "warning", duration = 4)
      return()
    }
    
    if (!es_semana_act) {
      showNotification("El ajuste automático está restringido a la semana actual en curso. Activa 'Ajustar cualquier semana' en el menú (⋮) para habilitarlo.", type = "warning", duration = 5)
      return()
    }
    
    if (hay_filtro_tabla_activo()) {
      ejecutar_ajuste_semana(solo_filtrado = TRUE)
    } else {
      sem_sel  <- as.numeric(input$filtro_semana)
      camp_sel <- as.character(input$filtro_campana)
      showModal(modalDialog(
        title = div(
          style = "display: flex; align-items: center; gap: 10px;",
          div(
            style = "width: 36px; height: 36px; border-radius: 10px; background: rgba(255, 149, 0, 0.15); color: #FF9500; display: flex; align-items: center; justify-content: center; font-size: 16px;",
            icon("calculator")
          ),
          div(
            div(style = "font-size: 15px; font-weight: 600; color: #18181b;", "Ajustar Toda la Semana"),
            div(style = "font-size: 12px; color: #71717a;", sprintf("Semana %s • Campaña %s", sem_sel, camp_sel))
          )
        ),
        div(
          style = "font-size: 13px; color: #3f3f46; line-height: 1.5; padding: 10px 0;",
          tags$p(
            tags$b("No tienes ningún filtro aplicado."),
            " Esta acción calculará y proyectará el ajuste automático para ", tags$strong("todos los sectores y módulos de la semana"), "."
          ),
          tags$p(style = "color: #71717a; font-size: 12px; margin-bottom: 0;",
            "Las celdas con ediciones manuales existentes o cosecha confirmada en SAP se conservarán intactas. ¿Deseas continuar?"
          )
        ),
        footer = tagList(
          modalButton("Cancelar"),
          actionButton("btn_confirmar_ajustar_todo", "Sí, ajustar todo", class = "ek-btn-primary")
        ),
        size = "m",
        easyClose = TRUE
      ))
    }
  })

  observeEvent(input$btn_confirmar_ajustar_todo, {
    removeModal()
    ejecutar_ajuste_semana(solo_filtrado = FALSE)
  })

  # ============================================================================== =
  # BOTÓN ↺ DESHACER AJUSTE AUTOMÁTICO
  # ============================================================================== =
  ejecutar_deshacer_ajuste <- function(solo_filtrado = TRUE) {
    sem_sel  <- as.numeric(input$filtro_semana)
    camp_sel <- as.character(input$filtro_campana)
    
    ed_actuales <- ediciones_manuales()
    if (is.null(ed_actuales) || nrow(ed_actuales) == 0) {
      showNotification("No hay ajustes automáticos para remover.", type = "message", duration = 3)
      return()
    }
    
    es_auto_sem <- as.character(ed_actuales$Campaña) == camp_sel &
                   as.numeric(ed_actuales$Semana) == sem_sel &
                   (is.na(ed_actuales$Tipo_Edicion) | ed_actuales$Tipo_Edicion %in% c("auto", "ruta_sin_kg"))
    
    if (solo_filtrado) {
      turnos_filtro <- turnos_activos_filtrados()
      if (is.null(turnos_filtro) || nrow(turnos_filtro) == 0) {
        showNotification("No hay turnos visibles en el filtro actual.", type = "warning", duration = 3)
        return()
      }
      claves_filtro <- paste(
        trimws(as.character(turnos_filtro$Módulo)),
        trimws(as.character(turnos_filtro$Turno)),
        limpiar_variedad(turnos_filtro$Variedad),
        sep = "___"
      )
      claves_ed <- paste(
        trimws(as.character(ed_actuales$Módulo)),
        trimws(as.character(ed_actuales$Turno)),
        limpiar_variedad(ed_actuales$Variedad),
        sep = "___"
      )
      a_borrar <- es_auto_sem & (claves_ed %in% claves_filtro)
    } else {
      a_borrar <- es_auto_sem
    }
    
    n_removidos <- sum(a_borrar)
    if (n_removidos == 0) {
      showNotification("No se encontraron ajustes automáticos en la selección.", type = "message", duration = 3)
      return()
    }
    
    ed_conservar <- ed_actuales[!a_borrar, ]
    ediciones_manuales(ed_conservar)
    guardar_ediciones_disco(ed_conservar)
    
    valvs_act <- rv_ajuste_valvulas()
    if (!is.null(valvs_act) && is.data.frame(valvs_act) && nrow(valvs_act) > 0) {
      es_valv_sem <- as.character(valvs_act$Campaña) == camp_sel & as.numeric(valvs_act$Semana) == sem_sel
      if (solo_filtrado) {
        claves_valvs <- paste(
          trimws(as.character(valvs_act$Módulo)),
          trimws(as.character(valvs_act$Turno)),
          limpiar_variedad(valvs_act$Variedad),
          sep = "___"
        )
        valvs_conservar <- valvs_act[!(es_valv_sem & (claves_valvs %in% claves_filtro)), ]
      } else {
        valvs_conservar <- valvs_act[!es_valv_sem, ]
      }
      rv_ajuste_valvulas(valvs_conservar)
      guardar_ajuste_valvulas_disco(valvs_conservar)
    }
    
    showNotification(
      sprintf("↺ Se removieron %d registros automáticos%s. Las ediciones manuales se conservaron intactas.",
              n_removidos,
              if (solo_filtrado) " de la selección filtrada" else " de toda la semana"),
      type = "message",
      duration = 4
    )
  }

  observeEvent(input$btn_deshacer_ajuste, {
    sem_sel  <- as.numeric(input$filtro_semana)
    camp_sel <- as.character(input$filtro_campana)
    
    ed_actuales <- ediciones_manuales()
    es_auto_sem <- if (!is.null(ed_actuales) && nrow(ed_actuales) > 0) {
      as.character(ed_actuales$Campaña) == camp_sel &
      as.numeric(ed_actuales$Semana) == sem_sel &
      (is.na(ed_actuales$Tipo_Edicion) | ed_actuales$Tipo_Edicion %in% c("auto", "ruta_sin_kg"))
    } else logical(0)
    
    if (!any(es_auto_sem)) {
      showNotification("No hay ajustes automáticos para remover en esta semana.", type = "message", duration = 3)
      return()
    }
    
    if (hay_filtro_tabla_activo()) {
      ejecutar_deshacer_ajuste(solo_filtrado = TRUE)
    } else {
      showModal(modalDialog(
        title = div(
          style = "display: flex; align-items: center; gap: 10px;",
          div(
            style = "width: 36px; height: 36px; border-radius: 10px; background: rgba(225, 29, 72, 0.12); color: #e11d48; display: flex; align-items: center; justify-content: center; font-size: 16px;",
            icon("rotate-left")
          ),
          div(
            div(style = "font-size: 15px; font-weight: 600; color: #18181b;", "Deshacer Todos los Ajustes Automáticos"),
            div(style = "font-size: 12px; color: #71717a;", sprintf("Semana %s • Campaña %s", sem_sel, camp_sel))
          )
        ),
        div(
          style = "font-size: 13px; color: #3f3f46; line-height: 1.5; padding: 10px 0;",
          tags$p(
            tags$b("No tienes ningún filtro aplicado."),
            " Esta acción eliminará los ", tags$strong("ajustes automáticos de toda la semana"), sprintf(" (%d registros)", sum(es_auto_sem)), " para todos los sectores y módulos."
          ),
          tags$p(style = "color: #71717a; font-size: 12px; margin-bottom: 0;",
            "Las ediciones manuales que hayas ingresado se conservarán intactas. ¿Deseas continuar?"
          )
        ),
        footer = tagList(
          modalButton("Cancelar"),
          actionButton("btn_confirmar_deshacer_todo", "Sí, deshacer todos", class = "ek-btn-danger")
        ),
        size = "m",
        easyClose = TRUE
      ))
    }
  })

  observeEvent(input$btn_confirmar_deshacer_todo, {
    removeModal()
    ejecutar_deshacer_ajuste(solo_filtrado = FALSE)
  })

  # ============================================================================== =
  # BOTÓN 🗑️ BORRAR AJUSTES MANUALES
  # ============================================================================== =
  ejecutar_borrado_manuales <- function(solo_filtrado = TRUE) {
    camp_sel <- as.character(input$filtro_campana)
    sem_sel  <- as.numeric(input$filtro_semana)
    
    ed_actuales <- ediciones_manuales()
    if (is.null(ed_actuales) || nrow(ed_actuales) == 0) {
      showNotification("No hay ajustes manuales para remover en esta semana.", type = "message", duration = 3)
      return()
    }
    
    es_manual_sem <- as.character(ed_actuales$Campaña) == camp_sel &
                     as.numeric(ed_actuales$Semana) == sem_sel &
                     coalesce(ed_actuales$Tipo_Edicion, "manual") == "manual"
    
    if (!any(es_manual_sem)) {
      showNotification("No se encontraron ajustes manuales en esta semana.", type = "message", duration = 3)
      return()
    }
    
    if (solo_filtrado) {
      turnos_filtro <- turnos_activos_filtrados()
      if (is.null(turnos_filtro) || nrow(turnos_filtro) == 0) {
        showNotification("No hay turnos visibles en el filtro actual.", type = "warning", duration = 3)
        return()
      }
      claves_filtro <- paste(
        trimws(as.character(turnos_filtro$Módulo)),
        trimws(as.character(turnos_filtro$Turno)),
        limpiar_variedad(turnos_filtro$Variedad),
        sep = "___"
      )
      claves_ed <- paste(
        trimws(as.character(ed_actuales$Módulo)),
        trimws(as.character(ed_actuales$Turno)),
        limpiar_variedad(ed_actuales$Variedad),
        sep = "___"
      )
      a_borrar <- es_manual_sem & (claves_ed %in% claves_filtro)
    } else {
      a_borrar <- es_manual_sem
    }
    
    n_a_borrar <- sum(a_borrar)
    if (n_a_borrar == 0) {
      showNotification("No se encontraron ajustes manuales en la selección filtrada.", type = "message", duration = 3)
      return()
    }
    
    ed_a_eliminar <- ed_actuales[a_borrar, ]
    ed_conservar  <- ed_actuales[!a_borrar, ]
    
    # Restaurar valores automáticos de rv_ajuste_valvulas si existían para las celdas eliminadas
    valvs_aj <- rv_ajuste_valvulas()
    reg_restaurados <- tibble()
    if (!is.null(valvs_aj) && is.data.frame(valvs_aj) && nrow(valvs_aj) > 0) {
      valvs_sem <- valvs_aj %>%
        filter(as.character(Campaña) == camp_sel, as.numeric(Semana) == sem_sel)
      
      if (nrow(valvs_sem) > 0) {
        claves_celdas_elim <- paste(
          trimws(as.character(ed_a_eliminar$Módulo)),
          trimws(as.character(ed_a_eliminar$Turno)),
          limpiar_variedad(ed_a_eliminar$Variedad),
          trimws(as.character(ed_a_eliminar$Campo)),
          sep = "___"
        )
        claves_valvs <- paste(
          trimws(as.character(valvs_sem$Módulo)),
          trimws(as.character(valvs_sem$Turno)),
          limpiar_variedad(valvs_sem$Variedad),
          trimws(as.character(valvs_sem$Dia_Asignado)),
          sep = "___"
        )
        valvs_restaurar <- valvs_sem[claves_valvs %in% claves_celdas_elim, ]
        if (nrow(valvs_restaurar) > 0) {
          reg_restaurados <- valvs_restaurar %>%
            group_by(Sector, Módulo, Turno, Variedad, Campo = Dia_Asignado) %>%
            summarise(
              Valor           = round(sum(Kg_Asignado, na.rm = TRUE), 2),
              Ha_Asignada     = round(sum(Ha_Asignada, na.rm = TRUE), 2),
              Ha_Asignada_1ra = round(sum(ifelse(coalesce(Num_Pasada, 1L) == 1L, Ha_Asignada, 0), na.rm = TRUE), 2),
              Kg_Asignado_1ra = round(sum(ifelse(coalesce(Num_Pasada, 1L) == 1L, Kg_Asignado, 0), na.rm = TRUE), 2),
              Ha_Asignada_2da = round(sum(ifelse(coalesce(Num_Pasada, 1L) == 2L, Ha_Asignada, 0), na.rm = TRUE), 2),
              Kg_Asignado_2da = round(sum(ifelse(coalesce(Num_Pasada, 1L) == 2L, Kg_Asignado, 0), na.rm = TRUE), 2),
              Tipo_Edicion    = "auto",
              Motivo_Ajuste   = first(na.omit(Motivo_Ajuste)),
              .groups         = "drop"
            ) %>%
            mutate(
              Campaña       = camp_sel,
              Semana        = sem_sel,
              Fecha_Edicion = Sys.time()
            )
        }
      }
    }
    
    ed_finales <- if (nrow(reg_restaurados) > 0 && nrow(ed_conservar) > 0) {
      bind_rows(ed_conservar, reg_restaurados)
    } else if (nrow(reg_restaurados) > 0) {
      reg_restaurados
    } else {
      ed_conservar
    }
    ediciones_manuales(ed_finales)
    guardar_ediciones_disco(ed_finales)
    
    txt_rest <- if (nrow(reg_restaurados) > 0) sprintf(" (Se restauraron %d valores a su estado automático)", nrow(reg_restaurados)) else ""
    showNotification(
      sprintf("🗑️ Se eliminaron %d ajustes manuales%s.%s",
              n_a_borrar,
              if (solo_filtrado) " de la selección filtrada" else " de toda la semana",
              txt_rest),
      type = "message",
      duration = 4
    )
  }

  observeEvent(input$btn_borrar_manuales, {
    sem_sel  <- as.numeric(input$filtro_semana)
    camp_sel <- as.character(input$filtro_campana)
    
    ed_actuales <- ediciones_manuales()
    es_manual_sem <- if (!is.null(ed_actuales) && nrow(ed_actuales) > 0) {
      as.character(ed_actuales$Campaña) == camp_sel &
      as.numeric(ed_actuales$Semana) == sem_sel &
      coalesce(ed_actuales$Tipo_Edicion, "manual") == "manual"
    } else logical(0)
    
    if (!any(es_manual_sem)) {
      showNotification("No hay ajustes manuales registrados en esta semana.", type = "message", duration = 3)
      return()
    }
    
    if (hay_filtro_tabla_activo()) {
      ejecutar_borrado_manuales(solo_filtrado = TRUE)
    } else {
      showModal(modalDialog(
        title = div(
          style = "display: flex; align-items: center; gap: 10px;",
          div(
            style = "width: 36px; height: 36px; border-radius: 10px; background: rgba(225, 29, 72, 0.12); color: #e11d48; display: flex; align-items: center; justify-content: center; font-size: 16px;",
            icon("trash-can")
          ),
          div(
            div(style = "font-size: 15px; font-weight: 600; color: #18181b;", "Borrar Todos los Ajustes Manuales"),
            div(style = "font-size: 12px; color: #71717a;", sprintf("Semana %s • Campaña %s", sem_sel, camp_sel))
          )
        ),
        div(
          style = "font-size: 13px; color: #3f3f46; line-height: 1.5; padding: 10px 0;",
          tags$p(
            tags$b("No tienes ningún filtro aplicado."),
            " Esta acción eliminará ", tags$strong("todos los ajustes manuales"), sprintf(" (%d registrados)", sum(es_manual_sem)), " de toda la semana."
          ),
          tags$p(style = "color: #71717a; font-size: 12px; margin-bottom: 0;",
            "Si algún turno contaba previamente con cálculo automático de válvulas, se restaurará su valor automático original. ¿Deseas continuar?"
          )
        ),
        footer = tagList(
          modalButton("Cancelar"),
          actionButton("btn_confirmar_borrar_manuales_todo", "Sí, borrar todos", class = "ek-btn-danger")
        ),
        size = "m",
        easyClose = TRUE
      ))
    }
  })

  observeEvent(input$btn_confirmar_borrar_manuales_todo, {
    removeModal()
    ejecutar_borrado_manuales(solo_filtrado = FALSE)
  })
  
  # ==============================================================================
  # DATOS HISTÓRICOS + ÁREA CONSOLIDADA POR TURNO (PARA KG Y KG/HA)
  # ==============================================================================
  datos_historicos_universo <- reactive({
    req(input$filtro_campana, input$filtro_semana)
    
    sem_sel <- as.numeric(input$filtro_semana)
    ord_sel <- orden_semana_campana(sem_sel)
    es_campana_2026 <- grepl("2026|^26", as.character(input$filtro_campana))
    
    df_hist <- rv_cosecha() %>%
      filter(
        as.character(Campaña) == as.character(input$filtro_campana),
        orden_semana_campana(Semana) < ord_sel,
        !coalesce(Es_Testblock, FALSE)
      )
    
    df_hist <- df_hist %>%
      mutate(
        Turno_num = suppressWarnings(as.numeric(Turno)),
        Sector    = case_when(as.character(Módulo) %in% c("9", "9.1", "9.2", "11", "11.1", "11.2", "12") ~ "9", TRUE ~ trimws(as.character(Sector))),
        Módulo    = trimws(as.character(Módulo)),
        Módulo    = case_when(
          es_campana_2026 & Módulo %in% c("13.1", "13.2") ~ "13",
          es_campana_2026 & Módulo %in% c("14.1", "14.2") ~ "14",
          as.character(Módulo) == "9" & between(Turno_num, 1, 11)  ~ "9.2",
          as.character(Módulo) == "9" & Turno_num >= 12            ~ "9.1",
          as.character(Módulo) == "11" & between(Turno_num, 1, 12) ~ "11.1",
          as.character(Módulo) == "11" & Turno_num >= 13           ~ "11.2",
          TRUE ~ Módulo
        ),
        Turno    = trimws(as.character(Turno)),
        Variedad = limpiar_variedad(Variedad)
      )
    
    if (!is.null(input$filtro_grupo) && length(input$filtro_grupo) > 0) {
      mods_sel <- unlist(grupos_modulos[input$filtro_grupo], use.names = FALSE)
      df_hist <- df_hist %>%
        filter(suppressWarnings(as.numeric(sub("\\..*", "", as.character(Módulo)))) %in% mods_sel)
    }
    
    if (!is.null(input$filtro_modulo) && length(input$filtro_modulo) > 0) {
      df_hist <- df_hist %>%
        filter(as.character(Módulo) %in% as.character(input$filtro_modulo))
    }
    
    if (!is.null(input$filtro_turno) && length(input$filtro_turno) > 0) {
      df_hist <- df_hist %>%
        filter(as.character(Turno) %in% as.character(input$filtro_turno))
    }
    
    if (!is.null(input$filtro_variedad) && length(input$filtro_variedad) > 0) {
      df_hist <- df_hist %>%
        filter(limpiar_variedad(Variedad) %in% limpiar_variedad(input$filtro_variedad))
    }
    
    hist_pasado <- if (nrow(df_hist) > 0) {
      df_hist %>%
        group_by(Módulo, Turno, Variedad, Semana) %>%
        summarise(
          Sector_Hist  = first(na.omit(Sector)),
          Kg_Real      = sum(`Kg Total`, na.rm = TRUE),
          Kg_Ajustado  = sum(`Kg Total`, na.rm = TRUE),
          Kg_Proy      = NA_real_,
          Área_Sem     = NA_real_,
          Ha_Recorrida = NA_real_,
          .groups      = "drop"
        )
    } else {
      tibble(Módulo = character(), Turno = character(), Variedad = character(), Semana = numeric(), Sector_Hist = character(), Kg_Real = numeric(), Kg_Ajustado = numeric(), Kg_Proy = numeric(), Área_Sem = numeric(), Ha_Recorrida = numeric())
    }
    
    # Universo de la semana actual filtrado por los filtros globales (a nivel de turno, antes de agrupar)
    df_act <- universo_completo()
    if (!is.null(df_act) && nrow(df_act) > 0) {
      if (!is.null(input$filtro_grupo) && length(input$filtro_grupo) > 0) {
        mods_sel <- unlist(grupos_modulos[input$filtro_grupo], use.names = FALSE)
        df_act <- df_act %>%
          filter(suppressWarnings(as.numeric(sub("\\..*", "", as.character(Módulo)))) %in% mods_sel)
      }
      if (!is.null(input$filtro_modulo) && length(input$filtro_modulo) > 0) {
        df_act <- df_act %>%
          filter(as.character(Módulo) %in% as.character(input$filtro_modulo))
      }
      if (!is.null(input$filtro_turno) && length(input$filtro_turno) > 0) {
        df_act <- df_act %>%
          filter(as.character(Turno) %in% as.character(input$filtro_turno))
      }
      if (!is.null(input$filtro_variedad) && length(input$filtro_variedad) > 0) {
        df_act <- df_act %>%
          filter(limpiar_variedad(Variedad) %in% limpiar_variedad(input$filtro_variedad))
      }
    }
    
    # Cosecha física oficial real de SAP de la semana seleccionada (100% Comercial)
    df_sap_actual_turno <- rv_cosecha() %>%
      filter(as.character(Campaña) == as.character(input$filtro_campana), Semana == sem_sel, !coalesce(Es_Testblock, FALSE))
    
    df_sap_actual_turno <- df_sap_actual_turno %>%
      mutate(
        Módulo   = trimws(as.character(Módulo)),
        Módulo   = case_when(
          es_campana_2026 & Módulo %in% c("13.1", "13.2") ~ "13",
          es_campana_2026 & Módulo %in% c("14.1", "14.2") ~ "14",
          TRUE ~ Módulo
        ),
        Turno    = trimws(as.character(Turno)),
        Variedad = limpiar_variedad(Variedad)
      )
    
    if (!is.null(input$filtro_grupo) && length(input$filtro_grupo) > 0) {
      mods_sel <- unlist(grupos_modulos[input$filtro_grupo], use.names = FALSE)
      df_sap_actual_turno <- df_sap_actual_turno %>%
        filter(suppressWarnings(as.numeric(sub("\\..*", "", as.character(Módulo)))) %in% mods_sel)
    }
    if (!is.null(input$filtro_modulo) && length(input$filtro_modulo) > 0) {
      df_sap_actual_turno <- df_sap_actual_turno %>%
        filter(as.character(Módulo) %in% as.character(input$filtro_modulo))
    }
    if (!is.null(input$filtro_turno) && length(input$filtro_turno) > 0) {
      df_sap_actual_turno <- df_sap_actual_turno %>%
        filter(as.character(Turno) %in% as.character(input$filtro_turno))
    }
    if (!is.null(input$filtro_variedad) && length(input$filtro_variedad) > 0) {
      df_sap_actual_turno <- df_sap_actual_turno %>%
        filter(limpiar_variedad(Variedad) %in% limpiar_variedad(input$filtro_variedad))
    }
    
    df_sap_actual_turno <- df_sap_actual_turno %>%
      group_by(Módulo, Turno, Variedad) %>%
      summarise(Kg_Real_SAP = sum(`Kg Total`, na.rm = TRUE), .groups = "drop")
    
    act_sem <- if (!is.null(df_act) && nrow(df_act) > 0) {
      df_act %>%
        group_by(
          Módulo   = trimws(as.character(Módulo)),
          Turno    = trimws(as.character(Turno)),
          Variedad = limpiar_variedad(Variedad)
        ) %>%
        summarise(
          Sector_Act   = first(trimws(as.character(Sector))),
          Semana       = sem_sel,
          Kg_Ajustado  = sum(Ajuste, na.rm = TRUE),
          Kg_Proy      = sum(Proyección, na.rm = TRUE),
          Área_Sem     = sum(Área, na.rm = TRUE),
          Ha_Recorrida = sum(`Ha Recorrida`, na.rm = TRUE),
          .groups      = "drop"
        ) %>%
        left_join(df_sap_actual_turno, by = c("Módulo", "Turno", "Variedad")) %>%
        mutate(
          Kg_Real = coalesce(Kg_Real_SAP, 0)
        )
    } else {
      tibble(Módulo = character(), Turno = character(), Variedad = character(), Sector_Act = character(), Semana = numeric(), Kg_Real = numeric(), Kg_Ajustado = numeric(), Kg_Proy = numeric(), Área_Sem = numeric(), Ha_Recorrida = numeric())
    }
    
    # 3. Proyección de la semana siguiente (para evaluar avance vs proyección futura)
    ord_next <- ord_sel + 1
    sem_next <- ifelse(ord_next > 53, ord_next - 53, ord_next)
    
    df_proy_next <- tryCatch({
      cargar_proyeccion(ruta_raiz_arandano, input$filtro_campana, sem_next)
    }, error = function(e) NULL)
    
    next_sem <- if (!is.null(df_proy_next) && nrow(df_proy_next) > 0) {
      df_p_filt <- df_proy_next %>%
        mutate(
          Sector_Next = trimws(as.character(Sector_join)),
          Módulo = case_when(
            es_campana_2026 & Módulo_join %in% c("13.1", "13.2") ~ "13",
            es_campana_2026 & Módulo_join %in% c("14.1", "14.2") ~ "14",
            TRUE ~ trimws(as.character(Módulo_join))
          ),
          Turno    = trimws(as.character(Turno_join)),
          Variedad = limpiar_variedad(Variedad)
        )
      
      if (!is.null(input$filtro_grupo) && length(input$filtro_grupo) > 0) {
        mods_sel <- unlist(grupos_modulos[input$filtro_grupo], use.names = FALSE)
        df_p_filt <- df_p_filt %>%
          filter(suppressWarnings(as.numeric(sub("\\..*", "", as.character(Módulo)))) %in% mods_sel)
      }
      
      if (!is.null(input$filtro_modulo) && length(input$filtro_modulo) > 0) {
        df_p_filt <- df_p_filt %>%
          filter(as.character(Módulo) %in% as.character(input$filtro_modulo))
      }
      
      if (!is.null(input$filtro_turno) && length(input$filtro_turno) > 0) {
        df_p_filt <- df_p_filt %>%
          filter(as.character(Turno) %in% as.character(input$filtro_turno))
      }
      
      if (!is.null(input$filtro_variedad) && length(input$filtro_variedad) > 0) {
        df_p_filt <- df_p_filt %>%
          filter(limpiar_variedad(Variedad) %in% limpiar_variedad(input$filtro_variedad))
      }
      
      if (nrow(df_p_filt) > 0) {
        df_p_filt %>%
          group_by(Módulo, Turno, Variedad) %>%
          summarise(
            Sector_Next  = first(na.omit(Sector_Next)),
            Semana       = sem_next,
            Kg_Real      = NA_real_,
            Kg_Proy      = sum(Proyección, na.rm = TRUE),
            Área_Sem     = NA_real_,
            Ha_Recorrida = NA_real_,
            .groups      = "drop"
          )
      } else {
        tibble(Módulo = character(), Turno = character(), Variedad = character(), Sector_Next = character(), Semana = numeric(), Kg_Real = numeric(), Kg_Proy = numeric(), Área_Sem = numeric(), Ha_Recorrida = numeric())
      }
    } else {
      tibble(Módulo = character(), Turno = character(), Variedad = character(), Sector_Next = character(), Semana = numeric(), Kg_Real = numeric(), Kg_Proy = numeric(), Área_Sem = numeric(), Ha_Recorrida = numeric())
    }
    
    cat_areas <- areas_por_turno()
    
    # Catálogo de Sector y Tipo por Módulo/Turno/Variedad para enriquecer series históricas
    cat_sector_area <- areas_base %>%
      filter(as.character(Campaña) == as.character(input$filtro_campana)) %>%
      mutate(
        Módulo = case_when(
          es_campana_2026 & as.character(Módulo) %in% c("13.1", "13.2") ~ "13",
          es_campana_2026 & as.character(Módulo) %in% c("14.1", "14.2") ~ "14",
          TRUE ~ trimws(as.character(Módulo))
        ),
        Turno    = trimws(as.character(Turno)),
        Var_join = limpiar_variedad(Variedad),
        Sector_Cat = trimws(as.character(Sector))
      ) %>%
      distinct(Módulo, Turno, Var_join, .keep_all = TRUE) %>%
      select(Módulo, Turno, Var_join, Sector_Cat)
    
    cat_tipo_mod <- tratamientos_base %>%
      filter(as.character(Campaña) == as.character(input$filtro_campana)) %>%
      mutate(
        Módulo = case_when(
          es_campana_2026 & as.character(Módulo) %in% c("13.1", "13.2") ~ "13",
          es_campana_2026 & as.character(Módulo) %in% c("14.1", "14.2") ~ "14",
          TRUE ~ trimws(as.character(Módulo))
        ),
        Tipo_Cat = case_when(
          grepl("org[aá]n", Tipo, ignore.case = TRUE) ~ "Orgánico",
          grepl("conv", Tipo, ignore.case = TRUE)     ~ "Convencional",
          TRUE                                        ~ tools::toTitleCase(tolower(trimws(Tipo)))
        )
      ) %>%
      distinct(Módulo, .keep_all = TRUE) %>%
      select(Módulo, Tipo_Cat)
    
    bind_rows(hist_pasado, act_sem, next_sem) %>%
      mutate(Var_join = limpiar_variedad(Variedad)) %>%
      left_join(cat_areas, by = c("Módulo", "Turno", "Var_join")) %>%
      left_join(cat_sector_area, by = c("Módulo", "Turno", "Var_join")) %>%
      left_join(cat_tipo_mod, by = "Módulo") %>%
      group_by(Módulo, Turno, Variedad) %>%
      mutate(
        Sector = coalesce(
          first(na.omit(Sector_Act)),
          first(na.omit(Sector_Cat)),
          first(na.omit(Sector_Next)),
          first(na.omit(Sector_Hist)),
          "-"
        ),
        Tipo = coalesce(Tipo_Cat, "Convencional"),
        Área = coalesce(
          ifelse(!is.na(Área_Cat) & Área_Cat > 0, Área_Cat, NA_real_),
          ifelse(!is.na(Área_Sem) & Área_Sem > 0, Área_Sem, NA_real_),
          suppressWarnings(max(Área_Sem, na.rm = TRUE))
        ),
        Área = ifelse(!is.finite(Área) | is.na(Área), 0, Área)
      ) %>%
      ungroup() %>%
      select(-any_of(c("Var_join", "Área_Sem", "Área_Cat", "Sector_Hist", "Sector_Act", "Sector_Next", "Sector_Cat", "Tipo_Cat", "Kg_Real_SAP")))
  })
  
  # Título reactivo del gráfico histórico según unidad seleccionada
  output$titulo_grafico_historico <- renderText({
    es_kgha <- identical(input$switch_unidad, "kgha")
    if (es_kgha) {
      "Tendencia Semanal de Rendimiento (Kg/ha)"
    } else {
      "Tendencia Semanal de Cosecha (Kg)"
    }
  })
  
  output$ui_badge_agrupacion_hist <- renderUI({
    dims <- cols_agrupar()
    if (length(dims) < 5) {
      span(
        style = "font-size: 11px; font-weight: 600; padding: 3px 9px; border-radius: 10px; background: rgba(0, 122, 255, 0.10); color: #007AFF; border: 1px solid rgba(0, 122, 255, 0.25);",
        icon("layer-group"),
        paste("Agrupado por:", paste(dims, collapse = " • "))
      )
    } else {
      NULL
    }
  })
  
  # ==============================================================================
  # GRÁFICO HISTÓRICO INTERACTIVO (PLOTLY):
  # - COSECHA REAL: Línea continua (solid)
  # - PROYECCIÓN:   Línea punteada (dot) del mismo color en la semana seleccionada
  # ==============================================================================
  output$grafico_historico <- renderPlotly({
    df_u <- datos_historicos_universo()
    req(df_u, nrow(df_u) > 0)
    
    sem_sel      <- as.numeric(input$filtro_semana)
    es_kgha      <- identical(input$switch_unidad, "kgha")
    unidad_txt   <- if (es_kgha) "Kg/ha" else "Kg"
    dims_activas <- intersect(dims_tabla_agrupar, cols_agrupar())
    if (length(dims_activas) == 0) dims_activas <- dims_tabla_agrupar
    esta_agrup   <- (length(dims_activas) < 5)
    
    es_campana_act <- grepl("2026|^26", as.character(input$filtro_campana))
    sem_actual_num <- lubridate::isoweek(Sys.Date())
    
    df_sub <- df_u
    
    if (nrow(df_sub) == 0) {
      return(
        plot_ly() %>%
          layout(
            title = list(text = "Sin datos para los filtros seleccionados", font = list(size = 13, color = "#8E8E93")),
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE),
            paper_bgcolor = "#FFFFFF",
            plot_bgcolor = "#FFFFFF"
          )
      )
    }
    
    # Construir secuencia ordenada de semanas de la campaña (soportando cruce de año)
    semanas_presentes <- unique(c(df_u$Semana, sem_sel))
    semanas_presentes <- semanas_presentes[order(orden_semana_campana(semanas_presentes))]
    
    ord_min <- min(orden_semana_campana(semanas_presentes), na.rm = TRUE)
    ord_max <- orden_semana_campana(sem_sel)
    if (!is.finite(ord_min) || ord_min > ord_max) ord_min <- max(1, ord_max - 10)
    
    ord_next <- ord_max + 1
    sem_next <- ifelse(ord_next > 53, ord_next - 53, ord_next)
    
    # Secuencia en eje X: incluye la semana siguiente para visualizar los puntos proyectados futuros
    ord_seq <- ord_min:ord_next
    semanas_rango <- ifelse(ord_seq > 53, ord_seq - 53, ord_seq)
    
    # 1. Completar malla semanal a nivel de Turno para mantener el área consolidada constante por grupo
    df_turnos_complete <- df_sub %>%
      complete(
        nesting(Sector, Módulo, Turno, Variedad, Tipo, Área),
        Semana = semanas_rango,
        fill = list(Kg_Real = 0, Kg_Ajustado = 0, Kg_Proy = 0, Ha_Recorrida = 0)
      )
    
    # 2. Agrupar según las dimensiones activas en 'Agrupar por'
    df_grouped <- df_turnos_complete %>%
      group_by(across(all_of(c(dims_activas, "Semana")))) %>%
      summarise(
        Kg_Real      = sum(Kg_Real, na.rm = TRUE),
        Kg_Ajustado  = sum(Kg_Ajustado, na.rm = TRUE),
        Kg_Proy      = sum(Kg_Proy, na.rm = TRUE),
        Área         = sum(Área, na.rm = TRUE),
        Ha_Recorrida = sum(coalesce(Ha_Recorrida, 0), na.rm = TRUE),
        .groups      = "drop"
      )
    
    # Construir etiqueta de serie según las dimensiones chequeadas
    formatear_etiqueta_serie <- function(df_in, dims) {
      if (!esta_agrup) {
        return(paste(df_in$Módulo, df_in$Turno, df_in$Variedad, sep = " - "))
      }
      partes <- lapply(dims, function(d) {
        val <- as.character(df_in[[d]])
        if (d == "Sector") paste0("S", val)
        else if (d == "Módulo") paste0("M", val)
        else if (d == "Turno") paste0("T", val)
        else val
      })
      do.call(paste, c(partes, list(sep = " - ")))
    }
    
    df_grouped$Serie_Label <- formatear_etiqueta_serie(df_grouped, dims_activas)
    
    df_complete <- df_grouped %>%
      mutate(
        Orden_Sem = orden_semana_campana(Semana),
        Val_Real  = if (es_kgha) {
          case_when(
            Semana == sem_sel & coalesce(Ha_Recorrida, 0) > 0 ~ coalesce(Kg_Real, 0) / Ha_Recorrida,
            Área > 0                                          ~ coalesce(Kg_Real, 0) / Área,
            TRUE                                              ~ 0
          )
        } else coalesce(Kg_Real, 0),
        Val_Ajus  = if (es_kgha) ifelse(Área > 0, coalesce(Kg_Ajustado, 0) / Área, 0) else coalesce(Kg_Ajustado, 0),
        Val_Proy  = if (es_kgha) ifelse(Área > 0 & !is.na(Kg_Proy), Kg_Proy / Área, 0) else coalesce(Kg_Proy, 0),
        # Cosecha real únicamente hasta donde haya cosecha física en SAP (en semanas futuras o <= 0 es NA)
        Val_Real_Plot = case_when(
          Orden_Sem > ord_max ~ NA_real_,
          Semana == sem_sel & Val_Real <= 0 ~ NA_real_,
          TRUE ~ Val_Real
        ),
        Hover_Real = sprintf(
          "<b>%s</b><br>Semana %d (Cosecha Real SAP): %s %s%s%s%s",
          Serie_Label, Semana,
          format(round(coalesce(Val_Real, 0), 1), big.mark = ","), unidad_txt,
          ifelse(Semana == sem_sel & Val_Ajus > 0, sprintf("<br>Cosecha Ajustada: %s %s", format(round(Val_Ajus, 1), big.mark = ","), unidad_txt), ""),
          ifelse(Semana == sem_sel & Val_Proy > 0, sprintf("<br>Proyección Oficial: %s %s", format(round(Val_Proy, 1), big.mark = ","), unidad_txt), ""),
          if (es_kgha) {
            ifelse(
              Semana == sem_sel & coalesce(Ha_Recorrida, 0) > 0,
              sprintf("<br>Ha Recorrida: %s ha (Área Total: %s ha)", format(round(Ha_Recorrida, 2), big.mark = ","), format(round(Área, 2), big.mark = ",")),
              sprintf("<br>Área: %s ha", format(round(Área, 2), big.mark = ","))
            )
          } else ""
        )
      ) %>%
      arrange(Orden_Sem)
    paleta_ios <- c("#007AFF", "#FF9500", "#5856D6", "#30B0C7", "#34C759", "#FF2D55", "#FFCC00", "#AF52DE", "#5AC8FA", "#FF3B30")
    
    series_unicas <- df_complete %>%
      distinct(across(all_of(c(dims_activas, "Serie_Label")))) %>%
      mutate(
        ord_sec = if ("Sector" %in% dims_activas) suppressWarnings(as.numeric(Sector)) else 0,
        chr_sec = if ("Sector" %in% dims_activas) as.character(Sector) else "",
        ord_mod = if ("Módulo" %in% dims_activas) suppressWarnings(as.numeric(Módulo)) else 0,
        chr_mod = if ("Módulo" %in% dims_activas) as.character(Módulo) else "",
        ord_tur = if ("Turno" %in% dims_activas) suppressWarnings(as.numeric(Turno)) else 0,
        chr_tur = if ("Turno" %in% dims_activas) as.character(Turno) else "",
        chr_var = if ("Variedad" %in% dims_activas) as.character(Variedad) else "",
        chr_tip = if ("Tipo" %in% dims_activas) as.character(Tipo) else ""
      ) %>%
      arrange(ord_sec, chr_sec, ord_mod, chr_mod, ord_tur, chr_tur, chr_var, chr_tip) %>%
      distinct(Serie_Label)
    
    p <- plot_ly()
    
    es_multi_series <- nrow(series_unicas) > 1
    
    for (i in seq_len(nrow(series_unicas))) {
      nombre_serie <- series_unicas$Serie_Label[i]
      
      sub_d <- df_complete %>%
        filter(Serie_Label == nombre_serie) %>%
        arrange(Orden_Sem)
      
      if (nrow(sub_d) == 0) next
      
      color_serie  <- paleta_ios[((i - 1) %% length(paleta_ios)) + 1]
      nombre_real  <- if (es_multi_series) paste0(nombre_serie, " (Real)") else "Cosecha Real (SAP)"
      nombre_ajus  <- if (es_multi_series) paste0(nombre_serie, " (Ajuste)") else "Cosecha Ajustada"
      nombre_proy  <- if (es_multi_series) paste0(nombre_serie, " (Proy)") else "Proyección Oficial"
      
      color_real <- if (es_multi_series) color_serie else "#007AFF"
      color_ajus <- if (es_multi_series) color_serie else "#34C759"
      color_proy <- if (es_multi_series) color_serie else "#FF9500"
      
      # -------------------------------------------------------------
      # 1. Trazo de Cosecha Real SAP (Línea sólida continua)
      # -------------------------------------------------------------
      sub_real <- sub_d %>% filter(!is.na(Val_Real_Plot))
      
      if (nrow(sub_real) > 0) {
        p <- p %>% add_trace(
          data        = sub_real,
          x           = ~Orden_Sem,
          y           = ~round(Val_Real_Plot, 1),
          name        = nombre_real,
          legendgroup = nombre_serie,
          showlegend  = TRUE,
          type        = "scatter",
          mode        = "lines+markers",
          line        = list(color = color_real, width = 2.5, dash = "solid"),
          marker      = list(color = color_real, size = 6.5),
          text        = ~Hover_Real,
          hoverinfo   = "text"
        )
      }
      
      # -------------------------------------------------------------
      # 2. Trazo de Cosecha Ajustada (Línea segmentada dash verde)
      # -------------------------------------------------------------
      fila_prev_real <- sub_d %>% filter(Orden_Sem < ord_max, !is.na(Val_Real_Plot)) %>% tail(1)
      fila_ajus_act  <- sub_d %>% filter(Semana == sem_sel, !is.na(Val_Ajus), Val_Ajus > 0)
      
      if (nrow(fila_ajus_act) > 0) {
        df_seg_ajus <- tibble(
          Orden_Sem  = numeric(),
          Valor_Seg  = numeric(),
          Hover_Seg  = character(),
          Tam_Marker = numeric()
        )
        
        if (nrow(fila_prev_real) > 0) {
          df_seg_ajus <- df_seg_ajus %>% add_row(
            Orden_Sem  = fila_prev_real$Orden_Sem[1],
            Valor_Seg  = round(fila_prev_real$Val_Real_Plot[1], 1),
            Hover_Seg  = fila_prev_real$Hover_Real[1],
            Tam_Marker = 0
          )
        }
        
        hover_ajus_txt <- sprintf(
          "<b>%s</b><br>Semana %d (Cosecha Ajustada): %s %s<br>%s: %s %s<br>Proyección Oficial: %s %s%s",
          nombre_serie, sem_sel,
          format(round(fila_ajus_act$Val_Ajus[1], 1), big.mark = ","), unidad_txt,
          if (es_kgha) "Rendimiento Real SAP" else "Cosecha Real SAP",
          format(round(coalesce(fila_ajus_act$Val_Real[1], 0), 1), big.mark = ","), unidad_txt,
          format(round(coalesce(fila_ajus_act$Val_Proy[1], 0), 1), big.mark = ","), unidad_txt,
          if (es_kgha) {
            if (coalesce(fila_ajus_act$Ha_Recorrida[1], 0) > 0) {
              sprintf("<br>Ha Recorrida: %s ha (Área Total: %s ha)", format(round(fila_ajus_act$Ha_Recorrida[1], 2), big.mark = ","), format(round(fila_ajus_act$Área[1], 2), big.mark = ","))
            } else {
              sprintf("<br>Área: %s ha", format(round(fila_ajus_act$Área[1], 2), big.mark = ","))
            }
          } else ""
        )
        
        df_seg_ajus <- df_seg_ajus %>% add_row(
          Orden_Sem  = fila_ajus_act$Orden_Sem[1],
          Valor_Seg  = round(fila_ajus_act$Val_Ajus[1], 1),
          Hover_Seg  = hover_ajus_txt,
          Tam_Marker = 8
        )
        
        modo_ajus <- if (nrow(df_seg_ajus) > 1) "lines+markers" else "markers"
        p <- p %>% add_trace(
          data        = df_seg_ajus,
          x           = ~Orden_Sem,
          y           = ~Valor_Seg,
          name        = nombre_ajus,
          legendgroup = nombre_serie,
          showlegend  = TRUE,
          type        = "scatter",
          mode        = modo_ajus,
          line        = list(color = color_ajus, width = 2.5, dash = "dash"),
          marker      = list(color = color_ajus, size = ~Tam_Marker, symbol = "square"),
          text        = ~Hover_Seg,
          hoverinfo   = "text"
        )
      }
      
      # -------------------------------------------------------------
      # 3. Trazo de Proyección Oficial (Línea punteada dot naranja)
      # -------------------------------------------------------------
      fila_proy_act  <- sub_d %>% filter(Semana == sem_sel, !is.na(Val_Proy), Val_Proy > 0)
      fila_proy_next <- sub_d %>% filter(Semana == sem_next, !is.na(Val_Proy), Val_Proy > 0)
      
      if (nrow(fila_proy_act) > 0 || nrow(fila_proy_next) > 0) {
        df_seg_proy <- tibble(
          Orden_Sem  = numeric(),
          Valor_Seg  = numeric(),
          Hover_Seg  = character(),
          Tam_Marker = numeric()
        )
        
        if (nrow(fila_prev_real) > 0) {
          df_seg_proy <- df_seg_proy %>% add_row(
            Orden_Sem  = fila_prev_real$Orden_Sem[1],
            Valor_Seg  = round(fila_prev_real$Val_Real_Plot[1], 1),
            Hover_Seg  = fila_prev_real$Hover_Real[1],
            Tam_Marker = 0
          )
        }
        
        if (nrow(fila_proy_act) > 0) {
          hover_proy_txt <- sprintf(
            "<b>%s</b><br>Semana %d (Proyectado Oficial): %s %s<br>%s: %s %s%s",
            nombre_serie, sem_sel,
            format(round(fila_proy_act$Val_Proy[1], 1), big.mark = ","), unidad_txt,
            if (es_kgha) "Rendimiento Real SAP" else "Cosecha Real SAP",
            format(round(coalesce(fila_proy_act$Val_Real[1], 0), 1), big.mark = ","), unidad_txt,
            if (es_kgha) {
              if (coalesce(fila_proy_act$Ha_Recorrida[1], 0) > 0) {
                sprintf("<br>Ha Recorrida: %s ha (Área Total: %s ha)", format(round(fila_proy_act$Ha_Recorrida[1], 2), big.mark = ","), format(round(fila_proy_act$Área[1], 2), big.mark = ","))
              } else {
                sprintf("<br>Área: %s ha", format(round(fila_proy_act$Área[1], 2), big.mark = ","))
              }
            } else ""
          )
          df_seg_proy <- df_seg_proy %>% add_row(
            Orden_Sem  = fila_proy_act$Orden_Sem[1],
            Valor_Seg  = round(fila_proy_act$Val_Proy[1], 1),
            Hover_Seg  = hover_proy_txt,
            Tam_Marker = 7
          )
        }
        
        if (nrow(fila_proy_next) > 0) {
          hover_next_txt <- sprintf(
            "<b>%s</b><br>Semana %d (Proyectado Siguiente): %s %s<br><i>Proyección próxima semana</i>%s",
            nombre_serie, sem_next,
            format(round(fila_proy_next$Val_Proy[1], 1), big.mark = ","), unidad_txt,
            if (es_kgha) sprintf("<br>Área: %s ha", format(round(fila_proy_next$Área[1], 2), big.mark = ",")) else ""
          )
          df_seg_proy <- df_seg_proy %>% add_row(
            Orden_Sem  = fila_proy_next$Orden_Sem[1],
            Valor_Seg  = round(fila_proy_next$Val_Proy[1], 1),
            Hover_Seg  = hover_next_txt,
            Tam_Marker = 7
          )
        }
        
        if (nrow(df_seg_proy) > 0) {
          modo_proy <- if (nrow(df_seg_proy) > 1) "lines+markers" else "markers"
          p <- p %>% add_trace(
            data        = df_seg_proy,
            x           = ~Orden_Sem,
            y           = ~Valor_Seg,
            name        = nombre_proy,
            legendgroup = nombre_serie,
            showlegend  = TRUE,
            type        = "scatter",
            mode        = modo_proy,
            line        = list(color = color_proy, width = 2, dash = "dot"),
            marker      = list(color = color_proy, size = ~Tam_Marker, symbol = "diamond"),
            text        = ~Hover_Seg,
            hoverinfo   = "text"
          )
        }
      }
    }
    
    shapes_list <- list()
    annotations_list <- list()
    ord_actual_num <- orden_semana_campana(sem_actual_num)
    
    if (es_campana_act && ord_actual_num >= ord_min && ord_actual_num <= ord_seq[length(ord_seq)]) {
      shapes_list <- list(
        list(
          type = "line",
          layer = "below",
          x0 = ord_actual_num,
          x1 = ord_actual_num,
          y0 = 0,
          y1 = 1,
          yref = "paper",
          line = list(
            color = "rgba(142, 142, 147, 0.55)",
            width = 1.5,
            dash = "dash"
          )
        )
      )
      
      annotations_list <- list(
        list(
          x = ord_actual_num,
          y = 1.03,
          yref = "paper",
          text = sprintf("<b>S%d (Actual)</b>", sem_actual_num),
          showarrow = FALSE,
          xanchor = "center",
          font = list(
            size = 11,
            color = "#636366",
            family = "-apple-system, BlinkMacSystemFont, 'Inter', sans-serif"
          ),
          bgcolor = "rgba(142, 142, 147, 0.12)",
          bordercolor = "rgba(142, 142, 147, 0.35)",
          borderwidth = 1,
          borderpad = 3
        )
      )
    }
    
    titulo_y <- if (es_kgha) "Rendimiento (Kg/ha)" else "Kilogramos (Kg)"
    
    p <- p %>% layout(
      shapes = shapes_list,
      annotations = annotations_list,
      xaxis = list(
        title = list(text = "Semana de Cosecha", font = list(size = 12, color = "#636366", family = "-apple-system, BlinkMacSystemFont, 'Inter', sans-serif")),
        tickmode = "array",
        tickvals = ord_seq,
        ticktext = as.character(semanas_rango),
        gridcolor = "#F0F0F2",
        zeroline = FALSE,
        tickfont = list(size = 11, color = "#8E8E93")
      ),
      yaxis = list(
        title = list(text = titulo_y, font = list(size = 12, color = "#636366", family = "-apple-system, BlinkMacSystemFont, 'Inter', sans-serif")),
        gridcolor = "#F0F0F2",
        zeroline = FALSE,
        tickformat = ",",
        tickfont = list(size = 11, color = "#8E8E93")
      ),
      hoverlabel = list(
        bgcolor = "#1C1C1E",
        bordercolor = "#1C1C1E",
        font = list(color = "#FFFFFF", family = "-apple-system, BlinkMacSystemFont, 'Inter', sans-serif", size = 12)
      ),
      legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = -0.18,
        font = list(size = 11, color = "#1C1C1E")
      ),
      margin = list(l = 55, r = 25, t = 30, b = 65),
      paper_bgcolor = "#FFFFFF",
      plot_bgcolor = "#FFFFFF"
    ) %>%
      plotly::config(displaylogo = FALSE, modeBarButtonsToRemove = c("lasso2d", "select2d"))
    
    p
  })
  
  # ==============================================================================
  # GRAFICO DONA: COSECHA DE LA SEMANA PREVIA POR DIA Y TURNO (KG O KG/HA)
  # ==============================================================================
  semana_previa_reactiva <- reactive({
    req(input$filtro_campana, input$filtro_semana)
    sem_sel <- as.numeric(input$filtro_semana)
    ord_sel <- orden_semana_campana(sem_sel)
    
    sems_camp <- rv_cosecha() %>%
      filter(
        as.character(Campaña) == as.character(input$filtro_campana),
        orden_semana_campana(Semana) < ord_sel
      ) %>%
      pull(Semana) %>%
      unique()
    
    if (length(sems_camp) > 0) {
      sems_camp <- sems_camp[order(orden_semana_campana(sems_camp))]
      return(tail(sems_camp, 1))
    }
    if (!is.na(sem_sel) && sem_sel > 1) sem_sel - 1 else 52
  })
  
  output$ui_titulo_dona_semana <- renderUI({
    sem_ant <- semana_previa_reactiva()
    es_kgha <- identical(input$switch_unidad, "kgha")
    prefijo <- if (es_kgha) "Rendimiento" else "Cosecha"
    span(sprintf("%s S%d por Día", prefijo, sem_ant))
  })
  
  output$txt_subtitulo_dona <- renderText({
    tur_sel <- input$filtro_turno
    if (is.null(tur_sel) || length(tur_sel) == 0) {
      "Todos los turnos"
    } else if (length(tur_sel) == 1) {
      paste("Turno:", tur_sel)
    } else if (length(tur_sel) <= 4) {
      paste("Turnos:", paste(tur_sel, collapse = ", "))
    } else {
      paste(length(tur_sel), "turnos seleccionados")
    }
  })
  
  output$grafico_dona_dias <- renderPlotly({
    req(input$filtro_campana, input$filtro_semana)
    
    sem_ant         <- semana_previa_reactiva()
    es_campana_2026 <- grepl("2026|^26", as.character(input$filtro_campana))
    es_kgha         <- identical(input$switch_unidad, "kgha")
    unidad_txt      <- if (es_kgha) "Kg/ha" else "Kg"
    
    df_ant <- rv_cosecha() %>%
      filter(as.character(Campaña) == as.character(input$filtro_campana), Semana == sem_ant, !coalesce(Es_Testblock, FALSE))
    
    df_ant <- df_ant %>%
      mutate(
        Turno_num = suppressWarnings(as.numeric(Turno)),
        Sector    = case_when(as.character(Módulo) %in% c("9", "9.1", "9.2", "11", "11.1", "11.2", "12") ~ "9", TRUE ~ trimws(as.character(Sector))),
        Módulo    = trimws(as.character(Módulo)),
        Módulo    = case_when(
          es_campana_2026 & Módulo %in% c("13.1", "13.2") ~ "13",
          es_campana_2026 & Módulo %in% c("14.1", "14.2") ~ "14",
          as.character(Módulo) == "9" & between(Turno_num, 1, 11)  ~ "9.2",
          as.character(Módulo) == "9" & Turno_num >= 12            ~ "9.1",
          as.character(Módulo) == "11" & between(Turno_num, 1, 12) ~ "11.1",
          as.character(Módulo) == "11" & Turno_num >= 13           ~ "11.2",
          TRUE ~ Módulo
        ),
        Turno    = trimws(as.character(Turno)),
        Variedad = limpiar_variedad(Variedad)
      )
    
    if (!is.null(input$filtro_grupo) && length(input$filtro_grupo) > 0) {
      mods_sel <- unlist(grupos_modulos[input$filtro_grupo], use.names = FALSE)
      df_ant <- df_ant %>%
        filter(suppressWarnings(as.numeric(sub("\\..*", "", as.character(Módulo)))) %in% mods_sel)
    }
    
    if (!is.null(input$filtro_modulo) && length(input$filtro_modulo) > 0) {
      df_ant <- df_ant %>%
        filter(as.character(Módulo) %in% as.character(input$filtro_modulo))
    }
    
    if (!is.null(input$filtro_turno) && length(input$filtro_turno) > 0) {
      df_ant <- df_ant %>%
        filter(as.character(Turno) %in% as.character(input$filtro_turno))
    }
    
    if (!is.null(input$filtro_variedad) && length(input$filtro_variedad) > 0) {
      df_ant <- df_ant %>%
        filter(limpiar_variedad(Variedad) %in% limpiar_variedad(input$filtro_variedad))
    }
    
    if (nrow(df_ant) == 0) {
      return(
        plot_ly() %>%
          layout(
            title = list(
              text = sprintf("Sin cosechas en Semana %d", sem_ant),
              font = list(size = 12, color = "#8E8E93", family = "-apple-system, BlinkMacSystemFont, 'Inter', sans-serif")
            ),
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE),
            paper_bgcolor = "transparent",
            plot_bgcolor = "transparent"
          )
      )
    }
    
    # Calcular el área consolidada de los turnos seleccionados
    df_u_areas <- datos_historicos_universo() %>%
      distinct(Módulo, Turno, Variedad, Área)
    area_sel_dona <- sum(df_u_areas$Área, na.rm = TRUE)
    
    if (is.na(area_sel_dona) || area_sel_dona <= 0) {
      turnos_presentes <- df_ant %>%
        distinct(Módulo, Turno, Variedad) %>%
        mutate(Var_join = limpiar_variedad(Variedad)) %>%
        left_join(areas_por_turno(), by = c("Módulo", "Turno", "Var_join"))
      area_sel_dona <- sum(turnos_presentes$Área_Cat, na.rm = TRUE)
    }
    
    dias_nom <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Domingo")
    colores_dias <- c(
      "Lunes"     = "#FF0000", # Rojo puro
      "Martes"    = "#00A2E8", # Celeste / Sky Blue
      "Miércoles" = "#FF7F27", # Naranja
      "Jueves"    = "#FFFF00", # Amarillo
      "Viernes"   = "#00C853", # Verde
      "Sábado"    = "#A349A4", # Púrpura / Morado
      "Domingo"   = "#8E8E93"  # Gris (respaldo si hubiera domingo)
    )
    
    df_dias <- df_ant %>%
      mutate(Dia = factor(dias_nom[wday(Fecha, week_start = 1)], levels = dias_nom)) %>%
      group_by(Dia) %>%
      summarise(Kg = sum(`Kg Total`, na.rm = TRUE), .groups = "drop") %>%
      filter(Kg > 0) %>%
      mutate(
        Valor_Dia = if (es_kgha && area_sel_dona > 0) Kg / area_sel_dona else Kg
      ) %>%
      arrange(Dia)
    
    if (nrow(df_dias) == 0 || sum(df_dias$Valor_Dia) == 0) {
      return(
        plot_ly() %>%
          layout(
            title = list(
              text = sprintf("Sin kilos registrados en Semana %d", sem_ant),
              font = list(size = 12, color = "#8E8E93", family = "-apple-system, BlinkMacSystemFont, 'Inter', sans-serif")
            ),
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE),
            paper_bgcolor = "transparent",
            plot_bgcolor = "transparent"
          )
      )
    }
    
    tot_val <- sum(df_dias$Valor_Dia)
    colores_usados <- unname(colores_dias[as.character(df_dias$Dia)])
    colores_texto  <- ifelse(as.character(df_dias$Dia) == "Jueves", "#1C1C1E", "#FFFFFF")
    etiqueta_hover <- if (es_kgha) "Rendimiento" else "Cosecha"
    
    plot_ly(
      df_dias,
      labels = ~Dia,
      values = ~round(Valor_Dia, 1),
      type = "pie",
      hole = 0.60,
      sort = FALSE,
      marker = list(colors = colores_usados, line = list(color = "#FFFFFF", width = 2)),
      textinfo = "percent",
      textposition = "inside",
      insidetextorientation = "horizontal",
      insidetextfont = list(color = colores_texto, size = 12, family = "-apple-system, BlinkMacSystemFont, 'Inter', sans-serif"),
      hovertemplate = sprintf("<b>%%{label} (S%%{customdata})</b><br>%s: %%{value:,.1f} %s<br>Participación: %%{percent}<extra></extra>", etiqueta_hover, unidad_txt),
      customdata = rep(sem_ant, nrow(df_dias))
    ) %>%
      layout(
        showlegend = TRUE,
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = -0.06,
          font = list(size = 11, color = "#1C1C1E", family = "-apple-system, BlinkMacSystemFont, 'Inter', sans-serif")
        ),
        margin = list(l = 10, r = 10, t = 10, b = 25),
        annotations = list(
          list(
            text = sprintf("<b>%s</b><br><span style='font-size:10px;color:#8E8E93;'>%s S%d</span>",
                           format(round(tot_val, if (es_kgha) 1 else 0), big.mark = ","), unidad_txt, sem_ant),
            x = 0.5, y = 0.5,
            font = list(size = 14, family = "-apple-system, BlinkMacSystemFont, 'Inter', sans-serif", color = "#1C1C1E"),
            showarrow = FALSE
          )
        ),
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent"
      ) %>%
      plotly::config(displayModeBar = FALSE)
  })
  
  # ============================================================================
  # ============================================================================
  # LÓGICA DEL MAPA 2D DE COSECHA OFICIAL (SOLO LECTURA)
  # ============================================================================
  # Módulos filtrados según el selector general superior (Módulo o Grupo)
  modulos_seleccionados_mapa <- reactive({
    if (!is.null(input$filtro_modulo) && length(input$filtro_modulo) > 0) {
      as.character(input$filtro_modulo)
    } else if (!is.null(input$filtro_grupo) && length(input$filtro_grupo) > 0) {
      as.character(unlist(grupos_modulos[input$filtro_grupo]))
    } else {
      character(0)
    }
  })
  
  # Semanas disponibles y última semana con datos de cosecha para los módulos/filtros seleccionados
  semanas_info_mapa <- reactive({
    mods   <- modulos_seleccionados_mapa()
    turnos <- if (!is.null(input$filtro_turno) && length(input$filtro_turno) > 0) as.character(input$filtro_turno) else character(0)
    vars   <- if (!is.null(input$filtro_variedad) && length(input$filtro_variedad) > 0) limpiar_variedad(input$filtro_variedad) else character(0)
    
    df_base <- if (exists("df_hist_oficial_wide", inherits = TRUE) && !is.null(df_hist_oficial_wide)) {
      df_tmp <- df_hist_oficial_wide %>%
        mutate(
          Turno_num_aux = suppressWarnings(as.numeric(if ("Turno_Norm" %in% names(.)) Turno_Norm else Turno)),
          Módulo_Sub = case_when(
            as.character(Módulo_Norm) == "9" & between(Turno_num_aux, 1, 11)  ~ "9.2",
            as.character(Módulo_Norm) == "9" & Turno_num_aux >= 12            ~ "9.1",
            as.character(Módulo_Norm) == "11" & between(Turno_num_aux, 1, 12) ~ "11.1",
            as.character(Módulo_Norm) == "11" & Turno_num_aux >= 13           ~ "11.2",
            TRUE                                                              ~ as.character(Módulo_Norm)
          )
        ) %>%
        select(-Turno_num_aux)
      
      if (length(mods) > 0) {
        df_tmp <- df_tmp %>% filter(as.character(Módulo_Norm) %in% mods | as.character(Módulo_Sub) %in% mods)
      }
      if (length(turnos) > 0) {
        if ("Turno_Norm" %in% names(df_tmp)) {
          df_tmp <- df_tmp %>% filter(as.character(Turno_Norm) %in% turnos)
        } else if ("Turno" %in% names(df_tmp)) {
          df_tmp <- df_tmp %>% filter(as.character(Turno) %in% turnos)
        }
      }
      if (length(vars) > 0) {
        df_tmp <- df_tmp %>% filter(limpiar_variedad(Variedad) %in% vars)
      }
      df_tmp
    } else {
      tibble()
    }
    
    sems_con_datos <- if (nrow(df_base) > 0) {
      df_base %>%
        filter(coalesce(Kg_Total_Sem, 0) > 0) %>%
        pull(Semana) %>%
        na.omit() %>%
        as.numeric() %>%
        unique() %>%
        sort(decreasing = TRUE)
    } else {
      numeric(0)
    }
    
    # Incluir semanas que tengan registros en rutas_cosecha_mapa
    rc_todas <- tryCatch(cargar_rutas_cosecha(), error = function(e) NULL)
    sems_rc <- if (!is.null(rc_todas) && nrow(rc_todas) > 0) {
      rc_sub <- rc_todas %>%
        mutate(
          Turno_num_aux = suppressWarnings(as.numeric(Turno)),
          Módulo_Sub = case_when(
            as.character(Módulo) == "9" & between(Turno_num_aux, 1, 11)  ~ "9.2",
            as.character(Módulo) == "9" & Turno_num_aux >= 12            ~ "9.1",
            as.character(Módulo) == "11" & between(Turno_num_aux, 1, 12) ~ "11.1",
            as.character(Módulo) == "11" & Turno_num_aux >= 13           ~ "11.2",
            TRUE                                                         ~ as.character(Módulo)
          )
        ) %>%
        select(-Turno_num_aux)
      
      if (length(mods) > 0) rc_sub <- rc_sub %>% filter(as.character(Módulo) %in% mods | as.character(Módulo_Sub) %in% mods)
      if (length(turnos) > 0) rc_sub <- rc_sub %>% filter(as.character(Turno) %in% turnos)
      if (length(vars) > 0) rc_sub <- rc_sub %>% filter(limpiar_variedad(Variedad) %in% vars)
      rc_sub %>%
        filter(coalesce(Ha_Total_Prog, 0) > 0 | coalesce(Kg_Total_Prog, 0) > 0) %>%
        pull(Semana) %>% na.omit() %>% as.numeric() %>% unique()
    } else numeric(0)
    
    sems_finales <- sort(unique(c(sems_con_datos, sems_rc)), decreasing = TRUE)
    if (length(sems_finales) == 0) sems_finales <- 43:20
    
    def_sem <- if (!is.null(input$filtro_semana) && suppressWarnings(as.numeric(input$filtro_semana)) %in% sems_finales) {
      as.numeric(input$filtro_semana)
    } else {
      sems_finales[1]
    }
    
    list(
      disponibles = sems_finales,
      defecto     = def_sem
    )
  })
  
  output$ui_filtro_semana_mapa <- renderUI({
    info <- semanas_info_mapa()
    selectInput(
      inputId  = "filtro_semana_mapa",
      label    = NULL,
      choices  = setNames(info$disponibles, paste("Semana", info$disponibles)),
      selected = info$defecto,
      width    = "140px"
    )
  })
  # ==============================================================================
  # REACTIVO Y GRÁFICO HISTÓRICO DE CONTEO FLORAL (BOTÓN Y FLOR)
  # ==============================================================================
  datos_conteo_reactivos <- reactive({
    req(input$filtro_campana)
    
    df_c <- if (exists("df_conteo_historico") && is.data.frame(df_conteo_historico) && nrow(df_conteo_historico) > 0) {
      df_conteo_historico
    } else if (file.exists("historico_conteo.rds")) {
      readRDS("historico_conteo.rds")
    } else if (file.exists("D:/ACP/Actualizacion Semanal/historico_conteo.rds")) {
      readRDS("D:/ACP/Actualizacion Semanal/historico_conteo.rds")
    } else if (file.exists(file.path(ruta_raiz_arandano, "historico_conteo.rds"))) {
      readRDS(file.path(ruta_raiz_arandano, "historico_conteo.rds"))
    } else {
      tibble()
    }
    
    if (nrow(df_c) == 0) return(tibble())
    
    camp_sel_txt <- as.character(input$filtro_campana)
    camp_num <- suppressWarnings(as.numeric(camp_sel_txt))
    
    df_f <- df_c %>%
      filter(toupper(trimws(coalesce(Fundo, "ACP"))) == "ACP") %>%
      filter(
        as.character(Campaña) == camp_sel_txt |
          as.character(Campaña_Original) == camp_sel_txt |
          (!is.na(camp_num) & Campaña == camp_num)
      )
    
    if (nrow(df_f) == 0) return(tibble())
    
    # Sanitizar Sector asegurando que siempre coincida con el catastro oficial
    if ("Sector" %in% names(df_f) && exists("df_valvulas", inherits = TRUE)) {
      mapa_s <- df_valvulas %>% distinct(Módulo, Sector) %>% filter(!is.na(Sector))
      idx_s <- match(sub("\\..*", "", df_f$Módulo), mapa_s$Módulo)
      s_imp <- mapa_s$Sector[idx_s]
      df_f$Sector <- ifelse(
        is.na(df_f$Sector) | df_f$Sector %in% c("TRUE", "FALSE", "", "NA"),
        as.character(s_imp),
        as.character(df_f$Sector)
      )
    }
    
    # Filtro Grupo
    if (!is.null(input$filtro_grupo) && length(input$filtro_grupo) > 0) {
      mods_permitidos <- unlist(grupos_modulos[input$filtro_grupo], use.names = FALSE)
      df_f <- df_f %>% filter(Módulo %in% mods_permitidos | sub("\\..*", "", Módulo) %in% sub("\\..*", "", mods_permitidos))
    }
    
    # Filtro Módulo
    if (!is.null(input$filtro_modulo) && length(input$filtro_modulo) > 0) {
      df_f <- df_f %>% filter(Módulo %in% input$filtro_modulo | sub("\\..*", "", Módulo) %in% sub("\\..*", "", input$filtro_modulo))
    }
    
    # Filtro Turno
    if (!is.null(input$filtro_turno) && length(input$filtro_turno) > 0) {
      df_f <- df_f %>% filter(as.character(Turno) %in% as.character(input$filtro_turno))
    }
    
    # Filtro Variedad
    if (!is.null(input$filtro_variedad) && length(input$filtro_variedad) > 0) {
      df_f <- df_f %>% filter(Variedad %in% input$filtro_variedad)
    }
    
    df_f
  })
  
  output$titulo_grafico_conteo <- renderText({
    es_planta <- identical(input$switch_conteo_metrica, "planta")
    if (es_planta) "Histórico de Conteo Floral: Órganos por Planta (Botón y Flor / Muestra)" else "Histórico de Conteo Floral: Total de Órganos"
  })
  
  
  output$ui_badge_conteo_agrupacion <- renderUI({
    dims_c <- intersect(c("Sector", "Módulo", "Turno", "Válvula", "Variedad"), cols_agrupar())
    if (length(dims_c) > 0) {
      span(
        style = "font-size: 11px; font-weight: 600; padding: 3px 9px; border-radius: 10px; background: rgba(175, 82, 222, 0.10); color: #AF52DE; border: 1px solid rgba(175, 82, 222, 0.25);",
        icon("layer-group"),
        paste("Nivel:", paste(dims_c, collapse = " • "))
      )
    } else {
      span(
        style = "font-size: 11px; font-weight: 600; padding: 3px 9px; border-radius: 10px; background: rgba(142, 142, 147, 0.10); color: #8E8E93; border: 1px solid rgba(142, 142, 147, 0.25);",
        "Total General"
      )
    }
  })
  
  output$grafico_conteo_historico <- renderPlotly({
    df_f <- datos_conteo_reactivos()
    if (is.null(df_f) || nrow(df_f) == 0) {
      return(
        plot_ly() %>%
          layout(
            title = list(text = "Sin datos de conteo para los filtros seleccionados", font = list(size = 13, color = "#8E8E93")),
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE),
            paper_bgcolor = "#FFFFFF",
            plot_bgcolor = "#FFFFFF"
          )
      )
    }
    
    es_planta    <- identical(input$switch_conteo_metrica, "planta")
    barmode_sel  <- if (!is.null(input$switch_conteo_barmode)) input$switch_conteo_barmode else "group"
    organo_sel   <- if (!is.null(input$switch_conteo_organo)) input$switch_conteo_organo else "ambos"
    unidad_eje_y <- if (es_planta) "Órganos / Planta (Conteo / Muestra)" else "Total Órganos"
    
    dims_activas <- cols_agrupar()
    dims_c <- intersect(c("Sector", "Módulo", "Turno", "Válvula", "Variedad"), dims_activas)
    
    # Agrupación según dimensiones activas
    if (length(dims_c) > 0) {
      df_grp <- df_f %>%
        group_by(across(all_of(c("Semana", "Año", dims_c)))) %>%
        summarise(
          Boton = sum(Boton, na.rm = TRUE),
          Flor  = sum(Flor, na.rm = TRUE),
          Muestra = sum(MuestraF, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          Boton_Planta = ifelse(Muestra > 0, round(Boton / Muestra, 2), 0),
          Flor_Planta  = ifelse(Muestra > 0, round(Flor / Muestra, 2), 0),
          Val_Boton    = if (es_planta) Boton_Planta else Boton,
          Val_Flor     = if (es_planta) Flor_Planta else Flor
        )
      
      partes <- lapply(dims_c, function(d) {
        val <- as.character(df_grp[[d]])
        if (d == "Sector") paste0("S", val)
        else if (d == "Módulo") paste0("M", val)
        else if (d == "Turno") paste0("T", val)
        else if (d == "Válvula") paste0("V", val)
        else val
      })
      df_grp$Serie_Label <- do.call(paste, c(partes, list(sep = " - ")))
    } else {
      df_grp <- df_f %>%
        group_by(Semana, Año) %>%
        summarise(
          Boton = sum(Boton, na.rm = TRUE),
          Flor  = sum(Flor, na.rm = TRUE),
          Muestra = sum(MuestraF, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          Boton_Planta = ifelse(Muestra > 0, round(Boton / Muestra, 2), 0),
          Flor_Planta  = ifelse(Muestra > 0, round(Flor / Muestra, 2), 0),
          Val_Boton    = if (es_planta) Boton_Planta else Boton,
          Val_Flor     = if (es_planta) Flor_Planta else Flor,
          Serie_Label  = "Total Fundo"
        )
    }
    
    # Ordenamiento cronológico estricto por Año y Semana
    df_grp <- df_grp %>%
      mutate(Orden_Cron = ifelse(!is.na(Año), as.numeric(Año) * 100 + as.numeric(Semana), as.numeric(Semana))) %>%
      arrange(Orden_Cron)
    
    sems_ordenadas <- unique(df_grp$Semana[order(df_grp$Orden_Cron)])
    df_grp$Semana_Factor <- factor(as.character(df_grp$Semana), levels = as.character(sems_ordenadas))
    
    # Formatear etiquetas de texto en barras y tooltips con la división explícita
    df_grp <- df_grp %>%
      mutate(
        Txt_Boton_Bar = if (es_planta) sprintf("%.1f", Val_Boton) else format(round(Val_Boton), big.mark = ","),
        Txt_Flor_Bar  = if (es_planta) sprintf("%.1f", Val_Flor) else format(round(Val_Flor), big.mark = ","),
        Hover_Boton = sprintf(
          "<b>Semana %s</b><br>%s<br><b>Botón: %s%s</b> (%s / %s)<br>Plantas evaluadas: %s",
          Semana, Serie_Label,
          format(round(Val_Boton, if (es_planta) 2 else 0), big.mark = ","),
          if (es_planta) " /planta" else "",
          format(round(Boton), big.mark = ","),
          format(round(Muestra), big.mark = ","),
          format(round(Muestra), big.mark = ",")
        ),
        Hover_Flor = sprintf(
          "<b>Semana %s</b><br>%s<br><b>Flor: %s%s</b> (%s / %s)<br>Plantas evaluadas: %s",
          Semana, Serie_Label,
          format(round(Val_Flor, if (es_planta) 2 else 0), big.mark = ","),
          if (es_planta) " /planta" else "",
          format(round(Flor), big.mark = ","),
          format(round(Muestra), big.mark = ","),
          format(round(Muestra), big.mark = ",")
        )
      )
    
    series_unicas <- unique(df_grp$Serie_Label)
    n_series <- length(series_unicas)
    
    p <- plot_ly()
    paleta_series <- c("#007AFF", "#34C759", "#AF52DE", "#FF9500", "#5856D6", "#30B0C7", "#FF2D55", "#FFCC00", "#5AC8FA", "#8E8E93")
    
    es_apilado <- identical(barmode_sel, "stack")
    
    # Colores definidos según muestra de usuario (#9AC8E0 Botón, #023B59 Flor)
    col_boton  <- "#9AC8E0"
    line_boton <- "#7EB2CE"
    col_flor   <- "#023B59"
    line_flor  <- "#01283D"
    
    if (n_series == 1) {
      d_s <- df_grp
      if (organo_sel %in% c("ambos", "boton")) {
        p <- p %>% add_trace(
          data = d_s, x = ~Semana_Factor, y = ~Val_Boton,
          type = "bar", name = "Botón Floral",
          text = ~Txt_Boton_Bar,
          textposition = if (es_apilado) "inside" else "outside",
          insidetextanchor = "middle",
          cliponaxis = FALSE,
          textfont = list(size = 10, color = if (es_apilado) "#1C1C1E" else "#636366"),
          marker = list(color = col_boton, line = list(color = line_boton, width = 1)),
          hovertext = ~Hover_Boton, hoverinfo = "text"
        )
      }
      if (organo_sel %in% c("ambos", "flor")) {
        p <- p %>% add_trace(
          data = d_s, x = ~Semana_Factor, y = ~Val_Flor,
          type = "bar", name = "Flor Abierta",
          text = ~Txt_Flor_Bar,
          textposition = if (es_apilado) "inside" else "outside",
          insidetextanchor = "middle",
          cliponaxis = FALSE,
          textfont = list(size = 10, color = if (es_apilado) "#FFFFFF" else "#636366"),
          marker = list(color = col_flor, line = list(color = line_flor, width = 1)),
          hovertext = ~Hover_Flor, hoverinfo = "text"
        )
      }
    } else {
      for (i in seq_along(series_unicas)) {
        s_name <- series_unicas[i]
        d_s <- df_grp %>% filter(Serie_Label == s_name)
        col_s <- paleta_series[((i - 1) %% length(paleta_series)) + 1]
        
        if (organo_sel == "ambos") {
          p <- p %>% add_trace(
            data = d_s, x = ~Semana_Factor, y = ~Val_Boton,
            type = "bar", name = paste(s_name, "- Botón"),
            text = ~Txt_Boton_Bar,
            textposition = if (es_apilado) "inside" else "outside",
            insidetextanchor = "middle",
            cliponaxis = FALSE,
            textfont = list(size = 9, color = if (es_apilado) "#1C1C1E" else "#636366"),
            marker = list(color = col_boton, line = list(color = line_boton, width = 1)),
            hovertext = ~Hover_Boton, hoverinfo = "text"
          ) %>% add_trace(
            data = d_s, x = ~Semana_Factor, y = ~Val_Flor,
            type = "bar", name = paste(s_name, "- Flor"),
            text = ~Txt_Flor_Bar,
            textposition = if (es_apilado) "inside" else "outside",
            insidetextanchor = "middle",
            cliponaxis = FALSE,
            textfont = list(size = 9, color = if (es_apilado) "#FFFFFF" else "#636366"),
            marker = list(color = col_flor, line = list(color = line_flor, width = 1)),
            hovertext = ~Hover_Flor, hoverinfo = "text"
          )
        } else if (organo_sel == "boton") {
          p <- p %>% add_trace(
            data = d_s, x = ~Semana_Factor, y = ~Val_Boton,
            type = "bar", name = s_name,
            text = ~Txt_Boton_Bar, textposition = "outside", cliponaxis = FALSE,
            textfont = list(size = 10, color = "#636366"),
            marker = list(color = col_boton, line = list(color = line_boton, width = 1)),
            hovertext = ~Hover_Boton, hoverinfo = "text"
          )
        } else if (organo_sel == "flor") {
          p <- p %>% add_trace(
            data = d_s, x = ~Semana_Factor, y = ~Val_Flor,
            type = "bar", name = s_name,
            text = ~Txt_Flor_Bar, textposition = "outside", cliponaxis = FALSE,
            textfont = list(size = 10, color = "#636366"),
            marker = list(color = col_flor, line = list(color = line_flor, width = 1)),
            hovertext = ~Hover_Flor, hoverinfo = "text"
          )
        }
      }
    }
    
    # Suma total de ambos órganos en la parte superior cuando las barras están apiladas
    if (es_apilado && organo_sel == "ambos") {
      df_tot_sem <- df_grp %>%
        group_by(Semana_Factor) %>%
        summarise(
          Val_Total = sum(Val_Boton + Val_Flor, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          Txt_Total = if (es_planta) {
            ifelse(abs(Val_Total - round(Val_Total)) < 0.05, sprintf("%.0f", Val_Total), sprintf("%.1f", Val_Total))
          } else {
            format(round(Val_Total), big.mark = ",")
          }
        )
      
      p <- p %>% add_trace(
        data = df_tot_sem,
        x = ~Semana_Factor,
        y = ~Val_Total,
        type = "scatter",
        mode = "text",
        text = ~Txt_Total,
        textposition = "top center",
        textfont = list(size = 11, color = "#1C1C1E", family = "Inter, sans-serif"),
        showlegend = FALSE,
        hoverinfo = "none",
        cliponaxis = FALSE
      )
      
      max_y <- max(df_tot_sem$Val_Total, na.rm = TRUE)
    } else {
      max_y <- max(c(df_grp$Val_Boton, df_grp$Val_Flor), na.rm = TRUE)
    }
    if (is.na(max_y) || is.infinite(max_y) || max_y <= 0) max_y <- 10
    # ==============================================================================
    # LÍNEA VERTICAL DE REFERENCIA: ORIGEN FLORAL DE LA COSECHA ACTUAL (MADURACIÓN)
    # ==============================================================================
    df_mad <- rv_maduracion()
    if (is.null(df_mad) || nrow(df_mad) == 0) {
      df_mad <- cargar_maduracion_disco()
    }
    
    combos_activos <- df_f %>%
      filter(!is.na(Módulo), nzchar(trimws(as.character(Módulo))),
             !is.na(Variedad), nzchar(trimws(as.character(Variedad)))) %>%
      distinct(Módulo = trimws(as.character(Módulo)), Variedad = trimws(as.character(Variedad)))
    
    dias_mad <- 45L
    if (nrow(combos_activos) > 0 && !is.null(df_mad) && nrow(df_mad) > 0) {
      df_mad_match <- combos_activos %>%
        left_join(df_mad, by = c("Módulo", "Variedad"))
      vals_dias <- df_mad_match$Dias_Maduracion[!is.na(df_mad_match$Dias_Maduracion) & df_mad_match$Dias_Maduracion > 0]
      if (length(vals_dias) > 0) {
        dias_mad <- as.integer(round(mean(vals_dias)))
      }
    }
    
    sem_cosecha_actual <- suppressWarnings(as.numeric(input$filtro_semana))
    if (is.na(sem_cosecha_actual) || sem_cosecha_actual <= 0) sem_cosecha_actual <- lubridate::isoweek(Sys.Date())
    sem_floracion_num  <- sem_cosecha_actual - round(as.numeric(dias_mad) / 7)
    sem_floracion_chr  <- as.character(sem_floracion_num)
    
    shapes_conteo <- list()
    annotations_conteo <- list()
    
    idx_sem_flor <- match(sem_floracion_chr, as.character(sems_ordenadas))
    if (!is.na(idx_sem_flor) && idx_sem_flor > 0) {
      idx_x_0based <- idx_sem_flor - 1
      shapes_conteo <- list(
        list(
          type = "line",
          layer = "above",
          x0 = idx_x_0based,
          x1 = idx_x_0based,
          y0 = 0,
          y1 = 1,
          yref = "paper",
          line = list(
            color = "#E11D48",
            width = 2.5,
            dash = "dash"
          )
        )
      )
      
      # Calcular pendiente entre la semana origen y la semana anterior para enriquecer la tarjeta
      sem_flor_ant_chr <- as.character(as.numeric(sem_floracion_chr) - 1)
      val_origen <- df_grp %>%
        filter(as.character(Semana) == sem_floracion_chr) %>%
        summarise(Total = sum(Val_Boton + Val_Flor, na.rm = TRUE)) %>%
        pull(Total)
      val_ant <- df_grp %>%
        filter(as.character(Semana) == sem_flor_ant_chr) %>%
        summarise(Total = sum(Val_Boton + Val_Flor, na.rm = TRUE)) %>%
        pull(Total)
      
      delta_sube_baja <- if (length(val_origen) > 0 && length(val_ant) > 0 && !is.na(val_origen) && !is.na(val_ant) && val_ant > 0) {
        (val_origen - val_ant) / val_ant
      } else {
        NA_real_
      }
      
      sens_factor <- rv_sensibilidad_floral() / 100
      badge_tendencia <- if (!is.na(delta_sube_baja)) {
        ajuste_sens <- pmin(0.40, pmax(-0.40, delta_sube_baja * sens_factor))
        if (delta_sube_baja > 0.005) {
          sprintf("<br><span style='color:#16A34A;font-weight:700;'>▲ +%.1f%% vs S-1</span> <span style='color:#15803D;'>(Premio %+0.1f%%)</span>", delta_sube_baja * 100, ajuste_sens * 100)
        } else if (delta_sube_baja < -0.005) {
          sprintf("<br><span style='color:#DC2626;font-weight:700;'>▼ %.1f%% vs S-1</span> <span style='color:#B91C1C;'>(Castigo %+0.1f%%)</span>", delta_sube_baja * 100, ajuste_sens * 100)
        } else {
          "<br><span style='color:#6B7280;font-weight:600;'>━ 0.0% vs S-1 (Neutro)</span>"
        }
      } else {
        ""
      }
      
      annotations_conteo <- list(
        list(
          x = idx_x_0based,
          y = 1.07,
          yref = "paper",
          text = sprintf(
            "<b>📍 Origen Cosecha S%d</b><br><span style='font-size:9.5px;color:#8E8E93;'>(-%d días • Sem %s)</span>%s",
            sem_cosecha_actual, dias_mad, sem_floracion_chr, badge_tendencia
          ),
          showarrow = FALSE,
          xanchor = "center",
          font = list(
            size = 10,
            color = "#1C1C1E",
            family = "-apple-system, BlinkMacSystemFont, 'Inter', sans-serif"
          ),
          bgcolor = "rgba(255, 255, 255, 0.96)",
          bordercolor = if (!is.na(delta_sube_baja) && delta_sube_baja > 0.005) "#16A34A" else if (!is.na(delta_sube_baja) && delta_sube_baja < -0.005) "#DC2626" else "#E11D48",
          borderwidth = 1.5,
          borderpad = 4
        )
      )
    }
    
    p %>% layout(
      barmode = barmode_sel,
      shapes = shapes_conteo,
      annotations = annotations_conteo,
      xaxis = list(
        title = list(text = "Semana de Conteo", font = list(size = 12, color = "#1C1C1E")),
        tickfont = list(size = 11, color = "#636366"),
        showgrid = FALSE
      ),
      yaxis = list(
        title = list(text = unidad_eje_y, font = list(size = 12, color = "#1C1C1E")),
        tickfont = list(size = 11, color = "#636366"),
        gridcolor = "rgba(0,0,0,0.06)",
        zerolinecolor = "rgba(0,0,0,0.15)",
        range = c(0, max_y * 1.15)
      ),
      legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = -0.15,
        font = list(size = 11, color = "#1C1C1E")
      ),
      margin = list(l = 50, r = 20, t = 58, b = 60),
      paper_bgcolor = "#FFFFFF",
      plot_bgcolor = "#FFFFFF"
    ) %>%
      plotly::config(displayModeBar = FALSE)
  })
  outputOptions(output, "ui_filtro_semana_mapa", suspendWhenHidden = FALSE)
  
  # Al cambiar cualquier filtro general, actualizar automáticamente la semana a la última con datos
  observeEvent(list(input$filtro_modulo, input$filtro_grupo, input$filtro_turno, input$filtro_variedad), {
    info <- semanas_info_mapa()
    updateSelectInput(
      session,
      "filtro_semana_mapa",
      choices  = setNames(info$disponibles, paste("Semana", info$disponibles)),
      selected = info$defecto
    )
  }, ignoreInit = TRUE)
  
  # Badge Superior Flotante Dinámico en el Canvas
  output$ui_mapa_badge_flotante <- renderUI({
    modo <- if (!is.null(input$filtro_modo_mapa)) as.character(input$filtro_modo_mapa) else "real"
    if (modo == "ajustado") {
      div(style = "position: absolute; left: 16px; top: 16px; background: rgba(255, 255, 255, 0.96); border: 1px solid rgba(255, 149, 0, 0.35); border-radius: 8px; padding: 6px 14px; font-size: 12px; font-weight: 600; color: #1C1C1E; box-shadow: 0 2px 10px rgba(255, 149, 0, 0.12); display: flex; align-items: center; gap: 8px; backdrop-filter: blur(4px); z-index: 10;",
          span(style = "display: inline-block; width: 8px; height: 8px; border-radius: 50%; background: #FF9500; box-shadow: 0 0 6px rgba(255, 149, 0, 0.6);"),
          span(style = "color: #D97706; font-weight: 700;", "MAPA PROYECTADO"),
          span(style = "color: #8E8E93;", "•"),
          span("Ajuste Semanal Activo (Simulación)")
      )
    } else {
      div(style = "position: absolute; left: 16px; top: 16px; background: rgba(255, 255, 255, 0.94); border: 1px solid rgba(0,0,0,0.1); border-radius: 8px; padding: 6px 12px; font-size: 12px; font-weight: 600; color: #1C1C1E; box-shadow: 0 2px 8px rgba(0,0,0,0.06); display: flex; align-items: center; gap: 6px; backdrop-filter: blur(4px); z-index: 10;",
          span(style = "display: inline-block; width: 8px; height: 8px; border-radius: 50%; background: #007AFF;"),
          "Mapa de Cosecha Oficial • Datos Cruzados (Solo Lectura)"
      )
    }
  })

  # Despachar datos del mapa hacia JavaScript
  observeEvent(list(input$filtro_semana_mapa, input$filtro_pasada_mapa, input$filtro_modulo, input$filtro_grupo, input$filtro_turno, input$filtro_variedad, input$filtro_modo_mapa, input$tabs_navegacion, rv_ajuste_valvulas(), ediciones_manuales()), {
    req(input$tabs_navegacion == "Ruta de cosecha")
    
    info <- semanas_info_mapa()
    
    sem_target <- if (!is.null(input$filtro_semana_mapa) && nzchar(as.character(input$filtro_semana_mapa))) {
      as.numeric(input$filtro_semana_mapa)
    } else {
      info$defecto
    }
    
    pasada_target <- if (!is.null(input$filtro_pasada_mapa) && nzchar(as.character(input$filtro_pasada_mapa))) {
      as.character(input$filtro_pasada_mapa)
    } else {
      "1"
    }
    
    modo_target <- if (!is.null(input$filtro_modo_mapa)) as.character(input$filtro_modo_mapa) else "real"
    camp_target <- if (!is.null(input$filtro_campana)) as.character(input$filtro_campana) else "2026"
    
    valvs_ajuste_tot <- rv_ajuste_valvulas()
    valvs_ajuste_sem <- if (!is.null(valvs_ajuste_tot) && is.data.frame(valvs_ajuste_tot) && nrow(valvs_ajuste_tot) > 0) {
      valvs_ajuste_tot %>%
        filter(
          as.character(Campaña) == camp_target,
          as.numeric(Semana)    == as.numeric(sem_target)
        )
    } else {
      tibble()
    }
    
    ed_totales <- ediciones_manuales()
    tiene_ed_manuales_sem <- !is.null(ed_totales) && nrow(ed_totales) > 0 &&
      any(as.character(ed_totales$Campaña) == camp_target & 
          as.numeric(ed_totales$Semana) == as.numeric(sem_target) &
          coalesce(ed_totales$Tipo_Edicion, "") %in% c("auto", "manual") &
          coalesce(ed_totales$Valor, 0) > 0)
    
    if (modo_target == "ajustado" && nrow(valvs_ajuste_sem) == 0 && !tiene_ed_manuales_sem) {
      showNotification(
        sprintf("ℹ️ No hay un ajuste calculado para la Semana %s. Se muestra la vista Real. Haz clic en 'Ajustar Semana' en Detalle Semanal para proyectar.", sem_target),
        type = "warning",
        duration = 4
      )
      updateRadioButtons(session, "filtro_modo_mapa", selected = "real")
      modo_target <- "real"
    }
    
    payload_mapa <- obtener_datos_mapa_semana(
      sem_num         = sem_target,
      pasada_filtro   = pasada_target,
      grupos_filtro   = input$filtro_grupo,
      mods_filtro     = input$filtro_modulo,
      turnos_filtro   = input$filtro_turno,
      vars_filtro     = input$filtro_variedad,
      modo_mapa       = modo_target,
      df_ajuste_valvs = valvs_ajuste_sem,
      df_ediciones    = ed_totales
    )
    session$sendCustomMessage("actualizar_mapa_semana", payload_mapa)
  }, ignoreInit = FALSE)
  
  # ==============================================================================
  # SECCIÓN: FRECUENCIA DE COSECHA (EVOLUCIÓN POR TURNO & TURNOS POR SEMANA)
  # ==============================================================================
  
  # Reactive val para turno seleccionado en tabla/gráfico
  rv_frecuencia_turno_sel <- reactiveVal(NULL)
  
  # Observer para click en fila de la tabla heatmap
  observeEvent(input$frecuencia_turno_click, {
    req(input$frecuencia_turno_click)
    current_sel <- rv_frecuencia_turno_sel()
    if (!is.null(current_sel) && identical(current_sel, input$frecuencia_turno_click)) {
      # Si ya estaba seleccionado, deseleccionar
      rv_frecuencia_turno_sel(NULL)
    } else {
      rv_frecuencia_turno_sel(input$frecuencia_turno_click)
    }
  })
  
  # Observer para limpiar selección desde el badge
  observeEvent(input$btn_reset_filtro_frec, {
    rv_frecuencia_turno_sel(NULL)
  })
  
  # Limpiar selección al cambiar filtros globales o agrupación
  observeEvent(list(input$filtro_campana, input$filtro_modulo, input$filtro_grupo, input$filtro_turno, input$filtro_variedad, cols_agrupar()), {
    rv_frecuencia_turno_sel(NULL)
  }, ignoreInit = TRUE)
  
  # Títulos y subtítulos dinámicos según el nivel de agrupación (Turno vs Válvula)
  output$titulo_grafico_frecuencia <- renderText({
    if ("Válvula" %in% cols_agrupar()) "Evolución por válvula" else "Evolución por turno"
  })
  
  output$subtitulo_grafico_frecuencia <- renderText({
    if ("Válvula" %in% cols_agrupar()) "Frecuencia semanal de cada válvula seleccionada." else "Frecuencia semanal de cada turno seleccionado."
  })
  
  output$titulo_tabla_frecuencia <- renderText({
    if ("Válvula" %in% cols_agrupar()) "Válvulas por semana" else "Turnos por semana"
  })
  
  output$ui_hint_grafico_frecuencia <- renderText({
    if ("Válvula" %in% cols_agrupar()) "Haz clic en una válvula para aislarla" else "Haz clic en un turno para aislarlo"
  })
  
  # Reactive que calcula las frecuencias de cosecha semanales por turno o válvula
  frecuencia_campana_data <- reactive({
    camp_sel <- input$filtro_campana
    req(camp_sel)
    es_camp_act      <- grepl("2026|^26", as.character(camp_sel))
    es_nivel_valvula <- "Válvula" %in% cols_agrupar()
    
    # 1. Base de datos
    df_res <- if (es_camp_act && exists("df_hist_oficial_wide", inherits = TRUE) && !is.null(df_hist_oficial_wide)) {
      dias_cols <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
      dias_map  <- setNames(1:6, dias_cols)
      df_w <- df_hist_oficial_wide %>% filter(!is.na(Codigo_Valv), nzchar(as.character(Codigo_Valv)))
      
      events_list <- list()
      for (d in dias_cols) {
        kg_col <- paste0("Kg_", d)
        if (kg_col %in% names(df_w)) {
          sub_d <- df_w %>%
            filter(!is.na(.data[[kg_col]]), .data[[kg_col]] > 0) %>%
            select(Semana, Módulo = Módulo_Norm, Turno = Turno_Norm, Variedad, Codigo_Valv, Kg = all_of(kg_col)) %>%
            mutate(Dia_Num = dias_map[d], Dia_Abs = (as.numeric(Semana) - 1) * 7 + dias_map[d])
          events_list[[d]] <- sub_d
        }
      }
      all_ev <- bind_rows(events_list) %>% arrange(Codigo_Valv, Dia_Abs)
      all_ev <- all_ev %>%
        group_by(Codigo_Valv) %>%
        mutate(diff_days = Dia_Abs - lag(Dia_Abs)) %>%
        ungroup() %>%
        filter(!is.na(diff_days), diff_days >= 1, diff_days <= 35)
      
      if (es_nivel_valvula) {
        all_ev <- all_ev %>%
          mutate(
            Valvula_num = suppressWarnings(as.numeric(sub(".*-V0*", "", Codigo_Valv))),
            Valvula_str = paste0("V", sub(".*-V0*", "", Codigo_Valv))
          )
        all_ev %>%
          group_by(Módulo, Turno, Codigo_Valv, Valvula_num, Valvula_str, Variedad, Semana) %>%
          summarise(
            Frecuencia = round(weighted.mean(diff_days, w = Kg), 1),
            Total_Kg = sum(Kg, na.rm = TRUE),
            .groups = "drop"
          )
      } else {
        all_ev %>%
          group_by(Módulo, Turno, Variedad, Semana) %>%
          summarise(
            Frecuencia = round(weighted.mean(diff_days, w = Kg), 1),
            Total_Kg = sum(Kg, na.rm = TRUE),
            .groups = "drop"
          )
      }
    } else {
      # Fallback histórico desde df_cosecha
      df_c <- rv_cosecha() %>%
        filter(
          as.character(Campaña) == as.character(camp_sel),
          coalesce(`Kg Total`, 0) > 0,
          coalesce(Es_Testblock, "NO") != "SI"
        ) %>%
        mutate(
          Fecha_d    = as.Date(Fecha),
          Semana_n   = as.numeric(Semana),
          Módulo_c   = as.character(Módulo),
          Turno_c    = as.character(Turno),
          Valvula_c  = as.character(Válvula),
          Variedad_c = limpiar_variedad(Variedad)
        )
      
      valv_ev <- df_c %>%
        group_by(Módulo_c, Turno_c, Valvula_c, Variedad_c, Fecha_d, Semana_n) %>%
        summarise(Kg = sum(`Kg Total`, na.rm = TRUE), .groups = "drop") %>%
        arrange(Módulo_c, Turno_c, Valvula_c, Fecha_d) %>%
        group_by(Módulo_c, Turno_c, Valvula_c, Variedad_c) %>%
        mutate(diff_days = as.numeric(Fecha_d - lag(Fecha_d))) %>%
        ungroup() %>%
        filter(!is.na(diff_days), diff_days >= 1, diff_days <= 35)
      
      if (es_nivel_valvula) {
        valv_ev %>%
          mutate(
            Valvula_num = suppressWarnings(as.numeric(Valvula_c)),
            Valvula_str = paste0("V", Valvula_c),
            Codigo_Valv = paste0("M", Módulo_c, "-T", Turno_c, "-V", Valvula_c)
          ) %>%
          group_by(Módulo = Módulo_c, Turno = Turno_c, Codigo_Valv, Valvula_num, Valvula_str, Variedad = Variedad_c, Semana = Semana_n) %>%
          summarise(
            Frecuencia = round(weighted.mean(diff_days, w = Kg), 1),
            Total_Kg = sum(Kg, na.rm = TRUE),
            .groups = "drop"
          )
      } else {
        valv_ev %>%
          group_by(Módulo = Módulo_c, Turno = Turno_c, Variedad = Variedad_c, Semana = Semana_n) %>%
          summarise(
            Frecuencia = round(weighted.mean(diff_days, w = Kg), 1),
            Total_Kg = sum(Kg, na.rm = TRUE),
            .groups = "drop"
          )
      }
    }
    
    if (nrow(df_res) == 0) return(df_res)
    
    # Normalizar turnos y submódulos (ej. 13.1 vs 13.2)
    df_res <- df_res %>%
      mutate(
        Turno_Num  = suppressWarnings(as.numeric(Turno)),
        Modulo_Disp = case_when(
          as.character(Módulo) == "13" & between(Turno_Num, 1, 9)  ~ "13.1",
          as.character(Módulo) == "13" & Turno_Num >= 10           ~ "13.2",
          as.character(Módulo) == "14" & between(Turno_Num, 1, 8)  ~ "14.1",
          as.character(Módulo) == "14" & Turno_Num >= 9            ~ "14.2",
          as.character(Módulo) == "9"  & between(Turno_Num, 1, 11) ~ "9.2",
          as.character(Módulo) == "9"  & Turno_Num >= 12           ~ "9.1",
          as.character(Módulo) == "11" & between(Turno_Num, 1, 12) ~ "11.1",
          as.character(Módulo) == "11" & Turno_Num >= 13          ~ "11.2",
          TRUE                                                     ~ as.character(Módulo)
        ),
        Turno_Key   = if (es_nivel_valvula) {
          paste0("M", Modulo_Disp, " · T", Turno, " · ", Valvula_str)
        } else {
          paste0("M", Modulo_Disp, " · T", Turno)
        },
        Variedad_c  = limpiar_variedad(Variedad),
        Serie_Label = if (es_nivel_valvula) {
          paste0("M", Modulo_Disp, " · T", Turno, " · ", Valvula_str, " · ", Variedad_c)
        } else {
          paste0("M", Modulo_Disp, " · T", Turno, " · ", Variedad_c)
        },
        Semana_Num  = as.numeric(Semana)
      )
    
    # 2. Aplicar filtros globales de la aplicación
    # Filtro Grupo
    if (!is.null(input$filtro_grupo) && length(input$filtro_grupo) > 0) {
      mods_grupo <- as.character(unlist(grupos_modulos[input$filtro_grupo], use.names = FALSE))
      df_res <- df_res %>%
        filter(as.character(Módulo) %in% mods_grupo | sub("\\..*", "", as.character(Módulo)) %in% mods_grupo |
                 Modulo_Disp %in% mods_grupo | sub("\\..*", "", Modulo_Disp) %in% mods_grupo)
    }
    
    # Filtro Módulo
    if (!is.null(input$filtro_modulo) && length(input$filtro_modulo) > 0) {
      df_res <- df_res %>%
        filter(as.character(Módulo) %in% as.character(input$filtro_modulo) |
                 sub("\\..*", "", as.character(Módulo)) %in% sub("\\..*", "", as.character(input$filtro_modulo)) |
                 Modulo_Disp %in% as.character(input$filtro_modulo) |
                 sub("\\..*", "", Modulo_Disp) %in% sub("\\..*", "", as.character(input$filtro_modulo)))
    }
    
    # Filtro Turno
    if (!is.null(input$filtro_turno) && length(input$filtro_turno) > 0) {
      df_res <- df_res %>%
        filter(as.character(Turno) %in% as.character(input$filtro_turno))
    }
    
    # Filtro Variedad
    if (!is.null(input$filtro_variedad) && length(input$filtro_variedad) > 0) {
      df_res <- df_res %>%
        filter(limpiar_variedad(Variedad) %in% limpiar_variedad(input$filtro_variedad))
    }
    
    df_res
  })
  
  # Badge de selección para la tarjeta de tabla heatmap
  output$ui_badge_filtro_turno <- renderUI({
    sel <- rv_frecuencia_turno_sel()
    if (!is.null(sel) && nzchar(sel)) {
      tagList(
        span(style = "font-weight: 700; color: #007AFF;", paste("Filtrado:", sel)),
        actionLink(
          "btn_reset_filtro_frec",
          label = "✕ Limpiar",
          style = "margin-left: 8px; color: #FF3B30; font-weight: 700; text-decoration: none; cursor: pointer; font-size: 11px;"
        )
      )
    } else {
      "Selecciona una fila"
    }
  })
  
  # Gráfico Plotly: Evolución por turno o válvula
  output$grafico_frecuencia_turno <- renderPlotly({
    df_freq <- frecuencia_campana_data()
    
    if (is.null(df_freq) || nrow(df_freq) == 0) {
      return(
        plot_ly() %>%
          layout(
            title = list(text = "Sin datos de frecuencia para los filtros seleccionados", font = list(size = 13, color = "#8E8E93")),
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE),
            paper_bgcolor = "#FFFFFF",
            plot_bgcolor = "#FFFFFF"
          )
      )
    }
    
    turno_sel        <- rv_frecuencia_turno_sel()
    es_nivel_valvula <- "Válvula" %in% cols_agrupar()
    
    # Determinar rango de semanas ordenado cronológicamente
    sems_camp <- sort(unique(df_freq$Semana_Num))
    sems_camp <- sems_camp[order(orden_semana_campana(sems_camp))]
    
    # Lista de series únicas ordenadas por submódulo, turno y válvula (si aplica)
    series_u <- if (es_nivel_valvula && "Valvula_num" %in% names(df_freq)) {
      df_freq %>%
        distinct(Turno_Key, Modulo_Disp, Turno_Num, Valvula_num, Serie_Label) %>%
        mutate(
          ord_mod = suppressWarnings(as.numeric(sub("^M", "", Modulo_Disp))),
          ord_tur = coalesce(Turno_Num, 0),
          ord_val = coalesce(Valvula_num, 0)
        ) %>%
        arrange(ord_mod, Modulo_Disp, ord_tur, ord_val)
    } else {
      df_freq %>%
        distinct(Turno_Key, Modulo_Disp, Turno_Num, Serie_Label) %>%
        mutate(
          ord_mod = suppressWarnings(as.numeric(sub("^M", "", Modulo_Disp))),
          ord_tur = coalesce(Turno_Num, 0)
        ) %>%
        arrange(ord_mod, Modulo_Disp, ord_tur)
    }
    
    # Paleta de colores distintivos estilo Image 1
    paleta_freq <- c(
      "#0E7A68", "#D9531E", "#0066CC", "#AF2B7B", "#5C7829", 
      "#5A4B81", "#D9822B", "#1D829B", "#995D3F", "#3B82F6",
      "#10B981", "#F59E0B", "#EF4444", "#8B5CF6", "#EC4899"
    )
    
    p <- plot_ly()
    
    for (i in seq_len(nrow(series_u))) {
      t_key   <- series_u$Turno_Key[i]
      s_label <- series_u$Serie_Label[i]
      
      sub_s <- df_freq %>%
        filter(Turno_Key == t_key) %>%
        arrange(orden_semana_campana(Semana_Num))
      
      if (nrow(sub_s) == 0) next
      
      col_serie <- paleta_freq[((i - 1) %% length(paleta_freq)) + 1]
      
      es_aislado <- !is.null(turno_sel)
      es_el_sel  <- identical(turno_sel, t_key)
      
      line_width <- if (es_aislado) {
        if (es_el_sel) 3.5 else 1.2
      } else {
        2.2
      }
      
      opacity_val <- if (es_aislado) {
        if (es_el_sel) 1.0 else 0.16
      } else {
        0.92
      }
      
      marker_size <- if (es_aislado) {
        if (es_el_sel) 7.5 else 4
      } else {
        5.5
      }
      
      hover_text <- sprintf("<b>%s</b><br>Semana S%d: <b>%.1f</b> días entre cosechas", s_label, sub_s$Semana_Num, sub_s$Frecuencia)
      
      p <- p %>% add_trace(
        data        = sub_s,
        x           = ~Semana_Num,
        y           = ~Frecuencia,
        name        = s_label,
        type        = "scatter",
        mode        = "lines+markers",
        line        = list(color = col_serie, width = line_width),
        marker      = list(color = col_serie, size = marker_size),
        opacity     = opacity_val,
        text        = hover_text,
        hoverinfo   = "text"
      )
    }
    
    tick_vals <- sems_camp
    tick_text <- paste0("S", tick_vals)
    
    p <- p %>% layout(
      xaxis = list(
        title = "",
        tickmode = "array",
        tickvals = tick_vals,
        ticktext = tick_text,
        gridcolor = "#F0F0F2",
        zeroline = FALSE,
        tickfont = list(size = 11, color = "#8E8E93", family = "-apple-system, BlinkMacSystemFont, 'Inter', sans-serif")
      ),
      yaxis = list(
        title = "",
        gridcolor = "#F0F0F2",
        zeroline = FALSE,
        tickfont = list(size = 11, color = "#8E8E93", family = "-apple-system, BlinkMacSystemFont, 'Inter', sans-serif"),
        dtick = 1.2
      ),
      hoverlabel = list(
        bgcolor = "#1C1C1E",
        bordercolor = "#1C1C1E",
        font = list(color = "#FFFFFF", family = "-apple-system, BlinkMacSystemFont, 'Inter', sans-serif", size = 12)
      ),
      legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = -0.25,
        font = list(size = 10.5, color = "#1C1C1E", family = "-apple-system, BlinkMacSystemFont, 'Inter', sans-serif")
      ),
      margin = list(l = 40, r = 20, t = 10, b = 75),
      paper_bgcolor = "#FFFFFF",
      plot_bgcolor = "#FFFFFF"
    ) %>%
      plotly::config(displaylogo = FALSE, modeBarButtonsToRemove = c("lasso2d", "select2d"))
    
    p
  })
  
  # Tabla Heatmap Matrix: Turnos o Válvulas por semana
  output$tabla_heatmap_frecuencia <- renderUI({
    df_freq <- frecuencia_campana_data()
    
    if (is.null(df_freq) || nrow(df_freq) == 0) {
      return(div(style = "text-align: center; color: #8E8E93; padding: 24px;", "No hay datos de frecuencia para los filtros seleccionados."))
    }
    
    turno_sel        <- rv_frecuencia_turno_sel()
    es_nivel_valvula <- "Válvula" %in% cols_agrupar()
    
    # Semanas de campaña
    sems_todas <- sort(unique(df_freq$Semana_Num))
    sems_todas <- sems_todas[order(orden_semana_campana(sems_todas))]
    
    # Completar rango continuo de semanas para la matriz visual (ej. S20 a S44)
    min_sem <- min(sems_todas, na.rm = TRUE)
    max_sem <- max(sems_todas, na.rm = TRUE)
    sems_matrix <- min_sem:max_sem
    
    # Listado de turnos o válvulas ordenados
    turnos_info <- if (es_nivel_valvula && "Valvula_num" %in% names(df_freq)) {
      df_freq %>%
        distinct(Turno_Key, Modulo_Disp, Turno_Num, Valvula_num, Variedad_c) %>%
        mutate(
          ord_mod = suppressWarnings(as.numeric(sub("^M", "", Modulo_Disp))),
          ord_tur = coalesce(Turno_Num, 0),
          ord_val = coalesce(Valvula_num, 0)
        ) %>%
        arrange(ord_mod, Modulo_Disp, ord_tur, ord_val)
    } else {
      df_freq %>%
        distinct(Turno_Key, Modulo_Disp, Turno_Num, Variedad_c) %>%
        mutate(
          ord_mod = suppressWarnings(as.numeric(sub("^M", "", Modulo_Disp))),
          ord_tur = coalesce(Turno_Num, 0)
        ) %>%
        arrange(ord_mod, Modulo_Disp, ord_tur)
    }
    
    # Lookup rápido de frecuencia por Turno_Key y Semana
    freq_lookup <- setNames(df_freq$Frecuencia, paste(df_freq$Turno_Key, df_freq$Semana_Num, sep = "___"))
    
    # Cabecera con título dinámico de columna
    header_col_title <- if (es_nivel_valvula) "Módulo · turno · válvula" else "Módulo · turno"
    header_cells <- list(
      tags$th(class = "col-turno-header", header_col_title)
    )
    for (s in sems_matrix) {
      header_cells[[length(header_cells) + 1]] <- tags$th(paste0("S", s))
    }
    thead <- tags$thead(tags$tr(header_cells))
    
    # Filas
    rows_list <- list()
    for (i in seq_len(nrow(turnos_info))) {
      t_key   <- turnos_info$Turno_Key[i]
      is_sel  <- !is.null(turno_sel) && identical(turno_sel, t_key)
      row_cls <- paste("freq-heatmap-row", if (is_sel) "row-selected" else "")
      
      cells <- list(
        tags$td(class = "cell-turno-label", t_key)
      )
      
      for (s in sems_matrix) {
        k <- paste(t_key, s, sep = "___")
        val <- freq_lookup[k]
        val_num <- suppressWarnings(as.numeric(val[1]))
        es_val <- !is.na(val_num) && is.finite(val_num)
        c_info <- color_frecuencia(val_num)
        
        cells[[length(cells) + 1]] <- tags$td(
          div(
            class = paste("freq-cell", if (es_val) "" else "freq-cell-empty"),
            style = if (es_val) sprintf("background: %s; color: %s;", c_info$bg, c_info$text) else NULL,
            c_info$str
          )
        )
      }
      
      rows_list[[length(rows_list) + 1]] <- tags$tr(
        class = row_cls,
        `data-turno-key` = t_key,
        title = paste("Haz clic para aislar", t_key, "en el gráfico"),
        cells
      )
    }
    
    tags$table(class = "freq-heatmap-table", thead, tags$tbody(rows_list))
  })
  

# Ejecutar la app
  # Badge Dinámico en Detalle Semanal cuando la vista es Ajustado
  # ==============================================================================
  # MÓDULO DE CONCILIACIÓN CAMPO VS SAP (AUDITORÍA DE CRUCE)
  # ==============================================================================
  datos_conciliacion_reactivo <- reactive({
    req(input$filtro_campana, input$filtro_semana)
    trigger_refresh()
    
    sem_sel  <- as.numeric(input$filtro_semana)
    camp_sel <- as.character(input$filtro_campana)
    
    df_concil <- tryCatch(obtener_cosecha_conciliada_semana(sem_sel, camp_sel), error = function(e) NULL)
    if (is.null(df_concil) || nrow(df_concil) == 0) return(tibble())
    
    obtener_dias_str <- function(lun, mar, mie, jue, vie, sab, sufijo = "") {
      vals <- c(Lunes = lun, Martes = mar, Miércoles = mie, Jueves = jue, Viernes = vie, Sábado = sab)
      activos <- names(vals)[vals > 0]
      if (length(activos) == 0) return("-")
      paste(sapply(activos, function(d) sprintf("%s (%s%s)", d, round(vals[d], 1), sufijo)), collapse = ", ")
    }
    
    df_res <- df_concil %>%
      rowwise() %>%
      mutate(
        Estado        = Estado_Concil,
        Detalle_Campo = obtener_dias_str(Ha_Lunes_rc, Ha_Martes_rc, Ha_Miercoles_rc, Ha_Jueves_rc, Ha_Viernes_rc, Ha_Sabado_rc, " ha"),
        Detalle_SAP   = obtener_dias_str(Kg_Lunes_sap, Kg_Martes_sap, Kg_Miercoles_sap, Kg_Jueves_sap, Kg_Viernes_sap, Kg_Sabado_sap, " kg")
      ) %>%
      ungroup()
    
    # Filtros globales superiores (Grupo, Módulo, Turno, Variedad)
    if (!is.null(input$filtro_grupo) && length(input$filtro_grupo) > 0) {
      mods_grupo <- as.character(unlist(grupos_modulos[input$filtro_grupo], use.names = FALSE))
      df_res <- df_res %>% filter(as.character(Módulo) %in% mods_grupo | sub("\\..*", "", as.character(Módulo)) %in% mods_grupo)
    }
    if (!is.null(input$filtro_modulo) && length(input$filtro_modulo) > 0) {
      df_res <- df_res %>% filter(as.character(Módulo) %in% as.character(input$filtro_modulo) | sub("\\..*", "", as.character(Módulo)) %in% as.character(input$filtro_modulo))
    }
    if (!is.null(input$filtro_turno) && length(input$filtro_turno) > 0) {
      df_res <- df_res %>% filter(as.character(Turno) %in% as.character(input$filtro_turno))
    }
    if (!is.null(input$filtro_variedad) && length(input$filtro_variedad) > 0) {
      df_res <- df_res %>% filter(limpiar_variedad(Variedad) %in% limpiar_variedad(input$filtro_variedad))
    }
    
    df_res
  })
  
  # Banner contextual de Regla D+1
  output$ui_alerta_regla_d1 <- renderUI({
    req(input$filtro_semana)
    sem_sel  <- as.numeric(input$filtro_semana)
    fecha_h  <- Sys.Date()
    sem_act  <- lubridate::isoweek(fecha_h)
    wday_h   <- lubridate::wday(fecha_h, week_start = 1)
    dias_nom <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
    dia_txt  <- if (wday_h <= 6) dias_nom[wday_h] else "Domingo"
    
    if (sem_sel == sem_act) {
      div(style = "background: rgba(0, 122, 255, 0.08); border: 1px solid rgba(0, 122, 255, 0.20); border-radius: 9px; padding: 8px 12px; font-size: 11.5px; color: #004085; display: flex; align-items: center; gap: 8px;",
          icon("circle-info", style = "color: #007AFF; font-size: 14px;"),
          span(HTML(sprintf("<b>Semana en Curso (%d):</b> Hoy es <b>%s</b>. Las válvulas cosechadas hoy en campo se encuentran en proceso natural en packing y sus kilos se reportarán mañana (ciclo D+1). Por regla operativa, <b>no se marcan como error</b> en la lista de inconsistencias.", sem_act, dia_txt)))
      )
    } else if (sem_sel < sem_act) {
      div(style = "background: rgba(52, 199, 89, 0.08); border: 1px solid rgba(52, 199, 89, 0.20); border-radius: 9px; padding: 8px 12px; font-size: 11.5px; color: #155724; display: flex; align-items: center; gap: 8px;",
          icon("check-double", style = "color: #28A745; font-size: 14px;"),
          span(HTML(sprintf("<b>Semana Cerrada (%d):</b> Todos los turnos y balanzas de packing ya concluyeron su reporte. Cualquier ruta sin kilos o desfase es una discrepancia definitiva a conciliar.", sem_sel)))
      )
    } else {
      div(style = "background: rgba(255, 149, 0, 0.08); border: 1px solid rgba(255, 149, 0, 0.20); border-radius: 9px; padding: 8px 12px; font-size: 11.5px; color: #856404; display: flex; align-items: center; gap: 8px;",
          icon("calendar-days", style = "color: #FF9500; font-size: 14px;"),
          span(HTML(sprintf("<b>Semana Futura (%d):</b> Datos de proyección y programación estimada.", sem_sel)))
      )
    }
  })
  
  # KPIs de Conciliación
  output$kpi_concil_ok <- renderText({
    df <- datos_conciliacion_reactivo()
    if (nrow(df) == 0) return("0")
    sum(df$Estado == "Conciliado OK", na.rm = TRUE)
  })
  output$kpi_concil_ok_sub <- renderText({
    df <- datos_conciliacion_reactivo()
    if (nrow(df) == 0) return("0% total")
    pct <- round((sum(df$Estado == "Conciliado OK", na.rm = TRUE) / nrow(df)) * 100, 1)
    sprintf("%s%% del universo", pct)
  })
  
  output$kpi_concil_sin_sap <- renderText({
    df <- datos_conciliacion_reactivo()
    if (nrow(df) == 0) return("0")
    sum(df$Estado == "Ruta sin Kilos SAP", na.rm = TRUE)
  })
  output$kpi_concil_sin_sap_sub <- renderText({
    df <- datos_conciliacion_reactivo()
    if (nrow(df) == 0 || !"Estado" %in% names(df)) return("0 ha afectadas")
    sub_df <- df %>% filter(Estado == "Ruta sin Kilos SAP")
    ha_tot <- round(sum(sub_df$Ha_Total_rc, na.rm = TRUE), 1)
    sprintf("%s ha afectadas", ha_tot)
  })
  
  output$kpi_concil_sin_ruta <- renderText({
    df <- datos_conciliacion_reactivo()
    if (nrow(df) == 0) return("0")
    sum(df$Estado == "Kilos SAP sin Ruta", na.rm = TRUE)
  })
  output$kpi_concil_sin_ruta_sub <- renderText({
    df <- datos_conciliacion_reactivo()
    if (nrow(df) == 0 || !"Estado" %in% names(df)) return("0 kg en balanza")
    sub_df <- df %>% filter(Estado == "Kilos SAP sin Ruta")
    kg_tot <- round(sum(sub_df$Kg_Total_sap, na.rm = TRUE), 0)
    sprintf("%s kg en balanza", format(kg_tot, big.mark = ","))
  })
  
  output$kpi_concil_desfase <- renderText({
    df <- datos_conciliacion_reactivo()
    if (nrow(df) == 0) return("0")
    sum(df$Estado %in% c("Desfase de Día", "Desfase Reasignado"), na.rm = TRUE)
  })
  output$kpi_concil_desfase_sub <- renderText({
    df <- datos_conciliacion_reactivo()
    if (nrow(df) == 0) return("0 válvulas")
    "Fecha Campo vs SAP"
  })
  
  output$kpi_concil_proceso <- renderText({
    df <- datos_conciliacion_reactivo()
    if (nrow(df) == 0) return("0")
    sum(df$Estado == "Cosecha Hoy (D+1)", na.rm = TRUE)
  })
  output$kpi_concil_proceso_sub <- renderText({
    df <- datos_conciliacion_reactivo()
    if (nrow(df) == 0 || !"Estado" %in% names(df)) return("0 ha en proceso")
    sub_df <- df %>% filter(Estado == "Cosecha Hoy (D+1)")
    ha_tot <- round(sum(sub_df$Ha_Total_rc, na.rm = TRUE), 1)
    sprintf("%s ha en proceso", ha_tot)
  })
  
  # Texto de resumen del filtro
  output$txt_conteo_filtro_conciliacion <- renderText({
    df <- datos_conciliacion_reactivo()
    if (nrow(df) == 0) return("0 registros")
    f_tipo <- if (!is.null(input$filtro_estado_conciliacion)) input$filtro_estado_conciliacion else "inconsistencias"
    n_inconsistencias <- sum(df$Tipo_Alerta == "inconsistencia", na.rm = TRUE)
    sprintf("Mostrando registros filtrados (Total inconsistencias: %d válvulas)", n_inconsistencias)
  })
  
  # Tabla Interactiva DT
  output$tabla_conciliacion_campo_sap <- renderDT({
    df <- datos_conciliacion_reactivo()
    if (is.null(df) || nrow(df) == 0) {
      return(datatable(tibble(Mensaje = "No hay registros disponibles para los filtros seleccionados."),
                       options = list(dom = "t"), rownames = FALSE))
    }
    
    f_tipo <- if (!is.null(input$filtro_estado_conciliacion)) input$filtro_estado_conciliacion else "inconsistencias"
    
    df_filtrado <- switch(
      f_tipo,
      "inconsistencias" = df %>% filter(Tipo_Alerta == "inconsistencia"),
      "ruta_sin_sap"    = df %>% filter(Estado == "Ruta sin Kilos SAP"),
      "sap_sin_ruta"    = df %>% filter(Estado == "Kilos SAP sin Ruta"),
      "desfase"         = df %>% filter(Estado %in% c("Desfase de Día", "Desfase Reasignado")),
      "proceso_hoy"     = df %>% filter(Estado == "Cosecha Hoy (D+1)"),
      "conciliado"      = df %>% filter(Estado == "Conciliado OK"),
      df
    )
    
    if (nrow(df_filtrado) == 0) {
      return(datatable(tibble(Mensaje = "No se encontraron discrepancias bajo este filtro específico."),
                       options = list(dom = "t"), rownames = FALSE))
    }
    
    df_tabla <- df_filtrado %>%
      mutate(
        Estado_Pill = case_when(
          Estado == "Conciliado OK"      ~ sprintf('<span class="badge-concil-pill pill-ok">● Conciliado OK</span>'),
          Estado == "Ruta sin Kilos SAP" ~ sprintf('<span class="badge-concil-pill pill-sin-sap">⚠️ Ruta sin SAP</span>'),
          Estado == "Kilos SAP sin Ruta" ~ sprintf('<span class="badge-concil-pill pill-sin-ruta">⚠️ SAP sin Ruta</span>'),
          Estado %in% c("Desfase de Día", "Desfase Reasignado")     ~ sprintf('<span class="badge-concil-pill pill-desfase">🔄 Desfase Día</span>'),
          Estado == "Cosecha Hoy (D+1)"  ~ sprintf('<span class="badge-concil-pill pill-proceso">⏳ Cosecha Hoy (D+1)</span>'),
          TRUE                           ~ Estado
        ),
        Area_txt = format(round(Area_ha, 2), nsmall = 2)
      ) %>%
      select(
        Sector, Módulo, Turno, Válvula, Variedad, `Área (ha)` = Area_txt,
        Estado = Estado_Pill, `Cosecha Campo (Días / Ha)` = Detalle_Campo,
        `Recepción SAP (Días / Kg)` = Detalle_SAP, `Diagnóstico y Acción Sugerida` = Diagnostico
      )
    
    datatable(
      df_tabla,
      escape   = FALSE,
      rownames = FALSE,
      options  = list(
        pageLength = 20,
        scrollX    = TRUE,
        dom        = "frtip",
        language   = list(
          url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json",
          search = "Buscar válvula:",
          paginate = list(previous = "‹", `next` = "›")
        )
      )
    )
  })
  
  # Descargar Excel de Inconsistencias
  output$btn_descarga_conciliacion_excel <- downloadHandler(
    filename = function() {
      sprintf("Inconsistencias_Campo_vs_SAP_Semana%s_%s.xlsx", input$filtro_semana, format(Sys.Date(), "%Y%m%d"))
    },
    content = function(file) {
      df <- datos_conciliacion_reactivo()
      if (is.null(df) || nrow(df) == 0) {
        writexl::write_xlsx(tibble(Aviso = "Sin registros para descargar"), file)
        return()
      }
      
      df_exp <- df %>%
        select(
          Sector, Módulo, Turno, Válvula, Codigo_Valvula = codigo_norm, Variedad, Area_ha,
          Estado, Tipo_Alerta, Cosecha_Campo = Detalle_Campo, Ha_Campo_Total = Ha_Total_rc,
          Recepcion_SAP = Detalle_SAP, Kg_SAP_Total = Kg_Total_sap, Diagnostico
        )
      
      hoja_inconsistencias <- df_exp %>% filter(Tipo_Alerta == "inconsistencia")
      hoja_todas           <- df_exp
      
      writexl::write_xlsx(
        list(
          "Inconsistencias a Corregir" = hoja_inconsistencias,
          "Universo Completo Semana"   = hoja_todas
        ),
        file
      )
    }
  )
  output$ui_detalle_badge_modo <- renderUI({
    modo <- if (!is.null(input$filtro_modo_detalle)) as.character(input$filtro_modo_detalle) else "real"
    if (modo == "ajustado") {
      span(style = "background: rgba(245, 158, 11, 0.14); color: #D97706; font-size: 10.5px; font-weight: 700; padding: 2px 8px; border-radius: 6px; border: 1px solid rgba(245, 158, 11, 0.3); vertical-align: middle;",
           "MODO AJUSTE (SIMULACIÓN)")
    } else {
      NULL
    }
  })

  # Validar que exista un ajuste guardado al seleccionar la vista Ajustado en Detalle Semanal
  observeEvent(input$filtro_modo_detalle, {
    req(input$filtro_modo_detalle == "ajustado")
    ed_todas <- ediciones_manuales()
    sem_sel  <- as.numeric(input$filtro_semana)
    camp_sel <- as.character(input$filtro_campana)
    tiene_ajuste <- FALSE
    if (!is.null(ed_todas) && nrow(ed_todas) > 0) {
      tiene_ajuste <- any(
        as.character(ed_todas$Campaña) == camp_sel &
        as.numeric(ed_todas$Semana) == sem_sel &
        coalesce(ed_todas$Tipo_Edicion, "") %in% c("auto", "ruta_sin_kg", "manual")
      )
    }
    if (!tiene_ajuste) {
      showNotification(
        sprintf("ℹ️ No hay un ajuste guardado para la Semana %s. Se muestra la vista Real.", sem_sel),
        type = "warning",
        duration = 4
      )
      updateRadioButtons(session, "filtro_modo_detalle", selected = "real")
    }
  }, ignoreInit = TRUE)

  # Si cambia la semana o campaña y no hay ajuste para esa semana, resetear a vista Real
  observeEvent(list(input$filtro_semana, input$filtro_campana), {
    ed_todas <- ediciones_manuales()
    sem_sel  <- as.numeric(input$filtro_semana)
    camp_sel <- as.character(input$filtro_campana)
    tiene_ajuste <- FALSE
    if (!is.null(ed_todas) && nrow(ed_todas) > 0) {
      tiene_ajuste <- any(
        as.character(ed_todas$Campaña) == camp_sel &
        as.numeric(ed_todas$Semana) == sem_sel &
        coalesce(ed_todas$Tipo_Edicion, "") %in% c("auto", "ruta_sin_kg", "manual")
      )
    }
    if (!tiene_ajuste && !is.null(input$filtro_modo_detalle) && input$filtro_modo_detalle == "ajustado") {
      updateRadioButtons(session, "filtro_modo_detalle", selected = "real")
    }
  }, ignoreInit = TRUE)

}

shinyApp(ui, server)