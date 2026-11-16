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

# Rutas y funciones para persistencia de Escenarios Guardados de Ajuste Semanal
ruta_escenarios_principal <- file.path(ruta_raiz_arandano, "escenarios_ajuste_semanal.rds")
ruta_escenarios_local     <- "D:/ACP/Actualizacion Semanal/escenarios_ajuste_semanal.rds"
ruta_escenarios_nube      <- "escenarios_ajuste_semanal.rds"

cargar_escenarios_disco <- function() {
  candidatos <- c(ruta_escenarios_principal, ruta_escenarios_local, ruta_escenarios_nube)
  for (r in candidatos) {
    if (file.exists(r)) {
      res <- tryCatch({
        df <- readRDS(r)
        if (is.data.frame(df)) df else NULL
      }, error = function(e) NULL)
      if (!is.null(res)) return(res)
    }
  }
  tibble(
    Id_Escenario         = character(),
    Nombre               = character(),
    Descripcion          = character(),
    Campaña              = character(),
    Semana               = numeric(),
    Dia_Desde            = character(),
    Dia_Hasta            = character(),
    Rango_Dias           = character(),
    Dias_Ajustados       = character(),
    Total_Kg_Ajustados   = numeric(),
    Total_Kg_Real_Prev   = numeric(),
    Cosecha_Ajustada_Tot = numeric(),
    Num_Turnos           = numeric(),
    Fecha_Creacion       = as.POSIXct(character()),
    Fecha_Modificacion   = as.POSIXct(character()),
    Ediciones_Data       = list(),
    Valvulas_Data        = list()
  )
}

guardar_escenarios_disco <- function(df_escenarios) {
  guardado <- FALSE
  candidatos <- c(ruta_escenarios_principal, ruta_escenarios_local, ruta_escenarios_nube)
  for (r in candidatos) {
    if (dir.exists(dirname(r))) {
      tryCatch({
        saveRDS(df_escenarios, r)
        guardado <- TRUE
      }, error = function(e) NULL)
    }
  }
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
# ==============================================================================
# CONFIGURACIÓN DE UMBRAL DE AVANCE SEMANAL VS S-1 (PERSISTENCIA)
# ==============================================================================
ruta_umbral_avance_principal <- file.path(Sys.getenv("USERPROFILE"), "Desktop", "umbral_avance_s1.rds")
ruta_umbral_avance_nube      <- "umbral_avance_s1.rds"

cargar_umbral_avance_disco <- function() {
  val <- NULL
  if (file.exists(ruta_umbral_avance_principal)) {
    val <- tryCatch(readRDS(ruta_umbral_avance_principal), error = function(e) NULL)
  } else if (file.exists(ruta_umbral_avance_nube)) {
    val <- tryCatch(readRDS(ruta_umbral_avance_nube), error = function(e) NULL)
  }
  val_num <- suppressWarnings(as.numeric(val))
  if (!is.null(val_num) && !is.na(val_num) && length(val_num) > 0 && val_num >= 1 && val_num <= 100) {
    return(round(val_num, 1))
  }
  return(30)
}

guardar_umbral_avance_disco <- function(val) {
  val_num <- suppressWarnings(as.numeric(val))
  if (is.null(val_num) || is.na(val_num) || val_num < 1 || val_num > 100) return(FALSE)
  val_save <- round(val_num, 1)
  if (dir.exists(dirname(ruta_umbral_avance_principal))) {
    tryCatch(saveRDS(val_save, ruta_umbral_avance_principal), error = function(e) NULL)
  }
  tryCatch(saveRDS(val_save, ruta_umbral_avance_nube), error = function(e) NULL)
  return(TRUE)
}

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
        Kg_Lunes_sr      = 0,
        Kg_Martes_sr     = 0,
        Kg_Miercoles_sr  = 0,
        Kg_Jueves_sr     = 0,
        Kg_Viernes_sr    = 0,
        Kg_Sabado_sr     = 0,
        Kg_Sin_Ruta      = 0,
        Kg_Desfase       = 0,
        
        Estado_Concil        = "Conciliado OK",
        Tipo_Alerta          = "conciliado",
        Diagnostico          = "Campo y SAP coinciden plenamente.",
        Es_Valido_Operativo  = TRUE
      )
    
    for (i in seq_len(nrow(df_rec))) {
      r <- df_rec[i, ]
      
      # REGLA 1: SAP sin Ruta total -> Excluir de cálculos y pintado
      if (!r$Tiene_Campo && r$Tiene_SAP) {
        df_rec$Estado_Concil[i]        <- "Kilos SAP sin Ruta"
        df_rec$Tipo_Alerta[i]          <- "inconsistencia"
        df_rec$Diagnostico[i]          <- sprintf("Pesado en SAP (%s kg), pero no programado en ruta de campo. Excluido de mapa y cálculos.", format(round(r$Kg_Total_sap, 0), big.mark = ","))
        df_rec$Es_Valido_Operativo[i]  <- FALSE
        df_rec$Kg_Sin_Ruta[i]          <- r$Kg_Total_sap
        df_rec$Kg_Lunes_sr[i]          <- r$Kg_Lunes_sap
        df_rec$Kg_Martes_sr[i]         <- r$Kg_Martes_sap
        df_rec$Kg_Miercoles_sr[i]      <- r$Kg_Miercoles_sap
        df_rec$Kg_Jueves_sr[i]         <- r$Kg_Jueves_sap
        df_rec$Kg_Viernes_sr[i]        <- r$Kg_Viernes_sap
        df_rec$Kg_Sabado_sr[i]         <- r$Kg_Sabado_sap
        
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
      
      # REGLA 2: Desfase de Día y Kilos sin Ruta en días cerrados
      if (r$Tiene_Campo && r$Tiene_SAP) {
        kg_dias <- c(Lunes = r$Kg_Lunes_sap, Martes = r$Kg_Martes_sap, Miércoles = r$Kg_Miercoles_sap,
                     Jueves = r$Kg_Jueves_sap, Viernes = r$Kg_Viernes_sap, Sábado = r$Kg_Sabado_sap)
        ha_dias <- c(Lunes = r$Ha_Lunes_rc, Martes = r$Ha_Martes_rc, Miércoles = r$Ha_Miercoles_rc,
                     Jueves = r$Ha_Jueves_rc, Viernes = r$Ha_Viernes_rc, Sábado = r$Ha_Sabado_rc)
        
        dias_c_act <- names(ha_dias)[ha_dias > 0]
        dias_s_act <- names(kg_dias)[kg_dias > 0]
        
        dias_c_cerr <- dias_c_act[dias_c_act %in% dias_cerrados_nom]
        dias_s_cerr <- dias_s_act[dias_s_act %in% dias_cerrados_nom]
        
        # Evaluar cada día donde SAP reportó kilos
        dias_match   <- character(0)
        dias_desfase <- character(0) # D+1 (cosechó día anterior)
        dias_sin_r   <- character(0) # Sin cosecha ese día ni el anterior
        
        kg_sin_r_tot <- 0
        kg_desf_tot  <- 0
        
        for (ds in dias_s_cerr) {
          if (ds %in% dias_c_cerr) {
            dias_match <- c(dias_match, ds)
          } else {
            # Verificar si cosechó el día inmediatamente anterior (desfase D+1)
            idx_s <- match(ds, dias_cols)
            dia_ant <- if (idx_s > 1) dias_cols[idx_s - 1] else NA_character_
            if (!is.na(dia_ant) && (dia_ant %in% dias_c_cerr)) {
              dias_desfase <- c(dias_desfase, ds)
              kg_desf_tot  <- kg_desf_tot + kg_dias[ds]
            } else {
              dias_sin_r   <- c(dias_sin_r, ds)
              kg_sin_r_tot <- kg_sin_r_tot + kg_dias[ds]
            }
          }
        }
        
        # Procesar Sin Ruta
        if (length(dias_sin_r) > 0) {
          df_rec$Kg_Sin_Ruta[i] <- round(kg_sin_r_tot, 2)
          for (dsr in dias_sin_r) {
            col_rec <- paste0("Kg_", tools::toTitleCase(chartr("áéíóúÁÉÍÓÚ", "aeiouAEIOU", tolower(dsr))), "_rec")
            col_sr  <- paste0("Kg_", tools::toTitleCase(chartr("áéíóúÁÉÍÓÚ", "aeiouAEIOU", tolower(dsr))), "_sr")
            df_rec[[col_rec]][i] <- 0
            df_rec[[col_sr]][i]  <- round(kg_dias[dsr], 2)
          }
        }
        
        # Procesar Desfase D+1 (reasignar al día de campo previo)
        if (length(dias_desfase) > 0) {
          df_rec$Kg_Desfase[i] <- round(kg_desf_tot, 2)
          for (dd in dias_desfase) {
            idx_s <- match(dd, dias_cols)
            dia_ant <- dias_cols[idx_s - 1]
            col_dest <- paste0("Kg_", tools::toTitleCase(chartr("áéíóúÁÉÍÓÚ", "aeiouAEIOU", tolower(dia_ant))), "_rec")
            col_orig <- paste0("Kg_", tools::toTitleCase(chartr("áéíóúÁÉÍÓÚ", "aeiouAEIOU", tolower(dd))), "_rec")
            df_rec[[col_orig]][i] <- 0
            df_rec[[col_dest]][i] <- round(df_rec[[col_dest]][i] + kg_dias[dd], 2)
          }
        }
        
        # Actualizar Kg_Total_rec
        df_rec$Kg_Total_rec[i] <- round(sum(c(df_rec$Kg_Lunes_rec[i], df_rec$Kg_Martes_rec[i], df_rec$Kg_Miercoles_rec[i],
                                              df_rec$Kg_Jueves_rec[i], df_rec$Kg_Viernes_rec[i], df_rec$Kg_Sabado_rec[i])), 2)
        
        # Estado final del registro
        if (length(dias_sin_r) > 0) {
          df_rec$Estado_Concil[i]        <- "Kilos SAP sin Ruta"
          df_rec$Tipo_Alerta[i]          <- "inconsistencia"
          df_rec$Diagnostico[i]          <- sprintf("Pesado en SAP sin ruta en campo en [%s] (%s kg). Excluido de mapa y cálculo agronómico.",
                                                    paste(dias_sin_r, collapse = ", "), format(round(kg_sin_r_tot, 0), big.mark = ","))
          # Si tiene campo en otros días válidos, mantenerlo operativo para esos días; si no tiene nada válido, FALSE
          df_rec$Es_Valido_Operativo[i]  <- (df_rec$Kg_Total_rec[i] > 0 || any(ha_dias[setdiff(dias_cols, dias_sin_r)] > 0))
        } else if (length(dias_desfase) > 0) {
          df_rec$Estado_Concil[i]        <- "Desfase Reasignado"
          df_rec$Tipo_Alerta[i]          <- "conciliado"
          df_rec$Diagnostico[i]          <- sprintf("Desfase de %s kg en [%s] reasignado por coincidencia con ruta de campo previa. Usado operativamente (Azul).",
                                                    format(round(kg_desf_tot, 0), big.mark = ","), paste(dias_desfase, collapse = ", "))
          df_rec$Es_Valido_Operativo[i]  <- TRUE
        } else if (length(dias_match) > 0) {
          df_rec$Estado_Concil[i]        <- "Conciliado OK"
          df_rec$Tipo_Alerta[i]          <- "conciliado"
          df_rec$Diagnostico[i]          <- "Campo y SAP coinciden plenamente en válvula y días."
          df_rec$Es_Valido_Operativo[i]  <- TRUE
        } else {
          df_rec$Estado_Concil[i]        <- "Desfase de Día"
          df_rec$Tipo_Alerta[i]          <- "inconsistencia"
          df_rec$Diagnostico[i]          <- sprintf("Desfase total: Campo [%s] vs SAP [%s]. Kilos fuera de cálculo.",
                                                    paste(dias_c_cerr, collapse = ", "), paste(dias_s_cerr, collapse = ", "))
          df_rec$Es_Valido_Operativo[i]  <- FALSE
          df_rec$Kg_Total_rec[i]         <- 0
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

solo_filtrado <- TRUE
turnos_filtro_sim <- tibble(Sector = "6", Módulo = "1", Turno = as.character(1:5), Variedad = "SEKOYA POP")
proy_sem_sim <- df_proyecciones %>% filter(Campaña == "2026", Semana == 45) %>% mutate(Módulo_join = Módulo, Turno_join = Turno, Variedad_join = Variedad)
# Simulate u_base_sim for M1:
u_base_sim <- df_proyecciones %>%
  filter(Campaña == "2026", Semana == 45, Módulo == "1") %>%
  mutate(
    Sector = "6", Módulo = "1", Turno = as.character(Turno), Variedad = "SEKOYA POP",
    `Ha Real 2ª Pas` = 0,
    Lunes = NA_real_, Lunes_bloqueado = FALSE,
    Martes = 1431.81, Martes_bloqueado = TRUE,
    Miércoles = 1684.68, Miércoles_bloqueado = TRUE,
    Jueves = NA_real_, Jueves_bloqueado = FALSE,
    Viernes = NA_real_, Viernes_bloqueado = FALSE,
    Sábado = NA_real_, Sábado_bloqueado = FALSE
  )
    permitir_cualquier_sem <- is.null(TRUE) || isTRUE(TRUE)
    es_campana_act <- grepl("2026|^26", as.character("2026"))
    sem_actual_num <- lubridate::isoweek(Sys.Date())
    es_semana_act  <- (as.numeric(45) == sem_actual_num) || permitir_cualquier_sem
    
    if (!es_campana_act) {
      cat("NOTIFICATION:", "El ajuste automático solo está habilitado para la campaña 2026.", type = "warning", duration = 4, "n")
      return()
    }
    
    if (!es_semana_act) {
      cat("NOTIFICATION:", "El ajuste automático está restringido a la semana actual en curso. Activa 'Ajustar cualquier semana' en el menú (⋮, "n") para habilitarlo.", type = "warning", duration = 5)
      return()
    }
    
    sem_sel <- as.numeric(45)
    sem_ant <- sem_sel - 1
    
    dias_nombres <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
    dias_cols_kg <- c("Kg_Lunes", "Kg_Martes", "Kg_Miércoles", "Kg_Jueves", "Kg_Viernes", "Kg_Sábado")
    
    dia_inicio_nom <- if (!is.null("Martes") && "Martes" %in% dias_nombres) {
      "Martes"
    } else {
      w_num <- lubridate::wday(Sys.Date(), week_start = 1)
      if (w_num > 6) "Lunes" else dias_nombres[w_num]
    }
    
    dia_inicio_idx <- which(dias_nombres == dia_inicio_nom)[1]
    if (is.na(dia_inicio_idx)) dia_inicio_idx <- 1
    
    dia_fin_nom <- if (!is.null("Viernes") && "Viernes" %in% dias_nombres) {
      "Viernes"
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
    modo_frec <- "auto"
    df_frec_mv <- tibble()
    if (is.null(df_frec_mv) || nrow(df_frec_mv) == 0) {
      df_frec_mv <- cargar_frecuencias_disco()
    }
    
    # Cargar porcentajes de 2da pasada de Rocío configurados por módulo
    df_pct_rocio <- tibble()
    if (is.null(df_pct_rocio) || nrow(df_pct_rocio) == 0) {
      df_pct_rocio <- cargar_pct_rocio_disco()
    }
    
    # Umbral de avance configurado para representatividad vs S-1
    umbral_avance_cfg <- 15
    if (is.null(umbral_avance_cfg) || is.na(umbral_avance_cfg) || umbral_avance_cfg < 1) {
      umbral_avance_cfg <- cargar_umbral_avance_disco()
    }
    prop_umbral_cfg <- umbral_avance_cfg / 100
    
    msg_detail_frec <- if (modo_frec == "manual") {
      "Aplicando frecuencias manuales por Módulo y Variedad"
    } else {
      "Calculando frecuencias reales de 2 semanas y combinatoria de rendimientos"
    }
    
    {
      # ====================================================================
      # FRECUENCIAS AUTOMÁTICAS POR VÁLVULA (HISTÓRICO 2 SEMANAS: S-2 vs S-1)
      # ====================================================================
      df_frec_valvs_2sem <- tryCatch({
        calcular_frecuencias_valvula_2semanas(
          df_cosecha  = df_cosecha,
          campana_sel = "2026",
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
              # Consolidar primero por válvula única para evitar inflar hectáreas si una válvula tiene múltiples filas en SAP/rutas
              group_by(Módulo, Turno, Variedad, codigo_norm) %>%
              summarise(
                Kg_Valv_Ant = sum(Kg_Total_Sem, na.rm = TRUE),
                Ha_Valv_Ant = max(coalesce(Ha_Total_Sem, Area_Ha), na.rm = TRUE),
                Ultimo_Dia_Valv = {
                  sub_mat <- mat_kg[cur_group_rows(), , drop = FALSE]
                  c_sums <- colSums(sub_mat, na.rm = TRUE)
                  d_con <- names(which(c_sums > 0))
                  if (length(d_con) > 0) sub("^Kg_", "", tail(d_con, 1)) else NA_character_
                },
                .groups = "drop"
              ) %>%
              group_by(Módulo, Turno, Variedad) %>%
              summarise(
                Kg_Ant_Turno   = sum(Kg_Valv_Ant, na.rm = TRUE),
                Ha_Ant_Turno   = sum(Ha_Valv_Ant, na.rm = TRUE),
                Rend_Ant_Turno = ifelse(Ha_Ant_Turno > 0, Kg_Ant_Turno / Ha_Ant_Turno, 0),
                Ultimo_Dia_Cosecha_Ant = {
                  d_dias <- na.omit(Ultimo_Dia_Valv)
                  orden_dias <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Domingo")
                  match_dias <- match(d_dias, orden_dias)
                  if (length(match_dias) > 0 && any(!is.na(match_dias))) {
                    orden_dias[max(match_dias, na.rm = TRUE)]
                  } else NA_character_
                },
                .groups = "drop"
              )
          }
        }
      }
      
      if (nrow(rend_ant_turno) == 0) {
        rend_ant_turno <- df_cosecha %>%
          filter(as.character(Campaña) == as.character("2026"), Semana == sem_ant) %>%
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
          group_by(Módulo, Turno, Variedad, Válvula = coalesce(Válvula, "")) %>%
          summarise(
            Kg_Valv_Ant = sum(`Kg Total`, na.rm = TRUE),
            Ha_Valv_Ant = max(coalesce(Área, 0), na.rm = TRUE),
            .groups     = "drop"
          ) %>%
          group_by(Módulo, Turno, Variedad) %>%
          summarise(
            Kg_Ant_Turno   = sum(Kg_Valv_Ant, na.rm = TRUE),
            Ha_Ant_Turno   = sum(Ha_Valv_Ant, na.rm = TRUE),
            Rend_Ant_Turno = ifelse(Ha_Ant_Turno > 0, Kg_Ant_Turno / Ha_Ant_Turno, 0),
            Ultimo_Dia_Cosecha_Ant = NA_character_,
            .groups        = "drop"
          )
      }
      
      # incProgress
      
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
          ha_prev_vec <- ha_prev_vec + val_h
          kg_prev_vec <- kg_prev_vec + val_k
        }
        rc_temp$Ha_Prev_Valv <- ha_prev_vec
        rc_temp$Kg_Prev_Valv <- kg_prev_vec
        
        rc_temp %>%
          filter(coalesce(suppressWarnings(as.numeric(Num_Pasada)), 1) == 1) %>%
          group_by(codigo_norm) %>%
          summarise(
            Ha_Prog_Rc   = sum(coalesce(Ha_Prev_Valv, 0), na.rm = TRUE),
            Kg_Prog_Rc   = sum(coalesce(Kg_Prev_Valv, 0), na.rm = TRUE),
            Ha_Lunes_Rc  = sum(coalesce(Ha_Lunes, 0), na.rm = TRUE),
            Kg_Lunes_Rc  = if ("Kg_Lunes" %in% names(.)) sum(coalesce(Kg_Lunes, 0), na.rm = TRUE) else 0,
            Ha_Martes_Rc = sum(coalesce(Ha_Martes, 0), na.rm = TRUE),
            Kg_Martes_Rc = if ("Kg_Martes" %in% names(.)) sum(coalesce(Kg_Martes, 0), na.rm = TRUE) else 0,
            Dia_Ruta_Sem = first(na.omit(Dia_Ruta_Sem)),
            .groups      = "drop"
          )
      } else tibble(codigo_norm = character(), Ha_Prog_Rc = numeric(), Kg_Prog_Rc = numeric(), Ha_Lunes_Rc = numeric(), Kg_Lunes_Rc = numeric(), Ha_Martes_Rc = numeric(), Kg_Martes_Rc = numeric(), Dia_Ruta_Sem = character())
      
      # Válvulas rectificadas el Martes en la semana actual (si se cosechó Lunes y se rectificó Martes, su frecuencia cuenta desde Martes)
      rectif_martes_ids <- if (nrow(rc_rectif_sem) > 0 && "codigo" %in% names(rc_rectif_sem) && "Martes" %in% names(rc_rectif_sem)) {
        unique(norm_codigo_valvula(rc_rectif_sem$codigo[coalesce(as.numeric(rc_rectif_sem$Martes), 0) > 0]))
      } else character(0)
      
      # Válvulas con avance oficial en SAP para la semana actual (solo días previos a dia_inicio_nom)
      # Válvulas con 2da pasada ya registrada/ejecutada en rutas de campo de la semana actual
      valvs_con_2da_rc <- if (nrow(rc_sem) > 0 && 'codigo' %in% names(rc_sem)) {
        rc_sem %>%
          filter(coalesce(suppressWarnings(as.numeric(Num_Pasada)), 1) == 2) %>%
          mutate(codigo_norm = norm_codigo_valvula(codigo)) %>%
          filter(coalesce(Ha_Total_Prog, 0) > 0 | coalesce(Kg_Total_Prog, 0) > 0) %>%
          pull(codigo_norm) %>%
          unique()
      } else character(0)

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
      
      df_cosecha_sem <- df_cosecha %>%
        filter(as.character(Campaña) == as.character("2026"), Semana == sem_sel, !coalesce(Es_Testblock, FALSE))
      
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
      
      proy_sem <- proy_sem_sim
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
        cat("NOTIFICATION:", "No se encontró el catálogo maestro de válvulas para proyectar.", type = "error", duration = 4, "n")
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
          # REGLA DE COHERENCIA BÁSCULA vs CAMPO:
          # Si SAP ya cerró la recepción de la válvula con pesaje oficial (Kg_Prog_Sap > 0 o Ha_Prog_Sap > 0),
          # se adopta la superficie real cerrada de SAP para evitar que hojas de rutas desfasadas
          # REGLA OPERATIVA ACP: Mandan las Ha de Campo (recorrido físico) y los Kilos de SAP
          Ha_Prog      = case_when(
            !is.null(rc_sem) && nrow(rc_sem) > 0 ~ pmin(Área, coalesce(Ha_Prog_Rc, 0)),
            coalesce(Ha_Prog_Sap, 0) > 0         ~ pmin(Área, Ha_Prog_Sap),
            TRUE                                 ~ 0
          ),
          Kg_Prog      = case_when(
            Ha_Prog > 0 & coalesce(Kg_Prog_Sap, 0) > 0 ~ Kg_Prog_Sap,
            Ha_Prog > 0 & coalesce(Kg_Prog_Rc, 0) > 0  ~ Kg_Prog_Rc,
            is.null(rc_sem) || nrow(rc_sem) == 0       ~ coalesce(Kg_Prog_Sap, 0),
            TRUE                                       ~ 0
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
          Tiene_Ciclo_Previo_Valv = !is.na(Dia_7d) | (!is.na(Dia_Auto_Valv) & nzchar(Dia_Auto_Valv)) | (!is.na(Dia_Ruta_Sem) & nzchar(Dia_Ruta_Sem)),
          
          # Detección agronómica de válvulas de cierre S-1 vs cosecha fresca del nuevo ciclo:
          Ha_Valv_Act = pmax(coalesce(Ha_Prog, 0), coalesce(Ha_Lunes_Rc, 0) + coalesce(Ha_Martes_Rc, 0)),
          Kg_Valv_Act = pmax(coalesce(Kg_Prog, 0), coalesce(Kg_Lunes_Valv, 0) + coalesce(Kg_Martes_Valv, 0)),
          # Válvula de cierre: se cosechó a inicio de semana actual pero NO cosechó en S-1 (remanente con días extra en planta)
          Es_Valv_Cierre_S1 = (coalesce(Kg_Total_Ant, 0) == 0 | is.na(Dia_7d)) & Ha_Valv_Act > 0,
          # Válvula de cosecha fresca: se cosechó en la semana actual y YA había cosechado en S-1 (nuevo ciclo)
          Es_Valv_Fresca_Sem = (coalesce(Kg_Total_Ant, 0) > 0 & !is.na(Dia_7d)) & Ha_Valv_Act > 0,
          
          Ha_Fresca_Valv = if_else(Es_Valv_Fresca_Sem, Ha_Valv_Act, 0),
          Kg_Fresca_Valv = if_else(Es_Valv_Fresca_Sem, Kg_Valv_Act, 0),
          
          Ha_Cierre_Valv = if_else(Es_Valv_Cierre_S1, Ha_Valv_Act, 0),
          Kg_Cierre_Valv = if_else(Es_Valv_Cierre_S1, Kg_Valv_Act, 0)
        )
      
      if (solo_filtrado) {
        turnos_filtro <- turnos_filtro_sim
        if (is.null(turnos_filtro) || nrow(turnos_filtro) == 0) {
          cat("NOTIFICATION:", "No hay turnos visibles en el filtro actual para ajustar.", type = "warning", duration = 4, "n")
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
          cat("NOTIFICATION:", "No se encontraron válvulas en el catálogo para los turnos filtrados.", type = "warning", duration = 4, "n")
          return()
        }
      }
      
      # Cargar ediciones manuales existentes de la semana para descontar hectáreas ya asignadas manualmente
      ed_act_sem <- readRDS("D:/ACP/Actualizacion Semanal/ediciones_manuales_cosecha.rds")
      ed_man_sem <- if (!is.null(ed_act_sem) && nrow(ed_act_sem) > 0) {
        ed_act_sem %>%
          filter(
            as.character(Campaña) == as.character("2026"),
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
      
      # Resumen de 2da pasada real por turno desde universo_base
      u_base_pre <- tryCatch(u_base_sim, error = function(e) NULL)
      ha_real_2da_map <- if (!is.null(u_base_pre) && nrow(u_base_pre) > 0) {
        u_base_pre %>%
          group_by(
            Módulo = trimws(as.character(Módulo)),
            Turno  = trimws(as.character(Turno)),
            Variedad = limpiar_variedad(Variedad)
          ) %>%
          summarise(
            Ha_Real_2da = sum(coalesce(`Ha Real 2ª Pas`, 0), na.rm = TRUE),
            .groups = 'drop'
          )
      } else {
        tibble(Módulo = character(), Turno = character(), Variedad = character(), Ha_Real_2da = numeric())
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
          Ha_Fresca_Turno   = sum(Ha_Fresca_Valv, na.rm = TRUE),
          Kg_Fresca_Turno   = sum(Kg_Fresca_Valv, na.rm = TRUE),
          Ha_Cierre_Turno   = sum(Ha_Cierre_Valv, na.rm = TRUE),
          Kg_Cierre_Turno   = sum(Kg_Cierre_Valv, na.rm = TRUE),
          .groups           = "drop"
        ) %>%
        left_join(resumen_man_turno, by = c("Módulo", "Turno", "Variedad")) %>%
        left_join(ha_real_2da_map, by = c("Módulo", "Turno", "Variedad")) %>%
        mutate(
          Ha_Manual_1ra          = coalesce(Ha_Manual_1ra, 0),
          Ha_Manual_2da          = coalesce(Ha_Manual_2da, 0),
          Ha_Faltante_Turno_Neto = pmax(0, Area_Total_Turno - Ha_Prog_Turno - Ha_Manual_1ra),
          Turno_Completo_1ra     = (Ha_Faltante_Turno_Neto <= 0.05),
          Ha_Real_2da            = coalesce(Ha_Real_2da, 0),
          Ha_Faltante_2da_Neto   = pmax(0, Area_Total_Turno - Ha_Real_2da - Ha_Manual_2da),
          Turno_Completo_2da     = (Ha_Faltante_2da_Neto <= 0.05)
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
        campana_sel      = "2026",
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
          
          # Cosecha Fresca vs Cierre de S-1
          Rend_Fresco_Turno    = ifelse(Ha_Fresca_Turno > 0, Kg_Fresca_Turno / Ha_Fresca_Turno, 0),
          Prop_Fresca_Turno    = ifelse(Area_Total_Turno > 0, pmin(1, Ha_Fresca_Turno / Area_Total_Turno), 0),
          Tiene_Cosecha_Fresca = (Ha_Fresca_Turno > 0 & Kg_Fresca_Turno > 0),
          
          # ====================================================================
          # NODOS 4, 5, 6 y 7: Árbol de Decisión de Rendimiento por Rama
          # ====================================================================
          Rend_Base_Turno = case_when(
            # RAMA 2A (Derecha -> 100% Lunes/Martes -> Es Rocío): 2da pasada con Rend Sem Actual
            Turno_Completo_Lun_Mar & Es_Ciclo_Corto ~ Rend_Sem_Actual,
            
            # REGLA CIERRE S-1 CON COSECHA FRESCA (Válvulas del nuevo ciclo > umbral configurado):
            # Se aísla y utiliza exclusivamente el rendimiento de esas válvulas frescas (ej. Martes V32: 693.1 kg/ha),
            # ya que las válvulas de cierre tienen rendimiento inflado por haber quedado días extra en planta.
            Semana_Ant_No_Finalizo & Tiene_Cosecha_Fresca & Prop_Fresca_Turno > prop_umbral_cfg ~ Rend_Fresco_Turno,
            
            # REGLA CIERRE S-1 SIN COSECHA FRESCA (o avance fresco <= umbral):
            # Se descartan las válvulas de cierre infladas y se utiliza el rendimiento de S-1 (o proy) con factor floral.
            Semana_Ant_No_Finalizo ~ ifelse(Rend_Ant_Turno > 0, Rend_Ant_Turno, Rend_Proy),
            
            # RAMA 3A (Derecha -> No Tiene Kilos / Avance en Sem Actual):
            # Usar rendimiento de la semana anterior S-1 (o Proy si S-1=0) ajustado con factor floral
            !Tiene_Cosecha_Act ~ ifelse(Rend_Ant_Turno > 0, Rend_Ant_Turno, Rend_Proy),
            
            # RAMA 3B-1 (Derecha -> Tiene Avance -> Cosechó > 30% del turno):
            # Usar directo Rend_Sem_Actual (rendimiento real observado en campo)
            Tiene_Cosecha_Act & Prop_Avance_Turno > prop_umbral_cfg ~ Rend_Sem_Actual,
            
            # RAMA 3B-2 (Derecha -> Tiene Avance -> Cosechó <= 30% del turno):
            # Muestra pequeña (<= 30%): Usar rendimiento de la semana anterior S-1 (o Proy si S-1=0) con factor floral
            Tiene_Cosecha_Act & Prop_Avance_Turno <= prop_umbral_cfg ~ ifelse(Rend_Ant_Turno > 0, Rend_Ant_Turno, Rend_Proy),
            
            TRUE ~ ifelse(Rend_Ant_Turno > 0, Rend_Ant_Turno, Rend_Proy)
          ),
          
          # Rendimiento ajustado con Premio o Castigo por tendencia floral de maduración
          # REGLA: Cuando el avance es > 30% del turno o proviene de cosecha fresca representativa, NO se aplica factor floral
          # porque el rendimiento observado ya viene afectado por las condiciones fenológicas vigentes.
          Rend_Base_Con_Floral = case_when(
            Semana_Ant_No_Finalizo & Tiene_Cosecha_Fresca & Prop_Fresca_Turno > prop_umbral_cfg ~ round(Rend_Base_Turno, 2),
            Tiene_Cosecha_Act & Prop_Avance_Turno > prop_umbral_cfg                              ~ round(Rend_Base_Turno, 2),
            TRUE                                                                                ~ round(Rend_Base_Turno * Factor_Floral, 2)
          ),
          
          # Si el turno NO está proyectado en la semana o si se cosechó 100% entre Lunes/Martes y NO es Rocío -> No ajustar (0)
          Rend_Turno_Aplicar = ifelse(!Esta_Proyectado | (Turno_Completo_Lun_Mar & !Es_Ciclo_Corto), 0, Rend_Base_Con_Floral),
          
          Motivo_Base = case_when(
            !Esta_Proyectado ~ "No proyectado",
            Turno_Completo_Lun_Mar & !Es_Ciclo_Corto ~ "Cerró 100% en Lun/Mar (0 kg pend.)",
            Turno_Completo_Lun_Mar & Es_Ciclo_Corto ~ sprintf("2da pasada Rocío (Rend Lunes: %s kg/ha • 60%% sin floración)", trimws(format(round(coalesce(Rend_Lunes_Turno, Rend_Sem_Actual)), big.mark = ","))),
            Semana_Ant_No_Finalizo & Tiene_Cosecha_Fresca & Prop_Fresca_Turno > prop_umbral_cfg ~ sprintf("Cierre S-1 + Muestra fresca %s%% (Rend fresco: %s kg/ha)", round(Prop_Fresca_Turno * 100, 1), trimws(format(round(Rend_Fresco_Turno), big.mark = ","))),
            Semana_Ant_No_Finalizo & Rend_Ant_Turno > 0 ~ sprintf("Cierre S-1 incompleta (Rend S-1: %s kg/ha)", trimws(format(round(Rend_Ant_Turno), big.mark = ","))),
            Semana_Ant_No_Finalizo                      ~ sprintf("Cierre S-1 incompleta (Rend proy: %s kg/ha)", trimws(format(round(Rend_Proy), big.mark = ","))),
            Tiene_Cosecha_Act & Prop_Avance_Turno > prop_umbral_cfg ~ sprintf("Avance >%d%% (Rend act sem: %s kg/ha)", round(umbral_avance_cfg), trimws(format(round(Rend_Sem_Actual), big.mark = ","))),
            !Tiene_Cosecha_Act & Rend_Ant_Turno > 0 ~ sprintf("Sin avance previo (Rend S-1: %s kg/ha)", trimws(format(round(Rend_Ant_Turno), big.mark = ","))),
            !Tiene_Cosecha_Act                      ~ sprintf("Sin avance previo (Rend proy: %s kg/ha)", trimws(format(round(Rend_Proy), big.mark = ","))),
            Tiene_Cosecha_Act & Prop_Avance_Turno <= prop_umbral_cfg & Rend_Ant_Turno > 0 ~ sprintf("Avance <=%d%% (Rend S-1: %s kg/ha)", round(umbral_avance_cfg), trimws(format(round(Rend_Ant_Turno), big.mark = ","))),
            Tiene_Cosecha_Act & Prop_Avance_Turno <= prop_umbral_cfg                      ~ sprintf("Avance <=%d%% (Rend proy: %s kg/ha)", round(umbral_avance_cfg), trimws(format(round(Rend_Proy), big.mark = ","))),
            TRUE ~ ifelse(Rend_Ant_Turno > 0, sprintf("Rend S-1 (%s kg/ha)", trimws(format(round(Rend_Ant_Turno), big.mark = ","))), sprintf("Rend proy (%s kg/ha)", trimws(format(round(Rend_Proy), big.mark = ","))))
          ),
          
          Pct_Ajuste_Flor = round((Factor_Floral - 1) * 100, 1),
          Detalle_Floral = case_when(
            !Esta_Proyectado | (Turno_Completo_Lun_Mar & !Es_Ciclo_Corto) | (Turno_Completo_Lun_Mar & Es_Ciclo_Corto) ~ "",
            Semana_Ant_No_Finalizo & Tiene_Cosecha_Fresca & Prop_Fresca_Turno > prop_umbral_cfg ~ "",
            Tiene_Cosecha_Act & Prop_Avance_Turno > prop_umbral_cfg ~ "",
            Tipo_Efecto_Floral == "Premio"  ~ sprintf("Premio floral %+0.1f%%", Pct_Ajuste_Flor),
            Tipo_Efecto_Floral == "Castigo" ~ sprintf("Castigo floral %+0.1f%%", Pct_Ajuste_Flor),
            TRUE                            ~ "Floral neutro (0%)"
          ),
          
          Motivo_Ajuste = case_when(
            !Esta_Proyectado ~ "No proyectado",
            Turno_Completo_Lun_Mar & !Es_Ciclo_Corto ~ "Cerró 100% en Lun/Mar",
            Turno_Completo_Lun_Mar & Es_Ciclo_Corto ~ Motivo_Base,
            Semana_Ant_No_Finalizo & Tiene_Cosecha_Fresca & Prop_Fresca_Turno > prop_umbral_cfg ~ Motivo_Base,
            nzchar(Detalle_Floral) ~ paste0(Motivo_Base, " • ", Detalle_Floral),
            TRUE ~ Motivo_Base
          )
        )
      
      valvulas_calc <- valvulas_calc %>%
        left_join(
          rend_turno %>% select(
            Módulo, Turno, Variedad, Rend_Turno_Aplicar, Rend_Base_Turno, Rend_Base_Con_Floral, Rend_Ant_Turno, Rend_Sem_Actual, Rend_Proy,
            Rend_Lunes_Turno, Rend_Martes_Turno, Rend_Fresco_Turno, Prop_Fresca_Turno, Tiene_Cosecha_Fresca,
            Esta_Proyectado, Tiene_Cosecha_Act, Semana_Ant_No_Finalizo, Turno_Completo_Lun_Mar, Es_Ciclo_Corto,
            Cosecho_Lunes, Cosecho_Martes, Kg_Lunes_Real, Kg_Martes_Real, Kg_Real_Sem, Ha_Prog_Turno,
            Area_Total_Turno, Ha_Manual_1ra, Ha_Manual_2da, Ha_Faltante_Turno_Neto, Turno_Completo_1ra, Ha_Real_2da, Ha_Faltante_2da_Neto, Turno_Completo_2da,
            Ultimo_Dia_Cosecha_Ant, Prop_Avance_Turno, Factor_Floral, Tipo_Efecto_Floral, Delta_Floral_Pct, Motivo_Ajuste
          ),
          by = c("Módulo", "Turno", "Variedad")
        ) %>%
        mutate(
          Ha_Prog_Turno = coalesce(Ha_Prog_Turno, 0),
          Kg_Real_Sem   = coalesce(Kg_Real_Sem, 0),
          Valvula_Cosecho_Lunes  = (Ha_Lunes_Rc > 0) | (Semana_Ant_No_Finalizo & Cosecho_Lunes & (is.na(Dia_7d) | coalesce(Kg_Total_Ant, 0) == 0) & Ha_Prog == 0),
          Valvula_Cosecho_Martes = (Ha_Martes_Rc > 0) | (Semana_Ant_No_Finalizo & !Cosecho_Lunes & Cosecho_Martes & (is.na(Dia_7d) | coalesce(Kg_Total_Ant, 0) == 0) & Ha_Prog == 0),
          Valvula_Cerro_En_Lun_Mar = Semana_Ant_No_Finalizo & (is.na(Dia_7d) | coalesce(Kg_Total_Ant, 0) == 0) & (Valvula_Cosecho_Lunes | Valvula_Cosecho_Martes),
          Valvula_Tiene_2da_Real   = codigo_norm %in% valvs_con_2da_rc,
          
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
      u_base <- u_base_sim
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
            Semana_Ant_No_Finalizo ~ Ha_Faltante,
            
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
        filter(
          coalesce(Esta_Proyectado, FALSE),
          Semana_Ant_No_Finalizo,
          Es_Ciclo_Corto,
          Valvula_Cerro_En_Lun_Mar,
          Ha_Cerrada_Lun_Mar > 0,
          !coalesce(Turno_Completo_2da, FALSE),
          !coalesce(Valvula_Tiene_2da_Real, FALSE)
        ) %>%
        group_by(Módulo, Turno, Variedad) %>%
        mutate(
          tot_2da_rem = sum(Ha_Cerrada_Lun_Mar, na.rm = TRUE),
          Ha_2da = if_else(
            tot_2da_rem > coalesce(Ha_Faltante_2da_Neto, tot_2da_rem) & tot_2da_rem > 0,
            round(Ha_Cerrada_Lun_Mar * (coalesce(Ha_Faltante_2da_Neto, tot_2da_rem) / tot_2da_rem), 2),
            Ha_Cerrada_Lun_Mar
          ),
          Dia_Cierre_Idx = ifelse(Valvula_Cosecho_Lunes, 1L, 2L),
          Dia_Asignado = case_when(
            Valvula_Cosecho_Lunes  ~ ifelse(dia_fin_idx >= 5L, 'Viernes', dia_fin_nom),
            Valvula_Cosecho_Martes ~ ifelse(dia_fin_idx >= 6L, 'Sábado', dia_fin_nom),
            TRUE                   ~ ifelse(dia_fin_idx >= 5L, 'Viernes', dia_fin_nom)
          ),
          Dia_Nuevo_Idx = match(Dia_Asignado, dias_nombres),
          Dias_Crecimiento = pmax(1L, Dia_Nuevo_Idx - Dia_Cierre_Idx),
          Factor_Crecimiento = case_when(
            limpiar_variedad(Variedad) == 'ROCIO' & Valvula_Cosecho_Lunes  ~ coalesce(Pct_Pasada_Lun, 60) / 100,
            limpiar_variedad(Variedad) == 'ROCIO' & Valvula_Cosecho_Martes ~ coalesce(Pct_Pasada_Mar, 60) / 100,
            limpiar_variedad(Variedad) == 'ROCIO'                          ~ coalesce(Pct_Pasada_Lun, 60) / 100,
            TRUE                                                           ~ pmin(1, Dias_Crecimiento / pmax(1, Frecuencia_Dias))
          ),
          Rend_Base_Dia  = ifelse(Valvula_Cosecho_Lunes, Rend_Valv_Lunes, Rend_Valv_Martes),
          Kg_Asignado    = round(Ha_2da * coalesce(Rend_Base_Dia, 0) * Factor_Crecimiento, 2),
          Ha_Asignada    = Ha_2da,
          Num_Pasada     = 2L,
          Es_Ruta_Sin_Kg = FALSE,
          Motivo_Ajuste  = sprintf('2da pasada Cierre S-1 (%s • %d%% sin floración)', 
                                   ifelse(Valvula_Cosecho_Lunes, 'Lunes', 'Martes'), round(Factor_Crecimiento * 100))
        ) %>%
        ungroup() %>%
        filter(Kg_Asignado > 0, Ha_Asignada > 0, Dia_Asignado %in% dias_ajustables_nom)
      
      # Componente 3A: SEGUNDA PASADA PARA COSECHA DEL LUNES (Rama Derecha -> Lun a Jue, ej. Rocío 3 días)
      reg_2da_lun <- valvulas_calc %>%
        filter(
          coalesce(Esta_Proyectado, FALSE),
          !Semana_Ant_No_Finalizo,
          Es_Ciclo_Corto,
          Ha_Cosechada_Lunes > 0,
          !coalesce(Turno_Completo_2da, FALSE),
          !coalesce(Valvula_Tiene_2da_Real, FALSE)
        ) %>%
        group_by(Módulo, Turno, Variedad) %>%
        mutate(
          tot_2da_lun = sum(Ha_Cosechada_Lunes, na.rm = TRUE),
          Ha_2da_Corta = if_else(
            tot_2da_lun > coalesce(Ha_Faltante_2da_Neto, tot_2da_lun) & tot_2da_lun > 0,
            round(Ha_Cosechada_Lunes * (coalesce(Ha_Faltante_2da_Neto, tot_2da_lun) / tot_2da_lun), 2),
            Ha_Cosechada_Lunes
          ),
          Dia_Base_Frec  = ifelse(codigo_norm %in% rectif_martes_ids, 'Martes', 'Lunes'),
          Dia_Asignado   = as.character(desplazar_dia_frecuencia(Dia_Base_Frec, Frecuencia_Dias, dias_nombres, dia_inicio_idx, dia_inicio_nom, dia_fin_idx, dia_fin_nom)),
          Pct_Rend       = if_else(limpiar_variedad(Variedad) == 'ROCIO', coalesce(Pct_Pasada_Lun, 60) / 100, 1.0),
          Rend_2da_Aplicar = round(Rend_Valv_Lunes * Pct_Rend, 2),
          Kg_Asignado    = round(Ha_2da_Corta * Rend_2da_Aplicar, 2),
          Ha_Asignada    = Ha_2da_Corta,
          Num_Pasada     = 2L,
          Es_Ruta_Sin_Kg = FALSE,
          Motivo_Ajuste  = sprintf('2da pasada Rocío (Rend Lunes: %s kg/ha • %d%% sin floración)', 
                                   format(round(Rend_Valv_Lunes), big.mark = ','), round(Pct_Rend * 100))
        ) %>%
        ungroup() %>%
        filter(Kg_Asignado > 0, Ha_Asignada > 0, Dia_Asignado %in% dias_ajustables_nom)
      
      # Componente 3B: SEGUNDA PASADA PARA COSECHA DEL MARTES (Rama Derecha -> Mar a Vie, ej. Rocío 3 días)
      reg_2da_mar <- valvulas_calc %>%
        filter(
          coalesce(Esta_Proyectado, FALSE),
          !Semana_Ant_No_Finalizo,
          Es_Ciclo_Corto,
          Ha_Cosechada_Martes > 0,
          !coalesce(Turno_Completo_2da, FALSE),
          !coalesce(Valvula_Tiene_2da_Real, FALSE)
        ) %>%
        group_by(Módulo, Turno, Variedad) %>%
        mutate(
          tot_2da_mar = sum(Ha_Cosechada_Martes, na.rm = TRUE),
          Ha_2da_Corta = if_else(
            tot_2da_mar > coalesce(Ha_Faltante_2da_Neto, tot_2da_mar) & tot_2da_mar > 0,
            round(Ha_Cosechada_Martes * (coalesce(Ha_Faltante_2da_Neto, tot_2da_mar) / tot_2da_mar), 2),
            Ha_Cosechada_Martes
          ),
          Dia_Asignado   = as.character(desplazar_dia_frecuencia('Martes', Frecuencia_Dias, dias_nombres, dia_inicio_idx, dia_inicio_nom, dia_fin_idx, dia_fin_nom)),
          Pct_Rend       = if_else(limpiar_variedad(Variedad) == 'ROCIO', coalesce(Pct_Pasada_Mar, 60) / 100, 1.0),
          Rend_2da_Aplicar = round(Rend_Valv_Martes * Pct_Rend, 2),
          Kg_Asignado    = round(Ha_2da_Corta * Rend_2da_Aplicar, 2),
          Ha_Asignada    = Ha_2da_Corta,
          Num_Pasada     = 2L,
          Es_Ruta_Sin_Kg = FALSE,
          Motivo_Ajuste  = sprintf('2da pasada Rocío (Rend Martes: %s kg/ha • %d%% sin floración)', 
                                   format(round(Rend_Valv_Martes), big.mark = ','), round(Pct_Rend * 100))
        ) %>%
        ungroup() %>%
        filter(Kg_Asignado > 0, Ha_Asignada > 0, Dia_Asignado %in% dias_ajustables_nom)
      
      # Componente 3C: SEGUNDA PASADA PARA COSECHA MANUAL DE MIÉRCOLES (Rocío -> Sábado)
      reg_2da_mie <- if (nrow(ed_man_sem) > 0 && "Miércoles" %in% ed_man_sem$Campo && dia_fin_idx >= 6L) {
        valvulas_calc %>%
          filter(
            coalesce(Esta_Proyectado, FALSE),
            limpiar_variedad(Variedad) == "ROCIO",
            !coalesce(Turno_Completo_2da, FALSE),
            !coalesce(Valvula_Tiene_2da_Real, FALSE),
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
          Campaña         = as.character("2026"),
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

      valvs_anteriores <- readRDS("D:/ACP/Actualizacion Semanal/ajuste_valvulas_historico.rds")
      valvs_filtradas <- if (!is.null(valvs_anteriores) && is.data.frame(valvs_anteriores) && nrow(valvs_anteriores) > 0) {
        if (solo_filtrado) {
          claves_valvs_ant <- paste(
            trimws(as.character(valvs_anteriores$Módulo)),
            trimws(as.character(valvs_anteriores$Turno)),
            limpiar_variedad(valvs_anteriores$Variedad),
            sep = "___"
          )
          valvs_anteriores %>%
            filter(!(as.character(Campaña) == as.character("2026") &
                     as.numeric(Semana)    == as.numeric(sem_sel) &
                     claves_valvs_ant %in% claves_filtro))
        } else {
          valvs_anteriores %>%
            filter(!(as.character(Campaña) == as.character("2026") & as.numeric(Semana) == as.numeric(sem_sel)))
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
      #
      cat("Guardando valvs:", nrow(valvs_guardar), "n")
      
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
          Campaña       = as.character("2026"),
          Semana        = sem_sel,
          Fecha_Edicion = Sys.time()
        )
      
      # incProgress
      
      ed_actuales <- readRDS("D:/ACP/Actualizacion Semanal/ediciones_manuales_cosecha.rds")
      
      ed_manuales_existentes <- ed_actuales %>%
        filter(
          as.character(Campaña) == as.character("2026"),
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
        cat("NOTIFICATION:", msg_v, type = "warning", duration = 4, "n")
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
            as.character(Campaña) == as.character("2026") &
              as.numeric(Semana)    == sem_sel &
              (is.na(Tipo_Edicion) | Tipo_Edicion %in% c("auto", "ruta_sin_kg")) &
              claves_ed_act %in% claves_filtro
          ))
      } else {
        ed_conservar <- ed_actuales %>%
          filter(!(
            as.character(Campaña) == as.character("2026") &
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
      
      n_premios  <- sum(rend_turno$Tipo_Efecto_Floral == "Premio" & rend_turno$Esta_Proyectado & !(rend_turno$Tiene_Cosecha_Act & rend_turno$Prop_Avance_Turno > prop_umbral_cfg) & !(rend_turno$Semana_Ant_No_Finalizo & rend_turno$Tiene_Cosecha_Fresca & rend_turno$Prop_Fresca_Turno > prop_umbral_cfg), na.rm = TRUE)
      n_castigos <- sum(rend_turno$Tipo_Efecto_Floral == "Castigo" & rend_turno$Esta_Proyectado & !(rend_turno$Tiene_Cosecha_Act & rend_turno$Prop_Avance_Turno > prop_umbral_cfg) & !(rend_turno$Semana_Ant_No_Finalizo & rend_turno$Tiene_Cosecha_Fresca & rend_turno$Prop_Fresca_Turno > prop_umbral_cfg), na.rm = TRUE)
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
    permitir_cualquier_sem <- is.null(TRUE) || isTRUE(TRUE)
    es_campana_act <- grepl("2026|^26", as.character("2026"))
    sem_actual_num <- lubridate::isoweek(Sys.Date())
    es_semana_act  <- (as.numeric(45) == sem_actual_num) || permitir_cualquier_sem
    
    if (!es_campana_act) {
      cat("NOTIFICATION:", "El ajuste automático solo está habilitado para la campaña 2026.", type = "warning", duration = 4, "n")
      return()
    }
    
    if (!es_semana_act) {
      cat("NOTIFICATION:", "El ajuste automático está restringido a la semana actual en curso. Activa 'Ajustar cualquier semana' en el menú (⋮, "n") para habilitarlo.", type = "warning", duration = 5)
      return()
    }
    
    if (hay_filtro_tabla_activo()) {
      ejecutar_ajuste_semana(solo_filtrado = TRUE)
    } else {
      sem_sel  <- as.numeric(45)
      camp_sel <- as.character("2026")
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
