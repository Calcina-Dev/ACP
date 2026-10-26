# ==============================================================================
# SHINY APP: CONTROL SEMANAL DE COSECHA (VERSIÓN CORREGIDA Y OPTIMIZADA)
# ==============================================================================

paquetes <- c("shiny", "dplyr", "tidyr", "lubridate", "DT", "readxl", "plotly", "writexl", "httr", "jsonlite")
nuevos <- paquetes[!(paquetes %in% installed.packages()[, "Package"])]
if (length(nuevos)) install.packages(nuevos)

library(shiny)
library(dplyr)
library(tidyr)
library(lubridate)
library(DT)
library(readxl)
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

norm_codigo_valvula <- function(x) {
  sub("^M0*([0-9]+)[-_]T0*([0-9]+)[-_]V0*([0-9]+)$", "M\\1-T\\2-V\\3", toupper(trimws(as.character(x))))
}

# Función para ordenar semanas cronológicamente en campañas que cruzan de año (ej. S25..S53 -> S1..S19)
orden_semana_campana <- function(sem) {
  sem_num <- suppressWarnings(as.numeric(sem))
  ifelse(!is.na(sem_num) & sem_num < 20, sem_num + 53, sem_num)
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
      Tipo_Edicion  = character(),
      Fecha_Edicion = as.POSIXct(character())
    ))
  }
  df %>%
    mutate(
      Campaña       = trimws(as.character(Campaña)),
      Semana        = suppressWarnings(as.numeric(Semana)),
      Sector        = trimws(as.character(Sector)),
      Módulo        = trimws(as.character(Módulo)),
      Turno         = trimws(as.character(Turno)),
      Variedad      = limpiar_variedad(Variedad),
      Campo         = trimws(as.character(Campo)),
      Valor         = suppressWarnings(as.numeric(Valor)),
      Tipo_Edicion  = if ("Tipo_Edicion" %in% names(.)) coalesce(as.character(Tipo_Edicion), "manual") else "manual",
      Fecha_Edicion = as.POSIXct(Fecha_Edicion)
    )
}

guardar_ediciones_disco <- function(df_ediciones) {
  guardado <- FALSE
  df_norm <- df_ediciones %>%
    mutate(
      Campaña       = trimws(as.character(Campaña)),
      Semana        = suppressWarnings(as.numeric(Semana)),
      Sector        = trimws(as.character(Sector)),
      Módulo        = trimws(as.character(Módulo)),
      Turno         = trimws(as.character(Turno)),
      Variedad      = limpiar_variedad(Variedad),
      Campo         = trimws(as.character(Campo)),
      Valor         = suppressWarnings(as.numeric(Valor)),
      Tipo_Edicion  = if ("Tipo_Edicion" %in% names(.)) coalesce(as.character(Tipo_Edicion), "manual") else "manual",
      Fecha_Edicion = as.POSIXct(Fecha_Edicion)
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

# ==============================================================================
# CATÁLOGO Y MOTOR DE DATOS PARA MAPA 2D DE COSECHA OFICIAL
# ==============================================================================
if (exists("df_valvulas") && is.data.frame(df_valvulas)) {
  if (!"Grupo" %in% names(df_valvulas)) {
    df_valvulas <- df_valvulas %>% mutate(Grupo = asignar_grupo_modulo(Módulo))
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
      Kg_Sábado      = get_num("kg.*sab", 0)
    ) %>%
      filter(!is.na(Semana), nzchar(Codigo_Valvula)) %>%
      mutate(
        Tipo_Registro = case_when(
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
        Fecha_Actualizacion = Sys.time()
      )
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
            df_act <- df_existente %>%
              filter(!(Campaña == "2026" & Semana == as.numeric(sem_key) & Tipo_Registro %in% c("Proyeccion", "CosechaCampo", "Cosecha"))) %>%
              bind_rows(df_remoto)
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

obtener_datos_mapa_semana <- function(sem_num, 
                                      pasada_filtro = "1",
                                      grupos_filtro = character(0),
                                      mods_filtro   = character(0),
                                      turnos_filtro = character(0),
                                      vars_filtro   = character(0)) {
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
        Pct_Sabado_rc    = numeric()
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
      Pct_Sabado_rc    = numeric()
    )
  }
  
  # 3. Fusión Unificada: Kilos Oficiales + Ha Cosecha Campo (con fallback oficial)
  df_merged <- df_valv_ref %>%
    left_join(df_oficial_cruzado, by = "id") %>%
    left_join(rc_cruzado, by = "id") %>%
    mutate(
      # Kilos siempre oficiales SAP
      Kg_Lunes     = coalesce(Kg_Lunes_ofic, 0),
      Kg_Martes    = coalesce(Kg_Martes_ofic, 0),
      Kg_Miercoles = coalesce(Kg_Miercoles_ofic, 0),
      Kg_Jueves    = coalesce(Kg_Jueves_ofic, 0),
      Kg_Viernes   = coalesce(Kg_Viernes_ofic, 0),
      Kg_Sabado    = coalesce(Kg_Sabado_ofic, 0),
      Kg_Total     = round(Kg_Lunes + Kg_Martes + Kg_Miercoles + Kg_Jueves + Kg_Viernes + Kg_Sabado, 2),
      
      # Bandera: ¿Se tiene información en Ruta de Cosecha Campo?
      tiene_campo = (!is.na(Ha_Total_rc) & Ha_Total_rc > 0) |
                    (coalesce(Pct_Lunes_rc, 0) + coalesce(Pct_Martes_rc, 0) + coalesce(Pct_Miercoles_rc, 0) +
                     coalesce(Pct_Jueves_rc, 0) + coalesce(Pct_Viernes_rc, 0) + coalesce(Pct_Sabado_rc, 0)) > 0,
      
      # Hectáreas: Priorizar Campo, fallback a Oficial
      Ha_Lunes     = ifelse(tiene_campo, coalesce(Ha_Lunes_rc, 0), coalesce(Ha_Lunes_ofic, 0)),
      Ha_Martes    = ifelse(tiene_campo, coalesce(Ha_Martes_rc, 0), coalesce(Ha_Martes_ofic, 0)),
      Ha_Miercoles = ifelse(tiene_campo, coalesce(Ha_Miercoles_rc, 0), coalesce(Ha_Miercoles_ofic, 0)),
      Ha_Jueves    = ifelse(tiene_campo, coalesce(Ha_Jueves_rc, 0), coalesce(Ha_Jueves_ofic, 0)),
      Ha_Viernes   = ifelse(tiene_campo, coalesce(Ha_Viernes_rc, 0), coalesce(Ha_Viernes_ofic, 0)),
      Ha_Sabado    = ifelse(tiene_campo, coalesce(Ha_Sabado_rc, 0), coalesce(Ha_Sabado_ofic, 0)),
      
      # Porcentajes diarios de dibujo: Priorizar Campo, fallback a Oficial
      Pct_Lunes     = ifelse(tiene_campo, coalesce(Pct_Lunes_rc, 0), coalesce(Pct_Lunes_ofic, 0)),
      Pct_Martes    = ifelse(tiene_campo, coalesce(Pct_Martes_rc, 0), coalesce(Pct_Martes_ofic, 0)),
      Pct_Miercoles = ifelse(tiene_campo, coalesce(Pct_Miercoles_rc, 0), coalesce(Pct_Miercoles_ofic, 0)),
      Pct_Jueves    = ifelse(tiene_campo, coalesce(Pct_Jueves_rc, 0), coalesce(Pct_Jueves_ofic, 0)),
      Pct_Viernes   = ifelse(tiene_campo, coalesce(Pct_Viernes_rc, 0), coalesce(Pct_Viernes_ofic, 0)),
      Pct_Sabado    = ifelse(tiene_campo, coalesce(Pct_Sabado_rc, 0), coalesce(Pct_Sabado_ofic, 0)),
      
      Pct_Total = pmin(100, Pct_Lunes + Pct_Martes + Pct_Miercoles + Pct_Jueves + Pct_Viernes + Pct_Sabado),
      Ha_Cosechada = ifelse(tiene_campo, coalesce(Ha_Total_rc, Ha_Lunes + Ha_Martes + Ha_Miercoles + Ha_Jueves + Ha_Viernes + Ha_Sabado),
                            coalesce(Ha_Total_ofic, round(Área * (Pct_Total / 100), 2)))
    ) %>%
    mutate(
      Ha_Cosechada = ifelse(Ha_Cosechada <= 0 & Pct_Total > 0 & Área > 0, round(Área * (Pct_Total / 100), 2), Ha_Cosechada),
      Pct_Total    = ifelse(Kg_Total > 0 & Pct_Total <= 0, 100, Pct_Total),
      Ha_Cosechada = ifelse(Kg_Total > 0 & Ha_Cosechada <= 0, Área, Ha_Cosechada)
    )
  
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
      df_valv_f <- df_valv_f %>%
        filter(as.character(Módulo) %in% as.character(mods_filtro))
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
          Kg_Total, Pct_Total, Ha_Cosechada
        )
    )
  )
}
} else {
  stop("ERROR CRÍTICO: Conexión exclusiva a Google Drive falló. No se encontró datos_maestros_cosecha_2026.RData en la unidad G: ni en Google Drive nube.")
}

# Homologar EB 9-2 a MAGICA y remover tildes en la base histórica de cosecha
df_cosecha_base <- df_cosecha %>%
  mutate(Variedad = limpiar_variedad(Variedad))

# 2. Pre-calcular el Área consolidada por Turno (desde el catálogo histórico)
areas_base <- df_cosecha_base %>%
  filter(!is.na(Área), Área > 0) %>%
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
    col_fecha    <- nms[grepl("^fecha$", nms, ignore.case = TRUE)][1]
    col_peso     <- nms[grepl("^pesoneto$", nms, ignore.case = TRUE)][1]
    col_consum   <- nms[grepl("^consumidor$", nms, ignore.case = TRUE)][1]
    col_sector   <- nms[grepl("^sector$", nms, ignore.case = TRUE)][1]
    col_modulo   <- nms[grepl("^modulo$|^m[oó]dulo$", nms, ignore.case = TRUE)][1]
    col_turno    <- nms[grepl("^turno$", nms, ignore.case = TRUE)][1]
    col_variedad <- nms[grepl("^variedad$", nms, ignore.case = TRUE)][1]
    
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
    }
    
    df_actual <- df_actual %>%
      mutate(
        Fecha    = fechas_parsed[which(es_semana_actual)],
        Real     = suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(.data[[col_peso]])))),
        Sector   = sec_vec,
        Módulo   = mod_vec,
        Turno    = tur_vec,
        Variedad = limpiar_variedad(sub("^.*\\|\\s*", "", as.character(.data[[col_variedad]]))),
        Semana   = semana_actual,
        Campaña  = campana_actual
      ) %>%
      filter(!is.na(Real), Real > 0) %>%
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
    
    cat(sprintf("  -> Éxito SAP: %d turnos consolidados de Semana %d (Total: %.1f Kg)\n", 
                nrow(df_actual), semana_actual, sum(df_actual$`Kg Total`, na.rm = TRUE)))
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
  return(df_hist_base)
}

# Carga inicial al arrancar la app: si ya viene con metadata de actualización en RData, la base ya está al día
df_cosecha_inicial <- if (exists("metadata_actualizacion")) {
  cat("[COSECHA INICIAL] Base cargada al día directamente desde RData maestro (Arranque ultra-rápido)\n")
  df_cosecha_base
} else {
  integrar_sap_en_historico(df_cosecha_base, ruta_raiz_arandano, campana_actual = 2026)
}


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
        grid-template-columns: repeat(auto-fit, minmax(220px, 1fr));
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

      /* Switch iOS para Kg vs Kg/ha en esquina superior derecha */
      .unit-switch-wrapper {
        position: absolute;
        top: 20px;
        right: 24px;
        z-index: 20;
        display: flex;
        align-items: center;
        gap: 10px;
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

      /* Botones de Acción estilo iOS */
      .btn-descargar-excel {
        background-color: #34C759 !important;
        color: #FFFFFF !important;
        border: none !important;
        border-radius: 10px !important;
        padding: 6px 14px !important;
        font-size: 12px !important;
        font-weight: 600 !important;
        display: inline-flex !important;
        align-items: center !important;
        gap: 6px !important;
        box-shadow: 0 2px 6px rgba(52, 199, 89, 0.25) !important;
        text-decoration: none !important;
        transition: all 0.2s ease !important;
      }
      .btn-descargar-excel:hover, .btn-descargar-excel:focus {
        background-color: #2DB84D !important;
        color: #FFFFFF !important;
        transform: translateY(-1px);
        box-shadow: 0 4px 10px rgba(52, 199, 89, 0.35) !important;
      }
      
      .btn-ajustar-semana {
        background: linear-gradient(135deg, #FF9500 0%, #FF5E3A 100%) !important;
        color: #FFFFFF !important;
        font-weight: 600 !important;
        font-size: 12px !important;
        border: none !important;
        border-radius: 8px !important;
        padding: 7px 14px !important;
        display: inline-flex !important;
        align-items: center !important;
        gap: 6px !important;
        box-shadow: 0 2px 8px rgba(255, 149, 0, 0.25) !important;
        transition: all 0.2s ease !important;
        cursor: pointer;
      }
      .btn-ajustar-semana:hover, .btn-ajustar-semana:focus {
        transform: translateY(-1px);
        box-shadow: 0 4px 12px rgba(255, 149, 0, 0.38) !important;
        color: #FFFFFF !important;
      }
      
      .btn-deshacer-ajuste {
        background: #FFFFFF !important;
        color: #8E8E93 !important;
        font-weight: 600 !important;
        font-size: 12px !important;
        border: 1px solid rgba(0, 0, 0, 0.14) !important;
        border-radius: 8px !important;
        padding: 7px 12px !important;
        display: inline-flex !important;
        align-items: center !important;
        gap: 5px !important;
        transition: all 0.2s ease !important;
        cursor: pointer;
      }
      .btn-deshacer-ajuste:hover, .btn-deshacer-ajuste:focus {
        background: #F2F2F7 !important;
        color: #1C1C1E !important;
      }
      
      .badge-dia-mapa {
        display: inline-flex;
        align-items: center;
        gap: 5px;
        transition: transform 0.15s ease;
      }
      .badge-dia-mapa:hover {
        transform: translateY(-1px);
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

      .filtros-grid-container {
        display: grid;
        grid-template-columns: 100px 105px 1.4fr 1fr 1fr 1.3fr 90px;
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
      @media (max-width: 1100px) {
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
        background-color: #007AFF !important;
        color: #FFFFFF !important;
      }
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
              div(class = "kpi-label", "Cosecha Total Real"),
              div(class = "kpi-value", textOutput("kpi_kilos")),
              span(class = "kpi-badge", "Kg (Ajuste)")
          ),
          div(class = "kpi-card",
              div(class = "kpi-label", "Proyección Total"),
              div(class = "kpi-value", textOutput("kpi_proyeccion")),
              span(class = "kpi-badge", style = "background: rgba(255, 149, 0, 0.12); color: #FF9500;", "Kg Proyectados")
          ),
          div(class = "kpi-card",
              div(class = "kpi-label", "Área en Evaluación"),
              div(class = "kpi-value", textOutput("kpi_area")),
              span(class = "kpi-badge", "Hectáreas")
          ),
          div(class = "kpi-card",
              div(class = "kpi-label", "Cumplimiento General"),
              div(class = "kpi-value", textOutput("kpi_cumplimiento")),
              span(class = "kpi-badge", style = "background: rgba(52, 199, 89, 0.12); color: #34C759;", "% Meta")
          )
      ),
      
      div(class = "ios-card", style = "position: relative;",
          # Switch Kg vs Kg/ha en esquina superior derecha (justo arriba de Segmentación)
          conditionalPanel(
            condition = "input.tabs_navegacion == 'Evolución Histórica'",
            div(class = "unit-switch-wrapper",
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
          
          tabsetPanel(
            id = "tabs_navegacion",
            type = "pills",
            tabPanel("Detalle Semanal",
                     div(style = "margin-top: 14px;",
                         div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 12px; flex-wrap: wrap; gap: 8px;",
                             div(
                               h3(style = "font-size: 15px; font-weight: 700; margin: 0 0 3px 0; color: #1C1C1E;", "Detalle General por Sector, Módulo y Turno"),
                               span(style = "font-size: 11.5px; color: #8E8E93;", "Cifras en Kilogramos • Doble clic para registrar en celdas vacías (Semana actual)")
                             ),
                             div(style = "display: flex; gap: 8px; align-items: center; flex-wrap: wrap;",
                                  actionButton("btn_ajustar_semana", "✨ Ajustar Semana", class = "btn-ajustar-semana", icon = icon("wand-magic-sparkles")),
                                  actionButton("btn_deshacer_ajuste", "↺ Deshacer Ajuste", class = "btn-deshacer-ajuste", icon = icon("rotate-left")),
                                  downloadButton("btn_descarga_excel", "Descargar Excel", class = "btn-descargar-excel", icon = icon("file-excel")),
                                  span(style = "font-size: 11px; font-weight: 600; padding: 4px 10px; border-radius: 12px; background: rgba(88, 86, 214, 0.12); color: #5856D6; border: 1px solid rgba(88, 86, 214, 0.25);", "🟣 Manual"),
                                  span(style = "font-size: 11px; font-weight: 600; padding: 4px 10px; border-radius: 12px; background: rgba(245, 158, 11, 0.14); color: #D97706; border: 1px solid rgba(245, 158, 11, 0.30);", "🟠 Auto (7d)"),
                                  span(style = "font-size: 11px; font-weight: 600; padding: 4px 10px; border-radius: 12px; background: rgba(0, 122, 255, 0.10); color: #007AFF; border: 1px solid rgba(0, 122, 255, 0.20);", "🔵 Real SAP"),
                                  span(style = "font-size: 11px; font-weight: 600; padding: 4px 10px; border-radius: 12px; background: rgba(52, 199, 89, 0.12); color: #34C759; border: 1px solid rgba(52, 199, 89, 0.25);", "🟢 Ajuste / kg/ha")
                             )
                         ),
                         DTOutput("tabla_control_cosecha")
                     )
            ),
            tabPanel("Evolución Histórica",
                     div(style = "margin-top: 14px;",
                         fluidRow(
                           column(8,
                                  div(style = "background: #FFFFFF; border-radius: 14px; padding: 14px 18px; border: 1px solid rgba(0, 0, 0, 0.05);",
                                      div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px; flex-wrap: wrap; gap: 8px;",
                                          h3(style = "font-size: 15px; font-weight: 700; margin: 0; color: #1C1C1E;", 
                                             textOutput("titulo_grafico_historico", inline = TRUE)),
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
                             div(style = "display: flex; align-items: center; gap: 8px; flex-wrap: wrap; margin-top: 12px; padding-top: 10px; border-top: 1px solid rgba(0, 0, 0, 0.05); font-size: 11.5px;",
                                 span(style = "font-weight: 700; color: #8E8E93; margin-right: 4px;", "Avance Diario:"),
                                 span(id = "mapa_badge_lunes",     class = "badge-dia-mapa", style = "background: rgba(255,0,0,0.1); color: #D32F2F; border: 1px solid rgba(255,0,0,0.3); padding: 4px 10px; border-radius: 6px; font-weight: 600;", "● Lunes: 0 kg"),
                                 span(id = "mapa_badge_martes",    class = "badge-dia-mapa", style = "background: rgba(0,162,232,0.12); color: #0288D1; border: 1px solid rgba(0,162,232,0.3); padding: 4px 10px; border-radius: 6px; font-weight: 600;", "● Martes: 0 kg"),
                                 span(id = "mapa_badge_miercoles", class = "badge-dia-mapa", style = "background: rgba(255,127,39,0.12); color: #E65100; border: 1px solid rgba(255,127,39,0.3); padding: 4px 10px; border-radius: 6px; font-weight: 600;", "● Miércoles: 0 kg"),
                                 span(id = "mapa_badge_jueves",    class = "badge-dia-mapa", style = "background: rgba(255,235,59,0.22); color: #F57F17; border: 1px solid rgba(251,192,45,0.4); padding: 4px 10px; border-radius: 6px; font-weight: 600;", "● Jueves: 0 kg"),
                                 span(id = "mapa_badge_viernes",   class = "badge-dia-mapa", style = "background: rgba(0,200,83,0.12); color: #2E7D32; border: 1px solid rgba(0,200,83,0.3); padding: 4px 10px; border-radius: 6px; font-weight: 600;", "● Viernes: 0 kg"),
                                 span(id = "mapa_badge_sabado",    class = "badge-dia-mapa", style = "background: rgba(163,73,164,0.12); color: #7B1FA2; border: 1px solid rgba(163,73,164,0.3); padding: 4px 10px; border-radius: 6px; font-weight: 600;", "● Sábado: 0 kg")
                             )
                         ),
                         # Contenedor Canvas 2D
                         div(id = "mapa-wrapper",
                             tags$canvas(id = "mapaCanvasLectura"),
                             # Badge Superior Flotante
                             div(style = "position: absolute; left: 16px; top: 16px; background: rgba(255, 255, 255, 0.94); border: 1px solid rgba(0,0,0,0.1); border-radius: 8px; padding: 6px 12px; font-size: 12px; font-weight: 600; color: #1C1C1E; box-shadow: 0 2px 8px rgba(0,0,0,0.06); display: flex; align-items: center; gap: 6px; backdrop-filter: blur(4px); z-index: 10;",
                                 span(style = "display: inline-block; width: 8px; height: 8px; border-radius: 50%; background: #007AFF;"),
                                 "Mapa de Cosecha Oficial • Datos Cruzados (Solo Lectura)"
                             ),
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
            )
          )
      )
  )
)

# ==============================================================================
# SERVER
# ==============================================================================

server <- function(input, output, session) {
  # Reactive Values para datos de cosecha y ediciones manuales
  rv_cosecha         <- reactiveVal(df_cosecha_inicial)
  ediciones_manuales <- reactiveVal(cargar_ediciones_disco())
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
    camp_act <- if (!is.null(input$filtro_campana)) input$filtro_campana else 2026
    nuevo_df <- integrar_sap_en_historico(df_cosecha_base, ruta_raiz_arandano, campana_actual = camp_act)
    rv_cosecha(nuevo_df)
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
        Módulo = case_when(
          es_campana_2026 & Módulo %in% c("13.1", "13.2") ~ "13",
          es_campana_2026 & Módulo %in% c("14.1", "14.2") ~ "14",
          TRUE ~ Módulo
        ),
        Var_join = limpiar_variedad(Variedad)
      ) %>%
      group_by(Módulo, Turno, Var_join) %>%
      summarise(Área_Cat = sum(Área, na.rm = TRUE), .groups = "drop")
  })
  
  # Reactive 2A: Universo Base sin ediciones manuales (Solo cambia al cambiar Campaña, Semana o Actualizar SAP)
  universo_base <- reactive({
    req(input$filtro_campana, input$filtro_semana)
    
    dias_semana <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Domingo")
    es_campana_2026 <- grepl("2026|^26", as.character(input$filtro_campana))
    
    # 1. Cosecha registrada en la semana (unificando 13.1/13.2 -> 13 y 14.1/14.2 -> 14 antes de cruzar áreas)
    df_filtrado <- rv_cosecha() %>%
      filter(as.character(Campaña) == as.character(input$filtro_campana), Semana == input$filtro_semana)
    
    cosecha_preparada <- if (nrow(df_filtrado) > 0) {
      df_filtrado %>%
        mutate(
          Dia      = factor(dias_semana[wday(Fecha, week_start = 1)], levels = dias_semana),
          Sector   = trimws(as.character(Sector)),
          Módulo   = trimws(as.character(Módulo)),
          Módulo   = case_when(
            es_campana_2026 & Módulo %in% c("13.1", "13.2") ~ "13",
            es_campana_2026 & Módulo %in% c("14.1", "14.2") ~ "14",
            TRUE ~ Módulo
          ),
          Turno    = trimws(as.character(Turno)),
          Variedad = limpiar_variedad(Variedad)
        ) %>%
        group_by(Sector, Módulo, Turno, Variedad, Dia) %>%
        summarise(Kg_Dia = sum(`Kg Total`, na.rm = TRUE), .groups = "drop") %>%
        pivot_wider(names_from = Dia, values_from = Kg_Dia, values_fill = 0)
    } else {
      tibble(Sector = character(), Módulo = character(), Turno = character(), Variedad = character())
    }
    
    for (d in dias_semana) {
      if (!d %in% names(cosecha_preparada)) cosecha_preparada[[d]] <- numeric(nrow(cosecha_preparada))
    }
    
    # 2. Obtener proyecciones
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
    
    ed_todas <- ediciones_manuales()
    if (!is.null(ed_todas) && nrow(ed_todas) > 0) {
      ed_filtradas <- ed_todas %>%
        filter(
          as.character(Campaña) == as.character(input$filtro_campana),
          as.numeric(Semana)    == as.numeric(input$filtro_semana)
        )
      
      if (nrow(ed_filtradas) > 0) {
        claves_u <- paste(trimws(as.character(universo$Sector)),
                          trimws(as.character(universo$Módulo)),
                          trimws(as.character(universo$Turno)),
                          limpiar_variedad(universo$Variedad), sep = "___")
        claves_ed <- paste(trimws(as.character(ed_filtradas$Sector)),
                           trimws(as.character(ed_filtradas$Módulo)),
                           trimws(as.character(ed_filtradas$Turno)),
                           limpiar_variedad(ed_filtradas$Variedad), sep = "___")
        dias_validos <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
        
        for (i in seq_len(nrow(ed_filtradas))) {
          idx <- match(claves_ed[i], claves_u)
          if (!is.na(idx)) {
            campo_i <- ed_filtradas$Campo[i]
            val_i   <- suppressWarnings(as.numeric(ed_filtradas$Valor[i]))
            tipo_i  <- if ("Tipo_Edicion" %in% names(ed_filtradas)) coalesce(as.character(ed_filtradas$Tipo_Edicion[i]), "manual") else "manual"
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
                if (!universo[[lock_col]][idx]) {
                  universo[[campo_i]][idx]  <- val_i
                  universo[[man_col]][idx]  <- (tipo_i == "manual")
                  universo[[tipo_col]][idx] <- tipo_i
                }
              }
            }
          }
        }
      }
    }
    
    # Cálculo de totales consolidados (incluyendo Domingo en Ajuste por si hubiera cosecha dominical)
    universo <- universo %>%
      mutate(
        across(c(Lunes, Martes, Miércoles, Jueves, Viernes, Sábado, Domingo), ~ coalesce(.x, 0)),
        Ajuste           = round(Lunes + Martes + Miércoles + Jueves + Viernes + Sábado + Domingo, 2),
        Proyección       = round(Proyección, 2),
        `Kg/Ha Proy`     = ifelse(Área > 0 & !is.na(Proyección), round(Proyección / Área, 2), NA_real_),
        Diferencia       = ifelse(!is.na(Proyección), round(Ajuste - Proyección, 2), NA_real_),
        `kg/ha ajustado` = ifelse(Área > 0, round(Ajuste / Área, 2), 0)
      )
    
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
  
  # Datos filtrados según las selecciones interactivas
  datos_reactivos <- reactive({
    universo <- universo_completo()
    if (is.null(universo) || nrow(universo) == 0) return(tibble())
    
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
    
    resultado <- universo %>%
      mutate(
        Variedad = limpiar_variedad(Variedad),
        Tipo     = case_when(
          grepl("org[aá]n", Tipo, ignore.case = TRUE) ~ "Orgánico",
          grepl("conv", Tipo, ignore.case = TRUE)     ~ "Convencional",
          TRUE                                        ~ tools::toTitleCase(tolower(trimws(Tipo)))
        ),
        across(c(Lunes, Martes, Miércoles, Jueves, Viernes, Sábado), ~ ifelse(.x == 0, NA_real_, round(.x, 2)))
      ) %>%
      select(
        Sector, Módulo, Turno, Variedad, Tipo,
        Proyección, Área, `Kg/Ha Proy`,
        Lunes, Martes, Miércoles, Jueves, Viernes, Sábado,
        Ajuste, Diferencia, `kg/ha ajustado`,
        Proy_manual, Lunes_manual, Martes_manual, Miércoles_manual, Jueves_manual, Viernes_manual, Sábado_manual,
        Proy_bloqueado, Lunes_bloqueado, Martes_bloqueado, Miércoles_bloqueado, Jueves_bloqueado, Viernes_bloqueado, Sábado_bloqueado,
        Lunes_tipo, Martes_tipo, Miércoles_tipo, Jueves_tipo, Viernes_tipo, Sábado_tipo
      ) %>%
      arrange(
        suppressWarnings(as.numeric(Sector)), 
        suppressWarnings(as.numeric(Módulo)), 
        suppressWarnings(as.numeric(Turno))
      )
    
    return(resultado)
  })
  
  # KPIs
  output$kpi_kilos <- renderText({
    df <- datos_reactivos()
    if (nrow(df) == 0) return("0")
    format(round(sum(df$Ajuste, na.rm = TRUE), 1), big.mark = ",")
  })
  
  output$kpi_proyeccion <- renderText({
    df <- datos_reactivos()
    if (nrow(df) == 0) return("0")
    format(round(sum(df$Proyección, na.rm = TRUE), 1), big.mark = ",")
  })
  
  output$kpi_area <- renderText({
    df <- datos_reactivos()
    if (nrow(df) == 0) return("0")
    format(round(sum(df$Área, na.rm = TRUE), 2), big.mark = ",")
  })
  
  output$kpi_cumplimiento <- renderText({
    df <- datos_reactivos()
    if (nrow(df) == 0) return("0%")
    tot_real <- sum(df$Ajuste, na.rm = TRUE)
    tot_proy <- sum(df$Proyección, na.rm = TRUE)
    if (tot_proy > 0) {
      paste0(round((tot_real / tot_proy) * 100, 1), "%")
    } else {
      "-"
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
      
      columnas_ocultas <- c(
        "Proy_manual", "Lunes_manual", "Martes_manual", "Miércoles_manual", 
        "Jueves_manual", "Viernes_manual", "Sábado_manual",
        "Proy_bloqueado", "Lunes_bloqueado", "Martes_bloqueado", "Miércoles_bloqueado", 
        "Jueves_bloqueado", "Viernes_bloqueado", "Sábado_bloqueado"
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
    
    es_campana_act <- grepl("2026|^26", as.character(input$filtro_campana))
    sem_actual_num <- lubridate::isoweek(Sys.Date())
    es_semana_act  <- (as.numeric(input$filtro_semana) == sem_actual_num)
    
    cols_deshabilitadas <- c(0:4, 6:7, 14:36)
    
    config_editable <- if (es_campana_act && es_semana_act) {
      list(target = "cell", disable = list(columns = cols_deshabilitadas))
    } else {
      FALSE
    }
    
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
        stateSave = TRUE,
        dom = 't<"bottom"ip>',
        language = list(
          search = "Buscar:",
          lengthMenu = "Mostrar _MENU_ turnos",
          info = "Mostrando _START_ a _END_ de _TOTAL_ turnos",
          paginate = list(previous = "←", `next` = "→")
        ),
        columnDefs = list(
          list(visible = FALSE, targets = 17:36),
          list(defaultContent = "-", targets = "_all"),
          list(className = 'dt-center', targets = 0:4),
          list(className = 'dt-right', targets = 5:16)
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
            
            $(api.column(0).footer()).html('TOTAL');
            for (var c = 1; c <= 4; c++) {
              $(api.column(c).footer()).html('');
            }
            
            var sumCols = [5, 6, 8, 9, 10, 11, 12, 13, 14, 15];
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
            var kghaAjus = areaTot > 0 ? (sums[14] / areaTot) : 0;
            
            $(api.column(7).footer()).html(fmt(kghaProy));
            $(api.column(16).footer()).html(fmt(kghaAjus));
          }
        ")
      ),
      callback = JS("
        // 1. Bloquear doble clic en celdas con datos reales de SAP
        table.on('dblclick', 'tbody td', function(e) {
          var cell = table.cell(this);
          if (!cell.index()) return;
          var colIdx = cell.index().column;
          var rowIdx = cell.index().row;
          var rowData = table.row(rowIdx).data();
          if (!rowData) return;
          
          var lockMap = { 5: 24, 8: 25, 9: 26, 10: 27, 11: 28, 12: 29, 13: 30 };
          if (lockMap.hasOwnProperty(colIdx)) {
            var lockIdx = lockMap[colIdx];
            var isLocked = rowData[lockIdx];
            if (isLocked === true || isLocked === 1 || isLocked === 'TRUE') {
              e.stopImmediatePropagation();
              e.stopPropagation();
              Shiny.setInputValue('alerta_celda_bloqueada', { time: new Date().getTime() }, { priority: 'event' });
              return false;
            }
          }
        });

        // 2. Guardar al presionar Enter (y cancelar con Escape) dentro de la celda editada
        table.on('keydown', 'tbody td input', function(e) {
          if (e.key === 'Enter' || e.keyCode === 13) {
            e.preventDefault();
            e.stopPropagation();
            $(this).blur(); // Dispara el guardado nativo de DT hacia Shiny
          } else if (e.key === 'Escape' || e.keyCode === 27) {
            e.preventDefault();
            e.stopPropagation();
            var td = $(this).closest('td');
            var cell = table.cell(td);
            cell.data(cell.data()); // Restaura el valor original sin guardar
          }
        });
      "),
    ) %>%
      formatRound(columns = c("Proyección", "Área", "Kg/Ha Proy", "Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Ajuste", "Diferencia", "kg/ha ajustado"), digits = 2) %>%
      formatStyle(
        'Proyección',
        'Proy_manual',
        color = styleEqual(c(TRUE, FALSE), c('#5856D6', '#FF9500')),
        backgroundColor = styleEqual(c(TRUE, FALSE), c('rgba(88, 86, 214, 0.12)', 'transparent')),
        fontWeight = '600'
      ) %>%
      formatStyle('Ajuste', fontWeight = 'bold', color = '#007AFF') %>%
      formatStyle('kg/ha ajustado', fontWeight = 'bold', color = '#34C759') %>%
      formatStyle(
        'Diferencia',
        color = styleInterval(0, c('#FF3B30', '#34C759')),
        fontWeight = '600'
      ) %>%
      formatStyle(
        'Lunes',
        'Lunes_tipo',
        color = styleEqual(c('manual', 'auto', 'sap', 'none'), c('#5856D6', '#D97706', '#007AFF', '#1C1C1E')),
        backgroundColor = styleEqual(c('manual', 'auto', 'sap', 'none'), c('rgba(88, 86, 214, 0.12)', 'rgba(245, 158, 11, 0.14)', 'transparent', 'transparent')),
        fontWeight = styleEqual(c('manual', 'auto', 'sap', 'none'), c('bold', 'bold', 'normal', 'normal'))
      ) %>%
      formatStyle(
        'Martes',
        'Martes_tipo',
        color = styleEqual(c('manual', 'auto', 'sap', 'none'), c('#5856D6', '#D97706', '#007AFF', '#1C1C1E')),
        backgroundColor = styleEqual(c('manual', 'auto', 'sap', 'none'), c('rgba(88, 86, 214, 0.12)', 'rgba(245, 158, 11, 0.14)', 'transparent', 'transparent')),
        fontWeight = styleEqual(c('manual', 'auto', 'sap', 'none'), c('bold', 'bold', 'normal', 'normal'))
      ) %>%
      formatStyle(
        'Miércoles',
        'Miércoles_tipo',
        color = styleEqual(c('manual', 'auto', 'sap', 'none'), c('#5856D6', '#D97706', '#007AFF', '#1C1C1E')),
        backgroundColor = styleEqual(c('manual', 'auto', 'sap', 'none'), c('rgba(88, 86, 214, 0.12)', 'rgba(245, 158, 11, 0.14)', 'transparent', 'transparent')),
        fontWeight = styleEqual(c('manual', 'auto', 'sap', 'none'), c('bold', 'bold', 'normal', 'normal'))
      ) %>%
      formatStyle(
        'Jueves',
        'Jueves_tipo',
        color = styleEqual(c('manual', 'auto', 'sap', 'none'), c('#5856D6', '#D97706', '#007AFF', '#1C1C1E')),
        backgroundColor = styleEqual(c('manual', 'auto', 'sap', 'none'), c('rgba(88, 86, 214, 0.12)', 'rgba(245, 158, 11, 0.14)', 'transparent', 'transparent')),
        fontWeight = styleEqual(c('manual', 'auto', 'sap', 'none'), c('bold', 'bold', 'normal', 'normal'))
      ) %>%
      formatStyle(
        'Viernes',
        'Viernes_tipo',
        color = styleEqual(c('manual', 'auto', 'sap', 'none'), c('#5856D6', '#D97706', '#007AFF', '#1C1C1E')),
        backgroundColor = styleEqual(c('manual', 'auto', 'sap', 'none'), c('rgba(88, 86, 214, 0.12)', 'rgba(245, 158, 11, 0.14)', 'transparent', 'transparent')),
        fontWeight = styleEqual(c('manual', 'auto', 'sap', 'none'), c('bold', 'bold', 'normal', 'normal'))
      ) %>%
      formatStyle(
        'Sábado',
        'Sábado_tipo',
        color = styleEqual(c('manual', 'auto', 'sap', 'none'), c('#5856D6', '#D97706', '#007AFF', '#1C1C1E')),
        backgroundColor = styleEqual(c('manual', 'auto', 'sap', 'none'), c('rgba(88, 86, 214, 0.12)', 'rgba(245, 158, 11, 0.14)', 'transparent', 'transparent')),
        fontWeight = styleEqual(c('manual', 'auto', 'sap', 'none'), c('bold', 'bold', 'normal', 'normal'))
      )
  })
  
  # Observador: Alerta celda bloqueada
  observeEvent(input$alerta_celda_bloqueada, {
    showNotification(
      "Esta celda contiene registros oficiales (Cosecha SAP / Proyección Oficial) y está bloqueada contra edición.",
      type = "warning",
      duration = 4
    )
  })
  
  # Observador: Edición de celda
  observeEvent(input$tabla_control_cosecha_cell_edit, {
    info <- input$tabla_control_cosecha_cell_edit
    df_actual <- datos_reactivos()
    
    if (is.null(info$row) || info$row < 1 || info$row > nrow(df_actual)) return()
    
    col_idx_r <- info$col + 1
    col_name <- names(df_actual)[col_idx_r]
    
    cols_permitidas <- c("Proyección", "Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
    if (!col_name %in% cols_permitidas) {
      showNotification("Esta columna no es editable.", type = "warning", duration = 3)
      replaceData(dataTableProxy("tabla_control_cosecha"), df_actual, resetPaging = FALSE, rownames = FALSE)
      return()
    }
    
    es_campana_act <- grepl("2026|^26", as.character(input$filtro_campana))
    sem_actual_num <- lubridate::isoweek(Sys.Date())
    es_semana_act  <- (as.numeric(input$filtro_semana) == sem_actual_num)
    
    if (!es_campana_act || !es_semana_act) {
      showNotification("Solo se permite editar celdas en la semana actual en curso.", type = "error", duration = 4)
      replaceData(dataTableProxy("tabla_control_cosecha"), df_actual, resetPaging = FALSE, rownames = FALSE)
      return()
    }
    
    fila <- df_actual[info$row, ]
    lock_col <- if (col_name == "Proyección") "Proy_bloqueado" else paste0(col_name, "_bloqueado")
    
    if (isTRUE(fila[[lock_col]])) {
      showNotification(
        sprintf("La celda [%s] del Turno %s contiene datos oficiales de SAP/Proyección y está bloqueada contra edición.", col_name, fila$Turno),
        type = "warning",
        duration = 4
      )
      replaceData(dataTableProxy("tabla_control_cosecha"), df_actual, resetPaging = FALSE, rownames = FALSE)
      return()
    }
    
    val_str <- trimws(as.character(info$value))
    val_num <- suppressWarnings(as.numeric(val_str))
    
    if (is.na(val_num) || val_str == "" || val_str == "-" || val_num == 0) {
      ed_actuales <- ediciones_manuales()
      ed_filtradas <- ed_actuales %>%
        filter(!(
          as.character(Campaña) == as.character(input$filtro_campana) &
            as.numeric(Semana)    == as.numeric(input$filtro_semana) &
            as.character(Sector)   == as.character(fila$Sector) &
            as.character(Módulo)   == as.character(fila$Módulo) &
            as.character(Turno)    == as.character(fila$Turno) &
            limpiar_variedad(Variedad) == limpiar_variedad(fila$Variedad) &
            as.character(Campo)    == as.character(col_name)
        ))
      ediciones_manuales(ed_filtradas)
      guardar_ediciones_disco(ed_filtradas)
      showNotification(sprintf("Se restableció la celda [%s] del Turno %s.", col_name, fila$Turno), type = "message", duration = 3)
      return()
    }
    
    if (val_num < 0) {
      showNotification("El valor debe ser un número positivo (>= 0).", type = "warning", duration = 4)
      replaceData(dataTableProxy("tabla_control_cosecha"), df_actual, resetPaging = FALSE, rownames = FALSE)
      return()
    }
    
    val_num <- round(val_num, 2)
    nueva_edicion <- tibble(
      Campaña       = as.character(input$filtro_campana),
      Semana        = as.numeric(input$filtro_semana),
      Sector        = as.character(fila$Sector),
      Módulo        = as.character(fila$Módulo),
      Turno         = as.character(fila$Turno),
      Variedad      = limpiar_variedad(fila$Variedad),
      Campo         = as.character(col_name),
      Valor         = val_num,
      Tipo_Edicion  = "manual",
      Fecha_Edicion = Sys.time()
    )
    
    ed_actuales <- ediciones_manuales()
    ed_actuales <- ed_actuales %>%
      mutate(Campaña = as.character(Campaña), Semana = as.numeric(Semana)) %>%
      filter(!(
        as.character(Campaña) == as.character(input$filtro_campana) &
          as.numeric(Semana)    == as.numeric(input$filtro_semana) &
          as.character(Sector)   == as.character(fila$Sector) &
          as.character(Módulo)   == as.character(fila$Módulo) &
          as.character(Turno)    == as.character(fila$Turno) &
          limpiar_variedad(Variedad) == limpiar_variedad(fila$Variedad) &
          as.character(Campo)    == as.character(col_name)
      )) %>%
      bind_rows(nueva_edicion)
    
    ediciones_manuales(ed_actuales)
    ok_disco <- guardar_ediciones_disco(ed_actuales)
    
    if (ok_disco) {
      showNotification(
        sprintf("Dato guardado y persistido: [%s] = %s Kg (Turno %s)", col_name, format(val_num, big.mark = ","), fila$Turno),
        type = "message",
        duration = 3
      )
    } else {
      showNotification(
        sprintf("Dato registrado en sesión: [%s] = %s Kg (Turno %s)", col_name, format(val_num, big.mark = ","), fila$Turno),
        type = "warning",
        duration = 4
      )
    }
  })

  # ============================================================================== =
  # BOTÓN ✨ AJUSTAR SEMANA (AUTOCOMPLETAR SEGÚN CICLO DE 7 DÍAS)
  # ============================================================================== =
  observeEvent(input$btn_ajustar_semana, {
    es_campana_act <- grepl("2026|^26", as.character(input$filtro_campana))
    sem_actual_num <- lubridate::isoweek(Sys.Date())
    es_semana_act  <- (as.numeric(input$filtro_semana) == sem_actual_num)
    
    if (!es_campana_act || !es_semana_act) {
      showNotification("El ajuste automático solo está habilitado para la semana actual en curso.", type = "warning", duration = 4)
      return()
    }
    
    sem_sel <- as.numeric(input$filtro_semana)
    sem_ant <- sem_sel - 1
    
    dias_nombres <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
    dias_cols_kg <- c("Kg_Lunes", "Kg_Martes", "Kg_Miércoles", "Kg_Jueves", "Kg_Viernes", "Kg_Sábado")
    
    dia_hoy_num <- lubridate::wday(Sys.Date(), week_start = 1)
    dia_hoy_idx <- min(6, dia_hoy_num)
    dia_manana_idx <- min(6, dia_hoy_idx + 1)
    dia_manana_nom <- dias_nombres[dia_manana_idx]
    dias_futuros_nom <- if (dia_hoy_idx < 6) dias_nombres[(dia_hoy_idx + 1):6] else "Sábado"
    
    withProgress(message = "Ajustando semana...", detail = "Analizando ciclo de 7 días y rendimientos", value = 0.2, {
      valv_7d_map <- tibble(codigo_norm = character(), Dia_7d = character())
      turno_dia_habitual <- tibble(Módulo = character(), Turno = character(), Dia_Habitual = character())
      
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
              select(codigo_norm, Dia_7d)
            
            turno_dia_habitual <- df_ant_valvs %>%
              filter(!is.na(Dia_7d)) %>%
              group_by(Módulo = as.character(Módulo_Norm), Turno = as.character(Turno_Norm), Dia_7d) %>%
              summarise(Total_Kg = sum(Kg_Total_Sem, na.rm = TRUE), .groups = "drop") %>%
              arrange(desc(Total_Kg)) %>%
              group_by(Módulo, Turno) %>%
              slice(1) %>%
              ungroup() %>%
              select(Módulo, Turno, Dia_Habitual = Dia_7d)
          }
        }
      }
      
      incProgress(0.3, detail = "Cruzando avance de válvulas y rendimientos...")
      
      rc <- tryCatch(obtener_rutas_campo_en_vivo(sem_sel), error = function(e) cargar_rutas_cosecha())
      if (is.null(rc) || nrow(rc) == 0) rc <- tryCatch(cargar_rutas_cosecha(), error = function(e) NULL)
      rc_sem <- if (!is.null(rc) && "Semana" %in% names(rc)) rc %>% filter(Semana == sem_sel) else tibble()
      
      rc_valv_avance <- if (nrow(rc_sem) > 0 && "codigo" %in% names(rc_sem)) {
        rc_sem %>%
          mutate(codigo_norm = norm_codigo_valvula(codigo)) %>%
          group_by(codigo_norm) %>%
          summarise(
            Ha_Prog = sum(coalesce(Ha_Total_Prog, 0), na.rm = TRUE),
            Kg_Prog = sum(coalesce(Kg_Total_Prog, 0), na.rm = TRUE),
            .groups = "drop"
          )
      } else tibble(codigo_norm = character(), Ha_Prog = numeric(), Kg_Prog = numeric())
      
      df_cosecha_sem <- rv_cosecha() %>%
        filter(as.character(Campaña) == as.character(input$filtro_campana), Semana == sem_sel) %>%
        mutate(
          Módulo   = trimws(as.character(Módulo)),
          Turno    = trimws(as.character(Turno)),
          Variedad = limpiar_variedad(Variedad)
        )
      
      kg_real_turno <- df_cosecha_sem %>%
        group_by(Módulo, Turno, Variedad) %>%
        summarise(Kg_Real_Sem = sum(`Kg Total`, na.rm = TRUE), .groups = "drop")
      
      proy_sem <- proyeccion_semanal()
      proy_turno <- if (!is.null(proy_sem) && nrow(proy_sem) > 0) {
        proy_sem %>%
          mutate(
            Módulo   = trimws(as.character(Módulo_join)),
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
          codigo_norm = norm_codigo_valvula(codigo_norm),
          Sector      = trimws(as.character(Sector)),
          Módulo      = trimws(as.character(Módulo)),
          Turno       = trimws(as.character(Turno)),
          Variedad    = limpiar_variedad(Variedad),
          Área        = suppressWarnings(as.numeric(Área))
        ) %>%
        left_join(rc_valv_avance, by = "codigo_norm") %>%
        mutate(
          Ha_Prog     = coalesce(Ha_Prog, 0),
          Ha_Faltante = pmax(0, Área - Ha_Prog)
        ) %>%
        left_join(valv_7d_map, by = "codigo_norm") %>%
        left_join(turno_dia_habitual, by = c("Módulo", "Turno")) %>%
        mutate(
          Dia_Asignado = case_when(
            !is.na(Dia_7d) & Dia_7d %in% dias_futuros_nom ~ Dia_7d,
            !is.na(Dia_7d) ~ dia_manana_nom,
            !is.na(Dia_Habitual) & Dia_Habitual %in% dias_futuros_nom ~ Dia_Habitual,
            TRUE ~ dia_manana_nom
          )
        )
      
      turno_avance_ha <- valvulas_calc %>%
        group_by(Módulo, Turno, Variedad) %>%
        summarise(
          Ha_Prog_Turno     = sum(Ha_Prog, na.rm = TRUE),
          Ha_Faltante_Turno = sum(Ha_Faltante, na.rm = TRUE),
          Area_Total_Turno  = sum(Área, na.rm = TRUE),
          .groups = "drop"
        )
      
      rend_turno <- turno_avance_ha %>%
        left_join(kg_real_turno, by = c("Módulo", "Turno", "Variedad")) %>%
        left_join(proy_turno, by = c("Módulo", "Turno", "Variedad")) %>%
        mutate(
          Kg_Real_Sem = coalesce(Kg_Real_Sem, 0),
          Proy_Kg     = coalesce(Proy_Kg, 0),
          # Fallback inteligente: si SAP ya tiene kilos y no hay avance en rutas o es desfasado
          Rutas_Vigente_Ok = (Ha_Prog_Turno > 0 & Ha_Prog_Turno >= 0.15 * Area_Total_Turno),
          Rend_Pond   = case_when(
            Rutas_Vigente_Ok & Kg_Real_Sem > 0 ~ Kg_Real_Sem / Ha_Prog_Turno,
            Area_Total_Turno > 0 & Proy_Kg > 0  ~ Proy_Kg / Area_Total_Turno,
            TRUE ~ 0
          ),
          Kg_Remanente = pmax(0, Proy_Kg - Kg_Real_Sem)
        )
      
      # Suma de área de válvulas futuras por turno para prorrateo de remanente si no hay rutas al día
      area_futura_turno <- valvulas_calc %>%
        filter(Dia_Asignado %in% dias_futuros_nom) %>%
        group_by(Módulo, Turno, Variedad) %>%
        summarise(Area_Futura_Turno = sum(Área, na.rm = TRUE), .groups = "drop")
      
      valvulas_calc <- valvulas_calc %>%
        left_join(rend_turno %>% select(Módulo, Turno, Variedad, Rend_Pond, Rutas_Vigente_Ok, Kg_Real_Sem, Kg_Remanente), by = c("Módulo", "Turno", "Variedad")) %>%
        left_join(area_futura_turno, by = c("Módulo", "Turno", "Variedad")) %>%
        mutate(
          Area_Futura_Turno = coalesce(Area_Futura_Turno, 0),
          Kg_Asignado = case_when(
            # Caso 1: Rutas de campo al día -> Hectáreas faltantes * Rendimiento ponderado
            coalesce(Rutas_Vigente_Ok, FALSE) ~ round(Ha_Faltante * coalesce(Rend_Pond, 0), 2),
            # Caso 2: SAP ya cosechó pero Rutas no se llenó -> Distribuir saldo remanente solo en días futuros
            Kg_Real_Sem > 0 & Dia_Asignado %in% dias_futuros_nom & Area_Futura_Turno > 0 ~ round(Kg_Remanente * (Área / Area_Futura_Turno), 2),
            # Caso 3: Semana sin cosecha previa -> Área * Rendimiento proyectado
            Kg_Real_Sem <= 0 ~ round(Ha_Faltante * coalesce(Rend_Pond, 0), 2),
            TRUE ~ 0
          )
        )
      auto_ediciones <- valvulas_calc %>%
        filter(Kg_Asignado > 0) %>%
        group_by(Sector, Módulo, Turno, Variedad, Campo = Dia_Asignado) %>%
        summarise(
          Valor = round(sum(Kg_Asignado, na.rm = TRUE), 2),
          .groups = "drop"
        ) %>%
        mutate(
          Campaña       = as.character(input$filtro_campana),
          Semana        = sem_sel,
          Tipo_Edicion  = "auto",
          Fecha_Edicion = Sys.time()
        )
      
      incProgress(0.3, detail = "Guardando ediciones y actualizando tabla...")
      
      u_base <- universo_base()
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
      
      claves_sap_bloqueadas <- character(0)
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
      
      auto_validas <- auto_ediciones[!(claves_auto %in% claves_manuales | claves_auto %in% claves_sap_bloqueadas), ]
      
      if (nrow(auto_validas) == 0) {
        showNotification("No hay válvulas o días pendientes para autocompletar en esta semana.", type = "warning", duration = 4)
        return()
      }
      
      ed_conservar <- ed_actuales %>%
        filter(!(
          as.character(Campaña) == as.character(input$filtro_campana) &
            as.numeric(Semana)    == sem_sel &
            (is.na(Tipo_Edicion) | Tipo_Edicion == "auto")
        ))
      
      ed_nuevas_totales <- bind_rows(ed_conservar, auto_validas)
      
      ediciones_manuales(ed_nuevas_totales)
      guardar_ediciones_disco(ed_nuevas_totales)
      
      showNotification(
        sprintf("✨ Se completaron automáticamente %d turnos proyectados según ciclo de 7 días (%s Kg).", 
                nrow(auto_validas), format(round(sum(auto_validas$Valor), 1), big.mark = ",")),
        type = "message",
        duration = 5
      )
    })
  })
  
  # ============================================================================== =
  # BOTÓN ↺ DESHACER AJUSTE AUTOMÁTICO
  # ============================================================================== =
  observeEvent(input$btn_deshacer_ajuste, {
    sem_sel <- as.numeric(input$filtro_semana)
    camp_sel <- as.character(input$filtro_campana)
    
    ed_actuales <- ediciones_manuales()
    if (is.null(ed_actuales) || nrow(ed_actuales) == 0) {
      showNotification("No hay ajustes automáticos para remover.", type = "message", duration = 3)
      return()
    }
    
    n_prev <- nrow(ed_actuales)
    ed_filtradas <- ed_actuales %>%
      filter(!(
        as.character(Campaña) == camp_sel &
          as.numeric(Semana)    == sem_sel &
          (is.na(Tipo_Edicion) | Tipo_Edicion == "auto")
      ))
    
    n_removidos <- n_prev - nrow(ed_filtradas)
    if (n_removidos == 0) {
      showNotification("No se encontraron ajustes automáticos en esta semana. Las ediciones manuales se mantienen.", type = "message", duration = 3)
      return()
    }
    
    ediciones_manuales(ed_filtradas)
    guardar_ediciones_disco(ed_filtradas)
    
    showNotification(
      sprintf("↺ Se removieron %d registros automáticos. Las ediciones manuales se conservaron intactas.", n_removidos),
      type = "message",
      duration = 4
    )
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
        orden_semana_campana(Semana) < ord_sel
      ) %>%
      mutate(
        Módulo = trimws(as.character(Módulo)),
        Módulo = case_when(
          es_campana_2026 & Módulo %in% c("13.1", "13.2") ~ "13",
          es_campana_2026 & Módulo %in% c("14.1", "14.2") ~ "14",
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
          Kg_Real     = sum(`Kg Total`, na.rm = TRUE),
          Kg_Ajustado = sum(`Kg Total`, na.rm = TRUE),
          Kg_Proy     = NA_real_,
          Área_Sem    = NA_real_,
          .groups     = "drop"
        )
    } else {
      tibble(Módulo = character(), Turno = character(), Variedad = character(), Semana = numeric(), Kg_Real = numeric(), Kg_Ajustado = numeric(), Kg_Proy = numeric(), Área_Sem = numeric())
    }
    df_act <- datos_reactivos()
    
    # Cosecha física oficial real de SAP de la semana seleccionada
    df_sap_actual_turno <- rv_cosecha() %>%
      filter(as.character(Campaña) == as.character(input$filtro_campana), Semana == sem_sel) %>%
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
          Semana      = sem_sel,
          Kg_Ajustado = sum(Ajuste, na.rm = TRUE),
          Kg_Proy     = sum(Proyección, na.rm = TRUE),
          Área_Sem    = sum(Área, na.rm = TRUE),
          .groups     = "drop"
        ) %>%
        left_join(df_sap_actual_turno, by = c("Módulo", "Turno", "Variedad")) %>%
        mutate(
          Kg_Real = coalesce(Kg_Real_SAP, 0)
        )
    } else {
      tibble(Módulo = character(), Turno = character(), Variedad = character(), Semana = numeric(), Kg_Real = numeric(), Kg_Ajustado = numeric(), Kg_Proy = numeric(), Área_Sem = numeric())
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
            Semana   = sem_next,
            Kg_Real  = NA_real_,
            Kg_Proy  = sum(Proyección, na.rm = TRUE),
            Área_Sem = NA_real_,
            .groups  = "drop"
          )
      } else {
        tibble(Módulo = character(), Turno = character(), Variedad = character(), Semana = numeric(), Kg_Real = numeric(), Kg_Proy = numeric(), Área_Sem = numeric())
      }
    } else {
      tibble(Módulo = character(), Turno = character(), Variedad = character(), Semana = numeric(), Kg_Real = numeric(), Kg_Proy = numeric(), Área_Sem = numeric())
    }
    
    cat_areas <- areas_por_turno()
    
    bind_rows(hist_pasado, act_sem, next_sem) %>%
      mutate(Var_join = limpiar_variedad(Variedad)) %>%
      left_join(cat_areas, by = c("Módulo", "Turno", "Var_join")) %>%
      group_by(Módulo, Turno, Variedad) %>%
      mutate(
        Área = coalesce(
          ifelse(!is.na(Área_Cat) & Área_Cat > 0, Área_Cat, NA_real_),
          ifelse(!is.na(Área_Sem) & Área_Sem > 0, Área_Sem, NA_real_),
          suppressWarnings(max(Área_Sem, na.rm = TRUE))
        ),
        Área = ifelse(!is.finite(Área) | is.na(Área), 0, Área)
      ) %>%
      ungroup() %>%
      select(-Var_join, -Área_Sem, -Área_Cat)
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
  
  # ==============================================================================
  # GRÁFICO HISTÓRICO INTERACTIVO (PLOTLY):
  # - COSECHA REAL: Línea continua (solid)
  # - PROYECCIÓN:   Línea punteada (dot) del mismo color en la semana seleccionada
  # ==============================================================================
  output$grafico_historico <- renderPlotly({
    df_u <- datos_historicos_universo()
    req(df_u, nrow(df_u) > 0)
    
    sem_sel    <- as.numeric(input$filtro_semana)
    es_kgha    <- identical(input$switch_unidad, "kgha")
    unidad_txt <- if (es_kgha) "Kg/ha" else "Kg"
    
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
    
    df_complete <- df_sub %>%
      complete(nesting(Módulo, Turno, Variedad, Área), Semana = semanas_rango, fill = list(Kg_Real = 0, Kg_Ajustado = 0, Kg_Proy = 0)) %>%
      mutate(
        Orden_Sem = orden_semana_campana(Semana),
        Val_Real  = if (es_kgha) ifelse(Área > 0, coalesce(Kg_Real, 0) / Área, 0) else coalesce(Kg_Real, 0),
        Val_Ajus  = if (es_kgha) ifelse(Área > 0, coalesce(Kg_Ajustado, 0) / Área, 0) else coalesce(Kg_Ajustado, 0),
        Val_Proy  = if (es_kgha) ifelse(Área > 0 & !is.na(Kg_Proy), Kg_Proy / Área, 0) else coalesce(Kg_Proy, 0),
        # Cosecha real únicamente hasta donde haya cosecha física en SAP (en semanas futuras o <= 0 es NA)
        Val_Real_Plot = case_when(
          Orden_Sem > ord_max ~ NA_real_,
          Semana == sem_sel & Val_Real <= 0 ~ NA_real_,
          TRUE ~ Val_Real
        ),
        Hover_Real = sprintf(
          "<b>%s - %s - %s</b><br>Semana %d (Cosecha Real SAP): %s %s%s%s%s",
          Módulo, Turno, Variedad, Semana,
          format(round(coalesce(Val_Real, 0), 1), big.mark = ","), unidad_txt,
          ifelse(Semana == sem_sel & Val_Ajus > 0, sprintf("<br>Cosecha Ajustada: %s %s", format(round(Val_Ajus, 1), big.mark = ","), unidad_txt), ""),
          ifelse(Semana == sem_sel & Val_Proy > 0, sprintf("<br>Proyección Oficial: %s %s", format(round(Val_Proy, 1), big.mark = ","), unidad_txt), ""),
          if (es_kgha) sprintf("<br>Área: %s ha", format(round(Área, 2), big.mark = ",")) else ""
        )
      ) %>%
      arrange(Orden_Sem)
    paleta_ios <- c("#007AFF", "#FF9500", "#5856D6", "#30B0C7", "#34C759", "#FF2D55", "#FFCC00", "#AF52DE", "#5AC8FA", "#FF3B30")
    
    series_unicas <- df_complete %>%
      distinct(Módulo, Turno, Variedad) %>%
      arrange(suppressWarnings(as.numeric(Módulo)), suppressWarnings(as.numeric(Turno)), Variedad)
    
    p <- plot_ly()
    
    es_multi_series <- nrow(series_unicas) > 1
    
    for (i in seq_len(nrow(series_unicas))) {
      mod_i <- series_unicas$Módulo[i]
      tur_i <- series_unicas$Turno[i]
      var_i <- series_unicas$Variedad[i]
      
      sub_d <- df_complete %>%
        filter(Módulo == mod_i, Turno == tur_i, Variedad == var_i) %>%
        arrange(Orden_Sem)
      
      if (nrow(sub_d) == 0) next
      
      color_serie  <- paleta_ios[((i - 1) %% length(paleta_ios)) + 1]
      nombre_serie <- paste(mod_i, tur_i, var_i, sep = " - ")
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
          "<b>%s - %s - %s</b><br>Semana %d (Cosecha Ajustada): %s %s<br>Cosecha Real SAP: %s %s<br>Proyección Oficial: %s %s%s",
          mod_i, tur_i, var_i, sem_sel,
          format(round(fila_ajus_act$Val_Ajus[1], 1), big.mark = ","), unidad_txt,
          format(round(coalesce(fila_ajus_act$Val_Real[1], 0), 1), big.mark = ","), unidad_txt,
          format(round(coalesce(fila_ajus_act$Val_Proy[1], 0), 1), big.mark = ","), unidad_txt,
          if (es_kgha) sprintf("<br>Área: %s ha", format(round(fila_ajus_act$Área[1], 2), big.mark = ",")) else ""
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
            "<b>%s - %s - %s</b><br>Semana %d (Proyectado Oficial): %s %s<br>Cosecha Real SAP: %s %s%s",
            mod_i, tur_i, var_i, sem_sel,
            format(round(fila_proy_act$Val_Proy[1], 1), big.mark = ","), unidad_txt,
            format(round(coalesce(fila_proy_act$Val_Real[1], 0), 1), big.mark = ","), unidad_txt,
            if (es_kgha) sprintf("<br>Área: %s ha", format(round(fila_proy_act$Área[1], 2), big.mark = ",")) else ""
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
            "<b>%s - %s - %s</b><br>Semana %d (Proyectado Siguiente): %s %s<br><i>Proyección próxima semana</i>%s",
            mod_i, tur_i, var_i, sem_next,
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
      filter(as.character(Campaña) == as.character(input$filtro_campana), Semana == sem_ant) %>%
      mutate(
        Módulo = trimws(as.character(Módulo)),
        Módulo = case_when(
          es_campana_2026 & Módulo %in% c("13.1", "13.2") ~ "13",
          es_campana_2026 & Módulo %in% c("14.1", "14.2") ~ "14",
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
      df_tmp <- df_hist_oficial_wide
      if (length(mods) > 0) {
        df_tmp <- df_tmp %>% filter(as.character(Módulo_Norm) %in% mods)
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
      rc_sub <- rc_todas
      if (length(mods) > 0) rc_sub <- rc_sub %>% filter(as.character(Módulo) %in% mods)
      if (length(turnos) > 0) rc_sub <- rc_sub %>% filter(as.character(Turno) %in% turnos)
      if (length(vars) > 0) rc_sub <- rc_sub %>% filter(limpiar_variedad(Variedad) %in% vars)
      rc_sub %>%
        filter(coalesce(Ha_Total_Prog, 0) > 0 | coalesce(Kg_Total_Prog, 0) > 0) %>%
        pull(Semana) %>% na.omit() %>% as.numeric() %>% unique()
    } else numeric(0)
    
    sems_finales <- sort(unique(c(sems_con_datos, sems_rc)), decreasing = TRUE)
    if (length(sems_finales) == 0) sems_finales <- 43:20
    
    list(
      disponibles = sems_finales,
      defecto     = sems_finales[1]
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

  # Despachar datos del mapa hacia JavaScript
  observeEvent(list(input$filtro_semana_mapa, input$filtro_pasada_mapa, input$filtro_modulo, input$filtro_grupo, input$filtro_turno, input$filtro_variedad, input$tabs_navegacion), {
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
    
    payload_mapa <- obtener_datos_mapa_semana(
      sem_num       = sem_target,
      pasada_filtro = pasada_target,
      grupos_filtro = input$filtro_grupo,
      mods_filtro   = input$filtro_modulo,
      turnos_filtro = input$filtro_turno,
      vars_filtro   = input$filtro_variedad
    )
    session$sendCustomMessage("actualizar_mapa_semana", payload_mapa)
  }, ignoreInit = FALSE)

}

# Ejecutar la app
shinyApp(ui, server)
