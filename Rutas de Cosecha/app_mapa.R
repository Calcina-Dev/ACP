# ==============================================================================
# MÓDULO INTERACTIVO DE MAPA 2D DE VÁLVULAS - PROYECCIÓN Y COSECHA REAL
# Ubicación: D:/modulo_mapa_valvulas/app_mapa.R
# Grilla de 10 Cuadros (10% c/u) • Proyección Oficial (Hoja Cálculo) + Cosecha Real
# ==============================================================================

paquetes <- c("shiny", "dplyr", "tidyr", "lubridate", "DT", "jsonlite", "readxl", "purrr", "httr", "base64enc")
nuevos <- paquetes[!(paquetes %in% installed.packages()[, "Package"])]
if (length(nuevos)) install.packages(nuevos)

library(shiny)
library(dplyr)
library(tidyr)
library(lubridate)
library(DT)
library(jsonlite)
library(readxl)
library(purrr)
library(httr)
library(base64enc)

# Registro de recursos estáticos web (directorio www)
tryCatch({
  if (dir.exists("www")) {
    shiny::addResourcePath("www", normalizePath("www"))
  } else if (dir.exists("D:/modulo_mapa_valvulas/www")) {
    shiny::addResourcePath("www", "D:/modulo_mapa_valvulas/www")
  }
}, error = function(e) NULL)

# Logo oficial Cerro Prieto para el Login (Base64 data URI para carga 100% autónoma y sin dependencias de red)
logo_cerro_prieto_src <- tryCatch({
  if (file.exists("logo_cerro_prieto_b64.R")) {
    e_logo <- new.env()
    sys.source("logo_cerro_prieto_b64.R", envir = e_logo)
    e_logo$logo_cerro_prieto_b64
  } else if (file.exists("D:/modulo_mapa_valvulas/logo_cerro_prieto_b64.R")) {
    e_logo <- new.env()
    sys.source("D:/modulo_mapa_valvulas/logo_cerro_prieto_b64.R", envir = e_logo)
    e_logo$logo_cerro_prieto_b64
  } else if (file.exists("www/logo_cerro_prieto.png")) {
    paste0("data:image/png;base64,", base64enc::base64encode("www/logo_cerro_prieto.png"))
  } else if (file.exists("D:/modulo_mapa_valvulas/www/logo_cerro_prieto.png")) {
    paste0("data:image/png;base64,", base64enc::base64encode("D:/modulo_mapa_valvulas/www/logo_cerro_prieto.png"))
  } else {
    "www/logo_cerro_prieto.png"
  }
}, error = function(e) "www/logo_cerro_prieto.png")

# Forzar zona horaria de Perú (UTC-5) para evitar desfase de +5h en shinyapps.io (UTC)
Sys.setenv(TZ = "America/Lima")

formatear_hora_peru <- function(t = Sys.time(), fmt = "%H:%M:%S") {
  if (is.null(t) || length(t) == 0 || all(is.na(t))) return("No disponible")
  t_posix <- tryCatch(as.POSIXct(t), error = function(e) NULL)
  if (is.null(t_posix) || is.na(t_posix[1])) return(as.character(t)[1])
  format(lubridate::with_tz(t_posix[1], tzone = "America/Lima"), fmt)
}

# Función auxiliar para remover tildes y mayúsculas en cruces
normalizar_texto <- function(x) {
  x_str <- toupper(trimws(as.character(x)))
  chartr("ÁÉÍÓÚÜ", "AEIOUU", x_str)
}

# Función auxiliar para homologar nombres de variedad (idéntica a Control Semanal)
limpiar_variedad <- function(x) {
  v <- normalizar_texto(x)
  dplyr::case_when(
    grepl("EB\\s*9\\s*[-_.]\\s*2", v, ignore.case = TRUE) ~ "MAGICA",
    grepl("^MEGA\\s*CRISP|^MEGACRISP", v, ignore.case = TRUE) ~ "MEGA CRISP",
    grepl("^MILAGRO", v, ignore.case = TRUE) ~ "MILAGROS",
    TRUE ~ v
  )
}

# Subdivisión geométrica de válvulas compuestas por múltiples variedades (ej: M12-T17-V59, M14-T17-V65)
particiones_multivariedad_default <- tibble::tribble(
  ~codigo, ~variedad, ~area_ha, ~orden,
  "M12-T17-V59", "MADEIRA", 1.12, 1,
  "M12-T17-V59", "MEGA GEM", 0.07, 2,
  "M12-T17-V59", "MEGA EARLY", 0.61, 3,
  "M10-T17-V50", "MALIBU", 0.42, 1,
  "M10-T17-V50", "MANILA", 1.49, 2,
  "M10-T13-V37", "SEKOYA BEAUTY", 1.58, 1,
  "M10-T13-V37", "SEKOYA POP", 0.34, 2,
  "M10-T16-V48", "MEGA CRISP", 0.70, 1,
  "M10-T16-V48", "MEGA EARLY", 0.07, 2,
  "M10-T16-V48", "SEKOYA POP", 0.02, 3,
  "M10-T16-V48", "ROCIO", 0.60, 4,
  "M10-T16-V48", "MADEIRA", 0.30, 5,
  "M14-T17-V65", "SEKOYA POP", 0.40, 1,
  "M14-T17-V65", "IMPERIAL", 1.16, 2,
  "M14-T17-V65", "MILAGROS", 0.04, 3,
  "M14-T17-V65", "IMPERIAL", 0.44, 4
)

aplicar_particiones_multivariedad <- function(df) {
  ruta_part <- "D:/modulo_mapa_valvulas/particiones_multivariedad.rds"
  if (!file.exists(ruta_part) && exists("ruta_raiz_arandano")) ruta_part <- file.path(ruta_raiz_arandano, "particiones_multivariedad.rds")
  if (!file.exists(ruta_part)) ruta_part <- "particiones_multivariedad.rds"
  
  parts_def <- if (file.exists(ruta_part)) {
    tryCatch(readRDS(ruta_part), error = function(e) NULL)
  } else NULL
  if (is.null(parts_def) || nrow(parts_def) == 0) {
    parts_def <- particiones_multivariedad_default
  }
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

# 1. Definir rutas de persistencia (Google Drive + Respaldo Local + Google Sheets Webhook)
ruta_raiz_arandano   <- "G:/Unidades compartidas/Compartido Proyecciones/3.- Arándano"
ruta_rutas_principal <- file.path(ruta_raiz_arandano, "rutas_cosecha_mapa.rds")
ruta_rutas_local     <- "D:/modulo_mapa_valvulas/rutas_cosecha_mapa.rds"
url_webhook_sheets   <- "https://script.google.com/macros/s/AKfycby6wa7fNcT-ofGLceojaX_HraGwZrJj4GzLyEZHTg-hQ8OrfzMjf-kUOHYz8PLfXKdubA/exec"

# Detectar si estamos ejecutando en la nube (shinyapps.io / servidor sin unidades G: ni D:)
es_entorno_nube <- !dir.exists("G:/") && !dir.exists("D:/")

# ID o enlace compartido de Google Drive de datos_maestros_cosecha_2026.RData (para shinyapps.io)
id_drive_data_maestra <- "1W2OXKfCIXtuOesplFXp4nzLrPpTvkeRc"
origen_rdata_actual   <- "DESCONOCIDO"
ultima_descarga_drive <- as.POSIXct(NA)

# Función para descargar la data maestra en tiempo real desde Google Drive (para shinyapps.io y conexiones nube)
descargar_data_maestra_drive <- function(id_archivo, destino = "datos_maestros_cosecha_2026.RData") {
  if (!nzchar(id_archivo)) return(FALSE)
  id_limpio <- sub(".*[?&]id=([^&]+).*", "\\1", id_archivo)
  id_limpio <- sub(".*/d/([^/]+).*", "\\1", id_limpio)
  url_drive <- sprintf("https://drive.usercontent.google.com/download?id=%s&export=download&confirm=t&nocache=%d",
                       id_limpio, as.integer(Sys.time()))
  
  # Intento 1: httr::GET (el método más confiable en Linux / shinyapps.io para manejar redirects de Drive)
  tryCatch({
    cat("[NUBE] Descargando datos_maestros_cosecha_2026.RData desde Google Drive (httr)...\n")
    res <- httr::GET(
      url_drive,
      httr::write_disk(destino, overwrite = TRUE),
      httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"),
      httr::timeout(90)
    )
    if (httr::status_code(res) == 200 && file.exists(destino) && file.size(destino) > 50000) {
      cat(sprintf("[NUBE] Descarga completada con éxito vía httr (%.2f MB)\n", file.size(destino) / (1024 * 1024)))
      ultima_descarga_drive <<- Sys.time()
      return(TRUE)
    }
  }, error = function(e) {
    cat("[AVISO NUBE] Error en descarga httr:", e$message, "\n")
  })
  
  # Intento 2: Fallback con download.file
  tryCatch({
    cat("[NUBE] Intentando descarga con download.file...\n")
    metodo <- if (capabilities("libcurl")) "libcurl" else "auto"
    download.file(url_drive, destfile = destino, mode = "wb", method = metodo, quiet = TRUE)
    if (file.exists(destino) && file.size(destino) > 50000) {
      cat(sprintf("[NUBE] Descarga completada con éxito vía download.file (%.2f MB)\n", file.size(destino) / (1024 * 1024)))
      ultima_descarga_drive <<- Sys.time()
      return(TRUE)
    }
  }, error = function(e) {
    cat("[AVISO NUBE] Error en download.file:", e$message, "\n")
  })
  
  return(FALSE)
}

# Carga de datos maestros unificados (RData): en la nube se conecta DIRECTAMENTE a Google Drive
ruta_rdata_drive_g <- file.path(ruta_raiz_arandano, "datos_maestros_cosecha_2026.RData")
ruta_rdata_local_1 <- "D:/modulo_mapa_valvulas/datos_maestros_cosecha_2026.RData"
ruta_rdata_local_2 <- "D:/datos_maestros_cosecha_2026.RData"

if (es_entorno_nube) {
  # En shinyapps.io (sin unidades G: ni D:): descarga directa y obligatoria desde Google Drive
  ruta_rdata_master <- "datos_maestros_cosecha_2026.RData"
  exito_dl <- descargar_data_maestra_drive(id_drive_data_maestra, ruta_rdata_master)
  if (exito_dl) {
    origen_rdata_actual <- "NUBE_DRIVE"
  } else if (file.exists(ruta_rdata_master)) {
    origen_rdata_actual <- "EMPAQUETADO_NUBE"
  }
} else if (file.exists(ruta_rdata_drive_g)) {
  ruta_rdata_master   <- ruta_rdata_drive_g
  origen_rdata_actual <- "DRIVE_LOCAL_G"
} else if (file.exists(ruta_rdata_local_1)) {
  ruta_rdata_master   <- ruta_rdata_local_1
  origen_rdata_actual <- "DISCO_LOCAL_D"
} else if (file.exists(ruta_rdata_local_2)) {
  ruta_rdata_master   <- ruta_rdata_local_2
  origen_rdata_actual <- "DISCO_LOCAL_D"
} else {
  ruta_rdata_master <- "datos_maestros_cosecha_2026.RData"
  if (nzchar(id_drive_data_maestra)) {
    exito_dl <- descargar_data_maestra_drive(id_drive_data_maestra, ruta_rdata_master)
    if (exito_dl) origen_rdata_actual <- "NUBE_DRIVE"
  }
}

if (file.exists(ruta_rdata_master)) {
  load(ruta_rdata_master)
  cat("[DATA MAESTRA] Cargado datos_maestros_cosecha_2026.RData con éxito desde:", ruta_rdata_master, "| Origen:", origen_rdata_actual, "\n")
} else {
  # Carga de catálogo histórico oficial cerrado (Balanza Packing) previo
  ruta_hist_oficial <- "D:/modulo_mapa_valvulas/historico_oficial_wide_2026.rds"
  if (!file.exists(ruta_hist_oficial)) ruta_hist_oficial <- "historico_oficial_wide_2026.rds"
  df_hist_oficial_wide <- if (file.exists(ruta_hist_oficial)) {
    tryCatch(readRDS(ruta_hist_oficial), error = function(e) NULL)
  } else NULL
}

# Función para refrescar en caliente el RData en shinyapps.io o desde unidad local
refrescar_data_maestra_en_caliente <- function(forzar_nube = FALSE) {
  hubo_cambio <- FALSE
  if (es_entorno_nube && nzchar(id_drive_data_maestra)) {
    segs_desde_dl <- if (is.na(ultima_descarga_drive)) Inf else as.numeric(difftime(Sys.time(), ultima_descarga_drive, units = "secs"))
    if (forzar_nube || segs_desde_dl > 60 || !file.exists("datos_maestros_cosecha_2026.RData")) {
      exito_dl <- descargar_data_maestra_drive(id_drive_data_maestra, "datos_maestros_cosecha_2026.RData")
      if (exito_dl) {
        ruta_rdata_master   <<- "datos_maestros_cosecha_2026.RData"
        origen_rdata_actual <<- "NUBE_DRIVE"
        hubo_cambio         <- TRUE
      }
    }
  } else if (!es_entorno_nube) {
    if (file.exists(ruta_rdata_drive_g)) {
      ruta_rdata_master   <<- ruta_rdata_drive_g
      origen_rdata_actual <<- "DRIVE_LOCAL_G"
    } else if (file.exists(ruta_rdata_local_1)) {
      ruta_rdata_master   <<- ruta_rdata_local_1
      origen_rdata_actual <<- "DISCO_LOCAL_D"
    } else if (file.exists(ruta_rdata_local_2)) {
      ruta_rdata_master   <<- ruta_rdata_local_2
      origen_rdata_actual <<- "DISCO_LOCAL_D"
    }
  }
  
  if (exists("ruta_rdata_master") && file.exists(ruta_rdata_master)) {
    curr_mtime <- file.mtime(ruta_rdata_master)
    if (hubo_cambio || is.null(cache_mtime_rdata) || !identical(cache_mtime_rdata, curr_mtime) || forzar_nube) {
      tryCatch({
        load(ruta_rdata_master, envir = .GlobalEnv)
        if (exists("df_valvulas", envir = .GlobalEnv) && exists("asignar_grupo_modulo", envir = .GlobalEnv)) {
          .GlobalEnv$df_valvulas <- aplicar_particiones_multivariedad(.GlobalEnv$df_valvulas)
          if (!"Grupo" %in% names(.GlobalEnv$df_valvulas)) {
            .GlobalEnv$df_valvulas <- .GlobalEnv$df_valvulas %>% mutate(Grupo = asignar_grupo_modulo(Módulo))
          }
          if (!"Válvula" %in% names(.GlobalEnv$df_valvulas)) {
            .GlobalEnv$df_valvulas <- .GlobalEnv$df_valvulas %>%
              mutate(Válvula = as.character(suppressWarnings(as.numeric(sub("^.*-V0*", "", as.character(codigo))))))
          } else {
            .GlobalEnv$df_valvulas <- .GlobalEnv$df_valvulas %>%
              mutate(Válvula = coalesce(
                as.character(suppressWarnings(as.numeric(Válvula))),
                as.character(suppressWarnings(as.numeric(sub("^.*-V0*", "", as.character(codigo)))))
              ))
          }
        }
        cache_mtime_rdata <<- curr_mtime
        meta_str <- if (exists("metadata_actualizacion", envir = .GlobalEnv)) .GlobalEnv$metadata_actualizacion$Fecha_Hora_Actualizacion_Str else "NA"
        cat(sprintf("[DATA MAESTRA] Recargada en caliente (%s) | Fecha meta: %s\n", origen_rdata_actual, meta_str))
        return(TRUE)
      }, error = function(e) {
        cat("[AVISO DATA MAESTRA] Error recargando RData:", e$message, "\n")
      })
    }
  }
  return(FALSE)
}

# Normalizador estándar de códigos de válvula (M1-T1-V1)
norm_codigo_valvula <- function(x) {
  sub("^M0*([0-9]+)[-_]T0*([0-9]+)[-_]V0*([0-9]+)$", "M\\1-T\\2-V\\3", toupper(trimws(as.character(x))))
}

# Generador de string de 10 celdas a partir de porcentajes diarios
generar_cells_str_vec <- function(p_lun, p_mar, p_mie, p_jue, p_vie, p_sab) {
  n <- length(p_lun)
  dias <- c("lunes", "martes", "miercoles", "jueves", "viernes", "sabado")
  res <- character(n)
  
  mat_pct <- cbind(
    as.numeric(p_lun), as.numeric(p_mar), as.numeric(p_mie),
    as.numeric(p_jue), as.numeric(p_vie), as.numeric(p_sab)
  )
  mat_pct[is.na(mat_pct)] <- 0
  
  for (i in seq_len(n)) {
    row_p <- mat_pct[i, ]
    tot <- sum(row_p)
    if (tot <= 0) {
      res[i] <- ""
      next
    }
    counts <- round(row_p / 10)
    diff <- 10 - sum(counts)
    if (diff != 0) {
      max_idx <- which.max(row_p)
      counts[max_idx] <- max(0, counts[max_idx] + diff)
    }
    cells <- rep(dias, times = pmax(0, counts))
    if (length(cells) > 10) cells <- cells[1:10]
    if (length(cells) < 10) cells <- c(cells, rep("", 10 - length(cells)))
    res[i] <- paste(cells, collapse = ",")
  }
  res
}

# Extractor de Cosecha Oficial para la semana seleccionada (con recarga automática si el archivo RData se actualizó en disco o nube)
cache_mtime_rdata <- NULL

obtener_cosecha_oficial_semana <- function(sem_num, forzar_nube = FALSE) {
  refrescar_data_maestra_en_caliente(forzar_nube = forzar_nube)
  
  if (!exists("df_hist_oficial_wide") || is.null(df_hist_oficial_wide) || nrow(df_hist_oficial_wide) == 0) return(NULL)
  
  df_sub <- df_hist_oficial_wide %>%
    filter(Semana == as.numeric(sem_num))
  
  if (nrow(df_sub) == 0) return(NULL)
  
  df_valv_ref <- df_valvulas %>%
    mutate(
      codigo_norm   = norm_codigo_valvula(codigo),
      variedad_norm = limpiar_variedad(Variedad)
    )
  
  codigos_multi <- df_valv_ref %>%
    group_by(codigo_norm) %>%
    filter(n() > 1) %>%
    pull(codigo_norm) %>%
    unique()
  
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
      across(starts_with("Kg_"), ~ round(. * prop_sub, 2))
    ) %>%
    ungroup() %>%
    select(-prop_sub)
  
  df_uni <- df_sub %>%
    filter(!codigo_norm %in% codigos_multi) %>%
    inner_join(
      df_valv_ref %>% distinct(codigo_norm, .keep_all = TRUE) %>% select(id, codigo_norm),
      by = "codigo_norm"
    )
  
  df_sub <- bind_rows(df_multi, df_uni) %>%
    mutate(
      Tipo_Registro = "CosechaOficial",
      Num_Pasada    = as.numeric(Pasada),
      Lunes         = coalesce(as.numeric(Pct_Lunes), 0),
      Martes        = coalesce(as.numeric(Pct_Martes), 0),
      Miércoles     = coalesce(as.numeric(Pct_Miércoles), 0),
      Jueves        = coalesce(as.numeric(Pct_Jueves), 0),
      Viernes       = coalesce(as.numeric(Pct_Viernes), 0),
      Sábado        = coalesce(as.numeric(Pct_Sábado), 0),
      Kg_Lunes      = round(coalesce(as.numeric(Kg_Lunes), 0), 2),
      Kg_Martes     = round(coalesce(as.numeric(Kg_Martes), 0), 2),
      Kg_Miércoles  = round(coalesce(as.numeric(Kg_Miércoles), 0), 2),
      Kg_Jueves     = round(coalesce(as.numeric(Kg_Jueves), 0), 2),
      Kg_Viernes    = round(coalesce(as.numeric(Kg_Viernes), 0), 2),
      Kg_Sábado     = round(coalesce(as.numeric(Kg_Sábado), 0), 2),
      cells_str     = generar_cells_str_vec(Lunes, Martes, Miércoles, Jueves, Viernes, Sábado)
    ) %>%
    select(id, Tipo_Registro, Num_Pasada, Lunes, Martes, Miércoles, Jueves, Viernes, Sábado,
           Kg_Lunes, Kg_Martes, Kg_Miércoles, Kg_Jueves, Kg_Viernes, Kg_Sábado, cells_str)
  
  df_sub
}

# 2. Definición oficial de Grupos de Módulos (idéntico a Control Semanal)
grupos_modulos <- list(
  "COSECHA MOD 1,2y3 / 2026"   = c(1, 2, 3),
  "COSECHA - LABORES SECTOR 8" = c(4, 5, 6, 7, 12),
  "REPORTES COSECHA S9"        = c(8, 9, 11),
  "Cosecha M10-M13-M14"        = c(10, 13, 14)
)

asignar_grupo_modulo <- function(mod_vec) {
  mod_num <- suppressWarnings(as.numeric(sub("\\..*", "", as.character(mod_vec))))
  dplyr::case_when(
    mod_num %in% c(1, 2, 3)        ~ "COSECHA MOD 1,2y3 / 2026",
    mod_num %in% c(4, 5, 6, 7, 12) ~ "COSECHA - LABORES SECTOR 8",
    mod_num %in% c(8, 9, 11)       ~ "REPORTES COSECHA S9",
    mod_num %in% c(10, 13, 14)     ~ "Cosecha M10-M13-M14",
    TRUE                           ~ "OTROS"
  )
}

# 3. Cargar catálogo normalizado de válvulas si no vino en el RData
if (!exists("df_valvulas") || is.null(df_valvulas) || nrow(df_valvulas) == 0) {
  ruta_valvs <- "D:/modulo_mapa_valvulas/valvulas_normalizadas.rds"
  if (!file.exists(ruta_valvs)) ruta_valvs <- "valvulas_normalizadas.rds"
  if (file.exists(ruta_valvs)) {
    df_valvulas <- readRDS(ruta_valvs)
  } else if (es_entorno_nube && nzchar(id_drive_data_maestra)) {
    descargar_data_maestra_drive(id_drive_data_maestra, "datos_maestros_cosecha_2026.RData")
    if (file.exists("datos_maestros_cosecha_2026.RData")) load("datos_maestros_cosecha_2026.RData")
  }
  if (!exists("df_valvulas") || is.null(df_valvulas) || nrow(df_valvulas) == 0) {
    stop("No se pudo cargar el catálogo de válvulas ni desde Google Drive ni desde respaldo local.")
  }
}

df_valvulas <- df_valvulas %>%
  filter(!grepl("^S[0-9]", codigo)) %>%
  mutate(
    id       = as.character(id),
    Módulo   = as.character(suppressWarnings(as.numeric(sub("\\..*", "", as.character(Módulo))))),
    Turno    = as.character(suppressWarnings(as.numeric(Turno))),
    Variedad = limpiar_variedad(Variedad),
    Área     = suppressWarnings(as.numeric(Área))
  )

df_valvulas <- aplicar_particiones_multivariedad(df_valvulas)

if (!"Válvula" %in% names(df_valvulas)) {
  df_valvulas <- df_valvulas %>%
    mutate(Válvula = as.character(suppressWarnings(as.numeric(sub("^.*-V0*", "", as.character(codigo))))))
} else {
  df_valvulas <- df_valvulas %>%
    mutate(Válvula = coalesce(
      as.character(suppressWarnings(as.numeric(Válvula))),
      as.character(suppressWarnings(as.numeric(sub("^.*-V0*", "", as.character(codigo)))))
    ))
}

if (!"Grupo" %in% names(df_valvulas)) {
  df_valvulas <- df_valvulas %>% mutate(Grupo = asignar_grupo_modulo(Módulo))
}

# Convertir catálogo a JSON para el motor Canvas JavaScript
json_valvulas <- toJSON(df_valvulas, dataframe = "rows", auto_unbox = TRUE)

# Listas de opciones iniciales completas para los filtros selectize del encabezado
mods_iniciales <- sort(unique(na.omit(as.character(df_valvulas$Módulo))))
mods_iniciales <- mods_iniciales[order(suppressWarnings(as.numeric(mods_iniciales)))]

turnos_iniciales <- sort(unique(na.omit(as.character(df_valvulas$Turno))))
turnos_iniciales <- turnos_iniciales[order(suppressWarnings(as.numeric(turnos_iniciales)))]

valvs_iniciales <- sort(unique(na.omit(as.character(df_valvulas$Válvula))))
valvs_iniciales <- valvs_iniciales[valvs_iniciales != ""]
valvs_iniciales <- valvs_iniciales[order(suppressWarnings(as.numeric(valvs_iniciales)))]

vars_iniciales <- sort(unique(na.omit(as.character(df_valvulas$Variedad))))
vars_iniciales <- vars_iniciales[vars_iniciales != ""]

# ==============================================================================
# TABLA MAESTRA DE USUARIOS Y PERMISOS POR MÓDULO (DESDE GOOGLE SHEETS)
# Permite agregar o editar usuarios directamente en la pestaña 'Usuarios' de Google Sheets
# ==============================================================================
parsear_modulos_vec <- function(mod_str) {
  if (is.null(mod_str) || is.na(mod_str) || !nzchar(trimws(as.character(mod_str)))) {
    return(1:14)
  }
  s <- tolower(trimws(as.character(mod_str)))
  if (s %in% c("all", "todos", "*", "1-14", "1:14")) return(1:14)
  
  # Si Google Sheets convirtió '1, 2, 3' o '1-2-3' en fecha ISO (ej. '2003-02-01T05:00:00.000Z')
  if (grepl("^\\d{4}-\\d{2}-\\d{2}t", s)) {
    yy <- suppressWarnings(as.integer(substr(s, 1, 4)) %% 100)
    mm <- suppressWarnings(as.integer(substr(s, 6, 7)))
    dd <- suppressWarnings(as.integer(substr(s, 9, 10)))
    res_dt <- sort(unique(na.omit(c(dd, mm, yy))))
    res_dt <- res_dt[res_dt >= 1 & res_dt <= 14]
    if (length(res_dt) > 0) return(res_dt)
  }
  
  partes <- unlist(strsplit(s, "[,;/\\s]+"))
  resultado <- integer(0)
  for (p in partes) {
    p <- trimws(p)
    if (!nzchar(p)) next
    if (grepl("^(\\d+)[\\-–](\\d+)$", p)) {
      m <- regmatches(p, regexec("^(\\d+)[\\-–](\\d+)$", p))[[1]]
      ini <- as.integer(m[2])
      fin <- as.integer(m[3])
      if (!is.na(ini) && !is.na(fin) && ini >= 1 && fin <= 14 && ini <= fin) {
        resultado <- c(resultado, ini:fin)
      }
    } else {
      n <- suppressWarnings(as.integer(gsub("[^0-9]", "", p)))
      if (!is.na(n) && n >= 1 && n <= 14) resultado <- c(resultado, n)
    }
  }
  resultado <- sort(unique(resultado))
  if (length(resultado) == 0) 1:14 else resultado
}

usuarios_db_default <- tibble::tribble(
  ~usuario,       ~password,   ~nombre,                          ~rol,             ~modulos,
  "cmendoza",     "76174232",  "Carlos Mendoza Calcina",         "Administrador",  1:14,
  "ksantisteban", "75182402",  "Katty Ideth Santisteban Huanila", "Sectorista",     c(1, 2, 3),
  "ldelgado",     "75210386",  "Lucy Mirella Delgado Inoñan",     "Sectorista",     c(10, 13, 14),
  "rayala",       "16770207",  "Roberto Ayala Chiroque",         "Sectorista",     c(4, 5, 6, 7, 12),
  "cuceda",       "73336004",  "Cristhian Boris Uceda Romero",    "Sectorista",     c(4, 5, 6, 7, 12)
)

# Función para consultar y descargar la lista de usuarios en tiempo real exclusivamente desde Google Sheets
obtener_usuarios_google_sheets <- function() {
  url <- paste0(url_webhook_sheets, "?action=usuarios&t=", as.integer(Sys.time()))
  tryCatch({
    res <- httr::GET(url, httr::timeout(15))
    if (httr::status_code(res) != 200) return(NULL)
    
    txt <- httr::content(res, as = "text", encoding = "UTF-8")
    if (!nzchar(txt) || txt == "[]" || txt == "null") return(NULL)
    
    raw_df <- jsonlite::fromJSON(txt)
    if (!is.data.frame(raw_df) || nrow(raw_df) == 0) return(NULL)
    
    nms <- names(raw_df)
    get_col <- function(pattern) {
      c_match <- nms[grepl(pattern, nms, ignore.case = TRUE)][1]
      if (!is.na(c_match)) raw_df[[c_match]] else NULL
    }
    
    usr_vec  <- trimws(as.character(get_col("^usuario$|^user$")))
    pwd_vec  <- trimws(as.character(get_col("^contrase[nñ]a$|^password$|^pass$")))
    nom_vec  <- trimws(as.character(get_col("^nombre$|^evaluador$")))
    rol_vec  <- trimws(as.character(get_col("^rol$|^perfil$")))
    mod_vec  <- trimws(as.character(get_col("^modulos$|^modulo$|^módulos$")))
    
    if (is.null(usr_vec) || is.null(pwd_vec)) return(NULL)
    
    filas_validas <- which(nzchar(usr_vec) & nzchar(pwd_vec))
    if (length(filas_validas) == 0) return(NULL)
    
    res_list <- vector("list", length(filas_validas))
    for (i in seq_along(filas_validas)) {
      idx <- filas_validas[i]
      u_val <- usr_vec[idx]
      p_val <- pwd_vec[idx]
      n_val <- if (!is.null(nom_vec) && nzchar(nom_vec[idx])) nom_vec[idx] else u_val
      r_val <- if (!is.null(rol_vec) && nzchar(rol_vec[idx])) rol_vec[idx] else "Sectorista"
      m_str <- if (!is.null(mod_vec)) mod_vec[idx] else "1-14"
      
      res_list[[i]] <- tibble(
        usuario  = u_val,
        password = p_val,
        nombre   = n_val,
        rol      = r_val,
        modulos  = list(parsear_modulos_vec(m_str))
      )
    }
    bind_rows(res_list)
  }, error = function(e) {
    NULL
  })
}

usuarios_db <- obtener_usuarios_google_sheets()

generar_opciones_usuarios <- function(df_usr) {
  if (is.null(df_usr) || nrow(df_usr) == 0) return(character(0))
  nombres <- trimws(as.character(df_usr[['nombre']]))
  usuarios <- as.character(df_usr[['usuario']])
  setNames(usuarios, nombres)
}

# Colores oficiales del Plano de Cosecha
colores_dias <- c(
  "Lunes"     = "#FF0000", # Rojo puro
  "Martes"    = "#00A2E8", # Celeste / Sky Blue
  "Miércoles" = "#FF7F27", # Naranja
  "Jueves"    = "#FFFF00", # Amarillo
  "Viernes"   = "#00C853", # Verde
  "Sábado"    = "#A349A4"  # Púrpura / Morado
)

# ==============================================================================
# FUNCIÓN OFICIAL: CARGAR PROYECCIÓN DESDE EXCEL (HOJA CÁLCULO, SKIP = 3)
# Exactamente la misma lógica de Control Semanal de Cosecha
# ==============================================================================
cargar_proyeccion <- function(ruta_raiz, campana = 2026, semana) {
  cat("\n=======================================================\n")
  cat("[MAPA - DIAGNÓSTICO PROYECCIÓN] Iniciando búsqueda...\n")
  cat("  -> Campaña solicitada:", campana, "| Semana solicitada:", semana, "\n")
  
  vacio <- tibble(Módulo = character(), Turno = character(), Variedad = character(), Kg_Proy_Turno = numeric())
  
  # 1. Búsqueda instantánea en datos maestros consolidados (RData)
  if (exists("df_proyecciones") && !is.null(df_proyecciones) && nrow(df_proyecciones) > 0) {
    df_sub <- df_proyecciones %>%
      filter(Semana == as.numeric(semana))
    if (nrow(df_sub) > 0) {
      cat(sprintf("  -> [MAPA PROYECCIÓN RDATA] Semana %s cargada instantáneamente (%d registros)\n", semana, nrow(df_sub)))
      cat("=======================================================\n\n")
      return(df_sub %>%
               group_by(Módulo, Turno, Variedad) %>%
               summarise(
                 Kg_Proy_Turno = sum(Proyección, na.rm = TRUE),
                 .groups       = "drop"
               ))
    }
  }
  
  # 2. Respaldo por lectura de Excel en Google Drive
  tryCatch({
    nums <- regmatches(as.character(campana), gregexpr("[0-9]+", as.character(campana)))[[1]]
    if (length(nums) == 0) return(vacio)
    y1 <- as.numeric(nums[1]) %% 100
    
    carpetas <- list.dirs(ruta_raiz, recursive = FALSE, full.names = TRUE)
    patron_campana <- sprintf("Campa[nñ]a\\s*0*%02d\\s*[-/]", y1)
    carpeta_campana <- carpetas[grepl(patron_campana, basename(carpetas), ignore.case = TRUE)][1]
    
    if (is.na(carpeta_campana)) {
      patron_campana_alt <- sprintf("Campa[nñ]a.*%02d", y1)
      carpeta_campana <- carpetas[grepl(patron_campana_alt, basename(carpetas), ignore.case = TRUE)][1]
    }
    
    if (is.na(carpeta_campana) || !dir.exists(carpeta_campana)) return(vacio)
    
    dirs_sub1 <- list.dirs(carpeta_campana, recursive = FALSE, full.names = TRUE)
    dir_proy <- dirs_sub1[grepl("Proyecci[oó]n.*Semanal", basename(dirs_sub1), ignore.case = TRUE)][1]
    if (is.na(dir_proy)) return(vacio)
    
    dirs_sub2 <- list.dirs(dir_proy, recursive = FALSE, full.names = TRUE)
    dir_detallada <- dirs_sub2[grepl("Detallada", basename(dirs_sub2), ignore.case = TRUE)][1]
    if (is.na(dir_detallada)) return(vacio)
    
    archivos <- list.files(dir_detallada, pattern = "\\.(xlsx|xls)$", full.names = TRUE)
    archivos <- archivos[!grepl("^~\\$", basename(archivos))]
    
    patron_semana <- sprintf("Semana\\s*0*%s(\\b|[^0-9])", semana)
    archivos_semana <- archivos[grepl(patron_semana, basename(archivos), ignore.case = TRUE)]
    if (length(archivos_semana) == 0) {
      cat("  [AVISO] No se encontró archivo de proyección para la Semana", semana, "\n")
      cat("=======================================================\n\n")
      return(vacio)
    }
    
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
    
    # Homologar Módulo (ej. 9.1/9.2 -> 9, 11.1/11.2 -> 11, 13.1/13.2 -> 13, 14.1/14.2 -> 14)
    # para cruzar exactamente con el catálogo de válvulas del plano 2D
    df_limpio <- raw_proy %>%
      filter(!is.na(.data[[col_s]]), !is.na(.data[[col_t]])) %>%
      mutate(
        Mod_raw  = trimws(as.character(.data[[col_m]])),
        Módulo   = as.character(suppressWarnings(as.numeric(sub("\\..*", "", Mod_raw)))),
        Turno    = as.character(suppressWarnings(as.numeric(trimws(as.character(.data[[col_t]]))))),
        Variedad = limpiar_variedad(.data[[col_v]]),
        proy_num = suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(.data[[col_p]]))))
      ) %>%
      filter(!is.na(Módulo), !is.na(Turno), !is.na(proy_num), proy_num > 0) %>%
      group_by(Módulo, Turno, Variedad) %>%
      summarise(
        Kg_Proy_Turno = sum(proy_num, na.rm = TRUE),
        .groups       = "drop"
      )
    
    cat(sprintf("  -> Éxito Proyección: %d turnos cargados (Total: %s Kg)\n",
                nrow(df_limpio), format(round(sum(df_limpio$Kg_Proy_Turno, na.rm = TRUE)), big.mark = ",")))
    cat("=======================================================\n\n")
    return(df_limpio)
  }, error = function(e) {
    cat("[ERROR PROYECCIÓN MAPA]:", e$message, "\n")
    return(vacio)
  })
}

# ==============================================================================
# FUNCIONES DE PERSISTENCIA EN DISCO Y GOOGLE SHEETS (PROYECCIÓN Y COSECHA + KILOS)
# ==============================================================================
esquema_vacio_rutas <- function() {
  tibble(
    Campaña             = character(),
    Semana              = numeric(),
    Tipo_Registro       = character(), # "Proyeccion" o "CosechaCampo"
    id                  = character(),
    codigo              = character(),
    Grupo               = character(),
    Sector              = character(),
    Módulo              = character(),
    Turno               = character(),
    Variedad            = character(),
    Área                = numeric(),
    Num_Pasada          = numeric(),
    Lunes               = numeric(),
    Martes              = numeric(),
    Miércoles           = numeric(),
    Jueves              = numeric(),
    Viernes             = numeric(),
    Sábado              = numeric(),
    Ha_Lunes            = numeric(),
    Ha_Martes           = numeric(),
    Ha_Miércoles        = numeric(),
    Ha_Jueves           = numeric(),
    Ha_Viernes          = numeric(),
    Ha_Sábado           = numeric(),
    Ha_Total_Prog       = numeric(),
    Kg_Lunes            = numeric(),
    Kg_Martes           = numeric(),
    Kg_Miércoles        = numeric(),
    Kg_Jueves           = numeric(),
    Kg_Viernes          = numeric(),
    Kg_Sábado           = numeric(),
    Kg_Total_Prog       = numeric(),
    cells_str           = character(),
    Fecha_Actualizacion = as.POSIXct(character())
  )
}

# Función para consultar y descargar rutas vigentes desde Google Sheets (GET)
obtener_rutas_google_sheets <- function(sem_num = NULL) {
  url <- url_webhook_sheets
  if (!is.null(sem_num) && !is.na(sem_num)) {
    url <- sprintf("%s?campana=2026&semana=%s", url, as.character(as.numeric(sem_num)))
  }
  
  tryCatch({
    res <- httr::GET(url, httr::timeout(20))
    if (httr::status_code(res) != 200) return(NULL)
    
    txt <- httr::content(res, as = "text", encoding = "UTF-8")
    if (!nzchar(txt) || txt == "[]" || txt == "null") return(esquema_vacio_rutas())
    
    raw_df <- jsonlite::fromJSON(txt)
    if (!is.data.frame(raw_df) || nrow(raw_df) == 0) return(esquema_vacio_rutas())
    
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
    
    if (nrow(df_clean) == 0) return(esquema_vacio_rutas())
    
    df_valv_map <- df_valvulas %>%
      mutate(
        codigo_norm   = norm_codigo_valvula(codigo),
        variedad_norm = limpiar_variedad(Variedad),
        Area_Round    = round(Área, 2)
      ) %>%
      select(id, codigo, Grupo, Sector, Módulo, Turno, Variedad, Área, Area_Round, codigo_norm, variedad_norm)
    
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
    
    df_res <- bind_rows(df_res_multi, df_res_uni) %>%
      mutate(
        Ha_Lunes            = round(Área * (Lunes / 100), 2),
        Ha_Martes           = round(Área * (Martes / 100), 2),
        Ha_Miércoles        = round(Área * (Miércoles / 100), 2),
        Ha_Jueves           = round(Área * (Jueves / 100), 2),
        Ha_Viernes          = round(Área * (Viernes / 100), 2),
        Ha_Sábado           = round(Área * (Sábado / 100), 2),
        Ha_Total_Prog       = round(Ha_Lunes + Ha_Martes + Ha_Miércoles + Ha_Jueves + Ha_Viernes + Ha_Sábado, 2),
        Kg_Total_Prog       = round(Kg_Lunes + Kg_Martes + Kg_Miércoles + Kg_Jueves + Kg_Viernes + Kg_Sábado, 2),
        cells_str           = generar_cells_str_vec(Lunes, Martes, Miércoles, Jueves, Viernes, Sábado),
        Fecha_Actualizacion = Sys.time()
      ) %>%
      select(
        Campaña, Semana, Tipo_Registro, id, codigo, Grupo, Sector, Módulo, Turno, Variedad, Área, Num_Pasada,
        Lunes, Martes, Miércoles, Jueves, Viernes, Sábado,
        Ha_Lunes, Ha_Martes, Ha_Miércoles, Ha_Jueves, Ha_Viernes, Ha_Sábado, Ha_Total_Prog,
        Kg_Lunes, Kg_Martes, Kg_Miércoles, Kg_Jueves, Kg_Viernes, Kg_Sábado, Kg_Total_Prog,
        cells_str, Fecha_Actualizacion
      )
    
    if (nrow(df_res) == 0) return(esquema_vacio_rutas())
    df_res
  }, error = function(e) {
    cat("[AVISO GET SHEETS]:", e$message, "\n")
    NULL
  })
}

cargar_rutas_disco <- function() {
  df <- NULL
  if (file.exists(ruta_rutas_principal)) {
    tryCatch({ df <- readRDS(ruta_rutas_principal) }, error = function(e) NULL)
  }
  if (is.null(df) && file.exists(ruta_rutas_local)) {
    tryCatch({ df <- readRDS(ruta_rutas_local) }, error = function(e) NULL)
  }
  if (is.null(df) && file.exists("rutas_cosecha_mapa.rds")) {
    tryCatch({ df <- readRDS("rutas_cosecha_mapa.rds") }, error = function(e) NULL)
  }
  if (is.null(df) || nrow(df) == 0) return(esquema_vacio_rutas())
  
  if (!"Tipo_Registro" %in% names(df)) df$Tipo_Registro <- "Proyeccion"
  df$Tipo_Registro <- as.character(ifelse(df$Tipo_Registro == "Cosecha", "CosechaCampo", df$Tipo_Registro))
  for (col_kg in c("Kg_Lunes", "Kg_Martes", "Kg_Miércoles", "Kg_Jueves", "Kg_Viernes", "Kg_Sábado", "Kg_Total_Prog")) {
    if (!col_kg %in% names(df)) df[[col_kg]] <- 0
  }
  
  df %>%
    mutate(
      Campaña       = as.character(Campaña),
      Semana        = suppressWarnings(as.numeric(Semana)),
      Tipo_Registro = coalesce(as.character(Tipo_Registro), "Proyeccion"),
      id            = as.character(id),
      Num_Pasada    = suppressWarnings(as.numeric(Num_Pasada)),
      cells_str     = coalesce(as.character(cells_str), ""),
      across(c(Kg_Lunes, Kg_Martes, Kg_Miércoles, Kg_Jueves, Kg_Viernes, Kg_Sábado, Kg_Total_Prog), ~ coalesce(as.numeric(.), 0))
    )
}

guardar_rutas_disco <- function(df_rutas) {
  guardado <- FALSE
  # 1. Intentar en Google Drive compartido (G:)
  tryCatch({
    dir_p <- dirname(ruta_rutas_principal)
    if (dir.exists(dir_p)) {
      saveRDS(df_rutas, ruta_rutas_principal)
      guardado <- TRUE
    }
  }, error = function(e) NULL)
  
  # 2. Intentar en carpeta local Windows fija D:
  tryCatch({
    dir_l <- dirname(ruta_rutas_local)
    if (dir.exists(dir_l)) {
      saveRDS(df_rutas, ruta_rutas_local)
      guardado <- TRUE
    }
  }, error = function(e) NULL)
  
  # 3. Guardar SIEMPRE en directorio de trabajo local (esencial para shinyapps.io y respaldos)
  tryCatch({
    saveRDS(df_rutas, "rutas_cosecha_mapa.rds")
    guardado <- TRUE
  }, error = function(e) NULL)
  
  guardado
}


# ==============================================================================
# UI - ESTILO UNIFICADO iOS / APPLE HIG
# ==============================================================================

semana_iso_actual <- lubridate::isoweek(Sys.Date())
opciones_semanas  <- setNames(1:53, sprintf("Semana %02d%s", 1:53, ifelse(1:53 == semana_iso_actual, " (Actual)", "")))

# Día por defecto: Día actual - 1 (ej: Miércoles -> Martes, Lunes -> Lunes)
dia_semana_hoy <- as.POSIXlt(Sys.Date())$wday
dia_defecto_ui <- switch(as.character(dia_semana_hoy),
  "2" = "lunes",
  "3" = "martes",
  "4" = "miercoles",
  "5" = "jueves",
  "6" = "viernes",
  "0" = "sabado",
  "1" = "lunes",
  "lunes"
)

ui <- fluidPage(
  tags$head(
    tags$meta(charset = "utf-8"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no"),
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap');
      
      :root {
        --ios-bg: #F2F2F7;
        --ios-card: #FFFFFF;
        --ios-primary: #007AFF;
        --ios-text: #1C1C1E;
        --ios-secondary: #8E8E93;
        --ios-border: rgba(0, 0, 0, 0.08);
      }

      html {
        overflow-y: scroll;
      }

      body {
        background-color: var(--ios-bg);
        font-family: -apple-system, BlinkMacSystemFont, 'Inter', 'SF Pro Text', sans-serif;
        color: var(--ios-text);
        -webkit-font-smoothing: antialiased;
        padding-bottom: 50px;
        padding-right: 0 !important;
        user-select: none;
        -webkit-user-select: none;
      }
      
      body.modal-open {
        padding-right: 0 !important;
        overflow-y: scroll !important;
      }
      
      .ios-header {
        position: relative;
        z-index: 1050;
        background: rgba(255, 255, 255, 0.94);
        backdrop-filter: blur(20px);
        -webkit-backdrop-filter: blur(20px);
        border-bottom: 1px solid var(--ios-border);
        padding: 16px 28px 18px 28px;
        margin-bottom: 18px;
        border-radius: 0 0 20px 20px;
        display: flex;
        flex-direction: column;
        gap: 12px;
      }
      
      .ios-header-top-row {
        display: flex;
        justify-content: space-between;
        align-items: flex-start;
        gap: 16px;
        width: 100%;
      }
      
      .ios-header-title-box {
        min-width: 0;
        flex: 1 1 auto;
      }
      
      .ios-header-user-corner {
        flex: 0 0 auto;
        display: flex;
        align-items: center;
        justify-content: flex-end;
      }
      
      .ios-header-bottom-row {
        display: flex;
        justify-content: space-between;
        align-items: center;
        gap: 14px;
        width: 100%;
        flex-wrap: wrap;
      }
      
      .ios-header-status-box {
        display: flex;
        align-items: center;
        gap: 10px;
        min-width: 0;
      }
      
      .ios-header-actions-box {
        display: flex;
        align-items: center;
        gap: 10px;
        flex-wrap: wrap;
        justify-content: flex-end;
      }
      
      .ios-header-actions-box .form-group {
        margin-bottom: 0 !important;
      }
      
      @media (max-width: 1180px) {
        .ios-header-top-row, .ios-header-bottom-row {
          flex-wrap: wrap;
        }
      }
      
      .ios-header .form-group {
        margin-bottom: 0 !important;
      }
      
      .ios-title {
        font-size: 20px;
        font-weight: 700;
        letter-spacing: -0.5px;
        margin: 0;
        color: var(--ios-text);
      }
      
      .ios-subtitle {
        font-size: 12.5px;
        color: var(--ios-secondary);
        margin-top: 2px;
        margin-bottom: 0;
      }
      
      .ios-card {
        position: relative;
        z-index: 10;
        background: var(--ios-card);
        border-radius: 16px;
        padding: 18px 22px;
        border: 1px solid rgba(0, 0, 0, 0.06);
        box-shadow: 0 4px 18px -2px rgba(0, 0, 0, 0.03);
        margin-bottom: 16px;
      }
      
      .filters-grid {
        display: grid;
        grid-template-columns: 1.45fr 0.95fr 0.95fr 0.95fr 1.15fr 135px;
        gap: 12px;
        align-items: start;
      }
      
      .filters-grid .form-group {
        margin-bottom: 0 !important;
      }
      
      @media (max-width: 1180px) {
        .filters-grid {
          grid-template-columns: repeat(auto-fit, minmax(170px, 1fr));
        }
      }
      
      /* Estilos Apple HIG para Menús y Selectize */
      .selectize-input {
        border-radius: 10px !important;
        border: 1px solid rgba(0, 0, 0, 0.12) !important;
        background: #FFFFFF !important;
        box-shadow: 0 1px 2px rgba(0, 0, 0, 0.04) !important;
        font-family: inherit !important;
        font-size: 13px !important;
        font-weight: 600 !important;
        color: #1C1C1E !important;
        padding: 8px 12px !important;
        transition: all 0.18s ease !important;
      }
      .selectize-input.focus {
        border-color: #007AFF !important;
        box-shadow: 0 0 0 3px rgba(0, 122, 255, 0.22) !important;
      }
      .ios-card-filtros {
        position: relative !important;
        z-index: 500 !important;
        overflow: visible !important;
      }
      .selectize-dropdown {
        z-index: 9999999 !important;
        border-radius: 14px !important;
        border: 1px solid rgba(0, 0, 0, 0.08) !important;
        background: rgba(255, 255, 255, 0.98) !important;
        backdrop-filter: blur(25px) !important;
        -webkit-backdrop-filter: blur(25px) !important;
        box-shadow: 0 16px 36px -4px rgba(0, 0, 0, 0.18), 0 0 0 1px rgba(0, 0, 0, 0.04) !important;
        padding: 6px !important;
        margin-top: 6px !important;
      }
      .selectize-dropdown .selectize-dropdown-content {
        max-height: 280px !important;
        padding: 2px !important;
      }
      .selectize-dropdown .option {
        border-radius: 8px !important;
        padding: 8px 12px !important;
        font-size: 13px !important;
        font-weight: 500 !important;
        color: #1C1C1E !important;
        transition: background 0.12s ease, color 0.12s ease !important;
        cursor: pointer !important;
      }
      .selectize-dropdown .option:hover,
      .selectize-dropdown .option.active {
        background: #007AFF !important;
        color: #FFFFFF !important;
        font-weight: 600 !important;
      }
      .selectize-dropdown .option.selected {
        background: rgba(0, 122, 255, 0.12) !important;
        color: #007AFF !important;
        font-weight: 700 !important;
      }
      .selectize-dropdown .option.selected.active,
      .selectize-dropdown .option.selected:hover {
        background: #007AFF !important;
        color: #FFFFFF !important;
      }
      
      /* Apple iOS HIG Modal Loader & Activity Indicator */
      .ios-loader-backdrop {
        position: fixed;
        top: 0;
        left: 0;
        width: 100vw;
        height: 100vh;
        background: rgba(15, 23, 42, 0.28);
        backdrop-filter: blur(12px);
        -webkit-backdrop-filter: blur(12px);
        display: flex;
        align-items: center;
        justify-content: center;
        z-index: 99999999;
        opacity: 0;
        visibility: hidden;
        pointer-events: none;
        transition: opacity 0.22s cubic-bezier(0.16, 1, 0.3, 1), visibility 0.22s;
      }
      .ios-loader-backdrop.active {
        opacity: 1;
        visibility: visible;
        pointer-events: auto;
      }
      .ios-loader-card {
        background: rgba(255, 255, 255, 0.94);
        backdrop-filter: blur(30px);
        -webkit-backdrop-filter: blur(30px);
        border-radius: 22px;
        padding: 26px 36px;
        box-shadow: 0 24px 50px -10px rgba(0, 0, 0, 0.22), 0 0 0 1px rgba(0, 0, 0, 0.06);
        display: flex;
        flex-direction: column;
        align-items: center;
        justify-content: center;
        min-width: 240px;
        max-width: 380px;
        text-align: center;
        transform: scale(0.92);
        transition: transform 0.22s cubic-bezier(0.16, 1, 0.3, 1);
      }
      .ios-loader-backdrop.active .ios-loader-card {
        transform: scale(1);
      }
      
      /* Cupertino 12-Blade Activity Spinner */
      .ios-spinner {
        position: relative;
        width: 38px;
        height: 38px;
        display: inline-block;
      }
      .ios-spinner div {
        position: absolute;
        left: 44.5%;
        top: 37%;
        width: 9%;
        height: 26%;
        background: #007AFF;
        border-radius: 50px;
        opacity: 0;
        animation: ios-spinner-fade 1.1s linear infinite;
      }
      .ios-spinner div:nth-child(1)  { transform: rotate(0deg) translate(0, -140%); animation-delay: 0s; }
      .ios-spinner div:nth-child(2)  { transform: rotate(30deg) translate(0, -140%); animation-delay: -1.008s; }
      .ios-spinner div:nth-child(3)  { transform: rotate(60deg) translate(0, -140%); animation-delay: -0.916s; }
      .ios-spinner div:nth-child(4)  { transform: rotate(90deg) translate(0, -140%); animation-delay: -0.825s; }
      .ios-spinner div:nth-child(5)  { transform: rotate(120deg) translate(0, -140%); animation-delay: -0.733s; }
      .ios-spinner div:nth-child(6)  { transform: rotate(150deg) translate(0, -140%); animation-delay: -0.641s; }
      .ios-spinner div:nth-child(7)  { transform: rotate(180deg) translate(0, -140%); animation-delay: -0.550s; }
      .ios-spinner div:nth-child(8)  { transform: rotate(210deg) translate(0, -140%); animation-delay: -0.458s; }
      .ios-spinner div:nth-child(9)  { transform: rotate(240deg) translate(0, -140%); animation-delay: -0.366s; }
      .ios-spinner div:nth-child(10) { transform: rotate(270deg) translate(0, -140%); animation-delay: -0.275s; }
      .ios-spinner div:nth-child(11) { transform: rotate(300deg) translate(0, -140%); animation-delay: -0.183s; }
      .ios-spinner div:nth-child(12) { transform: rotate(330deg) translate(0, -140%); animation-delay: -0.091s; }
      @keyframes ios-spinner-fade {
        0% { opacity: 1; }
        100% { opacity: 0.15; }
      }
      
      .ios-loader-title {
        font-size: 15px;
        font-weight: 700;
        color: #1C1C1E;
        letter-spacing: -0.3px;
        margin-top: 14px;
      }
      .ios-loader-subtitle {
        font-size: 12px;
        font-weight: 500;
        color: #8E8E93;
        margin-top: 4px;
        letter-spacing: -0.1px;
      }
      
      /* Apple iOS HIG Login Overlay */
      .login-backdrop {
        position: fixed;
        top: 0;
        left: 0;
        right: 0;
        bottom: 0;
        width: 100%;
        height: 100%;
        z-index: 999999999;
        background: rgba(242, 242, 247, 0.88);
        backdrop-filter: blur(25px);
        -webkit-backdrop-filter: blur(25px);
        display: flex;
        align-items: center;
        justify-content: center;
        padding: 20px;
        overflow-y: auto;
        box-sizing: border-box;
        transition: opacity 0.3s cubic-bezier(0.16, 1, 0.3, 1), visibility 0.3s;
      }
      
      .login-card {
        background: #FFFFFF;
        width: 100%;
        max-width: 420px;
        min-height: 450px;
        border-radius: 24px;
        padding: 34px 28px;
        box-shadow: 0 20px 48px -10px rgba(0, 0, 0, 0.16), 0 0 0 1px rgba(0, 0, 0, 0.04);
        display: flex;
        flex-direction: column;
        align-items: center;
        text-align: center;
        transform: scale(1);
        transition: transform 0.25s cubic-bezier(0.16, 1, 0.3, 1);
        box-sizing: border-box;
        position: relative;
      }
      
      .login-logo-container {
        width: 100%;
        display: flex;
        align-items: center;
        justify-content: center;
        margin-bottom: 18px;
        padding: 4px 0;
      }
      
      .login-logo-img {
        max-width: 185px;
        height: auto;
        max-height: 85px;
        object-fit: contain;
        filter: drop-shadow(0 2px 8px rgba(0, 0, 0, 0.05));
        transition: transform 0.2s cubic-bezier(0.16, 1, 0.3, 1);
        user-select: none;
        -webkit-user-drag: none;
      }
      
      .login-logo-img:hover {
        transform: scale(1.02);
      }
      
      .login-title {
        font-size: 20px;
        font-weight: 700;
        color: #1C1C1E;
        margin: 0;
        letter-spacing: -0.4px;
      }
      
      .login-subtitle {
        font-size: 13px;
        color: #8E8E93;
        margin-top: 5px;
        margin-bottom: 0;
      }
      
      .filtro-bloqueado .remove, .filtro-bloqueado .selectize-input a.remove { display: none !important; }
      .filtro-bloqueado .selectize-input { background: #F2F2F7 !important; cursor: not-allowed !important; opacity: 0.85; }
      
      .login-user-group {
        position: relative !important;
        z-index: 1000 !important;
      }
      .login-password-group {
        position: relative !important;
        z-index: 10 !important;
      }
      
      .login-card input.form-control {
        width: 100% !important;
        height: 46px !important;
        border-radius: 12px !important;
        border: 1px solid rgba(0, 0, 0, 0.12) !important;
        background-color: #F9F9FB !important;
        padding: 0 14px !important;
        font-size: 14px !important;
        font-weight: 500 !important;
        color: #1C1C1E !important;
        box-shadow: none !important;
        outline: none !important;
        box-sizing: border-box !important;
        transition: border-color 0.15s ease, box-shadow 0.15s ease, background-color 0.15s ease !important;
      }
      .login-card input.form-control:focus {
        background-color: #FFFFFF !important;
        border-color: #007AFF !important;
        box-shadow: 0 0 0 3px rgba(0, 122, 255, 0.18) !important;
      }
      
      .login-card .selectize-control {
        width: 100% !important;
        margin-bottom: 0 !important;
      }
      .login-card .selectize-input {
        width: 100% !important;
        min-height: 46px !important;
        height: 46px !important;
        border-radius: 12px !important;
        border: 1px solid rgba(0, 0, 0, 0.12) !important;
        background-color: #F9F9FB !important;
        padding: 10px 36px 10px 14px !important;
        font-size: 14px !important;
        font-weight: 500 !important;
        color: #1C1C1E !important;
        box-shadow: none !important;
        outline: none !important;
        box-sizing: border-box !important;
        display: flex !important;
        align-items: center !important;
        background-image: url('data:image/svg+xml,%3Csvg xmlns=%22http://www.w3.org/2000/svg%22 width=%2212%22 height=%2212%22 viewBox=%220 0 24 24%22 fill=%22none%22 stroke=%22%238E8E93%22 stroke-width=%222.5%22 stroke-linecap=%22round%22 stroke-linejoin=%22round%22%3E%3Cpolyline points=%226 9 12 15 18 9%22%3E%3C/polyline%3E%3C/svg%3E') !important;
        background-repeat: no-repeat !important;
        background-position: right 14px center !important;
        cursor: text !important;
        transition: border-color 0.15s ease, box-shadow 0.15s ease, background-color 0.15s ease !important;
      }
      .login-card .selectize-input.focus {
        background-color: #FFFFFF !important;
        border-color: #007AFF !important;
        box-shadow: 0 0 0 3px rgba(0, 122, 255, 0.18) !important;
      }
      .login-card .selectize-input input {
        font-size: 14px !important;
        font-weight: 500 !important;
        color: #1C1C1E !important;
      }
      .login-card .selectize-dropdown,
      .login-card .selectize-dropdown.form-control {
        position: absolute !important;
        top: 100% !important;
        left: 0 !important;
        right: 0 !important;
        width: 100% !important;
        height: auto !important;
        min-height: unset !important;
        max-height: none !important;
        background: #FFFFFF !important;
        border-radius: 14px !important;
        box-shadow: 0 18px 40px rgba(0, 0, 0, 0.22), 0 0 0 1px rgba(0, 0, 0, 0.08) !important;
        border: 1px solid rgba(0, 0, 0, 0.10) !important;
        z-index: 999999 !important;
        margin-top: 4px !important;
        padding: 6px !important;
        box-sizing: border-box !important;
        text-align: left !important;
        overflow: hidden !important;
      }
      .login-card .selectize-dropdown .selectize-dropdown-content {
        max-height: 180px !important;
        overflow-y: auto !important;
        overflow-x: hidden !important;
        padding: 2px !important;
      }
      .login-card .selectize-dropdown .option {
        padding: 9px 12px !important;
        border-radius: 8px !important;
        font-size: 13.5px !important;
        font-weight: 500 !important;
        color: #1C1C1E !important;
        cursor: pointer !important;
        transition: background 0.12s ease, color 0.12s ease !important;
      }
      .login-card .selectize-dropdown .option:hover,
      .login-card .selectize-dropdown .option.active {
        background: rgba(0, 122, 255, 0.10) !important;
        color: #007AFF !important;
        font-weight: 600 !important;
      }
      .login-card .selectize-dropdown .create {
        display: none !important;
      }
      .login-card .form-group {
        margin-bottom: 0 !important;
      }
      
      .btn-login-apple {
        position: relative !important;
        z-index: 5 !important;
        width: 100%;
        height: 46px;
        border-radius: 12px;
        background: #007AFF;
        color: #FFFFFF;
        font-size: 14.5px;
        font-weight: 600;
        border: none;
        cursor: pointer;
        margin-top: 22px;
        box-shadow: 0 4px 14px rgba(0, 122, 255, 0.3);
        transition: all 0.15s ease;
      }
      .btn-login-apple:hover {
        background: #0062CC;
        transform: translateY(-1px);
      }
      .btn-login-apple:active {
        transform: translateY(0);
      }
      
      .login-helper-box {
        margin-top: 22px;
        padding: 12px;
        background: rgba(142, 142, 147, 0.08);
        border-radius: 14px;
        width: 100%;
      }
      
      .login-pill-badge {
        display: inline-block;
        background: #FFFFFF;
        border: 1px solid rgba(0, 0, 0, 0.08);
        border-radius: 8px;
        padding: 4px 8px;
        font-size: 11px;
        font-weight: 600;
        color: #1C1C1E;
        cursor: pointer;
        transition: all 0.15s ease;
      }
      .login-pill-badge:hover {
        background: #007AFF;
        color: #FFFFFF;
        border-color: #007AFF;
      }
      
      /* Switches Segmentados Estilo iOS */
      .ios-segmented-pass {
        display: inline-flex;
        background: #E5E5EA;
        padding: 3px;
        border-radius: 10px;
        gap: 3px;
      }
      .pass-pill {
        border: none;
        background: transparent;
        padding: 6px 14px;
        border-radius: 8px;
        font-size: 12.5px;
        font-weight: 700;
        color: #636366;
        cursor: pointer;
        transition: all 0.18s ease;
      }
      .pass-pill.active {
        background: #FFFFFF;
        color: #007AFF;
        box-shadow: 0 2px 6px rgba(0, 0, 0, 0.12);
      }
      .pass-pill.active-eraser {
        background: #FFFFFF;
        color: #D70015;
        box-shadow: 0 2px 6px rgba(215, 0, 21, 0.16);
      }
      .pass-pill.active-pan {
        background: #FFFFFF;
        color: #3A3A3C;
        box-shadow: 0 2px 6px rgba(0, 0, 0, 0.12);
      }
      .pass-pill.active-cosecha {
        background: #FFFFFF;
        color: #16A34A;
        box-shadow: 0 2px 6px rgba(0, 0, 0, 0.12);
      }
      .pass-pill.active-oficial {
        background: #0F172A;
        color: #38BDF8;
        box-shadow: 0 2px 6px rgba(0, 0, 0, 0.22);
      }
      .pass-pill-danger {
        color: #D70015 !important;
      }
      .pass-pill-danger:hover {
        background: rgba(255, 59, 48, 0.14) !important;
      }
      .pass-pill:disabled {
        opacity: 0.42;
        cursor: not-allowed;
      }
      .lock-badge-floating {
        position: absolute;
        top: 14px;
        left: 50%;
        transform: translateX(-50%);
        background: rgba(15, 23, 42, 0.92);
        color: #38BDF8;
        padding: 6px 16px;
        border-radius: 20px;
        font-size: 12px;
        font-weight: 700;
        letter-spacing: 0.3px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.25);
        display: none;
        align-items: center;
        gap: 8px;
        z-index: 100;
        pointer-events: none;
        backdrop-filter: blur(8px);
        border: 1px solid rgba(56, 189, 248, 0.35);
      }
      
      /* Tarjetas KPI y Tarjetas de Día Seleccionables con Input de Kilos */
      .kpi-container {
        display: grid;
        grid-template-columns: repeat(9, minmax(0, 1fr));
        gap: 10px;
        margin-bottom: 16px;
      }
      
      @media (max-width: 1180px) {
        .kpi-container {
          grid-template-columns: repeat(auto-fit, minmax(145px, 1fr));
        }
      }
      
      .kpi-card {
        background: #FFFFFF;
        border-radius: 14px;
        padding: 12px 12px;
        min-width: 0;
        border: 1.5px solid rgba(0, 0, 0, 0.05);
        box-shadow: 0 2px 6px rgba(0, 0, 0, 0.02);
        position: relative;
        transition: all 0.18s cubic-bezier(0.25, 0.8, 0.25, 1);
      }
      
      .kpi-card.day-card {
        cursor: pointer;
      }
      
      .kpi-card.day-card:hover {
        transform: translateY(-2px);
        box-shadow: 0 6px 14px rgba(0, 0, 0, 0.06);
      }
      
      .kpi-card.day-card.active {
        border-color: var(--day-border, #1C1C1E) !important;
        background: var(--day-bg, #FFFFFF) !important;
        box-shadow: 0 8px 20px -4px var(--day-shadow, rgba(0,0,0,0.14)), 0 0 0 2px var(--day-border, #1C1C1E);
        transform: translateY(-3px);
      }
      
      .day-active-pill {
        display: none;
        position: absolute;
        top: 10px;
        right: 10px;
        font-size: 9.5px;
        font-weight: 800;
        padding: 2px 6px;
        border-radius: 20px;
        letter-spacing: 0.3px;
        text-transform: uppercase;
        background: #1C1C1E;
        color: #FFFFFF;
      }
      
      .kpi-card.day-card.active .day-active-pill {
        display: inline-block;
      }
      
      .kpi-label {
        font-size: 11px;
        font-weight: 600;
        text-transform: uppercase;
        letter-spacing: 0.4px;
        color: #8E8E93;
        margin-bottom: 3px;
      }
      
      .kpi-value {
        font-size: 20px;
        font-weight: 700;
        letter-spacing: -0.5px;
        color: #1C1C1E;
        font-variant-numeric: tabular-nums;
      }
      
      .kpi-badge {
        display: inline-block;
        font-size: 10px;
        font-weight: 600;
        padding: 2px 6px;
        border-radius: 6px;
        margin-top: 2px;
      }
      
      /* Input de Kilos dentro de cada Card de Día */
      .day-kg-box {
        margin-top: 8px;
        padding-top: 7px;
        border-top: 1px dashed rgba(0, 0, 0, 0.10);
      }
      .day-kg-label {
        font-size: 9.5px;
        font-weight: 700;
        color: #636366;
        display: block;
        margin-bottom: 3px;
      }
      .day-kg-input-wrap {
        position: relative;
        display: flex;
        align-items: center;
      }
      .day-kg-input {
        width: 100%;
        height: 30px !important;
        border-radius: 8px !important;
        border: 1px solid rgba(0, 0, 0, 0.15) !important;
        background: #FFFFFF !important;
        font-size: 12.5px !important;
        font-weight: 700 !important;
        color: #1C1C1E !important;
        padding: 3px 28px 3px 8px !important;
        font-variant-numeric: tabular-nums;
        box-shadow: inset 0 1px 2px rgba(0,0,0,0.03) !important;
      }
      .day-kg-input:focus {
        outline: none !important;
        border-color: #007AFF !important;
        box-shadow: 0 0 0 2px rgba(0, 122, 255, 0.18) !important;
      }
      .day-kg-unit {
        position: absolute;
        right: 8px;
        font-size: 10.5px;
        font-weight: 700;
        color: #8E8E93;
        pointer-events: none;
      }
      
      /* Controles iOS Form */
      select.form-control, input.form-control, .selectize-input {
        border-radius: 10px !important;
        border: 1px solid rgba(0, 0, 0, 0.12) !important;
        font-size: 13px !important;
        padding: 7px 12px !important;
        background-color: #F9F9FB !important;
        box-shadow: none !important;
        height: 38px !important;
      }
      
      .selectize-control.multi .selectize-input {
        height: auto !important;
        min-height: 38px !important;
      }
      
      label {
        font-size: 12px;
        font-weight: 600;
        color: #636366;
        margin-bottom: 5px;
      }
      
      /* ===================================================================== */
      /* LIENZO CANVAS 2D Y PALETA LATERAL DESPLEGADA (PAINT / PHOTOSHOP)      */
      /* ===================================================================== */
      .map-workspace-layout {
        display: flex;
        gap: 14px;
        align-items: stretch;
        width: 100%;
      }
      
      #canvas-wrapper {
        position: relative;
        flex: 1;
        min-width: 0;
        height: 680px;
        background-color: #F8FAFC;
        border-radius: 16px;
        overflow: hidden;
        border: 1px solid rgba(0, 0, 0, 0.09);
        box-shadow: inset 0 0 0 1px rgba(0, 0, 0, 0.02);
        cursor: crosshair;
        touch-action: none;
      }
      
      #mapaCanvas {
        display: block;
        width: 100%;
        height: 100%;
      }
      
      .map-tools-sidebar {
        width: 250px;
        flex-shrink: 0;
        height: 680px;
        background: #FFFFFF;
        border-radius: 16px;
        border: 1px solid rgba(0, 0, 0, 0.09);
        box-shadow: 0 4px 18px rgba(0, 0, 0, 0.04);
        padding: 14px 12px;
        display: flex;
        flex-direction: column;
        box-sizing: border-box;
        transition: opacity 0.2s ease;
      }
      
      .tools-sidebar-header {
        display: flex;
        align-items: center;
        justify-content: space-between;
        padding-bottom: 10px;
        margin-bottom: 6px;
        border-bottom: 1px solid rgba(0, 0, 0, 0.07);
      }
      
      .tools-sidebar-title {
        display: flex;
        align-items: center;
        gap: 6px;
        font-size: 13.5px;
        font-weight: 700;
        color: #1C1C1E;
        letter-spacing: -0.2px;
      }
      
      .active-tool-pill {
        font-size: 11px;
        font-weight: 700;
        padding: 3px 8px;
        border-radius: 7px;
        background: rgba(0, 122, 255, 0.12);
        color: #007AFF;
        white-space: nowrap;
      }
      
      .tools-sidebar-scroll {
        flex: 1;
        overflow-y: auto;
        padding-right: 2px;
        display: flex;
        flex-direction: column;
        gap: 2px;
      }
      
      .tools-sidebar-scroll::-webkit-scrollbar {
        width: 4px;
      }
      .tools-sidebar-scroll::-webkit-scrollbar-thumb {
        background: rgba(0, 0, 0, 0.15);
        border-radius: 4px;
      }
      
      .tools-group-label {
        font-size: 10px;
        font-weight: 700;
        color: #8E8E93;
        text-transform: uppercase;
        letter-spacing: 0.5px;
        margin-top: 8px;
        margin-bottom: 4px;
        padding-left: 4px;
      }
      .tools-group-label:first-child {
        margin-top: 0;
      }
      
      .sidebar-tool-btn, .menu-item-btn {
        background: transparent;
        border: 1px solid transparent;
        border-radius: 9px;
        padding: 7px 10px;
        min-height: 35px;
        font-size: 12.5px;
        font-weight: 600;
        color: #1C1C1E;
        cursor: pointer;
        text-align: left;
        display: flex;
        align-items: center;
        justify-content: space-between;
        transition: all 0.15s ease;
        width: 100%;
        margin-bottom: 2px;
        box-sizing: border-box;
      }
      
      .sidebar-tool-btn:hover, .menu-item-btn:hover {
        background: #F2F2F7;
        border-color: rgba(0, 0, 0, 0.04);
      }
      
      .sidebar-tool-btn.active, .menu-item-btn.active {
        background: #007AFF !important;
        color: #FFFFFF !important;
        box-shadow: 0 2px 8px rgba(0, 122, 255, 0.35);
        border-color: transparent;
      }
      
      .sidebar-tool-btn.active-eraser, .menu-item-btn.active-eraser {
        background: #D70015 !important;
        color: #FFFFFF !important;
        box-shadow: 0 2px 8px rgba(215, 0, 21, 0.35);
        border-color: transparent;
      }
      
      .sidebar-tool-btn.active-pan, .menu-item-btn.active-pan {
        background: #636366 !important;
        color: #FFFFFF !important;
        box-shadow: 0 2px 8px rgba(99, 99, 102, 0.35);
        border-color: transparent;
      }
      
      .sidebar-tool-btn.active .tool-sub,
      .sidebar-tool-btn.active-eraser .tool-sub,
      .sidebar-tool-btn.active-pan .tool-sub {
        color: rgba(255, 255, 255, 0.90) !important;
        background: rgba(255, 255, 255, 0.22) !important;
      }
      
      .sidebar-tool-btn:disabled, .menu-item-btn:disabled {
        opacity: 0.38;
        cursor: not-allowed;
        background: transparent !important;
      }
      
      .tool-btn-left {
        display: flex;
        align-items: center;
        gap: 8px;
      }
      
      .tool-icon {
        font-size: 14px;
        line-height: 1;
        width: 18px;
        text-align: center;
      }
      
      .tool-name {
        font-size: 12.5px;
        font-weight: 600;
        white-space: nowrap;
      }
      
      .tool-sub {
        font-size: 10px;
        font-weight: 600;
        color: #8E8E93;
        background: rgba(142, 142, 147, 0.12);
        padding: 2px 6px;
        border-radius: 6px;
        letter-spacing: 0.2px;
      }
      
      .tool-btn-danger {
        color: #D70015 !important;
      }
      .tool-btn-danger:hover {
        background: rgba(255, 59, 48, 0.10) !important;
      }
      
      .tool-sub-danger {
        color: #D70015 !important;
        background: rgba(255, 59, 48, 0.12) !important;
        font-weight: 700 !important;
      }
      
      .tool-btn-warning {
        color: #8E8E93 !important;
      }
      .tool-btn-warning:hover {
        background: rgba(142, 142, 147, 0.12) !important;
        color: #1C1C1E !important;
      }
      
      .tools-divider {
        height: 1px;
        background: rgba(0, 0, 0, 0.06);
        margin: 6px 2px;
      }
      
      @media (max-width: 900px) {
        .map-workspace-layout {
          flex-direction: column;
        }
        .map-tools-sidebar {
          width: 100%;
          height: auto;
          max-height: 280px;
        }
      }
      
      .map-floating-controls {
        position: absolute;
        bottom: 14px;
        right: 14px;
        background: rgba(255, 255, 255, 0.96);
        backdrop-filter: blur(12px);
        border: 1px solid rgba(0, 0, 0, 0.12);
        box-shadow: 0 4px 16px rgba(0, 0, 0, 0.08);
        border-radius: 12px;
        padding: 5px;
        display: flex;
        flex-direction: column;
        gap: 5px;
        z-index: 100;
      }
      
      .btn-zoom {
        background: #F2F2F7;
        border: 1px solid rgba(0, 0, 0, 0.08);
        border-radius: 9px;
        font-size: 18px;
        font-weight: 600;
        padding: 6px 12px;
        color: #1C1C1E;
        cursor: pointer;
      }
      
      .btn-zoom:hover {
        background: #E5E5EA;
      }
      
      #map-toast-alert {
        position: absolute;
        top: 16px;
        left: 50%;
        transform: translateX(-50%);
        background: rgba(28, 28, 30, 0.92);
        backdrop-filter: blur(10px);
        color: #FFFFFF;
        padding: 9px 18px;
        border-radius: 20px;
        font-size: 12.5px;
        font-weight: 600;
        box-shadow: 0 6px 20px rgba(0, 0, 0, 0.22);
        z-index: 250;
        display: none;
        pointer-events: none;
        border: 1px solid rgba(255, 255, 255, 0.15);
      }
      
      #valve-tooltip {
        position: absolute;
        display: none;
        background: rgba(28, 28, 30, 0.95);
        backdrop-filter: blur(8px);
        color: #FFFFFF;
        padding: 10px 13px;
        border-radius: 10px;
        font-size: 11.5px;
        line-height: 1.45;
        pointer-events: none;
        z-index: 200;
        box-shadow: 0 8px 24px rgba(0, 0, 0, 0.28);
        max-width: 310px;
      }
      
      /* Tabla DT Corporativa */
      table.dataTable {
        border-collapse: collapse !important;
        width: 100% !important;
        font-size: 12px !important;
      }
      table.dataTable thead th {
        background-color: #1E293B !important;
        color: #FFFFFF !important;
        font-weight: 600 !important;
        font-size: 10.5px !important;
        text-transform: uppercase !important;
        letter-spacing: 0.3px !important;
        padding: 10px 7px !important;
        border-bottom: 2px solid #0F172A !important;
        text-align: center !important;
      }
      table.dataTable tbody td {
        padding: 7px 7px !important;
        border-bottom: 1px solid #F1F5F9 !important;
        font-variant-numeric: tabular-nums;
      }
      table.dataTable tfoot th {
        background-color: #1E293B !important;
        color: #FFFFFF !important;
        font-weight: 700 !important;
        padding: 9px 7px !important;
        font-variant-numeric: tabular-nums;
      }
      
      /* Modal iOS HIG Asignar Área */
      .area-modal-backdrop {
        position: fixed;
        top: 0; left: 0; width: 100vw; height: 100vh;
        background: rgba(15, 23, 42, 0.45);
        backdrop-filter: blur(12px);
        -webkit-backdrop-filter: blur(12px);
        display: flex; align-items: center; justify-content: center;
        z-index: 999999;
        opacity: 0; visibility: hidden; pointer-events: none;
        transition: opacity 0.2s cubic-bezier(0.16, 1, 0.3, 1), visibility 0.2s;
      }
      .area-modal-backdrop.active {
        opacity: 1; visibility: visible; pointer-events: auto;
      }
      .area-modal-card {
        background: #FFFFFF;
        border-radius: 20px;
        padding: 22px 24px;
        width: 90%;
        max-width: 440px;
        box-shadow: 0 25px 60px rgba(0, 0, 0, 0.28), 0 0 0 1px rgba(0,0,0,0.06);
        transform: scale(0.95);
        transition: transform 0.2s cubic-bezier(0.16, 1, 0.3, 1);
      }
      .area-modal-backdrop.active .area-modal-card {
        transform: scale(1);
      }
      .quick-chip-btn {
        background: #F2F2F7;
        border: 1px solid #E5E5EA;
        border-radius: 8px;
        padding: 5px 10px;
        font-size: 11.5px;
        font-weight: 600;
        color: #1C1C1E;
        cursor: pointer;
        transition: background 0.15s, color 0.15s, border-color 0.15s;
      }
      .quick-chip-btn:hover {
        background: #007AFF;
        color: #FFFFFF;
        border-color: #007AFF;
      }
      .quick-chip-btn.danger {
        color: #FF3B30;
      }
      .quick-chip-btn.danger:hover {
        background: #FF3B30;
        color: #FFFFFF;
        border-color: #FF3B30;
      }
      .dir-seg-btn {
        flex: 1;
        padding: 7px 10px;
        border-radius: 9px;
        border: 1.5px solid #D1D1D6;
        background: #FFFFFF;
        font-size: 12.5px;
        font-weight: 600;
        color: #1C1C1E;
        cursor: pointer;
        display: flex;
        align-items: center;
        justify-content: center;
        gap: 6px;
        transition: all 0.16s cubic-bezier(0.16, 1, 0.3, 1);
        user-select: none;
      }
      .dir-seg-btn:hover {
        background: #F2F2F7;
        border-color: #007AFF;
      }
      .dir-seg-btn.active {
        background: #007AFF !important;
        color: #FFFFFF !important;
        border-color: #007AFF !important;
        box-shadow: 0 2px 8px rgba(0, 122, 255, 0.32);
      }
    "))
  ),
  
  # Apple iOS HIG Login Overlay (Modal de Autenticación 100% Nativo Shiny)
  div(id = "login-overlay", class = "login-backdrop",
      div(class = "login-card",
          div(class = "login-logo-container",
              tags$img(src = logo_cerro_prieto_src, class = "login-logo-img", alt = "Cerro Prieto")
          ),
          h2(class = "login-title", "Módulo de Cosecha y Rutas"),
          p(class = "login-subtitle", "Agricola Cerro Prieto · Acceso por Sector"),
          
          # Campo Unificado: Escribir o Seleccionar con Autocompletado Instantáneo
          div(class = "login-user-group", style = "width: 100%; margin-top: 20px; text-align: left; position: relative; z-index: 1000;",
              tags$div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 6px;",
                  tags$label(`for` = "login_usuario", style = "font-size: 11.5px; font-weight: 700; color: #8E8E93; text-transform: uppercase; margin-bottom: 0px; display: block; letter-spacing: 0.3px;", "EVALUADOR / USUARIO"),
                  actionLink("btn_refrescar_usuarios", label = tags$span(icon("sync-alt"), " Actualizar"), style = "font-size: 11px; font-weight: 600; color: #007AFF; text-decoration: none;")
              ),
              selectizeInput("login_usuario", label = NULL, 
                             choices = c("Escriba o seleccione su nombre..." = "", generar_opciones_usuarios(if (!is.null(usuarios_db)) usuarios_db else usuarios_db_default)),
                             selected = "",
                             options = list(
                               placeholder = "Escriba o seleccione su nombre...",
                               create = FALSE,
                               persist = FALSE,
                               openOnFocus = TRUE,
                               maxItems = 1,
                               selectOnTab = TRUE,
                               closeAfterSelect = TRUE
                             ), 
                             width = "100%")
          ),
          
          div(class = "login-password-group", style = "width: 100%; margin-top: 14px; text-align: left; position: relative; z-index: 10;",
              tags$label(`for` = "login_password", style = "font-size: 11.5px; font-weight: 700; color: #8E8E93; text-transform: uppercase; margin-bottom: 6px; display: block; letter-spacing: 0.3px;", "CONTRASEÑA (DNI)"),
              passwordInput("login_password", label = NULL, value = "", placeholder = "Ingrese su número de DNI", width = "100%")
          ),
          
          uiOutput("login_error_msg"),
          
          actionButton("btn_iniciar_sesion", label = "Iniciar Sesión", class = "btn-login-apple", width = "100%")
      )
  ),

  # Apple iOS HIG Activity Overlay (Modal Loader)
  div(id = "ios-loader-overlay", class = "ios-loader-backdrop",
      div(class = "ios-loader-card",
          div(class = "ios-spinner",
              div(), div(), div(), div(),
              div(), div(), div(), div(),
              div(), div(), div(), div()
          ),
          div(id = "ios-loader-title", class = "ios-loader-title", "Cargando datos..."),
          div(id = "ios-loader-subtitle", class = "ios-loader-subtitle", "Sincronizando información semanal")
      )
  ),
  
  # Header unificado con Perfil en la Esquina Derecha y Controles alineados abajo
  div(class = "ios-header",
      # Fila Superior: Título a la izquierda y Perfil de Usuario en la Esquina Derecha
      div(class = "ios-header-top-row",
          div(class = "ios-header-title-box",
              h1(class = "ios-title", style = "display: flex; align-items: center; gap: 10px;",
                 tags$img(src = logo_cerro_prieto_src, alt = "CP", 
                          style = "height: 30px; width: 30px; object-fit: contain; border-radius: 6px; vertical-align: middle; flex-shrink: 0; box-shadow: 0 1px 4px rgba(0,0,0,0.06);"),
                 tags$span("Ruta Semanal de Proyección y Cosecha")
              ),
              p(class = "ios-subtitle", "Cálculo Automático desde Hoja 'Cálculo' (Proyección) y Distribución de Kilos Cosechados por ha")
          ),
          div(class = "ios-header-user-corner",
              uiOutput("ui_perfil_usuario")
          )
      ),
      # Fila Inferior (un poco más abajo): Estado a la izquierda y Acciones/Semana a la derecha
      div(class = "ios-header-bottom-row",
          div(class = "ios-header-status-box",
              uiOutput("badge_origen_datos")
          ),
          div(class = "ios-header-actions-box",
              div(style = "width: 190px; flex-shrink: 0;",
                  selectInput("sel_semana", label = NULL, choices = opciones_semanas, selected = semana_iso_actual, width = "100%", selectize = FALSE)
              ),
              uiOutput("badge_estado_guardado"),
              actionButton("btn_sincronizar_sheets", label = HTML("🔄 Sincronizar"),
                           class = "btn-secondary-apple",
                           onclick = "const s = $('#sel_semana').val() || ''; showIosLoader('Sincronizando' + (s ? (' Semana ' + s) : '') + '...', 'Descargando rutas vigentes desde Google Sheets y Data Maestra');",
                           style = "border-radius: 10px; background: #FFFFFF; color: #007AFF; font-weight: 600; font-size: 12.5px; border: 1px solid #C7C7CC; height: 38px; padding: 0 13px; box-shadow: 0 1px 2px rgba(0,0,0,0.05); cursor: pointer; white-space: nowrap;"),
              tags$button(type = "button", id = "btn_guardar_manual", onclick = "guardarRutaCompleta()",
                          style = "border-radius: 10px; background: #007AFF; color: #FFFFFF; font-weight: 600; font-size: 12.5px; border: none; height: 38px; padding: 0 15px; cursor: pointer; white-space: nowrap;",
                          "💾 Guardar Ruta"),
              tags$button(type = "button", id = "btn_exportar_imagen", onclick = "exportarImagenReporte()",
                          style = "border-radius: 10px; background: #1C1C1E; color: #FFFFFF; font-weight: 600; font-size: 12.5px; border: none; height: 38px; padding: 0 15px; cursor: pointer; display: inline-flex; align-items: center; gap: 6px; box-shadow: 0 2px 6px rgba(0,0,0,0.12); white-space: nowrap;",
                          span("📸"), span("Exportar Imagen"))
          )
      )
  ),
  
  div(style = "max-width: 1580px; margin: 0 auto; padding: 0 16px;",
      
      # 1. Filtros en Cascada (Grupo -> Módulo -> Turno -> Válvula -> Variedad)
      div(class = "ios-card ios-card-filtros", style = "position: relative; z-index: 500; overflow: visible;",
          div(class = "filters-grid",
              div(
                selectizeInput("filtro_grupo", "Grupo de Módulos", 
                               choices = names(grupos_modulos), 
                               selected = NULL, 
                               multiple = TRUE,
                               width = "100%",
                               options = list(placeholder = "Todos los grupos", plugins = list("remove_button"), dropdownParent = "body"))
              ),
              div(
                selectizeInput("filtro_modulo", "Módulo", 
                               choices = mods_iniciales, 
                               selected = NULL, 
                               multiple = TRUE,
                               width = "100%",
                               options = list(placeholder = "Todos los módulos", plugins = list("remove_button"), dropdownParent = "body"))
              ),
              div(
                selectizeInput("filtro_turno", "Turno", 
                               choices = turnos_iniciales, 
                               selected = NULL, 
                               multiple = TRUE,
                               width = "100%",
                               options = list(placeholder = "Todos los turnos", plugins = list("remove_button"), dropdownParent = "body"))
              ),
              div(
                selectizeInput("filtro_valvula", "Válvula", 
                               choices = valvs_iniciales, 
                               selected = NULL, 
                               multiple = TRUE,
                               width = "100%",
                               options = list(placeholder = "Todas las válvulas", plugins = list("remove_button"), dropdownParent = "body"))
              ),
              div(
                selectizeInput("filtro_variedad", "Variedad Normalizada", 
                               choices = vars_iniciales, 
                               selected = NULL, 
                               multiple = TRUE,
                               width = "100%",
                               options = list(placeholder = "Todas", plugins = list("remove_button"), dropdownParent = "body"))
              ),
              div(style = "margin-top: 25px; display: flex; gap: 8px;",
                  actionButton("btn_reset_filtros", "Limpiar Filtros", 
                               style = "width: 100%; border-radius: 10px; background: #E5E5EA; border: none; font-weight: 600; font-size: 12.5px; color: #1C1C1E; height: 38px;")
              )
          )
      ),
      
      # 2. KPIs + Tarjetas de Día Interactivas con Casilla de Kilos
      div(class = "kpi-container",
          div(class = "kpi-card",
              div(class = "kpi-label", "Área Física Filtro"),
              div(class = "kpi-value", textOutput("kpi_ha_filtradas")),
              span(class = "kpi-badge", style = "background: rgba(142, 142, 147, 0.12); color: #636366;", textOutput("kpi_num_valvulas", inline = TRUE)),
              div(class = "day-kg-box",
                  span(class = "day-kg-label", "PROY. OFICIAL (EXCEL)"),
                  div(style = "font-size: 13px; font-weight: 800; color: #FF9500;", textOutput("kpi_kg_excel_filtro"))
              )
          ),
          div(class = "kpi-card",
              div(class = "kpi-label", textOutput("kpi_titulo_pasada_activa")),
              div(class = "kpi-value", style = "color: #007AFF;", textOutput("kpi_ha_total_prog")),
              span(class = "kpi-badge", style = "background: rgba(0, 122, 255, 0.12); color: #007AFF;", textOutput("kpi_pct_avance", inline = TRUE)),
              div(class = "day-kg-box",
                  span(class = "day-kg-label", "KILOS PASADA ACTIVA"),
                  div(style = "font-size: 13.5px; font-weight: 800; color: #007AFF;", textOutput("kpi_kg_pasada_activa"))
              )
          ),
          div(class = "kpi-card",
              div(class = "kpi-label", "Total Semana (1ª + 2ª)"),
              div(class = "kpi-value", style = "color: #16A34A;", textOutput("kpi_ha_ambas_pasadas")),
              span(class = "kpi-badge", style = "background: rgba(22, 163, 74, 0.12); color: #16A34A;", textOutput("kpi_pct_ambas_pasadas", inline = TRUE)),
              div(class = "day-kg-box",
                  span(class = "day-kg-label", "KILOS TOTAL SEMANA"),
                  div(style = "font-size: 13.5px; font-weight: 800; color: #16A34A;", textOutput("kpi_kg_ambas_pasadas"))
              )
          ),
          
          # Tarjetas de Días Seleccionables + Input de Kilos
          div(class = paste("kpi-card day-card", if (dia_defecto_ui == "lunes") "active" else ""), id = "day-card-lunes", onclick = "setDay('lunes')",
              style = "--day-border: #FF0000; --day-bg: rgba(255, 0, 0, 0.03); --day-shadow: rgba(255, 0, 0, 0.18);",
              span(class = "day-active-pill", style = "background: #FF0000;", "Activo"),
              div(class = "kpi-label", "🔴 Lunes"),
              div(class = "kpi-value", style = "color: #FF0000;", textOutput("kpi_ha_lunes")),
              span(class = "kpi-badge", style = "background: rgba(255, 0, 0, 0.12); color: #E60000;", textOutput("kpi_pct_lunes", inline = TRUE)),
              div(class = "day-kg-box", onclick = "event.stopPropagation();",
                  div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 2px;",
                      span(class = "day-kg-label lbl-modo-kg", "KG COSECHA CAMPO:"),
                      span(id = "badge_proy_sug_lunes", class = "badge-proy-sug",
                           style = "display: none; font-size: 10px; font-weight: 700; color: #D97706; background: rgba(245, 158, 11, 0.12); padding: 1px 6px; border-radius: 4px; border: 1px solid rgba(245, 158, 11, 0.28); cursor: pointer;",
                           title = "Kilos proyectados del filtro activo (haz clic para copiar)",
                           onclick = "copiarSugeridoADia('lunes')",
                           "Proy: 0 kg")
                  ),
                  div(class = "day-kg-input-wrap",
                      tags$input(id = "input_kg_lunes", type = "text", class = "day-kg-input", placeholder = "0",
                                 onkeydown = "handleDayKgKey(event, 'Lunes')",
                                 onchange = "handleDayKgBlur(this, 'Lunes')"),
                      span(class = "day-kg-unit", "kg")
                  )
              )
          ),
          div(class = paste("kpi-card day-card", if (dia_defecto_ui == "martes") "active" else ""), id = "day-card-martes", onclick = "setDay('martes')",
              style = "--day-border: #00A2E8; --day-bg: rgba(0, 162, 232, 0.03); --day-shadow: rgba(0, 162, 232, 0.18);",
              span(class = "day-active-pill", style = "background: #00A2E8;", "Activo"),
              div(class = "kpi-label", "🔵 Martes"),
              div(class = "kpi-value", style = "color: #00A2E8;", textOutput("kpi_ha_martes")),
              span(class = "kpi-badge", style = "background: rgba(0, 162, 232, 0.12); color: #0091D0;", textOutput("kpi_pct_martes", inline = TRUE)),
              div(class = "day-kg-box", onclick = "event.stopPropagation();",
                  div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 2px;",
                      span(class = "day-kg-label lbl-modo-kg", "KG COSECHA CAMPO:"),
                      span(id = "badge_proy_sug_martes", class = "badge-proy-sug",
                           style = "display: none; font-size: 10px; font-weight: 700; color: #D97706; background: rgba(245, 158, 11, 0.12); padding: 1px 6px; border-radius: 4px; border: 1px solid rgba(245, 158, 11, 0.28); cursor: pointer;",
                           title = "Kilos proyectados del filtro activo (haz clic para copiar)",
                           onclick = "copiarSugeridoADia('martes')",
                           "Proy: 0 kg")
                  ),
                  div(class = "day-kg-input-wrap",
                      tags$input(id = "input_kg_martes", type = "text", class = "day-kg-input", placeholder = "0",
                                 onkeydown = "handleDayKgKey(event, 'Martes')",
                                 onchange = "handleDayKgBlur(this, 'Martes')"),
                      span(class = "day-kg-unit", "kg")
                  )
              )
          ),
          div(class = paste("kpi-card day-card", if (dia_defecto_ui == "miercoles") "active" else ""), id = "day-card-miercoles", onclick = "setDay('miercoles')",
              style = "--day-border: #FF7F27; --day-bg: rgba(255, 127, 39, 0.03); --day-shadow: rgba(255, 127, 39, 0.18);",
              span(class = "day-active-pill", style = "background: #FF7F27;", "Activo"),
              div(class = "kpi-label", "🟠 Miércoles"),
              div(class = "kpi-value", style = "color: #FF7F27;", textOutput("kpi_ha_miercoles")),
              span(class = "kpi-badge", style = "background: rgba(255, 127, 39, 0.12); color: #E66810;", textOutput("kpi_pct_miercoles", inline = TRUE)),
              div(class = "day-kg-box", onclick = "event.stopPropagation();",
                  div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 2px;",
                      span(class = "day-kg-label lbl-modo-kg", "KG COSECHA CAMPO:"),
                      span(id = "badge_proy_sug_miercoles", class = "badge-proy-sug",
                           style = "display: none; font-size: 10px; font-weight: 700; color: #D97706; background: rgba(245, 158, 11, 0.12); padding: 1px 6px; border-radius: 4px; border: 1px solid rgba(245, 158, 11, 0.28); cursor: pointer;",
                           title = "Kilos proyectados del filtro activo (haz clic para copiar)",
                           onclick = "copiarSugeridoADia('miercoles')",
                           "Proy: 0 kg")
                  ),
                  div(class = "day-kg-input-wrap",
                      tags$input(id = "input_kg_miercoles", type = "text", class = "day-kg-input", placeholder = "0",
                                 onkeydown = "handleDayKgKey(event, 'Miércoles')",
                                 onchange = "handleDayKgBlur(this, 'Miércoles')"),
                      span(class = "day-kg-unit", "kg")
                  )
              )
          ),
          div(class = paste("kpi-card day-card", if (dia_defecto_ui == "jueves") "active" else ""), id = "day-card-jueves", onclick = "setDay('jueves')",
              style = "--day-border: #EAB308; --day-bg: rgba(250, 204, 21, 0.06); --day-shadow: rgba(234, 179, 8, 0.22);",
              span(class = "day-active-pill", style = "background: #CA8A04;", "Activo"),
              div(class = "kpi-label", "🟡 Jueves"),
              div(class = "kpi-value", style = "color: #B45309;", textOutput("kpi_ha_jueves")),
              span(class = "kpi-badge", style = "background: rgba(202, 138, 4, 0.15); color: #B45309;", textOutput("kpi_pct_jueves", inline = TRUE)),
              div(class = "day-kg-box", onclick = "event.stopPropagation();",
                  div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 2px;",
                      span(class = "day-kg-label lbl-modo-kg", "KG COSECHA CAMPO:"),
                      span(id = "badge_proy_sug_jueves", class = "badge-proy-sug",
                           style = "display: none; font-size: 10px; font-weight: 700; color: #D97706; background: rgba(245, 158, 11, 0.12); padding: 1px 6px; border-radius: 4px; border: 1px solid rgba(245, 158, 11, 0.28); cursor: pointer;",
                           title = "Kilos proyectados del filtro activo (haz clic para copiar)",
                           onclick = "copiarSugeridoADia('jueves')",
                           "Proy: 0 kg")
                  ),
                  div(class = "day-kg-input-wrap",
                      tags$input(id = "input_kg_jueves", type = "text", class = "day-kg-input", placeholder = "0",
                                 onkeydown = "handleDayKgKey(event, 'Jueves')",
                                 onchange = "handleDayKgBlur(this, 'Jueves')"),
                      span(class = "day-kg-unit", "kg")
                  )
              )
          ),
          div(class = paste("kpi-card day-card", if (dia_defecto_ui == "viernes") "active" else ""), id = "day-card-viernes", onclick = "setDay('viernes')",
              style = "--day-border: #00C853; --day-bg: rgba(0, 200, 83, 0.03); --day-shadow: rgba(0, 200, 83, 0.18);",
              span(class = "day-active-pill", style = "background: #00C853;", "Activo"),
              div(class = "kpi-label", "🟢 Viernes"),
              div(class = "kpi-value", style = "color: #00C853;", textOutput("kpi_ha_viernes")),
              span(class = "kpi-badge", style = "background: rgba(0, 200, 83, 0.12); color: #009940;", textOutput("kpi_pct_viernes", inline = TRUE)),
              div(class = "day-kg-box", onclick = "event.stopPropagation();",
                  div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 2px;",
                      span(class = "day-kg-label lbl-modo-kg", "KG COSECHA CAMPO:"),
                      span(id = "badge_proy_sug_viernes", class = "badge-proy-sug",
                           style = "display: none; font-size: 10px; font-weight: 700; color: #D97706; background: rgba(245, 158, 11, 0.12); padding: 1px 6px; border-radius: 4px; border: 1px solid rgba(245, 158, 11, 0.28); cursor: pointer;",
                           title = "Kilos proyectados del filtro activo (haz clic para copiar)",
                           onclick = "copiarSugeridoADia('viernes')",
                           "Proy: 0 kg")
                  ),
                  div(class = "day-kg-input-wrap",
                      tags$input(id = "input_kg_viernes", type = "text", class = "day-kg-input", placeholder = "0",
                                 onkeydown = "handleDayKgKey(event, 'Viernes')",
                                 onchange = "handleDayKgBlur(this, 'Viernes')"),
                      span(class = "day-kg-unit", "kg")
                  )
              )
          ),
          div(class = paste("kpi-card day-card", if (dia_defecto_ui == "sabado") "active" else ""), id = "day-card-sabado", onclick = "setDay('sabado')",
              style = "--day-border: #A349A4; --day-bg: rgba(163, 73, 164, 0.03); --day-shadow: rgba(163, 73, 164, 0.18);",
              span(class = "day-active-pill", style = "background: #A349A4;", "Activo"),
              div(class = "kpi-label", "🟣 Sábado"),
              div(class = "kpi-value", style = "color: #A349A4;", textOutput("kpi_ha_sabado")),
              span(class = "kpi-badge", style = "background: rgba(163, 73, 164, 0.12); color: #8C3A8D;", textOutput("kpi_pct_sabado", inline = TRUE)),
              div(class = "day-kg-box", onclick = "event.stopPropagation();",
                  div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 2px;",
                      span(class = "day-kg-label lbl-modo-kg", "KG COSECHA CAMPO:"),
                      span(id = "badge_proy_sug_sabado", class = "badge-proy-sug",
                           style = "display: none; font-size: 10px; font-weight: 700; color: #D97706; background: rgba(245, 158, 11, 0.12); padding: 1px 6px; border-radius: 4px; border: 1px solid rgba(245, 158, 11, 0.28); cursor: pointer;",
                           title = "Kilos proyectados del filtro activo (haz clic para copiar)",
                           onclick = "copiarSugeridoADia('sabado')",
                           "Proy: 0 kg")
                  ),
                  div(class = "day-kg-input-wrap",
                      tags$input(id = "input_kg_sabado", type = "text", class = "day-kg-input", placeholder = "0",
                                 onkeydown = "handleDayKgKey(event, 'Sábado')",
                                 onchange = "handleDayKgBlur(this, 'Sábado')"),
                      span(class = "day-kg-unit", "kg")
                  )
              )
          )
      ),
      
      # 3. Lienzo Canvas 2D Interactivo con Switch de Modo (Proyección / Cosecha) y Pasada (1ª / 2ª)
      div(class = "ios-card", style = "padding: 16px;",
          div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 12px; flex-wrap: wrap; gap: 12px;",
              div(
                h3(style = "font-size: 15px; font-weight: 700; margin: 0; color: #1C1C1E;", "Plano General 2D del Fundo"),
                span(style = "font-size: 12px; color: #8E8E93;", 
                     "En Proyección planifica la ruta; en Cosecha Campo anota la ejecución real; en Cosecha Oficial consulta los datos de balanza packing (🔒 solo lectura).")
              ),
              div(style = "display: flex; align-items: center; gap: 10px; flex-wrap: wrap;",
                  # Switch Modo: Proyección vs Cosecha Campo vs Cosecha Oficial
                  div(class = "ios-segmented-pass",
                      tags$button(type = "button", id = "btn-mode-proy", class = "pass-pill", onclick = "setMode('Proyeccion')", "📋 Proyección"),
                      tags$button(type = "button", id = "btn-mode-cos-campo", class = "pass-pill active-cosecha", onclick = "setMode('CosechaCampo')", "🚜 Cosecha Campo"),
                      tags$button(type = "button", id = "btn-mode-cos-oficial", class = "pass-pill", onclick = "setMode('CosechaOficial')", "🏛️ Cosecha Oficial 🔒")
                  ),
                  # Switch Pasada: 1ª vs 2ª Pasada
                  div(class = "ios-segmented-pass",
                      tags$button(type = "button", id = "btn-pass-1", class = "pass-pill active", onclick = "setPass(1)", "1ª Pasada"),
                      tags$button(type = "button", id = "btn-pass-2", class = "pass-pill", onclick = "setPass(2)", "2ª Pasada")
                  )
              )
          ),
          
          # Workspace estilo Paint / Photoshop: Mapa a la izquierda + Paleta Lateral a la derecha
          div(class = "map-workspace-layout",
              
              # Lienzo Canvas (Ancho flexible, adaptativo)
              div(id = "canvas-wrapper",
                  
                  # Cartel flotante de Sólo Lectura para Cosecha Oficial
                  div(id = "official-lock-badge", class = "lock-badge-floating",
                      span("🔒"),
                      span("COSECHA OFICIAL (SOLO LECTURA - BALANZA PACKING)")
                  ),
                  
                  # Aviso flotante de regla de bloqueo
                  div(id = "map-toast-alert"),
                  
                  # Controles flotantes de zoom
                  div(class = "map-floating-controls",
                      tags$button(type = "button", class = "btn-zoom", onclick = "zoomIn()", "+"),
                      tags$button(type = "button", class = "btn-zoom", onclick = "zoomOut()", "−")
                  ),
                  
                  # Tooltip flotante
                  div(id = "valve-tooltip"),
                  
                  # Canvas elemento nativo
                  tags$canvas(id = "mapaCanvas")
              ),
              
              # Paleta Lateral de Herramientas Desplegada (Photoshop / Paint Toolbar)
              div(class = "map-tools-sidebar", id = "map-menu-box",
                  
                  # Encabezado de la barra con Indicador Activo
                  div(class = "tools-sidebar-header",
                      div(class = "tools-sidebar-title",
                          span(style = "font-size: 15px;", "🎨"),
                          span("Herramientas"),
                          span(id = "sidebar-lock-indicator", style = "display: none; font-size: 11px; margin-left: 3px;", "🔒")
                      ),
                      span(id = "active-tool-indicator", class = "active-tool-pill", "🖌️ Pintar 10%")
                  ),
                  
                  # Contenedor con scroll suave para todas las herramientas
                  div(class = "tools-sidebar-scroll", onwheel = "event.stopPropagation();",
                      
                      # Grupo: Trazo y Pintado
                      div(class = "tools-group-label", "Pintar / Trazo"),
                      tags$button(type = "button", class = "sidebar-tool-btn sidebar-drawing-tool active", id = "mode-drag-cells-btn", onclick = "selectToolFromMenu('cells')",
                                  span(class = "tool-btn-left", span(class = "tool-icon", "🖌️"), span(class = "tool-name", "Pintar 10%")),
                                  span(class = "tool-sub", "Columna")),
                      tags$button(type = "button", class = "sidebar-tool-btn sidebar-drawing-tool", id = "mode-valve100-btn", onclick = "selectToolFromMenu('valve100')",
                                  span(class = "tool-btn-left", span(class = "tool-icon", "⚡"), span(class = "tool-name", "Válvula 100%")),
                                  span(class = "tool-sub", "Total")),
                      tags$button(type = "button", class = "sidebar-tool-btn sidebar-drawing-tool", id = "mode-area-btn", onclick = "selectToolFromMenu('area')",
                                  span(class = "tool-btn-left", span(class = "tool-icon", "📐"), span(class = "tool-name", "Ingresar Área")),
                                  span(class = "tool-sub", "ha")),
                      
                      div(class = "tools-divider"),
                      
                      # Grupo: Borradores por Día
                      div(class = "tools-group-label", "Borradores del Día"),
                      tags$button(type = "button", class = "sidebar-tool-btn sidebar-drawing-tool", id = "mode-eraser-btn", onclick = "selectEraserFromMenu('cells')",
                                  span(class = "tool-btn-left", span(class = "tool-icon", "🧹"), span(class = "tool-name", "Borrar 10%")),
                                  span(class = "tool-sub", "Celda")),
                      tags$button(type = "button", class = "sidebar-tool-btn sidebar-drawing-tool", id = "mode-eraser100-btn", onclick = "selectEraserFromMenu('valve100')",
                                  span(class = "tool-btn-left", span(class = "tool-icon", "🧹"), span(class = "tool-name", "Borrar 100%")),
                                  span(class = "tool-sub", "Válvula")),
                      
                      div(class = "tools-divider"),
                      
                      # Grupo: Navegación & Vista
                      div(class = "tools-group-label", "Navegación & Zoom"),
                      tags$button(type = "button", class = "sidebar-tool-btn", id = "mode-pan-btn", onclick = "selectToolFromMenu('pan')",
                                  span(class = "tool-btn-left", span(class = "tool-icon", "✋"), span(class = "tool-name", "Mover Mapa")),
                                  span(class = "tool-sub", "Arrastrar")),
                      tags$button(type = "button", class = "sidebar-tool-btn", id = "btn-undo", onclick = "undoLastStroke()", disabled = "disabled",
                                  span(class = "tool-btn-left", span(class = "tool-icon", "↩️"), span(class = "tool-name", "Deshacer")),
                                  span(class = "tool-sub", "Ctrl+Z")),
                      tags$button(type = "button", class = "sidebar-tool-btn", onclick = "fitSelection()",
                                  span(class = "tool-btn-left", span(class = "tool-icon", "🎯"), span(class = "tool-name", "Centrar Filtro")),
                                  span(class = "tool-sub", "Auto")),
                      tags$button(type = "button", class = "sidebar-tool-btn", onclick = "resetView()",
                                  span(class = "tool-btn-left", span(class = "tool-icon", "⛶"), span(class = "tool-name", "Ver Todo")),
                                  span(class = "tool-sub", "Fundo")),
                      
                      div(class = "tools-divider"),
                      
                      # Grupo: Acciones
                      div(class = "tools-group-label", "Acciones"),
                      tags$button(type = "button", class = "sidebar-tool-btn", onclick = "exportarImagenReporte()",
                                  span(class = "tool-btn-left", span(class = "tool-icon", "📸"), span(class = "tool-name", "Exportar Imagen")),
                                  span(class = "tool-sub", "HD")),
                      tags$button(type = "button", class = "sidebar-tool-btn sidebar-drawing-tool tool-btn-danger", onclick = "triggerResetDiaActivoModal()",
                                  span(class = "tool-btn-left", span(class = "tool-icon", "🗑️"), span(class = "tool-name", "Borrar Día")),
                                  span(id = "lbl-dia-activo-menu", class = "tool-sub tool-sub-danger", "Lunes")),
                      tags$button(type = "button", class = "sidebar-tool-btn sidebar-drawing-tool tool-btn-warning", onclick = "triggerResetPassModal()",
                                  span(class = "tool-btn-left", span(class = "tool-icon", "⚠️"), span(class = "tool-name", "Reiniciar Pasada")),
                                  span(class = "tool-sub", "Semana"))
                  )
              )
          )
      ),
      
      # Modal iOS HIG para Asignar Área Manualmente a una Válvula
      div(id = "modal-asignar-area", class = "area-modal-backdrop",
          div(class = "area-modal-card",
              div(style = "display: flex; justify-content: space-between; align-items: flex-start; margin-bottom: 12px;",
                  div(
                      h4(id = "area-modal-valvula-titulo", style = "margin: 0; font-size: 17px; font-weight: 700; color: #1C1C1E;", "📐 Asignar Área"),
                      span(id = "area-modal-valvula-sub", style = "font-size: 12.5px; color: #8E8E93;", "Válvula • Módulo • Turno")
                  ),
                  tags$button(type = "button", style = "border: none; background: #E5E5EA; width: 28px; height: 28px; border-radius: 50%; font-size: 14px; color: #636366; cursor: pointer; display: flex; align-items: center; justify-content: center;",
                              onclick = "cerrarModalAsignarArea()", "✕")
              ),
              div(id = "area-modal-info-box", style = "background: #F2F2F7; border-radius: 12px; padding: 10px 14px; margin-bottom: 14px; font-size: 12.5px; line-height: 1.5; color: #3A3A3C;"),
              
              div(style = "display: flex; gap: 10px; margin-bottom: 12px;",
                  div(style = "flex: 1;",
                      tags$label(style = "font-size: 12px; font-weight: 600; color: #1C1C1E; display: block; margin-bottom: 4px;", "Área a asignar (ha):"),
                      div(style = "display: flex; gap: 6px; align-items: center;",
                          tags$input(id = "input_asignar_ha", type = "number", step = "0.01", min = "0", placeholder = "0.00",
                                     style = "flex: 1; height: 38px; border-radius: 10px; border: 1.5px solid #007AFF; padding: 0 10px; font-size: 14.5px; font-weight: 700; outline: none; background: #FFFFFF; font-variant-numeric: tabular-nums;",
                                     oninput = "onInputModalArea(this.value)",
                                     onkeydown = "if(event.key==='Enter') aplicarAsignarArea();"),
                          span(style = "font-weight: 700; color: #8E8E93; font-size: 13px;", "ha")
                      )
                  ),
                  div(style = "flex: 1;",
                      tags$label(style = "font-size: 12px; font-weight: 600; color: #1C1C1E; display: block; margin-bottom: 4px;", "Porcentaje exacto (%):"),
                      div(style = "display: flex; gap: 6px; align-items: center;",
                          tags$input(id = "input_asignar_pct", type = "number", step = "0.01", min = "0", max = "100", placeholder = "0.00",
                                     style = "flex: 1; height: 38px; border-radius: 10px; border: 1.5px solid #34C759; padding: 0 10px; font-size: 14.5px; font-weight: 700; outline: none; background: #FFFFFF; font-variant-numeric: tabular-nums;",
                                     oninput = "onInputModalPct(this.value)",
                                     onkeydown = "if(event.key==='Enter') aplicarAsignarArea();"),
                          span(style = "font-weight: 700; color: #8E8E93; font-size: 13px;", "%")
                      )
                  )
              ),
              
              # Banner de Alerta / Error cuando se excede el espacio libre
              div(id = "area-modal-alerta-exceso", style = "display: none; background: #FFF2F2; border: 1.5px solid #FF3B30; border-radius: 10px; padding: 8px 12px; margin-bottom: 12px; font-size: 12px; color: #D70015; font-weight: 600; align-items: center; gap: 8px;",
                  span(style = "font-size: 15px;", "⚠️"),
                  span(id = "area-modal-alerta-texto", "Supera el espacio libre disponible.")
              ),
              
              div(style = "display: flex; gap: 6px; margin-bottom: 14px; flex-wrap: wrap;",
                  tags$button(type = "button", class = "quick-chip-btn", onclick = "fijarPorcentajeModal(10)", "10%"),
                  tags$button(type = "button", class = "quick-chip-btn", onclick = "fijarPorcentajeModal(25)", "25%"),
                  tags$button(type = "button", class = "quick-chip-btn", onclick = "fijarPorcentajeModal(33.33)", "33.3%"),
                  tags$button(type = "button", class = "quick-chip-btn", onclick = "fijarPorcentajeModal(50)", "50%"),
                  tags$button(type = "button", class = "quick-chip-btn", onclick = "fijarPorcentajeModal(75)", "75%"),
                  tags$button(type = "button", class = "quick-chip-btn", onclick = "fijarPorcentajeModal(100)", "100%"),
                  tags$button(type = "button", class = "quick-chip-btn danger", onclick = "fijarPorcentajeModal(0)", "0% (Borrar)")
              ),
              
              # Selector de Dirección de Inicio de Pintado (Izquierda/Centro/Derecha o Arriba/Centro/Abajo)
              div(id = "area-modal-dir-section", style = "margin-bottom: 14px;",
                  div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 6px;",
                      tags$label(style = "font-size: 12px; font-weight: 600; color: #1C1C1E; margin: 0;", "Iniciar pintado desde:"),
                      span(id = "area-modal-dir-selected-label", style = "font-size: 12px; font-weight: 700; color: #007AFF;", "Izquierda")
                  ),
                  div(id = "area-modal-direction-selector", style = "display: flex; gap: 8px; width: 100%;")
              ),
              
              div(style = "margin-bottom: 14px;",
                  div(style = "display: flex; justify-content: space-between; font-size: 11.5px; font-weight: 600; color: #8E8E93; margin-bottom: 5px;",
                      span("Vista previa física proporcional:"),
                      span(id = "area-modal-orientacion-lbl", "Horizontal")
                  ),
                  div(id = "area-modal-preview-bar", style = "position: relative; height: 26px; width: 100%; border-radius: 8px; overflow: hidden; background: #F2F2F7; border: 1px solid #D1D1D6; display: flex;")
              ),
              
              div(id = "area-modal-calculo-box", style = "background: rgba(0, 122, 255, 0.06); border-radius: 12px; padding: 10px 14px; margin-bottom: 16px; font-size: 12px; line-height: 1.5; color: #1C1C1E;"),
              
              div(style = "display: flex; gap: 10px; justify-content: flex-end;",
                  tags$button(type = "button", style = "padding: 8px 16px; border-radius: 10px; border: none; background: #E5E5EA; font-weight: 600; font-size: 13px; color: #1C1C1E; cursor: pointer;",
                              onclick = "cerrarModalAsignarArea()", "Cancelar"),
                  tags$button(type = "button", id = "btn-modal-aplicar-area", style = "padding: 8px 20px; border-radius: 10px; border: none; background: #007AFF; font-weight: 700; font-size: 13px; color: #FFFFFF; cursor: pointer; box-shadow: 0 2px 8px rgba(0, 122, 255, 0.35);",
                              onclick = "aplicarAsignarArea()", "✓ Aplicar y Pintar")
              )
          )
      ),
      
      # 4. Tabla Resumen de Hectáreas y Kilos por Turno y Día
      div(class = "ios-card",
          div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 12px; flex-wrap: wrap;",
              h3(style = "font-size: 15px; font-weight: 700; margin: 0; color: #1C1C1E;", 
                 "Resumen de Hectáreas y Kilos por Turno y Día"),
              span(style = "font-size: 12px; color: #8E8E93;", 
                   "Desglose de hectáreas y kilos asignados en el modo y pasada activos")
          ),
          DTOutput("tabla_resumen")
      )
  ),
  
  # Inyección de Catálogo de Válvulas, Logo Oficial ACP y Motor JavaScript Canvas
  tags$script(HTML(sprintf("
    const VALVES_DATA = %s;
    const LOGO_ACP_SRC = %s;
  ", json_valvulas, toJSON(logo_cerro_prieto_src, auto_unbox = TRUE)))),
  
  tags$script(HTML("
    const acpLogoImg = new Image();
    acpLogoImg.src = (typeof LOGO_ACP_SRC !== 'undefined' && LOGO_ACP_SRC) ? LOGO_ACP_SRC : 'www/logo_cerro_prieto.png';

    /* ==================================================================== */
    /* APPLE iOS HIG LOADER CONTROLLER                                     */
    /* ==================================================================== */
    let loaderTimeout = null;
    function showIosLoader(title, subtitle) {
      const overlay = document.getElementById('ios-loader-overlay');
      if (!overlay) return;
      if (title) document.getElementById('ios-loader-title').textContent = title;
      if (subtitle) document.getElementById('ios-loader-subtitle').textContent = subtitle;
      overlay.classList.add('active');
      
      if (loaderTimeout) clearTimeout(loaderTimeout);
      loaderTimeout = setTimeout(function() {
        hideIosLoader();
      }, 15000);
    }
    
    function hideIosLoader() {
      if (loaderTimeout) {
        clearTimeout(loaderTimeout);
        loaderTimeout = null;
      }
      const overlay = document.getElementById('ios-loader-overlay');
      if (!overlay) return;
      overlay.classList.remove('active');
    }

    const DAY_KEYS = ['lunes', 'martes', 'miercoles', 'jueves', 'viernes', 'sabado'];
    const DAY_NAMES = {
      'lunes':     'Lunes',
      'martes':    'Martes',
      'miercoles': 'Miércoles',
      'jueves':    'Jueves',
      'viernes':   'Viernes',
      'sabado':    'Sábado'
    };
    const DAY_COLORS = {
      'lunes':     '#FF0000',
      'martes':    '#00A2E8',
      'miercoles': '#FF7F27',
      'jueves':    '#FFFF00',
      'viernes':   '#00C853',
      'sabado':    '#A349A4'
    };
    
    function getDiaAyer() {
      const d = new Date().getDay(); // 0: Dom, 1: Lun, 2: Mar, 3: Mie, 4: Jue, 5: Vie, 6: Sab
      switch (d) {
        case 2: return 'lunes';       // Martes -> Lunes
        case 3: return 'martes';      // Miércoles -> Martes
        case 4: return 'miercoles';   // Jueves -> Miércoles
        case 5: return 'jueves';      // Viernes -> Jueves
        case 6: return 'viernes';     // Sábado -> Viernes
        case 0: return 'sabado';      // Domingo -> Sábado
        case 1: return 'lunes';       // Lunes -> Lunes (inicio de semana)
        default: return 'lunes';
      }
    }
    
    let activeDay    = getDiaAyer();
    let lastColorDay = getDiaAyer();
    let activeMode   = 'CosechaCampo'; // Por defecto Cosecha Campo
    let activePass   = 1;              // 1 o 2
    let paintTool    = 'cells';        // 'cells', 'valve100', 'pan'
    let isDrawing    = false;
    let isPanning    = false;
    let strokeChanged = false;
    let startPanX    = 0, startPanY = 0;
    let lastTouchDist = null;
    let toastTimer   = null;
    let lastToastTime = 0;
    
    window.USER_PERMISOS = null;

    function selectUsuarioLogin(u) {
      const userInp = document.getElementById('login_usuario');
      const passInp = document.getElementById('login_password');
      if (userInp) {
        userInp.value = u;
        if (window.$) $(userInp).trigger('change');
      }
      if (passInp) {
        passInp.value = '';
        if (window.$) $(passInp).trigger('change');
        passInp.focus();
      }
      if (window.Shiny && window.Shiny.setInputValue) {
        Shiny.setInputValue('login_usuario', u);
        Shiny.setInputValue('login_password', '');
      }
      const errBox = document.getElementById('login_error_msg');
      if (errBox) errBox.innerHTML = '';
    }

    function autofillLogin(u, p) {
      selectUsuarioLogin(u);
    }
    
    const MAX_UNDO = 20;
    let undoStack = [];
    
    function emptyCounts() {
      return { lunes: 0, martes: 0, miercoles: 0, jueves: 0, viernes: 0, sabado: 0 };
    }
    
    function createEmptySlot() {
      return {
        cells: new Array(10).fill(''),
        counts: emptyCounts(),
        cellFractions: {},
        hasExactPcts: false,
        align: 'start',
        ranges: [],
        kg: emptyCounts(),
        totalKg: 0
      };
    }
    
    VALVES_DATA.forEach(v => {
      v.slots = {
        'Proyeccion_1':     createEmptySlot(),
        'Proyeccion_2':     createEmptySlot(),
        'CosechaCampo_1':   createEmptySlot(),
        'CosechaCampo_2':   createEmptySlot(),
        'CosechaOficial_1': createEmptySlot(),
        'CosechaOficial_2': createEmptySlot()
      };
      v.slots['Cosecha_1'] = v.slots['CosechaCampo_1'];
      v.slots['Cosecha_2'] = v.slots['CosechaCampo_2'];
      if (v.poligono_rel && typeof v.poligono_rel === 'string' && v.poligono_rel.startsWith('[')) {
        try { v.pts = JSON.parse(v.poligono_rel); } catch(e) { v.pts = null; }
      } else {
        v.pts = null;
      }
    });
    
    function slotKey(mode, pass) {
      let m = mode;
      if (m === 'Cosecha') m = 'CosechaCampo';
      return m + '_' + pass;
    }
    
    function getActiveSlot(v) {
      return v.slots[slotKey(activeMode, activePass)];
    }
    
    function getOtherPassSlot(v) {
      const otherP = activePass === 1 ? 2 : 1;
      return v.slots[slotKey(activeMode, otherP)];
    }
    
    function detectSlotAlign(slot) {
      if (slot && slot.align) return slot.align;
      if (!slot || !slot.cells) return 'start';
      let firstIdx = -1, lastIdx = -1;
      for (let i = 0; i < 10; i++) {
        if (slot.cells[i]) {
          if (firstIdx === -1) firstIdx = i;
          lastIdx = i;
        }
      }
      if (firstIdx === -1) return 'start';
      if (lastIdx === 9 && firstIdx > 0) return 'end';
      if (firstIdx > 0 && lastIdx < 9) return 'center';
      return 'start';
    }

    function buildRangesFromSlot(slot) {
      if (!slot) return [];
      const counts = slot.counts || {};
      
      const orderedDays = [];
      const seen = new Set();
      if (slot.cells && Array.isArray(slot.cells)) {
        for (let c = 0; c < 10; c++) {
          const d = slot.cells[c];
          if (d && (Number(counts[d]) || 0) > 0 && !seen.has(d)) {
            seen.add(d);
            orderedDays.push(d);
          }
        }
      }
      for (let d of DAY_KEYS) {
        if ((Number(counts[d]) || 0) > 0 && !seen.has(d)) {
          seen.add(d);
          orderedDays.push(d);
        }
      }
      if (orderedDays.length === 0) return [];
      
      let totalPct = 0;
      for (let d of orderedDays) totalPct += (Number(counts[d]) || 0);
      totalPct = Math.min(100, Math.max(0, Math.round(totalPct * 100) / 100));
      if (totalPct <= 0) return [];
      
      const align = slot.align || detectSlotAlign(slot) || 'start';
      let startOffset = 0;
      if (align === 'end') startOffset = 100 - totalPct;
      else if (align === 'center') startOffset = (100 - totalPct) / 2;
      
      let curOffset = startOffset;
      const ranges = [];
      for (let d of orderedDays) {
        const p = Number(counts[d]) || 0;
        if (p <= 0) continue;
        const endOffset = Math.min(100, curOffset + p);
        ranges.push({ day: d, start: curOffset, end: endOffset, pct: p });
        curOffset = endOffset;
      }
      return ranges;
    }

    function getSlotRanges(slot) {
      if (slot && slot.ranges && Array.isArray(slot.ranges) && slot.ranges.length > 0) {
        let valid = true;
        for (let r of slot.ranges) {
          const cnt = Number(slot.counts && slot.counts[r.day]) || 0;
          if (cnt <= 0 || !r.pct || r.pct <= 0) { valid = false; break; }
        }
        if (valid) return slot.ranges;
      }
      return buildRangesFromSlot(slot);
    }

    function syncCellsFromRanges(slot) {
      if (!slot) return;
      if (!slot.cells) slot.cells = Array(10).fill('');
      slot.cells.fill('');
      slot.cellFractions = {};
      
      const ranges = slot.ranges || [];
      if (ranges.length === 0) {
        slot.hasExactPcts = false;
        return;
      }
      
      for (let c = 0; c < 10; c++) {
        const cellStart = c * 10;
        const cellEnd = (c + 1) * 10;
        
        let maxOverlap = 0;
        let dominantDay = '';
        let totalCellOverlap = 0;
        
        for (let r of ranges) {
          const oStart = Math.max(cellStart, r.start);
          const oEnd = Math.min(cellEnd, r.end);
          const overlap = Math.max(0, oEnd - oStart);
          if (overlap > 0.001) {
            totalCellOverlap += overlap;
            if (overlap > maxOverlap) {
              maxOverlap = overlap;
              dominantDay = r.day;
            }
          }
        }
        
        if (dominantDay && maxOverlap > 0) {
          slot.cells[c] = dominantDay;
          if (totalCellOverlap < 9.95) {
            const frac = Math.round((totalCellOverlap / 10) * 100) / 100;
            slot.cellFractions[c] = frac;
          }
        } else {
          slot.cells[c] = '';
        }
      }
      
      slot.hasExactPcts = Object.values(slot.counts || {}).some(p => (Number(p) || 0) % 10 !== 0);
    }

    function updateSlotDay(slot, targetDay, targetPct, direction = null) {
      if (!slot) return;
      if (!slot.counts) slot.counts = emptyCounts();
      if (!slot.cells) slot.cells = Array(10).fill('');
      if (!slot.cellFractions) slot.cellFractions = {};
      
      // 1. Rangos de los DEMÁS días (se respeta 100% su posición original)
      // OJO: leer currentRanges ANTES de cambiar slot.align para no alterar la orientación previa de los demás días
      const currentRanges = getSlotRanges(slot);
      const otherRanges = currentRanges.filter(r => r.day !== targetDay && (Number(slot.counts[r.day]) || 0) > 0 && r.pct > 0);
      
      const dir = direction || slot.align || 'start';
      slot.align = dir;
      
      let otherTotal = 0;
      for (let r of otherRanges) otherTotal += r.pct;
      otherTotal = Math.round(otherTotal * 100) / 100;
      
      const maxAvailable = Math.max(0, Math.round((100 - otherTotal) * 100) / 100);
      let pctToAssign = Math.min(Math.max(0, targetPct), maxAvailable);
      // Evitar franjas blancas por redondeo decimal de hectáreas (snap al 100% libre si la diferencia es mínima)
      if (Math.abs(pctToAssign - maxAvailable) < 0.15) {
        pctToAssign = maxAvailable;
      }
      const roundPct = Math.round(pctToAssign * 100) / 100;
      
      slot.counts[targetDay] = roundPct;
      
      if (roundPct <= 0) {
        slot.ranges = otherRanges;
        syncCellsFromRanges(slot);
        return;
      }
      
      // 2. Localizar intervalos LIBRES en la válvula
      otherRanges.sort((a, b) => a.start - b.start);
      const freeIntervals = [];
      let cursor = 0;
      for (let r of otherRanges) {
        if (r.start > cursor + 0.01) {
          freeIntervals.push({ start: cursor, end: r.start, len: r.start - cursor });
        }
        cursor = Math.max(cursor, r.end);
      }
      if (cursor < 100 - 0.01) {
        freeIntervals.push({ start: cursor, end: 100, len: 100 - cursor });
      }
      
      if (freeIntervals.length === 0) {
        slot.ranges = otherRanges;
        syncCellsFromRanges(slot);
        return;
      }
      
      // 3. Asignar el nuevo porcentaje en el espacio libre respetando la dirección
      const newSegments = [];
      if (Math.abs(roundPct - maxAvailable) < 0.01 && freeIntervals.length === 1) {
        // Asignación completa del espacio libre restante: ocupa exactamente todo el intervalo libre sin desfases ni líneas blancas
        newSegments.push({ day: targetDay, start: freeIntervals[0].start, end: freeIntervals[0].end, pct: freeIntervals[0].len });
      } else if (dir === 'end') {
        // Desde abajo / derecha hacia arriba
        let remaining = roundPct;
        for (let i = freeIntervals.length - 1; i >= 0 && remaining > 0.001; i--) {
          const fi = freeIntervals[i];
          const take = Math.min(remaining, fi.len);
          const s = fi.end - take;
          const e = fi.end;
          newSegments.push({ day: targetDay, start: s, end: e, pct: take });
          remaining -= take;
        }
      } else if (dir === 'center') {
        if (freeIntervals.length === 1) {
          const fi = freeIntervals[0];
          const offset = (fi.len - roundPct) / 2;
          const s = fi.start + Math.max(0, offset);
          const e = s + roundPct;
          newSegments.push({ day: targetDay, start: s, end: e, pct: roundPct });
        } else {
          let remaining = roundPct;
          for (let i = 0; i < freeIntervals.length && remaining > 0.001; i++) {
            const fi = freeIntervals[i];
            const take = Math.min(remaining, fi.len);
            newSegments.push({ day: targetDay, start: fi.start, end: fi.start + take, pct: take });
            remaining -= take;
          }
        }
      } else {
        // 'start' (Arriba / Izquierda): Llenar desde el inicio del espacio libre disponible
        let remaining = roundPct;
        for (let i = 0; i < freeIntervals.length && remaining > 0.001; i++) {
          const fi = freeIntervals[i];
          const take = Math.min(remaining, fi.len);
          newSegments.push({ day: targetDay, start: fi.start, end: fi.start + take, pct: take });
          remaining -= take;
        }
      }
      
      slot.ranges = otherRanges.concat(newSegments);
      slot.ranges.sort((a, b) => a.start - b.start);
      syncCellsFromRanges(slot);
    }

    function syncCellsFromCounts(slot, align = null) {
      if (!slot) return;
      if (align) slot.align = align;
      if (slot.ranges && slot.ranges.length > 0) {
        let valid = true;
        let rSum = 0;
        for (let r of slot.ranges) {
          const cnt = Number(slot.counts && slot.counts[r.day]) || 0;
          if (cnt <= 0 || !r.pct || r.pct <= 0) { valid = false; break; }
          rSum += r.pct;
        }
        let cSum = 0;
        for (let d of DAY_KEYS) cSum += (Number(slot.counts && slot.counts[d]) || 0);
        if (valid && Math.abs(rSum - cSum) < 0.1) {
          syncCellsFromRanges(slot);
          return;
        }
      }
      slot.ranges = buildRangesFromSlot(slot);
      syncCellsFromRanges(slot);
    }
    
    function recomputeValveSlotCounts(v, key) {
      const s = v.slots[key];
      if (!s.counts) s.counts = emptyCounts();
      if (!s.cellFractions) s.cellFractions = {};
      if (!s.kg) s.kg = emptyCounts();
      
      let sumCounts = 0;
      for (let d of DAY_KEYS) {
        s.counts[d] = Math.round((Number(s.counts[d]) || 0) * 100) / 100;
        sumCounts += s.counts[d];
      }
      
      // Solo si counts estaba en cero y hay celdas preexistentes, derivar desde celdas
      if (sumCounts === 0 && s.cells && s.cells.some(c => c !== '')) {
        for (let i = 0; i < 10; i++) {
          const d = s.cells[i];
          if (d && s.counts.hasOwnProperty(d)) {
            const frac = (s.cellFractions[i] !== undefined) ? s.cellFractions[i] : 1.0;
            s.counts[d] += frac * 10;
          }
        }
        for (let d of DAY_KEYS) {
          s.counts[d] = Math.round((s.counts[d] || 0) * 100) / 100;
        }
      }
      
      s.hasExactPcts = Object.values(s.counts).some(p => (Number(p) || 0) % 10 !== 0);
      
      let sumK = 0;
      for (let d of DAY_KEYS) {
        if ((s.counts[d] || 0) === 0) s.kg[d] = 0;
        sumK += (s.kg[d] || 0);
      }
      s.totalKg = Math.round(sumK);
    }

    function renderValveFills(c2d, v, slot, horiz) {
      if (!slot) return;
      const ranges = getSlotRanges(slot);
      if (!ranges || ranges.length === 0) return;
      
      for (let r of ranges) {
        if (!r.pct || r.pct <= 0.001) continue;
        const color = DAY_COLORS[r.day] || '#007AFF';
        c2d.fillStyle = color;
        const s = Math.max(0, Math.min(100, r.start));
        const e = Math.max(0, Math.min(100, r.end));
        const len = e - s;
        if (len <= 0) continue;
        
        if (horiz) {
          const y = v.y + (s / 100) * v.h;
          const h = (len / 100) * v.h;
          c2d.fillRect(v.x, y, v.w, h);
        } else {
          const x = v.x + (s / 100) * v.w;
          const w = (len / 100) * v.w;
          c2d.fillRect(x, v.y, w, v.h);
        }
      }
    }
    
    let hasUnsavedChanges = false;
    let previousSemana = null;

    function marcarCambiosPendientes(hayCambios) {
      hasUnsavedChanges = !!hayCambios;
      const btn = document.getElementById('btn_guardar_manual');
      if (btn) {
        btn.style.pointerEvents = 'auto';
        btn.style.opacity = '1';
        if (hasUnsavedChanges) {
          btn.style.setProperty('background', '#FF9500', 'important');
          btn.style.boxShadow = '0 2px 10px rgba(255, 149, 0, 0.45)';
          btn.innerHTML = '● Guardar Cambios';
          btn.title = 'Hay cambios pendientes de guardar';
        } else {
          btn.style.setProperty('background', '#007AFF', 'important');
          btn.style.boxShadow = 'none';
          btn.innerHTML = '💾 Guardar Ruta';
          btn.title = 'Todo sincronizado';
        }
      }
      if (window.Shiny && window.Shiny.setInputValue) {
        Shiny.setInputValue('hay_cambios_pendientes', hasUnsavedChanges, { priority: 'event' });
      }
    }
    
    function handleDayKgCommit(diaNombre, valStr, el, force = false) {
      if (activeMode === 'CosechaOficial') return;
      const rawVal = (valStr || '').replace(/[^0-9.]/g, '');
      const kgNum = Math.round(parseFloat(rawVal) || 0);
      
      const prevVal = el && el.dataset.lastVal !== undefined ? Math.round(parseFloat(el.dataset.lastVal) || 0) : null;
      if (!force && prevVal !== null && kgNum === prevVal) {
        return;
      }
      if (el) el.dataset.lastVal = String(kgNum);
      
      // Sincronizar inmediatamente trazos/áreas del canvas con Shiny antes de solicitar prorrateo
      syncStateToShiny(true);
      marcarCambiosPendientes(true);
      if (window.Shiny && window.Shiny.setInputValue) {
        Shiny.setInputValue('ingresar_kg_dia', {
          dia: diaNombre,
          kg: isNaN(kgNum) ? 0 : kgNum,
          modo: activeMode,
          pasada: activePass,
          ts: Date.now()
        }, { priority: 'event' });
      }
    }
    
    function handleDayKgKey(e, diaNombre) {
      if (e.key === 'Enter' || e.keyCode === 13) {
        e.preventDefault();
        e.stopPropagation();
        e.target.dataset.committedByEnter = '1';
        handleDayKgCommit(diaNombre, e.target.value, e.target, true);
        e.target.blur();
      }
    }
    
    function handleDayKgBlur(el, diaNombre) {
      if (el.dataset.committedByEnter === '1') {
        el.dataset.committedByEnter = '0';
        return;
      }
      handleDayKgCommit(diaNombre, el.value, el, false);
    }
    
    function toggleToolsMenu(e) {
      if (e) {
        e.preventDefault();
        e.stopPropagation();
      }
      const menu = document.getElementById('map-dropdown-menu');
      if (menu) {
        menu.classList.toggle('open');
      }
    }
    
    function closeToolsMenu() {
      const menu = document.getElementById('map-dropdown-menu');
      if (menu) {
        menu.classList.remove('open');
      }
    }
    
    function triggerResetDiaActivoModal() {
      if (activeMode === 'CosechaOficial') {
        showMapToast('🔒 No se puede borrar la Cosecha Oficial (datos oficiales de packing).');
        return;
      }
      const dayKey = (activeDay && activeDay !== 'borrar') ? activeDay : (lastColorDay || 'lunes');
      const dayLabel = DAY_NAMES[dayKey] || 'Lunes';
      if (window.Shiny && window.Shiny.setInputValue) {
        Shiny.setInputValue('btn_confirmar_limpiar_dia', {
          dia_key: dayKey,
          dia_nombre: dayLabel,
          ts: Date.now()
        }, { priority: 'event' });
      }
    }
    
    function triggerResetPassModal() {
      if (activeMode === 'CosechaOficial') {
        showMapToast('🔒 No se puede reiniciar la Cosecha Oficial (datos oficiales de packing).');
        return;
      }
      if (window.Shiny && window.Shiny.setInputValue) {
        Shiny.setInputValue('btn_confirmar_limpiar', Date.now(), { priority: 'event' });
      }
    }
    
    function updateMenuIndicator() {
      const pill = document.getElementById('active-tool-indicator');
      const toolIds = ['mode-drag-cells-btn', 'mode-valve100-btn', 'mode-area-btn', 'mode-eraser-btn', 'mode-eraser100-btn', 'mode-pan-btn'];
      toolIds.forEach(id => {
        const b = document.getElementById(id);
        if (b) b.classList.remove('active', 'active-eraser', 'active-pan');
      });
      
      if (paintTool === 'pan') {
        const b = document.getElementById('mode-pan-btn');
        if (b) b.classList.add('active', 'active-pan');
        if (pill) {
          pill.textContent = '✋ Mover Mapa';
          pill.style.background = 'rgba(142, 142, 147, 0.16)';
          pill.style.color = '#3A3A3C';
        }
      } else if (paintTool === 'eraser100') {
        const b = document.getElementById('mode-eraser100-btn');
        if (b) b.classList.add('active', 'active-eraser');
        if (pill) {
          const dName = DAY_NAMES[activeDay] || 'Día';
          pill.textContent = `🧹 Borrar 100% (${dName})`;
          pill.style.background = 'rgba(255, 59, 48, 0.14)';
          pill.style.color = '#D70015';
        }
      } else if (paintTool === 'eraser' || activeDay === 'borrar') {
        const b = document.getElementById('mode-eraser-btn');
        if (b) b.classList.add('active', 'active-eraser');
        if (pill) {
          const dName = DAY_NAMES[activeDay] || 'Día';
          pill.textContent = `🧹 Borrar 10% (${dName})`;
          pill.style.background = 'rgba(255, 59, 48, 0.14)';
          pill.style.color = '#D70015';
        }
      } else if (paintTool === 'valve100') {
        const b = document.getElementById('mode-valve100-btn');
        if (b) b.classList.add('active');
        if (pill) {
          pill.textContent = '⚡ Válvula 100%';
          pill.style.background = 'rgba(0, 122, 255, 0.12)';
          pill.style.color = '#007AFF';
        }
      } else if (paintTool === 'area') {
        const b = document.getElementById('mode-area-btn');
        if (b) b.classList.add('active');
        if (pill) {
          pill.textContent = '📐 Asignar Área';
          pill.style.background = 'rgba(0, 122, 255, 0.12)';
          pill.style.color = '#007AFF';
        }
      } else {
        const b = document.getElementById('mode-drag-cells-btn');
        if (b) b.classList.add('active');
        if (pill) {
          pill.textContent = '🖌️ Pintar 10%';
          pill.style.background = 'rgba(0, 122, 255, 0.12)';
          pill.style.color = '#007AFF';
        }
      }
    }
    
    function setDay(dayKey) {
      activeDay = dayKey;
      lastColorDay = dayKey;
      document.querySelectorAll('.kpi-card.day-card').forEach(c => c.classList.remove('active'));
      const card = document.getElementById('day-card-' + dayKey);
      if (card) card.classList.add('active');
      
      const lblDia = document.getElementById('lbl-dia-activo-menu');
      if (lblDia) lblDia.textContent = DAY_NAMES[dayKey] || dayKey;
      
      if (paintTool === 'pan' || paintTool === 'eraser' || paintTool === 'eraser100') {
        paintTool = 'cells';
        if (wrapper) wrapper.style.cursor = 'crosshair';
      }
      updateMenuIndicator();
    }
    
    function setMode(modeName) {
      activeMode = modeName;
      document.getElementById('btn-mode-proy').classList.toggle('active', modeName === 'Proyeccion');
      const btnCosCampo = document.getElementById('btn-mode-cos-campo');
      if (btnCosCampo) btnCosCampo.classList.toggle('active-cosecha', modeName === 'CosechaCampo' || modeName === 'Cosecha');
      const btnCosOficial = document.getElementById('btn-mode-cos-oficial');
      if (btnCosOficial) btnCosOficial.classList.toggle('active-oficial', modeName === 'CosechaOficial');
      
      const lockBanner = document.getElementById('official-lock-badge');
      if (lockBanner) {
        lockBanner.style.display = (modeName === 'CosechaOficial') ? 'flex' : 'none';
      }
      
      // Auto-activar día según modo: Proyección -> Lunes; Cosecha -> Día actual - 1
      if (modeName === 'Proyeccion') {
        setDay('lunes');
      } else {
        setDay(getDiaAyer());
      }
      
      // Actualizar etiqueta en las tarjetas de día según modo
      let txtLbl = 'KG PROGRAMADO:';
      let isReadOnly = false;
      const sugBadges = document.querySelectorAll('.badge-proy-sug');
      if (modeName === 'CosechaCampo' || modeName === 'Cosecha') {
        txtLbl = 'KG COSECHA CAMPO:';
        sugBadges.forEach(el => { el.style.display = 'none'; });
      } else if (modeName === 'CosechaOficial') {
        txtLbl = 'KG OFICIAL PACKING:';
        isReadOnly = true;
        sugBadges.forEach(el => { el.style.display = 'none'; });
      } else {
        txtLbl = 'KG PROGRAMADO:';
        sugBadges.forEach(el => { el.style.display = 'inline-block'; });
      }
      document.querySelectorAll('.lbl-modo-kg').forEach(el => { el.textContent = txtLbl; });
      document.querySelectorAll('.day-kg-input').forEach(el => { el.readOnly = isReadOnly; });

      const isOficial = (modeName === 'CosechaOficial');
      if (isOficial) {
        if (paintTool !== 'pan') paintTool = 'pan';
        if (wrapper) wrapper.style.cursor = 'grab';
      }
      document.querySelectorAll('.sidebar-drawing-tool').forEach(btn => {
        btn.disabled = isOficial;
        btn.style.opacity = isOficial ? '0.38' : '1';
        btn.style.pointerEvents = isOficial ? 'none' : 'auto';
      });
      const lockSidebarBadge = document.getElementById('sidebar-lock-indicator');
      if (lockSidebarBadge) lockSidebarBadge.style.display = isOficial ? 'inline-block' : 'none';
      const menuBox = document.getElementById('map-menu-box');
      if (menuBox) {
        menuBox.style.opacity = '1';
        menuBox.style.pointerEvents = 'auto';
      }
      updateMenuIndicator();
      
      if (window.Shiny && window.Shiny.setInputValue) {
        Shiny.setInputValue('modo_registro_activo', modeName, { priority: 'event' });
      }
      requestRender();
    }
    
    function copiarSugeridoADia(dia) {
      const b = document.getElementById('badge_proy_sug_' + dia);
      const inp = document.getElementById('input_kg_' + dia);
      if (b && inp && b.dataset.rawKg) {
        const kg = Math.round(Number(b.dataset.rawKg) || 0);
        if (kg > 0) {
          inp.value = kg.toLocaleString('en-US');
          inp.focus();
          showMapToast('✓ Copiado ' + kg.toLocaleString('en-US') + ' kg a ' + dia + '. Presiona Enter para prorratear.');
        }
      }
    }
    
    function setPass(passNum) {
      activePass = passNum;
      document.getElementById('btn-pass-1').classList.toggle('active', passNum === 1);
      document.getElementById('btn-pass-2').classList.toggle('active', passNum === 2);
      if (window.Shiny && window.Shiny.setInputValue) {
        Shiny.setInputValue('pasada_activa', passNum, { priority: 'event' });
      }
      requestRender();
    }
    
    function selectToolFromMenu(tool) {
      closeToolsMenu();
      paintTool = tool;
      if (wrapper) wrapper.style.cursor = tool === 'pan' ? 'grab' : 'crosshair';
      updateMenuIndicator();
    }
    
    function selectEraserFromMenu(type) {
      closeToolsMenu();
      paintTool = (type === 'valve100') ? 'eraser100' : 'eraser';
      if (wrapper) wrapper.style.cursor = 'crosshair';
      updateMenuIndicator();
      const dName = DAY_NAMES[activeDay] || activeDay;
      if (paintTool === 'eraser100') {
        showMapToast(`🧹 Borrador 100% activo: haz clic en una válvula para borrar todo lo de ${dName}.`);
      } else {
        showMapToast(`🧹 Borrador 10% activo: haz clic o arrastra para borrar cuadros de ${dName}.`);
      }
    }
    
    function guardarRutaCompleta() {
      if (!hasUnsavedChanges) {
        showMapToast('✓ No hay cambios pendientes por guardar');
        return;
      }
      
      // Asegurar que todos los inputs de kilos guarden su valor actual y pierdan foco
      // para evitar que un evento de blur posterior active cambios pendientes falsos
      document.querySelectorAll('.day-kg-input').forEach(inp => {
        const v = Math.round(parseFloat((inp.value || '').replace(/[^0-9.]/g, '')) || 0);
        inp.dataset.lastVal = String(v);
        inp.blur();
      });
      
      // Feedback visual inmediato: pasar a azul y mostrar estado de guardado
      marcarCambiosPendientes(false);
      const btn = document.getElementById('btn_guardar_manual');
      if (btn) {
        btn.innerHTML = '⏳ Guardando...';
        btn.style.pointerEvents = 'none';
        btn.style.opacity = '0.75';
      }
      
      showIosLoader('Guardando Ruta...', 'Sincronizando con Google Sheets y Disco');
      const kgs = {
        Lunes:     parseFloat(($('#input_kg_lunes').val() || '').replace(/[^0-9.]/g, '')) || 0,
        Martes:    parseFloat(($('#input_kg_martes').val() || '').replace(/[^0-9.]/g, '')) || 0,
        Miércoles: parseFloat(($('#input_kg_miercoles').val() || '').replace(/[^0-9.]/g, '')) || 0,
        Jueves:    parseFloat(($('#input_kg_jueves').val() || '').replace(/[^0-9.]/g, '')) || 0,
        Viernes:   parseFloat(($('#input_kg_viernes').val() || '').replace(/[^0-9.]/g, '')) || 0,
        Sábado:    parseFloat(($('#input_kg_sabado').val() || '').replace(/[^0-9.]/g, '')) || 0
      };
      if (window.Shiny && window.Shiny.setInputValue) {
        Shiny.setInputValue('guardar_ruta_con_kilos', {
          modo:   activeMode,
          pasada: activePass,
          kgs:    kgs,
          ts:     Date.now()
        }, { priority: 'event' });
      }
    }
    
    function exportarImagenReporte() {
      showIosLoader('Generando Imagen 4K Ultra-HD...', 'Componiendo plano vectorial y panel de métricas');
      
      setTimeout(function() {
        try {
          const semNum = $('#sel_semana').val() || 'Actual';
          let modoLabel = 'PROYECCIÓN';
          if (activeMode === 'CosechaCampo' || activeMode === 'Cosecha') modoLabel = 'COSECHA CAMPO';
          else if (activeMode === 'CosechaOficial') modoLabel = 'COSECHA OFICIAL (PACKING)';
          
          const pasadaLabel = activePass === 1 ? '1ª Pasada' : '2ª Pasada';
          const slotK = slotKey(activeMode, activePass);
          
          const mapInputIds = {
            lunes:     'input_kg_lunes',
            martes:    'input_kg_martes',
            miercoles: 'input_kg_miercoles',
            jueves:    'input_kg_jueves',
            viernes:   'input_kg_viernes',
            sabado:    'input_kg_sabado'
          };
          
          const dayStats = {
            lunes:     { name: 'Lunes',     icon: '🔴', color: DAY_COLORS.lunes,     ha: 0, kg: 0, valvs: 0 },
            martes:    { name: 'Martes',    icon: '🔵', color: DAY_COLORS.martes,    ha: 0, kg: 0, valvs: 0 },
            miercoles: { name: 'Miércoles', icon: '🟠', color: DAY_COLORS.miercoles, ha: 0, kg: 0, valvs: 0 },
            jueves:    { name: 'Jueves',    icon: '🟡', color: DAY_COLORS.jueves,    ha: 0, kg: 0, valvs: 0 },
            viernes:   { name: 'Viernes',   icon: '🟢', color: DAY_COLORS.viernes,   ha: 0, kg: 0, valvs: 0 },
            sabado:    { name: 'Sábado',    icon: '🟣', color: DAY_COLORS.sabado,    ha: 0, kg: 0, valvs: 0 }
          };
          
          let totalHaSemana = 0;
          let totalKgSemana = 0;
          let totalAreaFisica = 0;
          let totalValvulasVisibles = 0;
          
          let minX = Infinity, minY = Infinity, maxX = -Infinity, maxY = -Infinity;
          let paintedMinX = Infinity, paintedMinY = Infinity, paintedMaxX = -Infinity, paintedMaxY = -Infinity;
          let hasAnyPainted = false;
          
          const hasFilter = (filteredIds !== null && filteredIds.size < VALVES_DATA.length);
          
          for (let i = 0; i < VALVES_DATA.length; i++) {
            const v = VALVES_DATA[i];
            const isFiltered = (filteredIds === null || filteredIds.has(v.id));
            
            if (isFiltered) {
              totalValvulasVisibles++;
              totalAreaFisica += (Number(v.Área) || 0);
              
              minX = Math.min(minX, v.x);
              minY = Math.min(minY, v.y);
              maxX = Math.max(maxX, v.x + v.w);
              maxY = Math.max(maxY, v.y + v.h);
              
              const slot = (v.slots && v.slots[slotK]) ? v.slots[slotK] : createEmptySlot();
              if (!slot.counts) slot.counts = emptyCounts();
              let vHasPainted = false;
              
              for (let d of DAY_KEYS) {
                const cnt = slot.counts[d] || 0;
                if (cnt > 0) {
                  const ha = (Number(v.Área) || 0) * (cnt / 100);
                  const kg = (slot.kg && slot.kg[d]) ? slot.kg[d] : 0;
                  dayStats[d].ha += ha;
                  dayStats[d].kg += kg;
                  dayStats[d].valvs++;
                  totalHaSemana += ha;
                  totalKgSemana += kg;
                  vHasPainted = true;
                  hasAnyPainted = true;
                }
              }
              
              if (vHasPainted) {
                paintedMinX = Math.min(paintedMinX, v.x);
                paintedMinY = Math.min(paintedMinY, v.y);
                paintedMaxX = Math.max(paintedMaxX, v.x + v.w);
                paintedMaxY = Math.max(paintedMaxY, v.y + v.h);
              }
            }
          }
          
          // Reconciliar kilos con input boxes si el usuario los digitó directamente
          DAY_KEYS.forEach(d => {
            const inputEl = document.getElementById(mapInputIds[d]);
            if (inputEl) {
              const inVal = parseFloat((inputEl.value || '').replace(/[^0-9.]/g, '')) || 0;
              if (inVal > 0 && dayStats[d].kg === 0) {
                dayStats[d].kg = inVal;
              }
            }
          });
          
          totalKgSemana = DAY_KEYS.reduce((acc, d) => acc + (dayStats[d].kg || 0), 0);
          
          let bMinX = minX, bMinY = minY, bMaxX = maxX, bMaxY = maxY;
          if (filteredIds === null && hasAnyPainted) {
            bMinX = paintedMinX;
            bMinY = paintedMinY;
            bMaxX = paintedMaxX;
            bMaxY = paintedMaxY;
          }
          if (!isFinite(bMinX)) {
            bMinX = 0; bMinY = 0; bMaxX = 10000; bMaxY = 6000;
          }
          
          let bboxW = Math.max(bMaxX - bMinX, 600);
          let bboxH = Math.max(bMaxY - bMinY, 400);
          let bboxCX = (bMinX + bMaxX) / 2;
          let bboxCY = (bMinY + bMaxY) / 2;
          
          // Detección de selección de módulo para comportamiento de diseño inteligente
          const fM = $('#filtro_modulo').val();
          const visibleMods = [...new Set(VALVES_DATA.filter(v => filteredIds === null || filteredIds.has(v.id)).map(v => String(v.Módulo)))].sort((a,b) => Number(a) - Number(b));
          
          let modTitle = '';
          if (fM && fM.length) {
            const mods = Array.isArray(fM) ? fM : [fM];
            modTitle = mods.length === 1 ? `M ${mods[0]}` : `M ${mods.join(', ')}`;
          } else {
            if (visibleMods.length === 1) {
              modTitle = `M ${visibleMods[0]}`;
            } else if (visibleMods.length > 1 && visibleMods.length <= 4) {
              modTitle = `M ${visibleMods.join(', ')}`;
            } else {
              modTitle = 'FUNDO COMPLETO';
            }
          }
          const pasadaUpper = activePass === 1 ? '1ª PASADA' : '2ª PASADA';
          const titleText = `SEM ${semNum} - ${modTitle} - ${pasadaUpper}`;

          const isMod11 = (visibleMods.length === 1 && String(visibleMods[0]).trim() === '11') ||
                          (Array.isArray(fM) && fM.length === 1 && String(fM[0]).trim() === '11') ||
                          (String(fM).trim() === '11');
          const isMod12 = (visibleMods.length === 1 && String(visibleMods[0]).trim() === '12') ||
                          (Array.isArray(fM) && fM.length === 1 && String(fM[0]).trim() === '12') ||
                          (String(fM).trim() === '12');

          // Dimensiones del Cuadro de Resumen (Con tipografía amplia y legible)
          const tableW = 1280;
          const tableH = 660;

          const paddingFactor = 1.04;
          const headerY = 32;
          const headerH = 180; // Altura ejecutiva optimizada para logo ACP y tipografía de alto impacto
          const mapX = 36;
          const mapY = headerY + headerH + 24;

          // Estimación inicial de escala
          const maxMapW = 3328;
          const scaleW = maxMapW / (bboxW * paddingFactor);
          const mapContentH = Math.round(bboxH * scaleW);
          const mapH = Math.min(2300, Math.max(1200, mapContentH + 120));

          let fitScale = Math.min(maxMapW / (bboxW * paddingFactor), (mapH - 50) / (bboxH * paddingFactor));
          let contentW = Math.round(bboxW * fitScale);

          // Ancho adaptativo del mapa y lámina según módulo para reducir márgenes laterales vacíos
          let mapW = maxMapW;
          if (isMod12) {
            // Módulo 12: Reducir el espacio en blanco de la derecha manteniendo composición perfecta
            mapW = Math.max(2700, Math.min(2950, contentW + 140));
          } else if (isMod11) {
            // Módulo 11: Reducir márgenes laterales ajustando el ancho a las válvulas reales (~65px de margen a cada lado)
            mapW = Math.max(2540, Math.min(3328, Math.round(contentW + 130)));
          } else {
            // Demás casos (ej. M 1, 2, 3 o Módulo 1):
            // Reducir márgenes laterales ajustando el ancho del marco a las válvulas reales
            mapW = Math.max(2500, Math.min(3328, contentW + 140));
          }

          const outW = mapW + 72;

          // Ajuste fino final de fitScale y centrado con mapW definitivo
          fitScale = Math.min(mapW / (bboxW * paddingFactor), (mapH - 50) / (bboxH * paddingFactor));
          const mapPanX = mapX + mapW / 2 - bboxCX * fitScale;
          const mapPanY = mapY + mapH / 2 - bboxCY * fitScale;

          // Ubicación inteligente del Cuadro de Resumen
          let tableX, tableY, outH;

          if (isMod11) {
            // CASO MÓDULO 11:
            // Ubicar el cuadro resumen metido en la zona libre a la derecha:
            // - Esquina derecha alineada con la columna de la válvula 4 o 2 (M11-T1-V4 / M11-T1-V2)
            // - Nivel horizontal alineado con la válvula 42 (M11-T17-V42)
            let v4Right = null;
            let v42Top = null;

            for (let i = 0; i < VALVES_DATA.length; i++) {
              const v = VALVES_DATA[i];
              // Filtrar estrictamente solo válvulas pertenecientes al Módulo 11
              if (String(v.Módulo).trim() !== '11') continue;
              const cod = String(v.codigo || '');
              if (cod === 'M11-T1-V4' || cod === 'M11-T1-V2' || cod.endsWith('-T1-V4') || cod.endsWith('-T1-V2')) {
                const rx = (v.x + v.w) * fitScale + mapPanX;
                if (v4Right === null || rx > v4Right) v4Right = rx;
              }
              if (cod === 'M11-T17-V42' || cod.endsWith('-T17-V42')) {
                v42Top = v.y * fitScale + mapPanY;
              }
            }

            // Fallback seguro usando las coordenadas conocidas de M11 si no se hallaran por código
            if (v4Right === null) v4Right = 5848.0 * fitScale + mapPanX;
            if (v42Top === null) v42Top = 3515.1 * fitScale + mapPanY;

            tableX = Math.round(v4Right - tableW);
            tableY = Math.round(v42Top);

            // Asegurar que quede dentro del canvas visible con márgenes seguros
            tableX = Math.max(mapX + 24, Math.min(mapX + mapW - tableW - 24, tableX));
            tableY = Math.max(mapY + 24, Math.min(mapY + mapH - tableH - 24, tableY));
            outH = Math.max(mapY + mapH + 36, tableY + tableH + 36);
          } else {
            // Criterio inteligente: ¿cabe en la esquina superior izquierda dentro del mapa (ej. Módulo 12)?
            const candTL_X = mapX + 36;
            const candTL_Y = mapY + 36;
            let candTL_Free = true;

            for (let i = 0; i < VALVES_DATA.length; i++) {
              const v = VALVES_DATA[i];
              if (hasFilter && !filteredIds.has(v.id)) continue;
              const vx1 = v.x * fitScale + mapPanX;
              const vy1 = v.y * fitScale + mapPanY;
              const vx2 = (v.x + v.w) * fitScale + mapPanX;
              const vy2 = (v.y + v.h) * fitScale + mapPanY;

              // Verificación de colisión con margen de seguridad (25px)
              if (!(vx2 < candTL_X - 25 || vx1 > candTL_X + tableW + 25 || vy2 < candTL_Y - 25 || vy1 > candTL_Y + tableH + 25)) {
                candTL_Free = false;
                break;
              }
            }

            if (candTL_Free) {
              // Caso Módulo 12: Hay espacio libre arriba a la izquierda
              tableX = candTL_X;
              tableY = candTL_Y;
              outH = mapY + mapH + 36;
            } else {
              // Caso M 1, 2, 3 o Módulo 1: Ubicar debajo del plano
              // Alinear el inicio del cuadro resumen exactamente donde inicia la válvula 13 (columna izquierda de válvulas)
              const valvesLeftScreen = bMinX * fitScale + mapPanX;
              tableX = Math.max(mapX + 36, Math.min(mapX + mapW - tableW - 36, Math.round(valvesLeftScreen)));
              tableY = mapY + mapH + 24;
              outH = tableY + tableH + 36;
            }
          }

          const outCanvas = document.createElement('canvas');
          outCanvas.width = outW;
          outCanvas.height = outH;
          const octx = outCanvas.getContext('2d');

          function drawRoundRect(c, x, y, width, height, radius) {
            c.beginPath();
            c.moveTo(x + radius, y);
            c.lineTo(x + width - radius, y);
            c.quadraticCurveTo(x + width, y, x + width, y + radius);
            c.lineTo(x + width, y + height - radius);
            c.quadraticCurveTo(x + width, y + height, x + width - radius, y + height);
            c.lineTo(x + radius, y + height);
            c.quadraticCurveTo(x, y + height, x, y + height - radius);
            c.lineTo(x, y + radius);
            c.quadraticCurveTo(x, y, x + radius, y);
            c.closePath();
          }

          // Fondo general de la lámina
          octx.fillStyle = '#F8FAFC';
          octx.fillRect(0, 0, outW, outH);

          // -------------------------------------------------------------
          // 1. ENCABEZADO EJECUTIVO
          // -------------------------------------------------------------
          octx.save();
          octx.fillStyle = '#FFFFFF';
          octx.strokeStyle = 'rgba(0, 0, 0, 0.08)';
          octx.lineWidth = 2;
          drawRoundRect(octx, 36, headerY, outW - 72, headerH, 16);
          octx.fill();
          octx.stroke();

          // Renderizado del Logo ACP en el Encabezado Ejecutivo (Ajuste dinámico según relación de aspecto real)
          const logoH = 118;
          const logoAspect = (typeof acpLogoImg !== 'undefined' && acpLogoImg && acpLogoImg.naturalWidth && acpLogoImg.naturalHeight) 
                             ? (acpLogoImg.naturalWidth / acpLogoImg.naturalHeight) 
                             : 1.0;
          const logoW = Math.round(logoH * logoAspect);
          const logoX = 64;
          const logoY = headerY + Math.round((headerH - logoH) / 2);
          let textX = 64;

          if (typeof acpLogoImg !== 'undefined' && acpLogoImg && acpLogoImg.complete && acpLogoImg.naturalWidth > 0) {
            octx.drawImage(acpLogoImg, logoX, logoY, logoW, logoH);

            // Línea divisoria vertical sutil entre logo y textos
            const divX = logoX + logoW + 28;
            octx.strokeStyle = '#CBD5E1';
            octx.lineWidth = 2.5;
            octx.beginPath();
            octx.moveTo(divX, headerY + 24);
            octx.lineTo(divX, headerY + headerH - 24);
            octx.stroke();

            textX = divX + 28;
          }

          octx.textAlign = 'left';
          octx.textBaseline = 'top';

          octx.fillStyle = '#64748B';
          octx.font = 'bold 18px Inter, -apple-system, sans-serif';
          octx.fillText('AGRÍCOLA CERRO PRIETO • CAMPAÑA 2026', textX, headerY + 22);

          octx.fillStyle = '#0F172A';
          octx.font = 'bold 36px Inter, -apple-system, sans-serif';
          octx.fillText('PROGRAMA Y RUTA DE COSECHA DE ARÁNDANO', textX, headerY + 52);

          let filtroInfo = [];
          const fG = $('#filtro_grupo').val();
          const fT = $('#filtro_turno').val();
          const fVal = $('#filtro_valvula').val();
          const fV = $('#filtro_variedad').val();
          if (fG && fG.length) filtroInfo.push('Grupos: ' + (Array.isArray(fG) ? fG.join(', ') : fG));
          if (fT && fT.length) filtroInfo.push('Turnos: ' + (Array.isArray(fT) ? fT.join(', ') : fT));
          if (fVal && fVal.length) filtroInfo.push('Válvulas: ' + (Array.isArray(fVal) ? fVal.join(', ') : fVal));
          if (fV && fV.length) filtroInfo.push('Variedades: ' + (Array.isArray(fV) ? fV.join(', ') : fV));
          const txtExtra = filtroInfo.length > 0 ? (' • ' + filtroInfo.join(' • ')) : '';

          octx.fillStyle = '#0F172A';
          octx.font = 'bold 30px Inter, -apple-system, sans-serif';
          octx.fillText(titleText, textX, headerY + 112);

          if (txtExtra) {
            const tw = octx.measureText(titleText).width;
            octx.fillStyle = '#64748B';
            octx.font = '600 22px Inter, -apple-system, sans-serif';
            octx.fillText(txtExtra, textX + tw, headerY + 118);
          }
          
          // Badges del Encabezado (Derecha)
          octx.textAlign = 'right';
          octx.textBaseline = 'middle';
          
          const modeBg = activeMode === 'CosechaCampo' ? '#16A34A' : (activeMode === 'CosechaOficial' ? '#0F172A' : '#007AFF');
          const modeTxt = `${modoLabel} (${pasadaLabel})`;
          octx.font = 'bold 22px Inter, -apple-system, sans-serif';
          const mTextW = octx.measureText(modeTxt).width;
          const badgeH = 50;
          const badgeY = headerY + 28;
          
          octx.fillStyle = modeBg;
          drawRoundRect(octx, outW - 64 - mTextW - 44, badgeY, mTextW + 44, badgeH, 25);
          octx.fill();
          octx.fillStyle = '#FFFFFF';
          octx.fillText(modeTxt, outW - 64 - 22, badgeY + badgeH / 2);
          
          const nowStr = new Date().toLocaleString('es-PE', { dateStyle: 'short', timeStyle: 'short' });
          octx.fillStyle = '#1E293B';
          octx.font = 'bold 22px Inter, -apple-system, sans-serif';
          octx.fillText(`Exportado: ${nowStr} • ${totalValvulasVisibles} válvulas (${totalAreaFisica.toFixed(2)} ha física)`, outW - 64, headerY + 128);
          octx.restore();
          
          // -------------------------------------------------------------
          // 2. MARCO Y VIEWPORT DEL MAPA
          // -------------------------------------------------------------
          octx.save();
          octx.fillStyle = '#FFFFFF';
          octx.strokeStyle = 'rgba(0, 0, 0, 0.08)';
          octx.lineWidth = 2;
          drawRoundRect(octx, mapX, mapY, mapW, mapH, 16);
          octx.fill();
          octx.stroke();
          
          // Clip del Viewport del Mapa
          octx.save();
          drawRoundRect(octx, mapX, mapY, mapW, mapH, 16);
          octx.clip();
          
          octx.translate(mapPanX, mapPanY);
          octx.scale(fitScale, fitScale);
          
          // -------------------------------------------------------------
          // 3. DIBUJO DE POLÍGONOS DE VÁLVULAS
          // -------------------------------------------------------------
          for (let i = 0; i < VALVES_DATA.length; i++) {
            const v = VALVES_DATA[i];
            const isFiltered = (filteredIds === null || filteredIds.has(v.id));
            
            // Si hay filtro activo, OMITIR totalmente las válvulas de otros módulos (fondo limpio sin fantasmas)
            if (hasFilter && !isFiltered) continue;
            
            if ((v.x + v.w) * fitScale + mapPanX < mapX - 100 || v.x * fitScale + mapPanX > mapX + mapW + 100 ||
                (v.y + v.h) * fitScale + mapPanY < mapY - 100 || v.y * fitScale + mapPanY > mapY + mapH + 100) {
              continue;
            }
            
            octx.globalAlpha = 1.0;
            
            const activeSlot = (v.slots && v.slots[slotK]) ? v.slots[slotK] : createEmptySlot();
            const slot = activeSlot;
            const cells = activeSlot.cells;
            const otherSlot = (v.slots && v.slots[slotKey(activeMode, activePass === 1 ? 2 : 1)]) ? v.slots[slotKey(activeMode, activePass === 1 ? 2 : 1)] : createEmptySlot();
            
            let hasOtherPass = false;
            for (let d of DAY_KEYS) {
              if ((otherSlot.counts && otherSlot.counts[d] || 0) > 0) { hasOtherPass = true; break; }
            }
            
            let firstDay = cells[0];
            let isUniform = true;
            for (let k = 1; k < 10; k++) {
              if (cells[k] !== firstDay) { isUniform = false; break; }
            }
            
            const horiz = isHorizontalValve(v);
            const hasPoly = (v.pts && v.pts.length >= 3);
            
            if (hasPoly) {
              octx.save();
              octx.beginPath();
              octx.moveTo(v.x + v.pts[0][0] * v.w, v.y + v.pts[0][1] * v.h);
              for (let p = 1; p < v.pts.length; p++) {
                octx.lineTo(v.x + v.pts[p][0] * v.w, v.y + v.pts[p][1] * v.h);
              }
              octx.closePath();
              octx.clip();
            }
            
            octx.fillStyle = '#FFFFFF';
            octx.fillRect(v.x, v.y, v.w, v.h);
            
            renderValveFills(octx, v, activeSlot, horiz);
            
            // Rejilla de celdas
            octx.strokeStyle = 'rgba(0, 0, 0, 0.16)';
            octx.lineWidth = Math.max(0.8 / fitScale, 0.5);
            octx.beginPath();
            strokeValveGrid(octx, v, horiz);
            octx.stroke();
            
            // Borde exterior
            if (hasPoly) {
              octx.restore();
              octx.strokeStyle = '#0F172A';
              octx.lineWidth = Math.max(1.6 / fitScale, 1.0);
              octx.beginPath();
              octx.moveTo(v.x + v.pts[0][0] * v.w, v.y + v.pts[0][1] * v.h);
              for (let p = 1; p < v.pts.length; p++) {
                octx.lineTo(v.x + v.pts[p][0] * v.w, v.y + v.pts[p][1] * v.h);
              }
              octx.closePath();
              octx.stroke();
            } else {
              octx.strokeStyle = '#0F172A';
              octx.lineWidth = Math.max(1.6 / fitScale, 1.0);
              octx.strokeRect(v.x, v.y, v.w, v.h);
            }
            
            // Muesca de orientación (cabecera de riego)
            const isTriangle = hasPoly && v.pts.length === 3;
            const isSlopedTop = hasPoly && !isTriangle && (v.pts.some(p => p[0] < 0.20 && p[1] > 0.40) || (v.pts[0][0] < 0.05 && v.pts[0][1] > 0.40));
            if (!isSlopedTop && !isTriangle) {
              const notchW = v.w * 0.22;
              const notchH = Math.max(v.h * 0.08, 4 / fitScale);
              octx.fillStyle = '#0F172A';
              octx.fillRect(v.x + (v.w - notchW) / 2, v.y - notchH, notchW, notchH);
            }
            
            // Badge indicador de 2ª Pasada si aplica
            if (hasOtherPass && v.w >= 14) {
              const badgeText = activePass === 1 ? '2P' : '1P';
              const bw = Math.min(30 / fitScale, v.w * 0.30);
              const bh = Math.min(18 / fitScale, v.h * 0.24);
              octx.fillStyle = '#1E293B';
              octx.fillRect(v.x + v.w - bw - 2 / fitScale, v.y + 2 / fitScale, bw, bh);
              octx.fillStyle = '#FFFFFF';
              octx.font = 'bold ' + Math.max(8 / fitScale, bh * 0.72) + 'px Inter, sans-serif';
              octx.textAlign = 'center';
              octx.textBaseline = 'middle';
              octx.fillText(badgeText, v.x + v.w - bw / 2 - 2 / fitScale, v.y + 2 / fitScale + bh / 2);
            }
            
            // -----------------------------------------------------------
            // 4. TIPOGRAFÍA VECTORIAL DE ALTO CONTRASTE (NEGRO PURO SIN CONTORNO)
            // -----------------------------------------------------------
            if (v.w >= 12) {
              octx.save();
              octx.textAlign = 'center';
              octx.textBaseline = 'middle';
              
              const cx = isTriangle ? (v.x + v.w * 0.68) : (v.x + v.w * (isSlopedTop ? 0.60 : 0.50));
              const maxTextW = v.w * (isTriangle ? 0.62 : 0.94);
              const ry1 = isTriangle ? 0.58 : (isSlopedTop ? 0.42 : 0.36);
              const ry2 = isTriangle ? 0.82 : (isSlopedTop ? 0.74 : 0.68);
              
              // Tamaño NORMALIZADO uniforme para todas las válvulas del módulo
              // Basado en altura estándar (~100 unidades) para evitar saltos bruscos entre válvulas
              const normCodePx = Math.max(20, Math.min(34, Math.floor(100 * fitScale * 0.20)));
              const normAreaPx = Math.max(16, Math.min(27, Math.floor(normCodePx * 0.80)));
              
              let fSizeCode = normCodePx / fitScale;
              let fSizeHa   = normAreaPx / fitScale;
              
              // A. Código de Válvula (En formato T - V, sin prefijo M, negro puro, sin contorno)
              let codeLabel = v.codigo || '';
              if (v.Turno && v.Válvula) {
                codeLabel = 'T' + v.Turno + '-V' + v.Válvula;
              } else {
                codeLabel = codeLabel.replace(/^M[0-9]+-/, '');
              }
              octx.font = 'bold ' + fSizeCode + 'px Inter, -apple-system, sans-serif';
              let codeW = octx.measureText(codeLabel).width;
              if (codeW > maxTextW) {
                fSizeCode = Math.max(11 / fitScale, fSizeCode * (maxTextW / codeW));
                octx.font = 'bold ' + fSizeCode + 'px Inter, -apple-system, sans-serif';
              }
              if (fSizeCode > v.h * 0.38) {
                fSizeCode = v.h * 0.38;
                octx.font = 'bold ' + fSizeCode + 'px Inter, -apple-system, sans-serif';
              }
              const y1 = v.y + v.h * ry1;
              octx.fillStyle = '#000000';
              octx.fillText(codeLabel, cx, y1);
              
              // B. Área Física (En negro puro, sin contorno, normalizado, solo ha, sin kilos)
              const textHa = (v.Área !== undefined && v.Área !== null ? v.Área : '') + ' ha';
              octx.font = 'bold ' + fSizeHa + 'px Inter, -apple-system, sans-serif';
              let haW = octx.measureText(textHa).width;
              if (haW > maxTextW) {
                fSizeHa = Math.max(9 / fitScale, fSizeHa * (maxTextW / haW));
                octx.font = 'bold ' + fSizeHa + 'px Inter, -apple-system, sans-serif';
              }
              if (fSizeHa > v.h * 0.34) {
                fSizeHa = v.h * 0.34;
                octx.font = 'bold ' + fSizeHa + 'px Inter, -apple-system, sans-serif';
              }
              const y2 = v.y + v.h * ry2;
              octx.fillStyle = '#000000';
              octx.fillText(textHa, cx, y2);
              
              octx.restore();
            }
          }
          
          octx.restore(); // fin del clip del mapa
          octx.restore(); // fin contenedor mapa
          
          // -------------------------------------------------------------
          // 5. RENDERIZADO DEL CUADRO DE RESUMEN (TIPOGRAFÍA GRANDE Y ELEGANTE)
          // -------------------------------------------------------------
          const dayList = ['lunes', 'martes', 'miercoles', 'jueves', 'viernes', 'sabado'];
          const pctAvanceFiltro = totalAreaFisica > 0 ? ((totalHaSemana / totalAreaFisica) * 100).toFixed(1) : '0.0';
          
          // Contenedor general en FONDO BLANCO con borde nítido
          octx.save();
          octx.fillStyle = '#FFFFFF';
          octx.strokeStyle = '#94A3B8';
          octx.lineWidth = 2.5;
          drawRoundRect(octx, tableX, tableY, tableW, tableH, 18);
          octx.fill();
          octx.stroke();
          
          // A. Barra Superior del Resumen (Fondo Blanco con línea divisoria elegante)
          const barTopH = 78;
          octx.fillStyle = '#FFFFFF';
          drawRoundRect(octx, tableX, tableY, tableW, barTopH, 18);
          octx.fill();
          
          octx.strokeStyle = '#CBD5E1';
          octx.lineWidth = 2;
          octx.beginPath();
          octx.moveTo(tableX, tableY + barTopH);
          octx.lineTo(tableX + tableW, tableY + barTopH);
          octx.stroke();
          
          // Título a la izquierda
          octx.textAlign = 'left';
          octx.textBaseline = 'middle';
          octx.fillStyle = '#0F172A';
          octx.font = 'bold 26px Inter, -apple-system, sans-serif';
          octx.fillText('📊 RESUMEN GENERAL', tableX + 32, tableY + barTopH / 2);
          
          // Avance Físico a la derecha (grande y destacado en verde)
          octx.textAlign = 'right';
          octx.fillStyle = '#15803D';
          octx.font = 'bold 28px Inter, -apple-system, sans-serif';
          octx.fillText(`✓ ${pctAvanceFiltro}% avance físico (${totalAreaFisica.toFixed(2)} ha)`, tableX + tableW - 32, tableY + barTopH / 2);
          
          // B. Encabezado de Columnas de la Tabla (3 Columnas Limpias y Espaciosas)
          const thY = tableY + barTopH;
          const thH = 54;
          octx.fillStyle = '#F8FAFC';
          octx.fillRect(tableX, thY, tableW, thH);
          octx.strokeStyle = '#94A3B8';
          octx.lineWidth = 2;
          octx.beginPath();
          octx.moveTo(tableX, thY + thH);
          octx.lineTo(tableX + tableW, thY + thH);
          octx.stroke();
          
          const colX_Dia   = tableX + 32;
          const colX_Ha    = tableX + Math.round(tableW * 0.58);
          const colX_Kg    = tableX + tableW - 32;
          
          octx.textBaseline = 'middle';
          octx.fillStyle = '#334155';
          octx.font = 'bold 23px Inter, -apple-system, sans-serif';
          
          octx.textAlign = 'left';
          octx.fillText('DÍA DE COSECHA', colX_Dia, thY + thH / 2);
          
          octx.textAlign = 'right';
          octx.fillText('ÁREA (HA)', colX_Ha, thY + thH / 2);
          octx.fillText('KILOS (KG)', colX_Kg, thY + thH / 2);
          
          // C. Filas Diarias (Lunes a Sábado)
          const rowStartY = thY + thH;
          const footH = 76;
          const totalRowsH = tableH - barTopH - thH - footH;
          const rowH = totalRowsH / 6;
          
          dayList.forEach((dKey, idx) => {
            const ry = rowStartY + idx * rowH;
            const st = dayStats[dKey];
            
            // Fondo alternado sutil (blanco y casi blanco)
            octx.fillStyle = (idx % 2 === 0) ? '#FFFFFF' : '#F8FAFC';
            octx.fillRect(tableX, ry, tableW, rowH);
            
            // Línea separadora
            octx.strokeStyle = '#E2E8F0';
            octx.lineWidth = 1.5;
            octx.beginPath();
            octx.moveTo(tableX + 24, ry + rowH);
            octx.lineTo(tableX + tableW - 24, ry + rowH);
            octx.stroke();
            
            const textY = ry + rowH / 2;
            
            // Columna 1: Indicador y Nombre del Día (Letra grande y limpia)
            octx.textAlign = 'left';
            octx.fillStyle = st.color;
            octx.beginPath();
            octx.arc(colX_Dia + 12, textY, 13, 0, Math.PI * 2);
            octx.fill();
            octx.strokeStyle = 'rgba(0, 0, 0, 0.25)';
            octx.lineWidth = 1.5;
            octx.stroke();
            
            octx.fillStyle = '#0F172A';
            octx.font = 'bold 27px Inter, -apple-system, sans-serif';
            octx.fillText(st.name.toUpperCase(), colX_Dia + 36, textY);
            
            // Columna 2: Hectáreas
            octx.textAlign = 'right';
            octx.fillStyle = st.ha > 0 ? '#0F172A' : '#94A3B8';
            octx.font = 'bold 29px Inter, -apple-system, sans-serif';
            octx.fillText(st.ha > 0 ? `${st.ha.toFixed(2)} ha` : '0.00 ha', colX_Ha, textY);
            
            // Columna 3: Kilos
            octx.fillStyle = st.kg > 0 ? '#D97706' : '#94A3B8';
            octx.font = 'bold 29px Inter, -apple-system, sans-serif';
            octx.fillText(st.kg > 0 ? `${Math.round(st.kg).toLocaleString()} kg` : '0 kg', colX_Kg, textY);
          });
          
          // D. Fila de Total General (Footer en Fondo Blanco con línea superior fuerte)
          const footY = tableY + tableH - footH;
          octx.fillStyle = '#F8FAFC';
          drawRoundRect(octx, tableX, footY, tableW, footH, 18);
          octx.fill();
          
          octx.strokeStyle = '#0F172A';
          octx.lineWidth = 3;
          octx.beginPath();
          octx.moveTo(tableX, footY);
          octx.lineTo(tableX + tableW, footY);
          octx.stroke();
          
          const footTextY = footY + footH / 2;
          
          octx.textAlign = 'left';
          octx.fillStyle = '#0F172A';
          octx.font = 'bold 27px Inter, -apple-system, sans-serif';
          octx.fillText(`TOTAL (${totalValvulasVisibles} válvulas)`, colX_Dia, footTextY);
          
          octx.textAlign = 'right';
          octx.fillStyle = '#0284C7';
          octx.font = 'bold 32px Inter, -apple-system, sans-serif';
          octx.fillText(`${totalHaSemana.toFixed(2)} ha`, colX_Ha, footTextY);
          
          octx.fillStyle = '#D97706';
          octx.font = 'bold 32px Inter, -apple-system, sans-serif';
          octx.fillText(`${Math.round(totalKgSemana).toLocaleString()} kg`, colX_Kg, footTextY);
          octx.restore(); // Fin contenedor tabla
          
          // -------------------------------------------------------------
          // 6. DESCARGA AUTOMÁTICA DEL ARCHIVO PNG EN ALTA DEFINICIÓN
          // -------------------------------------------------------------
          outCanvas.toBlob(function(blob) {
            hideIosLoader();
            if (!blob) {
              showMapToast('⚠️ Error generando imagen HD');
              return;
            }
            const nowFile = new Date().toISOString().slice(0, 10);
            const fileMod = modTitle ? (modTitle.replace(/[^A-Za-z0-9]+/g, '_') + '_') : '';
            const fileName = `Ruta_Cosecha_${fileMod}Sem_${semNum}_Pasada${activePass}_${nowFile}.png`;
            const link = document.createElement('a');
            link.download = fileName;
            link.href = URL.createObjectURL(blob);
            document.body.appendChild(link);
            link.click();
            document.body.removeChild(link);
            setTimeout(() => URL.revokeObjectURL(link.href), 1500);
            showMapToast('📸 Imagen Ultra-HD descargada: ' + fileName);
          }, 'image/png');
          
        } catch (err) {
          hideIosLoader();
          console.error(err);
          showMapToast('⚠️ Error al exportar imagen: ' + err.message);
        }
      }, 70);
    }
    
    function showMapToast(msg) {
      const now = Date.now();
      if (now - lastToastTime < 900) return;
      lastToastTime = now;
      const el = document.getElementById('map-toast-alert');
      if (!el) return;
      el.textContent = msg;
      el.style.display = 'block';
      if (toastTimer) clearTimeout(toastTimer);
      toastTimer = setTimeout(() => { el.style.display = 'none'; }, 2400);
    }
    
    let filteredIds = null;
    
    let minX = 999999, minY = 999999, maxX = -999999, maxY = -999999;
    VALVES_DATA.forEach(v => {
      if (v.x < minX) minX = v.x;
      if (v.y < minY) minY = v.y;
      if ((v.x + v.w) > maxX) maxX = v.x + v.w;
      if ((v.y + v.h) > maxY) maxY = v.y + v.h;
    });
    
    const worldW = (maxX - minX) + 160;
    const worldH = (maxY - minY) + 160;
    
    let canvas, ctx, wrapper, tooltip;
    let scale = 1, panX = 0, panY = 0;
    let animationFrameId = null;
    
    function saveSnapshotForUndo() {
      const key = slotKey(activeMode, activePass);
      const snap = {
        key: key,
        items: VALVES_DATA.map(v => ({
          id: v.id,
          cells: v.slots[key].cells.slice(),
          counts: Object.assign({}, v.slots[key].counts),
          cellFractions: Object.assign({}, v.slots[key].cellFractions || {}),
          ranges: (v.slots[key].ranges || []).map(r => Object.assign({}, r)),
          hasExactPcts: !!v.slots[key].hasExactPcts,
          align: v.slots[key].align || 'start'
        }))
      };
      undoStack.push(snap);
      if (undoStack.length > MAX_UNDO) undoStack.shift();
      updateUndoButton();
    }
    
    function updateUndoButton() {
      const btn = document.getElementById('btn-undo');
      if (btn) btn.disabled = (undoStack.length === 0);
    }
    
    function undoLastStroke() {
      if (undoStack.length === 0) return;
      const snap = undoStack.pop();
      const mapSnap = new Map(snap.items.map(s => [s.id, s]));
      VALVES_DATA.forEach(v => {
        const item = mapSnap.get(v.id);
        if (item) {
          v.slots[snap.key].cells = item.cells.slice();
          if (item.counts) v.slots[snap.key].counts = Object.assign({}, item.counts);
          v.slots[snap.key].cellFractions = Object.assign({}, item.cellFractions || {});
          v.slots[snap.key].ranges = (item.ranges || []).map(r => Object.assign({}, r));
          v.slots[snap.key].hasExactPcts = !!item.hasExactPcts;
          v.slots[snap.key].align = item.align || 'start';
          recomputeValveSlotCounts(v, snap.key);
        }
      });
      updateUndoButton();
      requestRender();
      marcarCambiosPendientes(true);
      syncStateToShiny(true);
    }
    
    function initCanvas() {
      wrapper = document.getElementById('canvas-wrapper');
      canvas = document.getElementById('mapaCanvas');
      ctx = canvas.getContext('2d', { alpha: false });
      tooltip = document.getElementById('valve-tooltip');
      
      resizeCanvas();
      window.addEventListener('resize', resizeCanvas);
      if (window.ResizeObserver && wrapper) {
        const ro = new ResizeObserver(() => {
          resizeCanvas();
        });
        ro.observe(wrapper);
      }
      
      resetView();
      setupEvents();
      setMode('CosechaCampo');
      document.querySelectorAll('.day-kg-input').forEach(el => {
        el.dataset.lastVal = el.value ? String(Math.round(parseFloat(el.value.replace(/[^0-9.]/g, '')) || 0)) : '0';
      });
      const lblDia = document.getElementById('lbl-dia-activo-menu');
      if (lblDia) lblDia.textContent = DAY_NAMES[activeDay] || 'Lunes';
      marcarCambiosPendientes(false);
    }
    
    function resizeCanvas() {
      if (!wrapper || !canvas) return;
      const rect = wrapper.getBoundingClientRect();
      const dpr = window.devicePixelRatio || 1;
      canvas.width = rect.width * dpr;
      canvas.height = rect.height * dpr;
      requestRender();
    }
    
    function resetView() {
      if (!wrapper) return;
      const rect = wrapper.getBoundingClientRect();
      const padding = 25;
      const scaleX = (rect.width - padding * 2) / worldW;
      const scaleY = (rect.height - padding * 2) / worldH;
      scale = Math.min(scaleX, scaleY);
      
      panX = (rect.width - worldW * scale) / 2 - (minX - 80) * scale;
      panY = (rect.height - worldH * scale) / 2 - (minY - 80) * scale;
      requestRender();
    }
    
    function fitSelection() {
      if (!wrapper) return;
      const rect = wrapper.getBoundingClientRect();
      const padding = 35;
      
      let targetValves = VALVES_DATA;
      if (filteredIds && filteredIds.size > 0) {
        targetValves = VALVES_DATA.filter(v => filteredIds.has(v.id));
      }
      if (targetValves.length === 0) {
        resetView();
        return;
      }
      
      let bMinX = Infinity, bMaxX = -Infinity, bMinY = Infinity, bMaxY = -Infinity;
      for (let i = 0; i < targetValves.length; i++) {
        const v = targetValves[i];
        if (v.x < bMinX) bMinX = v.x;
        if ((v.x + v.w) > bMaxX) bMaxX = v.x + v.w;
        if (v.y < bMinY) bMinY = v.y;
        if ((v.y + v.h) > bMaxY) bMaxY = v.y + v.h;
      }
      
      bMinY -= 55;
      bMaxX += 85;
      
      const selW = (bMaxX - bMinX) + 40;
      const selH = (bMaxY - bMinY) + 40;
      
      const scaleX = (rect.width - padding * 2) / selW;
      const scaleY = (rect.height - padding * 2) / selH;
      scale = Math.min(Math.max(Math.min(scaleX, scaleY), 0.05), 3.0);
      
      panX = (rect.width - selW * scale) / 2 - (bMinX - 20) * scale;
      panY = (rect.height - selH * scale) / 2 - (bMinY - 20) * scale;
      requestRender();
    }
    
    function zoomIn() {
      const rect = wrapper.getBoundingClientRect();
      const cx = rect.width / 2, cy = rect.height / 2;
      const newScale = Math.min(scale * 1.3, 4.0);
      panX = cx - (cx - panX) * (newScale / scale);
      panY = cy - (cy - panY) * (newScale / scale);
      scale = newScale;
      requestRender();
    }
    
    function zoomOut() {
      const rect = wrapper.getBoundingClientRect();
      const cx = rect.width / 2, cy = rect.height / 2;
      const newScale = Math.max(scale / 1.3, 0.03);
      panX = cx - (cx - panX) * (newScale / scale);
      panY = cy - (cy - panY) * (newScale / scale);
      scale = newScale;
      requestRender();
    }
    
    function screenToWorld(sx, sy) {
      const rect = wrapper.getBoundingClientRect();
      const rx = sx - rect.left;
      const ry = sy - rect.top;
      return {
        x: (rx - panX) / scale,
        y: (ry - panY) / scale
      };
    }
    
    function isPointInValve(wx, wy, v) {
      if (wx < v.x || wx > (v.x + v.w) || wy < v.y || wy > (v.y + v.h)) return false;
      if (!v.pts || v.pts.length < 3) return true;
      let inside = false;
      const n = v.pts.length;
      for (let i = 0, j = n - 1; i < n; j = i++) {
        const xi = v.x + v.pts[i][0] * v.w, yi = v.y + v.pts[i][1] * v.h;
        const xj = v.x + v.pts[j][0] * v.w, yj = v.y + v.pts[j][1] * v.h;
        const intersect = ((yi > wy) !== (yj > wy)) && (wx < (xj - xi) * (wy - yi) / (yj - yi) + xi);
        if (intersect) inside = !inside;
      }
      return inside;
    }
    
    function findValveAt(wx, wy) {
      for (let i = 0; i < VALVES_DATA.length; i++) {
        const v = VALVES_DATA[i];
        if (filteredIds && !filteredIds.has(v.id)) continue;
        if (isPointInValve(wx, wy, v)) return v;
      }
      return null;
    }
    
    // Orientación del pintado: módulos 1, 2 y 3 se pintan en franjas HORIZONTALES (10 bandas apiladas
    // de arriba a abajo); el resto de módulos mantiene franjas VERTICALES (10 columnas de izquierda a derecha).
    // El orden de las 10 celdas (cells_str) no cambia, solo su geometría en pantalla.
    function isHorizontalValve(v) {
      const m = Number(v.Módulo);
      return m === 1 || m === 2 || m === 3;
    }
    
    function cellIndexAt(v, wx, wy) {
      const t = isHorizontalValve(v) ? (wy - v.y) / (v.h / 10) : (wx - v.x) / (v.w / 10);
      return Math.max(0, Math.min(9, Math.floor(t)));
    }
    
    function fillValveCell(c2d, v, c, horiz, fraction = 1.0, align = 'start') {
      const f = Math.max(0, Math.min(1.0, fraction));
      if (f <= 0) return;
      if (horiz) {
        const cellH = v.h / 10;
        const h = cellH * f;
        const y0 = v.y + c * cellH;
        let y = y0;
        if (align === 'end') y = y0 + cellH - h;
        else if (align === 'center') y = y0 + (cellH - h) / 2;
        c2d.fillRect(v.x, y, v.w, h);
      } else {
        const cellW = v.w / 10;
        const w = cellW * f;
        const x0 = v.x + c * cellW;
        let x = x0;
        if (align === 'end') x = x0 + cellW - w;
        else if (align === 'center') x = x0 + (cellW - w) / 2;
        c2d.fillRect(x, v.y, w, v.h);
      }
    }
    
    function strokeValveGrid(c2d, v, horiz) {
      for (let c = 1; c < 10; c++) {
        if (horiz) {
          const gy = v.y + c * (v.h / 10);
          c2d.moveTo(v.x, gy);
          c2d.lineTo(v.x + v.w, gy);
        } else {
          const gx = v.x + c * (v.w / 10);
          c2d.moveTo(gx, v.y);
          c2d.lineTo(gx, v.y + v.h);
        }
      }
    }
    
    // =========================================================================
    // REGLAS ESTRICTAS DE PINTADO:
    // 1. Solo se puede pintar sobre cuadros que están SIN COLOR ('').
    // 2. Una misma válvula NO puede tener 2 pasadas el mismo día en el mismo modo.
    // =========================================================================
    function applyPaint(v, wx, wy) {
      if (!v) return false;
      if (filteredIds && !filteredIds.has(v.id)) return false;
      
      if (window.USER_PERMISOS && !window.USER_PERMISOS.es_admin) {
        const modNum = Number(v.Módulo);
        if (!window.USER_PERMISOS.modulos.includes(modNum)) {
          showMapToast('🔒 Módulo ' + v.Módulo + ' es de solo lectura para tu usuario.');
          return false;
        }
      }
      
      if (activeMode === 'CosechaOficial') {
        showMapToast('🔒 La Cosecha Oficial es de sólo lectura (alimentada por Packing).');
        return false;
      }
      
      const key = slotKey(activeMode, activePass);
      const slot = v.slots[key];
      if (!slot.counts) slot.counts = emptyCounts();
      const cells = slot.cells;
      let changed = false;
      
      // -----------------------------------------------------------------------
      // MODO BORRADOR (🧹 Borrar cuadros o válvula completa del DÍA ACTIVO únicamente)
      // -----------------------------------------------------------------------
      if (paintTool === 'eraser' || paintTool === 'eraser100' || activeDay === 'borrar') {
        const targetDay = activeDay === 'borrar' ? (lastColorDay || 'lunes') : activeDay;
        const curPct = Number(slot.counts[targetDay]) || 0;
        
        // BORRAR AL 100% (Limpiar TODO lo que tenga la válvula del DÍA ACTIVO)
        if (paintTool === 'eraser100') {
          if (curPct <= 0) {
            if (!strokeChanged) {
              showMapToast(`ℹ️ V-${v.codigo} no tiene asignación en ${DAY_NAMES[targetDay]}.`);
            }
            return false;
          }
          saveSnapshotForUndo();
          updateSlotDay(slot, targetDay, 0);
          recomputeValveSlotCounts(v, key);
          changed = true;
          requestRender();
          marcarCambiosPendientes(true);
          syncStateToShiny(true);
          showMapToast(`✓ V-${v.codigo}: Se borró el 100% de ${DAY_NAMES[targetDay]} (${curPct}%). Los demás días quedaron intactos.`);
          return true;
        }
        
        // BORRAR AL 10% (Limpiar cuadros del DÍA ACTIVO)
        if (curPct <= 0) {
          if (!strokeChanged) {
            showMapToast(`ℹ️ V-${v.codigo} no tiene asignación en ${DAY_NAMES[targetDay]}.`);
          }
          return false;
        }
        
        const c = cellIndexAt(v, wx, wy);
        if (cells[c] !== '' && cells[c] !== targetDay) {
          if (!strokeChanged) {
            showMapToast(`🔒 Cuadro pertenece a ${DAY_NAMES[cells[c]]}. Solo puedes borrar cuadros de ${DAY_NAMES[targetDay]} (día activo).`);
          }
          return false;
        }
        
        saveSnapshotForUndo();
        const step = Math.min(10, curPct);
        const newPct = Math.round((curPct - step) * 100) / 100;
        updateSlotDay(slot, targetDay, newPct);
        recomputeValveSlotCounts(v, key);
        changed = true;
        marcarCambiosPendientes(true);
        requestRender();
        syncStateToShiny(true);
        showMapToast(`- ${step}% de ${DAY_NAMES[targetDay]} en V-${v.codigo} (queda ${slot.counts[targetDay]}%).`);
        return true;
      }
      
      // -----------------------------------------------------------------------
      // PINTAR DÍA ACTIVO
      // -----------------------------------------------------------------------
      const dayVal = activeDay;
      const otherSlot = getOtherPassSlot(v);
      const otherPassLabel = activePass === 1 ? '2ª Pasada' : '1ª Pasada';
      
      if ((otherSlot.counts[dayVal] || 0) > 0) {
        showMapToast(`⚠️ Válvula ${v.codigo} ya tiene ${DAY_NAMES[dayVal]} en la ${otherPassLabel}`);
        return false;
      }
      
      // Porcentaje ocupado por los DEMÁS días en esta pasada
      let otherDaysPct = 0;
      for (let d of DAY_KEYS) {
        if (d !== dayVal) {
          otherDaysPct += (Number(slot.counts[d]) || 0);
        }
      }
      otherDaysPct = Math.round(otherDaysPct * 100) / 100;
      const maxAvailablePct = Math.max(0, Math.round((100 - otherDaysPct) * 100) / 100);
      
      if (maxAvailablePct <= 0) {
        showMapToast(`🔒 Válvula ${v.codigo} ya está al 100% con otros días. Usa 🧹 Borrar para liberar espacio.`);
        return false;
      }
      
      if (paintTool === 'valve100') {
        const curDayPct = Number(slot.counts[dayVal]) || 0;
        if (curDayPct >= maxAvailablePct) {
          showMapToast(`🔒 Válvula ${v.codigo} ya está al 100% (tiene ${curDayPct}% en ${DAY_NAMES[dayVal]}).`);
          return false;
        }
        
        saveSnapshotForUndo();
        // Asignar el 100% de la diferencia disponible al día activo
        updateSlotDay(slot, dayVal, maxAvailablePct, slot.align || 'start');
        recomputeValveSlotCounts(v, key);
        changed = true;
        
        const totalHa = parseFloat(v.Área) || 0;
        const totalAsig = Number(slot.counts[dayVal]) || maxAvailablePct;
        const haAsig = ((totalAsig / 100) * totalHa).toFixed(2);
        showMapToast(`⚡ V-${v.codigo}: ${totalAsig}% (${haAsig} ha) asignado a ${DAY_NAMES[dayVal]}. Total: 100%.`);
      } else if (paintTool === 'cells') {
        const curDayPct = Number(slot.counts[dayVal]) || 0;
        const remainingForDay = Math.max(0, Math.round((maxAvailablePct - curDayPct) * 100) / 100);
        if (remainingForDay <= 0) {
          showMapToast(`🔒 Válvula ${v.codigo} ya no tiene más espacio libre disponible.`);
          return false;
        }
        
        const c = cellIndexAt(v, wx, wy);
        if (cells[c] !== '' && cells[c] !== dayVal) {
          const otherD = cells[c];
          if ((slot.cellFractions && slot.cellFractions[c]) === undefined) {
            showMapToast(`🔒 Cuadro pertenece a ${DAY_NAMES[otherD]}. Haz clic en el espacio libre para pintar ${DAY_NAMES[dayVal]}.`);
            return false;
          }
        }
        
        saveSnapshotForUndo();
        const step = Math.min(10, remainingForDay);
        const newDayPct = Math.round((curDayPct + step) * 100) / 100;
        updateSlotDay(slot, dayVal, newDayPct, slot.align || 'start');
        recomputeValveSlotCounts(v, key);
        changed = true;
        
        const totalHa = parseFloat(v.Área) || 0;
        const haAsig = ((slot.counts[dayVal] / 100) * totalHa).toFixed(2);
        showMapToast(`+${step}% a ${DAY_NAMES[dayVal]} en V-${v.codigo} (Total: ${slot.counts[dayVal]}% / ${haAsig} ha).`);
      }
      
      if (changed) {
        marcarCambiosPendientes(true);
        requestRender();
        syncStateToShiny(true);
      }
      return changed;
    }
    
    // =========================================================================
    // MODAL DE ASIGNACIÓN MANUAL DE ÁREA A VÁLVULA (iOS HIG)
    // =========================================================================
    let valveSeleccionadaModal = null;
    let targetPctModal = 0;
    let modalPaintDirection = 'start';

    function seleccionarDireccionModal(dir) {
      modalPaintDirection = dir;
      const container = document.getElementById('area-modal-direction-selector');
      if (container) {
        const btns = container.querySelectorAll('.dir-seg-btn');
        btns.forEach(b => {
          if (b.getAttribute('data-dir') === dir) {
            b.classList.add('active');
            const lbl = document.getElementById('area-modal-dir-selected-label');
            if (lbl) lbl.textContent = b.getAttribute('data-label') || dir;
          } else {
            b.classList.remove('active');
          }
        });
      }
      renderModalPreview();
    }

    function abrirModalAsignarArea(v) {
      if (!v) return;
      if (filteredIds && !filteredIds.has(v.id)) return;
      
      if (window.USER_PERMISOS && !window.USER_PERMISOS.es_admin) {
        const modNum = Number(v.Módulo);
        if (!window.USER_PERMISOS.modulos.includes(modNum)) {
          showMapToast('🔒 Módulo ' + v.Módulo + ' es de solo lectura para tu usuario.');
          return;
        }
      }
      
      if (activeMode === 'CosechaOficial') {
        showMapToast('🔒 La Cosecha Oficial es de sólo lectura (alimentada por Packing).');
        return;
      }
      
      const key = slotKey(activeMode, activePass);
      const slot = v.slots[key];
      const otherSlot = getOtherPassSlot(v);
      const otherPassLabel = activePass === 1 ? '2ª Pasada' : '1ª Pasada';
      
      if ((otherSlot.counts[activeDay] || 0) > 0) {
        showMapToast('⚠️ Válvula ' + v.codigo + ' ya tiene ' + DAY_NAMES[activeDay] + ' en la ' + otherPassLabel);
        return;
      }
      
      valveSeleccionadaModal = v;
      
      const tit = document.getElementById('area-modal-valvula-titulo');
      const sub = document.getElementById('area-modal-valvula-sub');
      const infoBox = document.getElementById('area-modal-info-box');
      const orientLbl = document.getElementById('area-modal-orientacion-lbl');
      const inpHa = document.getElementById('input_asignar_ha');
      const inpPct = document.getElementById('input_asignar_pct');
      
      const totalHa = parseFloat(v.Área) || 0;
      const isHoriz = isHorizontalValve(v);
      
      if (tit) tit.innerHTML = '📐 Válvula <strong>' + v.codigo + '</strong>';
      if (sub) sub.textContent = 'Módulo ' + v.Módulo + ' • Turno ' + v.Turno + ' • ' + (v.Variedad || '');
      if (orientLbl) {
        orientLbl.textContent = isHoriz ? 'Franjas Horizontales (↕)' : 'Franjas Verticales (↔)';
        orientLbl.style.color = isHoriz ? '#E66810' : '#007AFF';
      }
      
      const passTxt = activePass === 1 ? '1ª Pasada' : '2ª Pasada';
      const modoTxt = activeMode === 'Proyeccion' ? '📋 Proyección' : '🚜 Cosecha Campo';
      const dayColor = DAY_COLORS[activeDay] || '#007AFF';
      const dayName = DAY_NAMES[activeDay] || activeDay;
      
      targetPctModal = Number(slot.counts[activeDay]) || 0;
      const currentDayHa = ((targetPctModal / 100) * totalHa).toFixed(2);
      
      let otherDaysPct = 0;
      for (let d of DAY_KEYS) {
        if (d !== activeDay) otherDaysPct += (Number(slot.counts[d]) || 0);
      }
      otherDaysPct = Math.round(otherDaysPct * 100) / 100;
      const maxAvailablePct = Math.max(0, 100 - otherDaysPct);
      const maxAvailableHa = ((maxAvailablePct / 100) * totalHa).toFixed(2);
      
      if (infoBox) {
        infoBox.innerHTML = `
          <div style='display: flex; justify-content: space-between; margin-bottom: 4px;'>
            <span><strong>Área física total:</strong> ${totalHa.toFixed(2)} ha</span>
            <span style='font-weight: 700; color: ${dayColor};'>● ${dayName} (${modoTxt} - ${passTxt})</span>
          </div>
          <div style='display: flex; justify-content: space-between; color: #636366; font-size: 11.5px;'>
            <span>Asignado hoy: <strong>${targetPctModal.toFixed(2)}% (${currentDayHa} ha)</strong></span>
            <span>Espacio máx. libre: <strong>${maxAvailablePct.toFixed(2)}% (${maxAvailableHa} ha)</strong></span>
          </div>
        `;
      }
      
      if (inpHa) {
        inpHa.max = maxAvailableHa;
        inpHa.value = targetPctModal > 0 ? ((targetPctModal / 100) * totalHa).toFixed(2) : '';
      }
      if (inpPct) {
        inpPct.max = maxAvailablePct;
        inpPct.value = targetPctModal > 0 ? targetPctModal.toFixed(2) : '';
      }
      
      modalPaintDirection = slot.align || detectSlotAlign(slot) || 'start';
      
      const dirSelector = document.getElementById('area-modal-direction-selector');
      const dirLabel = document.getElementById('area-modal-dir-selected-label');
      
      const dirOptions = isHoriz ? [
        { key: 'start',  icon: '⬆', label: 'Arriba' },
        { key: 'center', icon: '⏺', label: 'Centro' },
        { key: 'end',    icon: '⬇', label: 'Abajo' }
      ] : [
        { key: 'start',  icon: '⬅', label: 'Izquierda' },
        { key: 'center', icon: '⏺', label: 'Centro' },
        { key: 'end',    icon: '➡', label: 'Derecha' }
      ];
      
      if (dirSelector) {
        dirSelector.innerHTML = '';
        dirOptions.forEach(opt => {
          const btn = document.createElement('button');
          btn.type = 'button';
          btn.className = 'dir-seg-btn' + (opt.key === modalPaintDirection ? ' active' : '');
          btn.setAttribute('data-dir', opt.key);
          btn.setAttribute('data-label', opt.label);
          btn.innerHTML = `<span style='font-size: 13.5px;'>${opt.icon}</span> <span>${opt.label}</span>`;
          btn.onclick = function() { seleccionarDireccionModal(opt.key); };
          dirSelector.appendChild(btn);
        });
      }
      const activeOpt = dirOptions.find(o => o.key === modalPaintDirection) || dirOptions[0];
      if (dirLabel) dirLabel.textContent = activeOpt.label;
      
      renderModalPreview();
      
      const modal = document.getElementById('modal-asignar-area');
      if (modal) {
        modal.classList.add('active');
        setTimeout(() => { if (inpHa) inpHa.focus(); }, 100);
      }
    }

    function cerrarModalAsignarArea() {
      const modal = document.getElementById('modal-asignar-area');
      if (modal) modal.classList.remove('active');
      ocultarAlertaModal();
      valveSeleccionadaModal = null;
    }

    function mostrarAlertaModal(msg) {
      const box = document.getElementById('area-modal-alerta-exceso');
      const txt = document.getElementById('area-modal-alerta-texto');
      if (box && txt) {
        txt.textContent = msg;
        box.style.display = 'flex';
      }
      showMapToast(msg);
    }

    function ocultarAlertaModal() {
      const box = document.getElementById('area-modal-alerta-exceso');
      if (box) box.style.display = 'none';
      const inpHa = document.getElementById('input_asignar_ha');
      const inpPct = document.getElementById('input_asignar_pct');
      if (inpHa) {
        inpHa.style.borderColor = '#007AFF';
        inpHa.style.boxShadow = 'none';
      }
      if (inpPct) {
        inpPct.style.borderColor = '#34C759';
        inpPct.style.boxShadow = 'none';
      }
    }

    function onInputModalArea(val) {
      if (!valveSeleccionadaModal) return;
      const totalHa = parseFloat(valveSeleccionadaModal.Área) || 0;
      const inpHa = document.getElementById('input_asignar_ha');
      const inpPct = document.getElementById('input_asignar_pct');
      
      const key = slotKey(activeMode, activePass);
      const slot = valveSeleccionadaModal.slots[key];
      let otherDaysPct = 0;
      for (let d of DAY_KEYS) {
        if (d !== activeDay) otherDaysPct += (Number(slot.counts[d]) || 0);
      }
      otherDaysPct = Math.round(otherDaysPct * 100) / 100;
      const maxAvailablePct = Math.max(0, 100 - otherDaysPct);
      const maxAvailableHa = (totalHa * (maxAvailablePct / 100));
      
      if (val === '' || val === null) {
        targetPctModal = 0;
        if (inpPct) inpPct.value = '';
        ocultarAlertaModal();
        renderModalPreview();
        return;
      }
      
      const valHa = parseFloat(val);
      if (isNaN(valHa) || valHa <= 0) {
        targetPctModal = 0;
        if (inpPct) inpPct.value = '';
        ocultarAlertaModal();
        renderModalPreview();
        return;
      }
      
      if (valHa > maxAvailableHa + 0.001) {
        targetPctModal = maxAvailablePct;
        if (inpHa) {
          inpHa.value = maxAvailableHa.toFixed(2);
          inpHa.style.borderColor = '#FF3B30';
          inpHa.style.boxShadow = '0 0 0 3px rgba(255, 59, 48, 0.2)';
        }
        if (inpPct) {
          inpPct.value = maxAvailablePct.toFixed(2);
        }
        mostrarAlertaModal(`⚠️ No puedes ingresar más de ${maxAvailableHa.toFixed(2)} ha (${maxAvailablePct.toFixed(2)}%), que es el espacio libre restante.`);
      } else {
        targetPctModal = totalHa > 0 ? Math.round((valHa / totalHa) * 10000) / 100 : 0;
        if (inpPct) inpPct.value = targetPctModal > 0 ? targetPctModal.toFixed(2) : '';
        ocultarAlertaModal();
      }
      
      renderModalPreview();
    }

    function onInputModalPct(val) {
      if (!valveSeleccionadaModal) return;
      const totalHa = parseFloat(valveSeleccionadaModal.Área) || 0;
      const inpPct = document.getElementById('input_asignar_pct');
      const inpHa = document.getElementById('input_asignar_ha');
      
      const key = slotKey(activeMode, activePass);
      const slot = valveSeleccionadaModal.slots[key];
      let otherDaysPct = 0;
      for (let d of DAY_KEYS) {
        if (d !== activeDay) otherDaysPct += (Number(slot.counts[d]) || 0);
      }
      otherDaysPct = Math.round(otherDaysPct * 100) / 100;
      const maxAvailablePct = Math.max(0, 100 - otherDaysPct);
      const maxAvailableHa = ((maxAvailablePct / 100) * totalHa).toFixed(2);
      
      if (val === '' || val === null) {
        targetPctModal = 0;
        if (inpHa) inpHa.value = '';
        ocultarAlertaModal();
        renderModalPreview();
        return;
      }
      
      const valPct = parseFloat(val);
      if (isNaN(valPct) || valPct <= 0) {
        targetPctModal = 0;
        if (inpHa) inpHa.value = '';
        ocultarAlertaModal();
        renderModalPreview();
        return;
      }
      
      if (valPct > maxAvailablePct) {
        targetPctModal = maxAvailablePct;
        if (inpPct) {
          inpPct.value = maxAvailablePct.toFixed(2);
          inpPct.style.borderColor = '#FF3B30';
          inpPct.style.boxShadow = '0 0 0 3px rgba(255, 59, 48, 0.2)';
        }
        if (inpHa) {
          inpHa.value = maxAvailableHa;
        }
        mostrarAlertaModal(`⚠️ No puedes asignar más de ${maxAvailablePct.toFixed(2)}% (${maxAvailableHa} ha). Espacio libre superado.`);
      } else {
        targetPctModal = Math.round(valPct * 100) / 100;
        if (inpHa) inpHa.value = targetPctModal > 0 ? ((targetPctModal / 100) * totalHa).toFixed(2) : '';
        ocultarAlertaModal();
      }
      
      renderModalPreview();
    }

    function fijarPorcentajeModal(pct) {
      if (!valveSeleccionadaModal) return;
      const totalHa = parseFloat(valveSeleccionadaModal.Área) || 0;
      const inpHa = document.getElementById('input_asignar_ha');
      const inpPct = document.getElementById('input_asignar_pct');
      
      const key = slotKey(activeMode, activePass);
      const slot = valveSeleccionadaModal.slots[key];
      let otherDaysPct = 0;
      for (let d of DAY_KEYS) {
        if (d !== activeDay) otherDaysPct += (Number(slot.counts[d]) || 0);
      }
      otherDaysPct = Math.round(otherDaysPct * 100) / 100;
      const maxAvailablePct = Math.max(0, 100 - otherDaysPct);
      const maxAvailableHa = ((maxAvailablePct / 100) * totalHa).toFixed(2);
      
      if (pct > maxAvailablePct) {
        targetPctModal = maxAvailablePct;
        if (inpPct) {
          inpPct.value = maxAvailablePct.toFixed(2);
          inpPct.style.borderColor = '#FF3B30';
          inpPct.style.boxShadow = '0 0 0 3px rgba(255, 59, 48, 0.2)';
        }
        if (inpHa) inpHa.value = maxAvailableHa;
        mostrarAlertaModal(`⚠️ El espacio libre disponible solo alcanza ${maxAvailablePct.toFixed(2)}% (${maxAvailableHa} ha).`);
      } else {
        targetPctModal = Math.round(pct * 100) / 100;
        if (inpHa) inpHa.value = targetPctModal > 0 ? ((targetPctModal / 100) * totalHa).toFixed(2) : '0';
        if (inpPct) inpPct.value = targetPctModal > 0 ? targetPctModal.toFixed(2) : '0';
        ocultarAlertaModal();
      }
      
      renderModalPreview();
    }

    function pasoCeldasModal(delta) {
      if (!valveSeleccionadaModal) return;
      fijarPorcentajeModal(Math.max(0, Math.min(100, targetPctModal + delta * 10)));
    }

    function renderModalPreview() {
      if (!valveSeleccionadaModal) return;
      const totalHa = parseFloat(valveSeleccionadaModal.Área) || 0;
      const key = slotKey(activeMode, activePass);
      const slot = valveSeleccionadaModal.slots[key];
      const bar = document.getElementById('area-modal-preview-bar');
      const calcBox = document.getElementById('area-modal-calculo-box');
      
      const dayVal = activeDay;
      const dayColor = DAY_COLORS[dayVal] || '#007AFF';
      const dayName = DAY_NAMES[dayVal] || dayVal;
      const isHoriz = isHorizontalValve(valveSeleccionadaModal);
      
      let otherDaysPct = 0;
      for (let d of DAY_KEYS) {
        if (d !== activeDay) otherDaysPct += (Number(slot.counts[d]) || 0);
      }
      otherDaysPct = Math.round(otherDaysPct * 100) / 100;
      
      const curPct = Math.min(Math.max(0, targetPctModal), Math.max(0, 100 - otherDaysPct));
      const curHa = (totalHa * (curPct / 100));
      const librePct = Math.max(0, 100 - curPct - otherDaysPct);
      const libreHa = (totalHa * (librePct / 100));
      
      if (bar) {
        bar.innerHTML = '';
        
        const tempSlot = {
          counts: Object.assign({}, slot.counts),
          ranges: (slot.ranges || []).map(r => Object.assign({}, r)),
          cells: Array(10).fill(''),
          cellFractions: {},
          align: slot.align || 'start'
        };
        updateSlotDay(tempSlot, dayVal, curPct, modalPaintDirection);
        const tempRanges = getSlotRanges(tempSlot);
        
        // Renderizar los 10 cuadros físicos
        for (let i = 0; i < 10; i++) {
          const cellDiv = document.createElement('div');
          cellDiv.style.flex = '1';
          cellDiv.style.height = '100%';
          cellDiv.style.position = 'relative';
          cellDiv.style.borderRight = (i < 9) ? '1px solid #D1D1D6' : 'none';
          cellDiv.style.display = 'flex';
          cellDiv.style.alignItems = 'center';
          cellDiv.style.justifyContent = 'center';
          cellDiv.style.fontSize = '10px';
          cellDiv.style.fontWeight = '700';
          cellDiv.style.boxSizing = 'border-box';
          
          const cellStart = i * 10;
          const cellEnd = (i + 1) * 10;
          
          // Encontrar todos los rangos que intersectan este cuadro
          const cellSegs = [];
          for (let r of tempRanges) {
            const oStart = Math.max(cellStart, r.start);
            const oEnd = Math.min(cellEnd, r.end);
            if (oEnd > oStart + 0.001) {
              cellSegs.push({
                day: r.day,
                start: (oStart - cellStart) * 10,
                end: (oEnd - cellStart) * 10,
                color: DAY_COLORS[r.day] || '#007AFF',
                pct: oEnd - oStart
              });
            }
          }
          cellSegs.sort((a, b) => a.start - b.start);
          
          // Rellenar espacios libres dentro del cuadro con blanco (#FFFFFF)
          const fullSegs = [];
          let curPos = 0;
          for (let s of cellSegs) {
            if (s.start > curPos + 0.01) {
              fullSegs.push({ start: curPos, end: s.start, color: '#FFFFFF', isFree: true, pct: s.start - curPos });
            }
            fullSegs.push(s);
            curPos = Math.max(curPos, s.end);
          }
          if (curPos < 100 - 0.01) {
            fullSegs.push({ start: curPos, end: 100, color: '#FFFFFF', isFree: true, pct: 100 - curPos });
          }
          
          if (fullSegs.length === 0 || (fullSegs.length === 1 && fullSegs[0].isFree)) {
            cellDiv.style.background = '#FFFFFF';
            cellDiv.style.color = '#C7C7CC';
            cellDiv.textContent = `${i + 1}`;
            cellDiv.title = `Cuadro ${i + 1}: Libre`;
          } else if (fullSegs.length === 1 && !fullSegs[0].isFree) {
            const s = fullSegs[0];
            cellDiv.style.background = s.color;
            cellDiv.style.color = '#FFFFFF';
            cellDiv.textContent = DAY_NAMES[s.day] ? DAY_NAMES[s.day].substring(0, 3) : '';
            if (s.day !== dayVal) cellDiv.style.opacity = '0.85';
            cellDiv.title = `Cuadro ${i + 1}: ${DAY_NAMES[s.day]} (100%)`;
          } else {
            // Múltiples segmentos o segmento parcial en el cuadro
            const stops = [];
            for (let s of fullSegs) {
              stops.push(`${s.color} ${s.start.toFixed(1)}%`);
              stops.push(`${s.color} ${s.end.toFixed(1)}%`);
            }
            cellDiv.style.background = `linear-gradient(to right, ${stops.join(', ')})`;
            
            // Texto y tooltip informativo
            const curDaySeg = cellSegs.find(s => s.day === dayVal);
            if (curDaySeg) {
              cellDiv.style.color = '#1C1C1E';
              cellDiv.textContent = `${curDaySeg.pct.toFixed(1)}%`;
            } else if (cellSegs.length > 0) {
              cellDiv.style.color = '#1C1C1E';
              cellDiv.textContent = `${cellSegs[0].pct.toFixed(1)}%`;
            } else {
              cellDiv.style.color = '#C7C7CC';
              cellDiv.textContent = `${i + 1}`;
            }
            
            const titles = cellSegs.map(s => `${DAY_NAMES[s.day] || s.day} (${s.pct.toFixed(1)}%)`);
            const freeSeg = fullSegs.find(s => s.isFree);
            if (freeSeg) titles.push(`Libre (${freeSeg.pct.toFixed(1)}%)`);
            cellDiv.title = `Cuadro ${i + 1}: ${titles.join(', ')}`;
          }
          bar.appendChild(cellDiv);
        }
      }
      
      if (calcBox) {
        calcBox.innerHTML = `
          <div style='display: flex; justify-content: space-between; margin-bottom: 4px;'>
            <span><strong style='color: ${dayColor};'>● Asignar a ${dayName}:</strong> ${curPct.toFixed(2)}%</span>
            <strong style='color: ${dayColor};'>${curHa.toFixed(2)} ha</strong>
          </div>
          <div style='display: flex; justify-content: space-between; color: #16A34A;'>
            <span><strong>○ Diferencia Libre (no cosechada):</strong> ${librePct.toFixed(2)}%</span>
            <strong>${libreHa.toFixed(2)} ha</strong>
          </div>
        `;
      }
    }

    function aplicarAsignarArea() {
      if (!valveSeleccionadaModal) return;
      const v = valveSeleccionadaModal;
      const totalHa = parseFloat(v.Área) || 0;
      const key = slotKey(activeMode, activePass);
      const slot = v.slots[key];
      const dayVal = activeDay;
      const isHoriz = isHorizontalValve(v);
      
      saveSnapshotForUndo();
      
      let otherDaysPct = 0;
      for (let d of DAY_KEYS) {
        if (d !== dayVal) otherDaysPct += (Number(slot.counts[d]) || 0);
      }
      otherDaysPct = Math.round(otherDaysPct * 100) / 100;
      const maxPct = Math.max(0, 100 - otherDaysPct);
      
      const inpHa = document.getElementById('input_asignar_ha');
      const inpPct = document.getElementById('input_asignar_pct');
      let pct = targetPctModal;
      if (inpPct && inpPct.value !== '' && !isNaN(parseFloat(inpPct.value))) {
        pct = parseFloat(inpPct.value);
      } else if (inpHa && inpHa.value !== '' && !isNaN(parseFloat(inpHa.value)) && totalHa > 0) {
        pct = (parseFloat(inpHa.value) / totalHa) * 100;
      }
      pct = Math.min(Math.max(0, pct), maxPct);
      if (Math.abs(pct - maxPct) < 0.15) {
        pct = maxPct;
      }
      pct = Math.round(pct * 100) / 100;
      
      const oldPct = Number(slot.counts[dayVal]) || 0;
      if (!slot.kg) slot.kg = emptyCounts();
      if (oldPct > 0 && (slot.kg[dayVal] || 0) > 0 && pct < oldPct) {
        slot.kg[dayVal] = Math.round((slot.kg[dayVal] * pct) / oldPct);
      } else if (pct === 0) {
        slot.kg[dayVal] = 0;
      }
      
      updateSlotDay(slot, dayVal, pct, modalPaintDirection);
      recomputeValveSlotCounts(v, key);
      updateUndoButton();
      requestRender();
      marcarCambiosPendientes(true);
      syncStateToShiny(true);
      
      const haAsig = ((pct / 100) * totalHa).toFixed(2);
      const haLib = (((100 - pct - otherDaysPct) / 100) * totalHa).toFixed(2);
      const dirText = modalPaintDirection === 'start' ? (isHoriz ? 'Arriba' : 'Izquierda') :
                      (modalPaintDirection === 'end' ? (isHoriz ? 'Abajo' : 'Derecha') : 'Centro');
      showMapToast(`✓ V-${v.codigo}: ${pct}% (${haAsig} ha) asignado a ${DAY_NAMES[dayVal]} desde ${dirText}. Libre: ${haLib} ha.`);
      
      cerrarModalAsignarArea();
    }
    
    function setupEvents() {
      document.addEventListener('click', function(e) {
        const menuBox = document.getElementById('map-menu-box');
        if (menuBox && !menuBox.contains(e.target)) {
          closeToolsMenu();
        }
      });

      window.addEventListener('keydown', function(e) {
        if ((e.ctrlKey || e.metaKey) && e.key.toLowerCase() === 'z') {
          e.preventDefault();
          undoLastStroke();
        }
      });

      wrapper.addEventListener('wheel', function(e) {
        e.preventDefault();
        closeToolsMenu();
        const rect = wrapper.getBoundingClientRect();
        const mouseX = e.clientX - rect.left;
        const mouseY = e.clientY - rect.top;
        
        const zoomFactor = e.deltaY < 0 ? 1.15 : 0.87;
        const newScale = Math.min(Math.max(scale * zoomFactor, 0.03), 4.0);
        
        panX = mouseX - (mouseX - panX) * (newScale / scale);
        panY = mouseY - (mouseY - panY) * (newScale / scale);
        scale = newScale;
        requestRender();
      }, { passive: false });
      
      wrapper.addEventListener('mousedown', function(e) {
        const menuBox = document.getElementById('map-menu-box');
        if (menuBox && menuBox.contains(e.target)) return;
        closeToolsMenu();
        
        if (e.button === 2 || e.shiftKey || paintTool === 'pan') {
          isPanning = true;
          startPanX = e.clientX - panX;
          startPanY = e.clientY - panY;
          wrapper.style.cursor = 'grabbing';
        } else if (e.button === 0) {
          const pos = screenToWorld(e.clientX, e.clientY);
          const v = findValveAt(pos.x, pos.y);
          if (paintTool === 'area') {
            if (v) abrirModalAsignarArea(v);
            return;
          }
          saveSnapshotForUndo();
          isDrawing = true;
          strokeChanged = false;
          if (v && applyPaint(v, pos.x, pos.y)) {
            strokeChanged = true;
          }
        }
      });
      
      wrapper.addEventListener('mousemove', function(e) {
        if (isPanning) {
          panX = e.clientX - startPanX;
          panY = e.clientY - startPanY;
          requestRender();
          return;
        }
        
        const pos = screenToWorld(e.clientX, e.clientY);
        const v = findValveAt(pos.x, pos.y);
        
        if (isDrawing) {
          if (v && applyPaint(v, pos.x, pos.y)) {
            strokeChanged = true;
            syncStateToShiny(false);
          }
        } else {
          updateTooltip(v, e.clientX, e.clientY);
        }
      });
      
      window.addEventListener('mouseup', function() {
        if (isDrawing) {
          isDrawing = false;
          if (strokeChanged) {
            marcarCambiosPendientes(true);
            syncStateToShiny(true);
          } else {
            undoStack.pop();
            updateUndoButton();
          }
        }
        if (isPanning) {
          isPanning = false;
          if (wrapper) wrapper.style.cursor = paintTool === 'pan' ? 'grab' : 'crosshair';
        }
      });

      wrapper.addEventListener('touchstart', function(e) {
        const menuBox = document.getElementById('map-menu-box');
        if (menuBox && menuBox.contains(e.target)) return;
        
        e.preventDefault();
        closeToolsMenu();
        tooltip.style.display = 'none';
        
        if (e.touches.length === 2) {
          isDrawing = false;
          isPanning = true;
          const t1 = e.touches[0], t2 = e.touches[1];
          const midX = (t1.clientX + t2.clientX) / 2;
          const midY = (t1.clientY + t2.clientY) / 2;
          startPanX = midX - panX;
          startPanY = midY - panY;
          lastTouchDist = Math.hypot(t2.clientX - t1.clientX, t2.clientY - t1.clientY);
        } else if (e.touches.length === 1) {
          const t = e.touches[0];
          if (paintTool === 'pan') {
            isPanning = true;
            startPanX = t.clientX - panX;
            startPanY = t.clientY - panY;
          } else if (paintTool === 'area') {
            const pos = screenToWorld(t.clientX, t.clientY);
            const v = findValveAt(pos.x, pos.y);
            if (v) abrirModalAsignarArea(v);
          } else {
            const pos = screenToWorld(t.clientX, t.clientY);
            const v = findValveAt(pos.x, pos.y);
            saveSnapshotForUndo();
            isDrawing = true;
            strokeChanged = false;
            if (v && applyPaint(v, pos.x, pos.y)) {
              strokeChanged = true;
            }
          }
        }
      }, { passive: false });

      wrapper.addEventListener('touchmove', function(e) {
        const menuBox = document.getElementById('map-menu-box');
        if (menuBox && menuBox.contains(e.target)) return;
        
        e.preventDefault();
        if (e.touches.length === 2) {
          const t1 = e.touches[0], t2 = e.touches[1];
          const midX = (t1.clientX + t2.clientX) / 2;
          const midY = (t1.clientY + t2.clientY) / 2;
          const dist = Math.hypot(t2.clientX - t1.clientX, t2.clientY - t1.clientY);
          
          const rect = wrapper.getBoundingClientRect();
          const localMidX = midX - rect.left;
          const localMidY = midY - rect.top;
          
          if (lastTouchDist && lastTouchDist > 10) {
            const zoomFactor = dist / lastTouchDist;
            const newScale = Math.min(Math.max(scale * zoomFactor, 0.03), 4.0);
            panX = localMidX - (localMidX - panX) * (newScale / scale);
            panY = localMidY - (localMidY - panY) * (newScale / scale);
            scale = newScale;
          }
          lastTouchDist = dist;
          requestRender();
        } else if (e.touches.length === 1) {
          const t = e.touches[0];
          if (isPanning) {
            panX = t.clientX - startPanX;
            panY = t.clientY - startPanY;
            requestRender();
          } else if (isDrawing) {
            const pos = screenToWorld(t.clientX, t.clientY);
            const v = findValveAt(pos.x, pos.y);
            if (v && applyPaint(v, pos.x, pos.y)) {
              strokeChanged = true;
              syncStateToShiny(false);
            }
          }
        }
      }, { passive: false });

      wrapper.addEventListener('touchend', function(e) {
        if (e.touches.length === 0) {
          if (isDrawing) {
            isDrawing = false;
            if (strokeChanged) {
              marcarCambiosPendientes(true);
              syncStateToShiny(true);
            } else {
              undoStack.pop();
              updateUndoButton();
            }
          }
          isPanning = false;
          lastTouchDist = null;
        }
      });
      
      wrapper.addEventListener('mouseleave', function() {
        tooltip.style.display = 'none';
      });
      
      wrapper.addEventListener('contextmenu', function(e) {
        e.preventDefault();
        // Anticlick (clic derecho) queda 100% reservado para arrastrar/mover el mapa sin abrir modales
      });
      
      wrapper.addEventListener('dblclick', function(e) {
        e.preventDefault();
        const pos = screenToWorld(e.clientX, e.clientY);
        const v = findValveAt(pos.x, pos.y);
        if (v) {
          if (strokeChanged) {
            undoLastStroke();
          }
          abrirModalAsignarArea(v);
        }
      });
    }
    
    function formatPassTooltip(v, slotObj, labelPasada) {
      let totalPct = 0;
      let totalKg  = 0;
      let rows = [];
      for (let d of DAY_KEYS) {
        const cnt = slotObj.counts[d] || 0;
        const kgD = slotObj.kg[d] || 0;
        if (cnt > 0) {
          totalPct += cnt;
          totalKg  += kgD;
          const haD = ((v.Área * cnt) / 100).toFixed(2);
          const kgTxt = kgD > 0 ? ` • <b>${Math.round(kgD).toLocaleString()} kg</b>` : '';
          rows.push(`<span style='color: ${DAY_COLORS[d]}; font-weight: 700;'>• ${DAY_NAMES[d]}: ${cnt}% (${haD} ha)${kgTxt}</span>`);
        }
      }
      if (totalPct === 0) {
        return `<div style='margin-top:4px;'><b>${labelPasada}:</b> <span style='color:#94A3B8;'>Sin programar (0%)</span></div>`;
      }
      const totHa = ((v.Área * totalPct) / 100).toFixed(2);
      const totKgStr = totalKg > 0 ? ` • <span style='color:#FBBF24; font-weight:700;'>${Math.round(totalKg).toLocaleString()} kg</span>` : '';
      return `
        <div style='margin-top: 5px; padding-top: 4px; border-top: 1px solid rgba(255,255,255,0.16);'>
          <b>${labelPasada}:</b> <span style='color: #4ADE80; font-weight: 700;'>${totalPct}%</span> (${totHa} ha)${totKgStr}
          <div style='display: flex; flex-direction: column; gap: 1px; margin-top: 2px;'>${rows.join('')}</div>
        </div>
      `;
    }
    
    function updateTooltip(v, clientX, clientY) {
      if (!v) {
        tooltip.style.display = 'none';
        return;
      }
      const rect = wrapper.getBoundingClientRect();
      const s1 = v.slots[slotKey(activeMode, 1)];
      const s2 = v.slots[slotKey(activeMode, 2)];
      let modoLabel = '📋 PROYECCIÓN';
      if (activeMode === 'CosechaCampo' || activeMode === 'Cosecha') {
        modoLabel = '🚜 COSECHA CAMPO';
      } else if (activeMode === 'CosechaOficial') {
        modoLabel = '🏛️ COSECHA OFICIAL 🔒';
      }
      
      tooltip.innerHTML = `
        <div style='display:flex; justify-content:space-between; align-items:center; margin-bottom:2px;'>
          <span style='font-weight: 700; font-size: 13px;'>🏷️ Válvula ${v.codigo}</span>
          <span style='font-size:9.5px; background:rgba(255,255,255,0.18); padding:1px 6px; border-radius:6px; font-weight:700;'>${modoLabel}</span>
        </div>
        <div><b>Módulo:</b> ${v.Módulo} | <b>Turno:</b> ${v.Turno}</div>
        <div><b>Variedad:</b> ${v.Variedad} | <b>Área Física:</b> ${v.Área} ha</div>
        ${formatPassTooltip(v, s1, '1ª Pasada')}
        ${formatPassTooltip(v, s2, '2ª Pasada')}
      `;
      tooltip.style.display = 'block';
      let tx = clientX - rect.left + 14;
      let ty = clientY - rect.top + 14;
      if (tx + 300 > rect.width) tx = tx - 310;
      if (ty + 200 > rect.height) ty = ty - 210;
      tooltip.style.left = Math.max(8, tx) + 'px';
      tooltip.style.top  = Math.max(8, ty) + 'px';
    }
    
    let syncTimeout = null;
    function syncStateToShiny(immediate = false) {
      if (syncTimeout) clearTimeout(syncTimeout);
      
      const doSync = function() {
        const payload = [];
        const combos = [
          { mode: 'Proyeccion',   pass: 1, key: 'Proyeccion_1' },
          { mode: 'Proyeccion',   pass: 2, key: 'Proyeccion_2' },
          { mode: 'CosechaCampo', pass: 1, key: 'CosechaCampo_1' },
          { mode: 'CosechaCampo', pass: 2, key: 'CosechaCampo_2' }
        ];
        
        for (let i = 0; i < VALVES_DATA.length; i++) {
          const v = VALVES_DATA[i];
          for (let cb of combos) {
            const s = v.slots[cb.key];
            let totPct = 0;
            for (let d of DAY_KEYS) totPct += (s.counts[d] || 0);
            if (totPct > 0) {
              payload.push({
                id:            v.id,
                Tipo_Registro: cb.mode,
                Num_Pasada:    cb.pass,
                lunes:         s.counts['lunes'] || 0,
                martes:        s.counts['martes'] || 0,
                miercoles:     s.counts['miercoles'] || 0,
                jueves:        s.counts['jueves'] || 0,
                viernes:       s.counts['viernes'] || 0,
                sabado:        s.counts['sabado'] || 0,
                cells_str:     s.cells.join(',')
              });
            }
          }
        }
        
        if (window.Shiny && window.Shiny.setInputValue) {
          Shiny.setInputValue('mapa_programacion_json', JSON.stringify(payload), { priority: 'event' });
        }
      };
      
      if (immediate) {
        doSync();
      } else {
        syncTimeout = setTimeout(doSync, 80);
      }
    }
    
    function requestRender() {
      if (!animationFrameId) {
        animationFrameId = requestAnimationFrame(render);
      }
    }
    
    function render() {
      animationFrameId = null;
      if (!ctx || !canvas) return;
      
      const dpr = window.devicePixelRatio || 1;
      const w = canvas.width / dpr;
      const h = canvas.height / dpr;
      
      ctx.save();
      ctx.scale(dpr, dpr);
      
      ctx.fillStyle = '#F8FAFC';
      ctx.fillRect(0, 0, w, h);
      
      ctx.save();
      ctx.translate(panX, panY);
      ctx.scale(scale, scale);
      
      const viewLeft   = -panX / scale;
      const viewTop    = -panY / scale;
      const viewRight  = (w - panX) / scale;
      const viewBottom = (h - panY) / scale;
      
      for (let i = 0; i < VALVES_DATA.length; i++) {
        const v = VALVES_DATA[i];
        
        if ((v.x + v.w) < viewLeft || v.x > viewRight || (v.y + v.h) < viewTop || v.y > viewBottom) {
          continue;
        }
        
        const isFiltered = (filteredIds === null || filteredIds.has(v.id));
        ctx.globalAlpha = isFiltered ? 1.0 : 0.18;
        
        const activeSlot = getActiveSlot(v);
        const cells      = activeSlot.cells;
        const otherSlot  = getOtherPassSlot(v);
        
        let hasOtherPass = false;
        for (let d of DAY_KEYS) {
          if ((otherSlot.counts[d] || 0) > 0) { hasOtherPass = true; break; }
        }
        
        let firstDay = cells[0];
        let isUniform = true;
        for (let k = 1; k < 10; k++) {
          if (cells[k] !== firstDay) {
            isUniform = false;
            break;
          }
        }
        
        const horiz = isHorizontalValve(v);
        const hasPoly = (v.pts && v.pts.length >= 3);
        
        if (hasPoly) {
          ctx.save();
          ctx.beginPath();
          ctx.moveTo(v.x + v.pts[0][0] * v.w, v.y + v.pts[0][1] * v.h);
          for (let p = 1; p < v.pts.length; p++) {
            ctx.lineTo(v.x + v.pts[p][0] * v.w, v.y + v.pts[p][1] * v.h);
          }
          ctx.closePath();
          ctx.clip();
        }
        
        ctx.fillStyle = '#FFFFFF';
        ctx.fillRect(v.x, v.y, v.w, v.h);
        
        renderValveFills(ctx, v, activeSlot, horiz);
        
        if (scale > 0.12) {
          ctx.strokeStyle = 'rgba(0, 0, 0, 0.12)';
          ctx.lineWidth = Math.max(0.6 / scale, 0.4);
          ctx.beginPath();
          strokeValveGrid(ctx, v, horiz);
          ctx.stroke();
        }
        
        if (hasPoly) {
          ctx.restore();
          ctx.strokeStyle = isFiltered ? '#000000' : 'rgba(0, 0, 0, 0.25)';
          ctx.lineWidth = Math.max(1.3 / scale, 0.9);
          ctx.beginPath();
          ctx.moveTo(v.x + v.pts[0][0] * v.w, v.y + v.pts[0][1] * v.h);
          for (let p = 1; p < v.pts.length; p++) {
            ctx.lineTo(v.x + v.pts[p][0] * v.w, v.y + v.pts[p][1] * v.h);
          }
          ctx.closePath();
          ctx.stroke();
        } else {
          ctx.strokeStyle = isFiltered ? '#000000' : 'rgba(0, 0, 0, 0.25)';
          ctx.lineWidth = Math.max(1.3 / scale, 0.9);
          ctx.strokeRect(v.x, v.y, v.w, v.h);
        }
        
        const isTriangle = hasPoly && v.pts.length === 3;
        const isSlopedTop = hasPoly && !isTriangle && (v.pts.some(p => p[0] < 0.20 && p[1] > 0.40) || (v.pts[0][0] < 0.05 && v.pts[0][1] > 0.40));
        if (!isSlopedTop && !isTriangle) {
          const notchW = v.w * 0.22;
          const notchH = Math.max(v.h * 0.08, 4);
          ctx.fillStyle = isFiltered ? '#000000' : 'rgba(0, 0, 0, 0.25)';
          ctx.fillRect(v.x + (v.w - notchW) / 2, v.y - notchH, notchW, notchH);
        }
        
        if (hasOtherPass && scale > 0.16 && v.w >= 18) {
          const badgeText = activePass === 1 ? '2P' : '1P';
          const bw = Math.min(24, v.w * 0.26);
          const bh = Math.min(14, v.h * 0.22);
          ctx.fillStyle = '#1E293B';
          ctx.fillRect(v.x + v.w - bw - 3, v.y + 3, bw, bh);
          ctx.fillStyle = '#FFFFFF';
          ctx.font = 'bold ' + Math.max(7, Math.floor(bh * 0.72)) + 'px Inter, sans-serif';
          ctx.textAlign = 'center';
          ctx.textBaseline = 'middle';
          ctx.fillText(badgeText, v.x + v.w - bw / 2 - 3, v.y + 3 + bh / 2);
        }
        
        if (scale > 0.12 && v.w >= 18) {
          ctx.save();
          ctx.textAlign = 'center';
          ctx.textBaseline = 'middle';
          ctx.strokeStyle = 'rgba(255, 255, 255, 0.88)';
          ctx.fillStyle = '#000000';
          ctx.lineJoin = 'round';
          
          const cx = isTriangle ? (v.x + v.w * 0.68) : (v.x + v.w * (isSlopedTop ? 0.60 : 0.50));
          const maxTextW = v.w * (isTriangle ? 0.58 : 0.88);
          const ry1 = isTriangle ? 0.58 : (isSlopedTop ? (v.pts[0][1] < 0.80 ? 0.42 : 0.36) : 0.27);
          const ry2 = isTriangle ? 0.74 : (isSlopedTop ? (v.pts[0][1] < 0.80 ? 0.64 : 0.60) : 0.55);
          const ry3 = isTriangle ? 0.88 : (isSlopedTop ? (v.pts[0][1] < 0.80 ? 0.85 : 0.83) : 0.81);
          
          if (scale > 0.20) {
            let fSizeCode = Math.min(19, Math.max(8, Math.floor(v.w * 0.13)));
            if (v.w < 90) fSizeCode = Math.min(10.5, fSizeCode);
            ctx.font = 'bold ' + fSizeCode + 'px Inter, sans-serif';
            let codeW = ctx.measureText(v.codigo).width;
            if (codeW > maxTextW) {
              fSizeCode = Math.max(7, Math.floor(fSizeCode * (maxTextW / codeW)));
              ctx.font = 'bold ' + fSizeCode + 'px Inter, sans-serif';
            }
            ctx.lineWidth = Math.min(Math.max(2.4 / scale, 1.2), Math.max(1.1, fSizeCode * 0.22));
            const y1 = v.y + v.h * ry1;
            ctx.strokeText(v.codigo, cx, y1);
            ctx.fillText(v.codigo, cx, y1);
            
            let fSizeVar = Math.min(13, Math.max(6.5, Math.round(fSizeCode * 0.68)));
            ctx.font = '600 ' + fSizeVar + 'px Inter, sans-serif';
            const varStr = v.Variedad || '';
            let varW = ctx.measureText(varStr).width;
            if (varW > maxTextW) {
              fSizeVar = Math.max(6, Math.floor(fSizeVar * (maxTextW / varW)));
              ctx.font = '600 ' + fSizeVar + 'px Inter, sans-serif';
            }
            const y2 = v.y + v.h * ry2;
            ctx.strokeText(varStr, cx, y2);
            ctx.fillText(varStr, cx, y2);
            
            // Línea 3: Área en ha + Kilos asignados (si > 0)
            const kgVal = activeSlot.totalKg || 0;
            const textHa = (v.Área !== undefined && v.Área !== null ? v.Área : '') + ' ha' +
                           (kgVal > 0 ? ' • ' + Math.round(kgVal).toLocaleString() + ' kg' : '');
            let fSizeHa = Math.min(12, Math.max(6.2, Math.round(fSizeCode * 0.62)));
            ctx.font = '700 ' + fSizeHa + 'px Inter, sans-serif';
            let haW = ctx.measureText(textHa).width;
            if (haW > maxTextW) {
              fSizeHa = Math.max(5.8, Math.floor(fSizeHa * (maxTextW / haW)));
              ctx.font = '700 ' + fSizeHa + 'px Inter, sans-serif';
            }
            const y3 = v.y + v.h * ry3;
            ctx.strokeText(textHa, cx, y3);
            ctx.fillText(textHa, cx, y3);
          } else {
            let fSizeZoom = Math.min(21, Math.max(8, Math.floor(v.w * 0.15)));
            ctx.font = 'bold ' + fSizeZoom + 'px Inter, sans-serif';
            let zW = ctx.measureText(v.codigo).width;
            if (zW > maxTextW) {
              fSizeZoom = Math.max(7, Math.floor(fSizeZoom * (maxTextW / zW)));
              ctx.font = 'bold ' + fSizeZoom + 'px Inter, sans-serif';
            }
            ctx.lineWidth = Math.min(Math.max(2.8 / scale, 1.4), Math.max(1.3, fSizeZoom * 0.25));
            const midY = isTriangle ? 0.72 : (isSlopedTop ? 0.62 : 0.50);
            ctx.strokeText(v.codigo, cx, v.y + v.h * midY);
            ctx.fillText(v.codigo, cx, v.y + v.h * midY);
          }
          ctx.restore();
        }
      }
      
      // Elementos CAD Oficiales para Módulo 8 (Visibles solo al acercarse al módulo)
      if (scale > 0.25 && viewRight > 4700 && viewLeft < 6100 && viewBottom > 3100 && viewTop < 4350) {
        ctx.save();
        ctx.textAlign = 'center';
        ctx.textBaseline = 'middle';
        
        ctx.fillStyle = '#FEF08A';
        ctx.strokeStyle = '#EAB308';
        ctx.lineWidth = Math.max(1.5 / scale, 1.2);
        ctx.fillRect(4790, 3212, 326, 32);
        ctx.strokeRect(4790, 3212, 326, 32);
        ctx.fillStyle = '#854D0E';
        ctx.font = 'bold 15px Inter, sans-serif';
        ctx.fillText('PARADERO 29', 4790 + 163, 3228);
        
        ctx.fillStyle = '#FEF08A';
        ctx.strokeStyle = '#EAB308';
        ctx.fillRect(5156, 3212, 326, 32);
        ctx.strokeRect(5156, 3212, 326, 32);
        ctx.fillStyle = '#854D0E';
        ctx.font = 'bold 15px Inter, sans-serif';
        ctx.fillText('PARADERO 30', 5156 + 163, 3228);
        
        ctx.fillStyle = '#FEF08A';
        ctx.strokeStyle = '#EAB308';
        ctx.fillRect(5522, 3212, 326, 32);
        ctx.strokeRect(5522, 3212, 326, 32);
        ctx.fillStyle = '#854D0E';
        ctx.font = 'bold 15px Inter, sans-serif';
        ctx.fillText('PARADERO 31', 5522 + 163, 3228);
        
        const roadBadges = [
          { y: 3228, text: 'PARADERO 31', bg: '#FEF08A', border: '#EAB308', col: '#854D0E' },
          { y: 3450, text: 'INGRESO', bg: '#F1F5F9', border: '#CBD5E1', col: '#475569' },
          { y: 3705, text: 'PARADERO 32', bg: '#FEF08A', border: '#EAB308', col: '#854D0E' },
          { y: 3960, text: 'INGRESO', bg: '#F1F5F9', border: '#CBD5E1', col: '#475569' },
          { y: 4215, text: 'PARADERO 33', bg: '#FEF08A', border: '#EAB308', col: '#854D0E' }
        ];
        
        roadBadges.forEach(b => {
          ctx.fillStyle = b.bg;
          ctx.strokeStyle = b.border;
          ctx.fillRect(5875, b.y - 14, 118, 28);
          ctx.strokeRect(5875, b.y - 14, 118, 28);
          ctx.fillStyle = b.col;
          ctx.font = 'bold 12px Inter, sans-serif';
          ctx.fillText(b.text, 5875 + 59, b.y);
        });
        
        ctx.restore();
      }
      
      // Banners Oficiales de Identificación de Módulos (Visibles al hacer zoom en módulos)
      if (scale > 0.25) {
        ctx.save();
        ctx.textAlign = 'center';
        ctx.textBaseline = 'middle';
        
        const moduleBanners = [
          { text: 'MÓDULO 12', x: 4294, y: 1420, w: 1554, h: 28, bg: '#FCE7F3', border: '#F472B6', col: '#831843' },
          { text: 'MÓDULO 7',  x: 5900, y: 865,  w: 748,  h: 28, bg: '#CFFAFE', border: '#06B6D4', col: '#164E63' },
          { text: 'MÓDULO 6',  x: 6788, y: 355,  w: 1412, h: 28, bg: '#D1FAE5', border: '#10B981', col: '#064E3B' },
          { text: 'MÓDULO 5',  x: 6788, y: 1505, w: 1412, h: 28, bg: '#D1FAE5', border: '#10B981', col: '#064E3B' },
          { text: 'MÓDULO 4',  x: 6788, y: 2020, w: 1412, h: 28, bg: '#D1FAE5', border: '#10B981', col: '#064E3B' },
          { text: 'MÓDULO 3',  x: 8320, y: 355,  w: 2243, h: 28, bg: '#E0F2FE', border: '#38BDF8', col: '#0C4A6E' },
          { text: 'MÓDULO 2',  x: 8320, y: 1505, w: 1186, h: 28, bg: '#FFEDD5', border: '#FB923C', col: '#7C2D12' },
          { text: 'MÓDULO 1',  x: 9596, y: 1505, w: 967,  h: 28, bg: '#FEF08A', border: '#FACC15', col: '#713F12' },
          { text: 'MÓDULO 13', x: 8320, y: 2655, w: 1186, h: 28, bg: '#E0E7FF', border: '#818CF8', col: '#312E81' },
          { text: 'MÓDULO 14', x: 9596, y: 2655, w: 967,  h: 28, bg: '#EDE9FE', border: '#A78BFA', col: '#4C1D95' },
          { text: 'MÓDULO 10', x: 4060, y: 5075, w: 1788, h: 28, bg: '#FFEDD5', border: '#FB923C', col: '#7C2D12' },
          { text: 'MÓDULO 11', x: 4790, y: 3165, w: 1058, h: 26, bg: '#86EFAC', border: '#22C55E', col: '#14532D' },
          { text: 'MÓDULO 9',  x: 4790, y: 4275, w: 1058, h: 26, bg: '#86EFAC', border: '#22C55E', col: '#14532D' }
        ];

        moduleBanners.forEach(mb => {
          if (mb.x + mb.w >= viewLeft && mb.x <= viewRight && mb.y + mb.h >= viewTop && mb.y <= viewBottom) {
            ctx.fillStyle = mb.bg;
            ctx.strokeStyle = mb.border;
            ctx.lineWidth = Math.max(1.5 / scale, 1.2);
            ctx.fillRect(mb.x, mb.y, mb.w, mb.h);
            ctx.strokeRect(mb.x, mb.y, mb.w, mb.h);
            ctx.fillStyle = mb.col;
            ctx.font = 'bold 13px Inter, sans-serif';
            ctx.fillText(mb.text, mb.x + mb.w / 2, mb.y + mb.h / 2);
          }
        });
        ctx.restore();
      }
      
      ctx.restore();
      ctx.restore();
    }
    
    document.addEventListener('DOMContentLoaded', function() {
      setTimeout(initCanvas, 100);
    });
    
    let shinyHandlersRegistered = false;
    function registerShinyHandlers() {
      if (shinyHandlersRegistered) return;
      if (window.Shiny && window.Shiny.addCustomMessageHandler) {
        shinyHandlersRegistered = true;
        Shiny.addCustomMessageHandler('actualizar_filtro_mapa', function(message) {
          if (message.ids && message.ids.length > 0) {
            filteredIds = new Set(message.ids.map(String));
            fitSelection();
          } else {
            filteredIds = null;
            resetView();
          }
          requestRender();
        });
        
        Shiny.addCustomMessageHandler('cargar_estado_semana', function(message) {
          undoStack = [];
          updateUndoButton();
          
          VALVES_DATA.forEach(v => {
            v.slots = {
              'Proyeccion_1':     createEmptySlot(),
              'Proyeccion_2':     createEmptySlot(),
              'CosechaCampo_1':   createEmptySlot(),
              'CosechaCampo_2':   createEmptySlot(),
              'CosechaOficial_1': createEmptySlot(),
              'CosechaOficial_2': createEmptySlot()
            };
            v.slots['Cosecha_1'] = v.slots['CosechaCampo_1'];
            v.slots['Cosecha_2'] = v.slots['CosechaCampo_2'];
          });
          
          const rows = message.rows || [];
          const byId = new Map(VALVES_DATA.map(v => [String(v.id), v]));
          
          rows.forEach(r => {
            const v = byId.get(String(r.id));
            if (!v) return;
            let mode = 'Proyeccion';
            if (r.Tipo_Registro === 'Cosecha' || r.Tipo_Registro === 'CosechaCampo') {
              mode = 'CosechaCampo';
            } else if (r.Tipo_Registro === 'CosechaOficial') {
              mode = 'CosechaOficial';
            }
            const pass = Number(r.Num_Pasada) === 2 ? 2 : 1;
            const k = slotKey(mode, pass);
            const targetSlot = v.slots[k];
            
            if (r.cells_str && typeof r.cells_str === 'string') {
              const parts = r.cells_str.split(',');
              for (let idx = 0; idx < 10; idx++) {
                targetSlot.cells[idx] = parts[idx] || '';
              }
            }
            targetSlot.counts = {
              lunes:     Number(r.Lunes) || 0,
              martes:    Number(r.Martes) || 0,
              miercoles: Number(r.Miércoles) || 0,
              jueves:    Number(r.Jueves) || 0,
              viernes:   Number(r.Viernes) || 0,
              sabado:    Number(r.Sábado) || 0
            };
            targetSlot.kg = {
              lunes:     Number(r.Kg_Lunes) || 0,
              martes:    Number(r.Kg_Martes) || 0,
              miercoles: Number(r.Kg_Miércoles) || 0,
              jueves:    Number(r.Kg_Jueves) || 0,
              viernes:   Number(r.Kg_Viernes) || 0,
              sabado:    Number(r.Kg_Sábado) || 0
            };
            
            const hasCounts = Object.values(targetSlot.counts).some(p => p > 0);
            if (hasCounts) {
              syncCellsFromCounts(targetSlot);
            }
            recomputeValveSlotCounts(v, k);
          });
          
          marcarCambiosPendientes(false);
          requestRender();
          hideIosLoader();
        });
        
        Shiny.addCustomMessageHandler('actualizar_kilos_ui', function(message) {
          const dayTotals = message.day_totals || {};
          const mapInputIds = {
            'Lunes':     'input_kg_lunes',
            'Martes':    'input_kg_martes',
            'Miércoles': 'input_kg_miercoles',
            'Jueves':    'input_kg_jueves',
            'Viernes':   'input_kg_viernes',
            'Sábado':    'input_kg_sabado'
          };
          
          Object.keys(mapInputIds).forEach(dName => {
            const el = document.getElementById(mapInputIds[dName]);
            if (el && document.activeElement !== el) {
              const val = Math.round(Number(dayTotals[dName]) || 0);
              el.value = val > 0 ? val.toLocaleString('en-US') : '';
              el.dataset.lastVal = String(val);
            }
          });
          
          const valveKilos = message.valve_kilos || [];
          const byId = new Map(VALVES_DATA.map(v => [String(v.id), v]));
          valveKilos.forEach(vk => {
            const v = byId.get(String(vk.id));
            if (!v) return;
            const k = slotKey(vk.Tipo_Registro, vk.Num_Pasada);
            if (v.slots[k]) {
              v.slots[k].kg = {
                lunes:     Number(vk.Kg_Lunes) || 0,
                martes:    Number(vk.Kg_Martes) || 0,
                miercoles: Number(vk.Kg_Miércoles) || 0,
                jueves:    Number(vk.Kg_Jueves) || 0,
                viernes:   Number(vk.Kg_Viernes) || 0,
                sabado:    Number(vk.Kg_Sábado) || 0
              };
              v.slots[k].totalKg = Math.round(
                v.slots[k].kg.lunes + v.slots[k].kg.martes + v.slots[k].kg.miercoles +
                v.slots[k].kg.jueves + v.slots[k].kg.viernes + v.slots[k].kg.sabado
              );
            }
          });
          requestRender();
        });
        
        Shiny.addCustomMessageHandler('actualizar_proy_sugerido', function(message) {
          const kgSugerido = Math.round(Number(message.kg_proy_filtro) || 0);
          const kgFmt = kgSugerido > 0 ? (kgSugerido >= 1000 ? Math.round(kgSugerido).toLocaleString('en-US') : String(kgSugerido)) : '0';
          const dias = ['lunes', 'martes', 'miercoles', 'jueves', 'viernes', 'sabado'];
          dias.forEach(d => {
            const b = document.getElementById('badge_proy_sug_' + d);
            if (b) {
              b.textContent = 'Proy: ' + kgFmt + ' kg';
              b.dataset.rawKg = String(kgSugerido);
            }
          });
        });
        
        Shiny.addCustomMessageHandler('limpiar_dia_activo', function(message) {
          $('body').removeClass('modal-open').css({ 'padding-right': '', 'overflow': '' });
          $('.modal-backdrop').remove();
          saveSnapshotForUndo();
          const key = slotKey(activeMode, activePass);
          const dayKey = message.dia_key || activeDay;
          
          VALVES_DATA.forEach(v => {
            // Solo válvulas que coincidan con los filtros activos de la pantalla
            if (filteredIds && !filteredIds.has(v.id)) return;
            // Solo válvulas de los módulos asignados al usuario si no es admin
            if (window.USER_PERMISOS && !window.USER_PERMISOS.es_admin) {
              if (!window.USER_PERMISOS.modulos.includes(Number(v.Módulo))) return;
            }
            const slot = v.slots[key];
            if (!slot) return;
            
            // Borrar ÚNICAMENTE el día activo; los demás días quedan intactos
            if (slot.counts) {
              slot.counts[dayKey] = 0;
            }
            if (slot.kg) {
              slot.kg[dayKey] = 0;
            }
            syncCellsFromCounts(slot);
            recomputeValveSlotCounts(v, key);
          });
          
          // Limpiar input de kilos de ese día
          const mapInputIds = {
            'lunes':     'input_kg_lunes',
            'martes':    'input_kg_martes',
            'miercoles': 'input_kg_miercoles',
            'jueves':    'input_kg_jueves',
            'viernes':   'input_kg_viernes',
            'sabado':    'input_kg_sabado'
          };
          const inpId = mapInputIds[dayKey];
          if (inpId) {
            const el = document.getElementById(inpId);
            if (el) {
              el.value = '';
              el.dataset.lastVal = '0';
            }
          }
          
          marcarCambiosPendientes(true);
          requestRender();
          syncStateToShiny(true);
        });
        
        Shiny.addCustomMessageHandler('limpiar_pasada_activa', function(message) {
          $('body').removeClass('modal-open').css({ 'padding-right': '', 'overflow': '' });
          $('.modal-backdrop').remove();
          saveSnapshotForUndo();
          const key = slotKey(activeMode, activePass);
          VALVES_DATA.forEach(v => {
            if (filteredIds && !filteredIds.has(v.id)) return;
            if (window.USER_PERMISOS && !window.USER_PERMISOS.es_admin) {
              if (!window.USER_PERMISOS.modulos.includes(Number(v.Módulo))) return;
            }
            v.slots[key].cells.fill('');
            v.slots[key].cellFractions = {};
            v.slots[key].counts = emptyCounts();
            v.slots[key].hasExactPcts = false;
            v.slots[key].kg = emptyCounts();
            recomputeValveSlotCounts(v, key);
          });
          marcarCambiosPendientes(true);
          requestRender();
          syncStateToShiny(true);
        });
        
        function bloquearFiltroGrupo(bloquear) {
          const el = document.getElementById('filtro_grupo');
          if (!el || !el.selectize) return;
          if (bloquear) {
            el.selectize.lock();
            el.selectize.$wrapper.addClass('filtro-bloqueado');
          } else {
            el.selectize.unlock();
            el.selectize.$wrapper.removeClass('filtro-bloqueado');
          }
        }

        Shiny.addCustomMessageHandler('login_exitoso', function(data) {
          window.USER_PERMISOS = data;
          const ov = document.getElementById('login-overlay');
          if (ov) ov.style.display = 'none';
          const btn = document.getElementById('btn_iniciar_sesion');
          if (btn) {
            btn.innerHTML = 'Iniciar Sesión';
            btn.disabled = false;
          }
          setTimeout(function() { bloquearFiltroGrupo(!data.es_admin); }, 150);
          if (typeof resizeCanvas === 'function') resizeCanvas();
        });

        Shiny.addCustomMessageHandler('logout_exitoso', function() {
          window.USER_PERMISOS = null;
          bloquearFiltroGrupo(false);
          const ov = document.getElementById('login-overlay');
          if (ov) ov.style.display = 'flex';
          const btn = document.getElementById('btn_iniciar_sesion');
          if (btn) {
            btn.innerHTML = 'Iniciar Sesión';
            btn.disabled = false;
          }
          const passInp = document.getElementById('login_password');
          if (passInp) passInp.value = '';
          if (typeof resizeCanvas === 'function') resizeCanvas();
        });
        
        Shiny.addCustomMessageHandler('mostrar_loader', function(message) {
          showIosLoader(message.titulo || 'Cargando datos...', message.subtitulo || 'Procesando información');
        });
        
        Shiny.addCustomMessageHandler('ocultar_loader', function(message) {
          hideIosLoader();
          const btn = document.getElementById('btn_guardar_manual');
          if (btn) {
            btn.style.pointerEvents = 'auto';
            btn.style.opacity = '1';
            if (!hasUnsavedChanges) {
              btn.style.setProperty('background', '#007AFF', 'important');
              btn.style.boxShadow = 'none';
              btn.innerHTML = '💾 Guardar Ruta';
              btn.title = 'Todo sincronizado';
            }
          }
        });
        
        Shiny.addCustomMessageHandler('limpiar_cambios_pendientes', function(message) {
          marcarCambiosPendientes(false);
        });
        
        Shiny.addCustomMessageHandler('forzar_cambio_semana', function(data) {
          hasUnsavedChanges = false;
          marcarCambiosPendientes(false);
          $('body').removeClass('modal-open').css({ 'padding-right': '', 'overflow': '' });
          $('.modal-backdrop').remove();
          const semStr = String(data.semana || '');
          previousSemana = semStr;
          $('#sel_semana').val(semStr);
          showIosLoader('Cargando Semana ' + semStr + '...', 'Calculando proyección, histórico y distribución de kilos');
          if (window.Shiny && window.Shiny.setInputValue) {
            Shiny.setInputValue('sel_semana', semStr, { priority: 'event' });
          }
        });
        
        Shiny.addCustomMessageHandler('revertir_selector_semana', function(data) {
          $('body').removeClass('modal-open').css({ 'padding-right': '', 'overflow': '' });
          $('.modal-backdrop').remove();
          const semStr = String(data.semana || '');
          if (semStr) {
            previousSemana = semStr;
            $('#sel_semana').val(semStr);
          }
          hideIosLoader();
        });
      }
    }
    
    if (window.$) {
      previousSemana = $('#sel_semana').val() || previousSemana;
      
      $(document).on('shiny:connected', function() {
        registerShinyHandlers();
        previousSemana = $('#sel_semana').val() || previousSemana;
      });
      
      // Al pulsar Salir, volver de inmediato a la pantalla de login
      $(document).on('click', '#btn_logout', function() {
        window.USER_PERMISOS = null;
        const ov = document.getElementById('login-overlay');
        if (ov) {
          ov.style.display = 'flex';
          $('body').css('overflow', 'hidden');
        }
        const sel = $('#login_usuario')[0] && $('#login_usuario')[0].selectize;
        if (sel) {
          sel.clear();
        } else {
          $('#login_usuario').val('');
        }
        $('#login_password').val('');
      });
      
      // Garantizar que lo escrito en el autocompletado no se borre al pasar a la contraseña
      function asegurarAutoSeleccionUsuario() {
        const $el = $('#login_usuario');
        if (!$el.length || !$el[0].selectize) {
          setTimeout(asegurarAutoSeleccionUsuario, 150);
          return;
        }
        const sel = $el[0].selectize;
        // Al desenfocar el selector, si hay texto escrito y opciones filtradas, autoseleccionar el primer resultado
        sel.on('blur', function() {
          const curVal = sel.getValue();
          if (!curVal) {
            const query = (sel.$control_input && sel.$control_input.val() || '').trim();
            if (query && sel.currentResults && sel.currentResults.items && sel.currentResults.items.length > 0) {
              const bestMatch = sel.currentResults.items[0].id;
              sel.setValue(bestMatch, false);
              sel.close();
            }
          }
        });
      }
      setTimeout(asegurarAutoSeleccionUsuario, 300);
      
      // Al seleccionar o ingresar un evaluador, enfocar automáticamente el campo de contraseña
      $(document).on('change', '#login_usuario', function() {
        const val = ($(this).val() || '').trim();
        if (val) {
          setTimeout(function() {
            const pass = document.getElementById('login_password');
            if (pass) pass.focus();
          }, 120);
        }
      });
      
      $(document).on('keypress', '#login_password', function(e) {
        if (e.which === 13) {
          e.preventDefault();
          $('#btn_iniciar_sesion').click();
        }
      });
      
      $(document).on('click', '#btn_iniciar_sesion', function() {
        const sel = $('#login_usuario')[0] && $('#login_usuario')[0].selectize;
        if (sel && !sel.getValue()) {
          const query = (sel.$control_input && sel.$control_input.val() || '').trim();
          if (query && sel.currentResults && sel.currentResults.items && sel.currentResults.items.length > 0) {
            sel.setValue(sel.currentResults.items[0].id, false);
          }
        }
        const u = ($('#login_usuario').val() || '').trim();
        const p = ($('#login_password').val() || '').trim();
        if (window.Shiny && window.Shiny.setInputValue) {
          Shiny.setInputValue('login_usuario', u);
          Shiny.setInputValue('login_password', p);
        }
        if (u && p) {
          $(this).html('Iniciando sesion..');
        }
      });
      
      $(document).on('focus mousedown', '#sel_semana', function() {
        const cur = $(this).val();
        if (cur) previousSemana = cur;
      });
      
      // Selector de semana con confirmación si hay cambios sin guardar
      $(document).on('change', '#sel_semana', function(e) {
        const valSem = $(this).val();
        if (!valSem) return;
        if (hasUnsavedChanges) {
          const prev = previousSemana || valSem;
          if (e && e.stopImmediatePropagation) e.stopImmediatePropagation();
          if (e && e.preventDefault) e.preventDefault();
          $(this).val(prev);
          if (window.Shiny && window.Shiny.setInputValue) {
            Shiny.setInputValue('intentar_cambiar_semana', {
              semana_anterior: String(prev),
              semana_nueva: String(valSem),
              ts: Date.now()
            }, { priority: 'event' });
          }
          return false;
        }
        previousSemana = valSem;
        showIosLoader('Cargando Semana ' + valSem + '...', 'Calculando proyección, histórico y distribución de kilos');
      });
      
      // Levantar loader inmediatamente al hacer clic en Sincronizar
      $(document).on('click', '#btn_sincronizar_sheets', function() {
        const s = $('#sel_semana').val() || '';
        showIosLoader('Sincronizando' + (s ? (' Semana ' + s) : '') + '...', 'Descargando rutas vigentes desde Google Sheets y Data Maestra');
      });
    }
    registerShinyHandlers();
  "))
)

# ==============================================================================
# SERVER
# ==============================================================================

server <- function(input, output, session) {
  
  rutas_disco_rv            <- reactiveVal(cargar_rutas_disco())
  ultimo_guardado_rv        <- reactiveVal(NULL)
  cambios_pendientes_rv     <- reactiveVal(FALSE)
  semana_activa_rv          <- reactiveVal(as.numeric(semana_iso_actual))
  semana_pendiente_modal_rv <- reactiveVal(NULL)
  forzando_cambio_semana_rv <- reactiveVal(FALSE)
  usuario_autenticado_rv    <- reactiveVal(NULL)
  rv_mtime_rdata            <- reactiveVal(if (exists("ruta_rdata_master") && file.exists(ruta_rdata_master)) file.mtime(ruta_rdata_master) else Sys.time())
  
  # En shinyapps.io (entorno nube), verificar si la data de Google Drive está fresca al iniciar la sesión
  if (es_entorno_nube) {
    tryCatch({
      refrescar_data_maestra_en_caliente(forzar_nube = FALSE)
      rv_mtime_rdata(Sys.time())
    }, error = function(e) cat("[AVISO NUBE SESION]:", e$message, "\n"))
  }
  
  # Sincronizar usuarios desde Google Sheets en tiempo real al conectarse cada sesión
  session$onFlushed(function() {
    isolate({
      db_live <- tryCatch(obtener_usuarios_google_sheets(), error = function(e) NULL)
      if (!is.null(db_live) && nrow(db_live) > 0) {
        usuarios_db <<- db_live
        cur_post <- isolate(input$login_usuario)
        if (is.null(cur_post) || !nzchar(cur_post)) {
          updateSelectizeInput(session, "login_usuario",
                               choices = c("Escriba o seleccione su nombre..." = "", generar_opciones_usuarios(db_live)),
                               selected = "",
                               server = FALSE)
        }
      }
    })
  }, once = TRUE)

  # Botón para refrescar la lista de usuarios en tiempo real desde Google Sheets sin recargar la página
  observeEvent(input$btn_refrescar_usuarios, {
    db_live <- tryCatch(obtener_usuarios_google_sheets(), error = function(e) NULL)
    if (!is.null(db_live) && nrow(db_live) > 0) {
      usuarios_db <<- db_live
      cur_sel <- isolate(input$login_usuario)
      updateSelectizeInput(session, "login_usuario",
                           choices = c("Escriba o seleccione su nombre..." = "", generar_opciones_usuarios(db_live)),
                           selected = cur_sel,
                           server = FALSE)
      showNotification("Lista de usuarios actualizada desde Google Sheets.", type = "message", duration = 3)
    } else {
      showNotification("No se pudo conectar a Google Sheets en este momento.", type = "warning", duration = 3)
    }
  })
  
  # Autenticación nativa al hacer clic en Iniciar Sesión (valida contra Google Sheets)
  observeEvent(input$btn_iniciar_sesion, {
    u_ing <- trimws(as.character(input$login_usuario))
    p_ing <- trimws(as.character(input$login_password))
    
    if (!nzchar(u_ing) || !nzchar(p_ing)) {
      output$login_error_msg <- renderUI({
        tagList(
          div(style = "color: #FF3B30; font-size: 12.5px; font-weight: 600; text-align: center; margin-top: 12px; background: rgba(255, 59, 48, 0.08); padding: 7px 10px; border-radius: 8px;",
              "⚠️ Por favor ingrese usuario y contraseña (DNI)."),
          tags$script(HTML("$('#btn_iniciar_sesion').html('Iniciar Sesión').prop('disabled', false);"))
        )
      })
      return()
    }
    
    buscar_coincidencia <- function(df, u, p) {
      if (is.null(df) || nrow(df) == 0) return(NULL)
      res <- df %>% filter(
        (tolower(trimws(as.character(usuario))) == tolower(u) | 
         trimws(as.character(password)) == u | 
         tolower(trimws(as.character(nombre))) == tolower(u) |
         (tolower(u) == "admin" & rol == "Administrador")),
        trimws(as.character(password)) == p
      )
      if (nrow(res) > 0) res[1, ] else NULL
    }
    
    # Consultar Google Sheets en tiempo real para validar credenciales
    db_live <- tryCatch(obtener_usuarios_google_sheets(), error = function(e) NULL)
    if (!is.null(db_live) && nrow(db_live) > 0) {
      usuarios_db <<- db_live
    }
    
    match_row <- buscar_coincidencia(usuarios_db, u_ing, p_ing)
    
    if (!is.null(match_row) && nrow(match_row) > 0) {
      usr <- as.list(match_row)
      usuario_autenticado_rv(usr)
      output$login_error_msg <- renderUI({
        tags$script(HTML("$('#login-overlay').hide(); $('body').css('overflow', ''); $('#btn_iniciar_sesion').html('Iniciar Sesión').prop('disabled', false);"))
      })
      
      # Informar al motor 2D Canvas sobre permisos y usuario activo
      session$sendCustomMessage("login_exitoso", list(
        usuario  = usr$usuario,
        nombre   = usr$nombre,
        rol      = usr$rol,
        modulos  = as.list(as.numeric(unlist(usr$modulos))),
        es_admin = (usr$rol == "Administrador")
      ))
      
      # Si es sectorista, auto-seleccionar su grupo de módulos correspondiente; si es Admin, mostrar todos
      if (usr$rol != "Administrador") {
        mods_u <- as.numeric(unlist(usr$modulos))
        grp_match <- names(grupos_modulos)[vapply(grupos_modulos, function(m) any(m %in% mods_u), logical(1))]
        # Solo el/los grupo(s) asignado(s) y únicamente sus módulos asignados
        updateSelectizeInput(session, "filtro_grupo", choices = grp_match, selected = grp_match)
        updateSelectizeInput(session, "filtro_modulo", choices = as.character(sort(mods_u)), selected = character(0), server = FALSE)
      } else {
        updateSelectizeInput(session, "filtro_grupo", choices = names(grupos_modulos), selected = character(0))
        updateSelectizeInput(session, "filtro_modulo", choices = mods_iniciales, selected = character(0), server = FALSE)
      }
      
      showNotification(sprintf("👋 Bienvenido, %s (%s)", usr$nombre, usr$rol), type = "message", duration = 4)
    } else {
      updateTextInput(session, "login_password", value = "")
      output$login_error_msg <- renderUI({
        tagList(
          div(style = "color: #FF3B30; font-size: 12.5px; font-weight: 600; text-align: center; margin-top: 12px; background: rgba(255, 59, 48, 0.08); padding: 7px 10px; border-radius: 8px;",
              "❌ Usuario o contraseña (DNI) incorrectos."),
          tags$script(HTML("$('#btn_iniciar_sesion').html('Iniciar Sesión').prop('disabled', false);"))
        )
      })
    }
  })
  
  observeEvent(input$btn_logout, {
    usuario_autenticado_rv(NULL)
    updateSelectizeInput(session, "login_usuario", selected = "")
    updateTextInput(session, "login_password", value = "")
    output$login_error_msg <- renderUI(NULL)
    updateSelectizeInput(session, "filtro_grupo", choices = names(grupos_modulos), selected = character(0))
    updateSelectizeInput(session, "filtro_modulo", choices = mods_iniciales, selected = character(0), server = FALSE)
    cambios_pendientes_rv(FALSE)
    session$sendCustomMessage("limpiar_cambios_pendientes", list())
    session$sendCustomMessage("logout_exitoso", list())
    
    # Refrescar lista de usuarios desde Google Sheets al desloguearse
    db_live <- tryCatch(obtener_usuarios_google_sheets(), error = function(e) NULL)
    if (!is.null(db_live) && nrow(db_live) > 0) {
      usuarios_db <<- db_live
      updateSelectizeInput(session, "login_usuario",
                           choices = c("Escriba o seleccione su nombre..." = "", generar_opciones_usuarios(db_live)),
                           selected = "",
                           server = FALSE)
    }
    
    showNotification("Sesión cerrada.", type = "message", duration = 3)
  })
  
  output$ui_perfil_usuario <- renderUI({
    u <- usuario_autenticado_rv()
    if (is.null(u)) {
      tags$button(type = "button", onclick = "document.getElementById('login-overlay').style.display='flex'; document.body.style.overflow='hidden';",
                  class = "btn-secondary-apple",
                  style = "border-radius: 10px; background: #FFFFFF; color: #007AFF; font-weight: 600; font-size: 12px; border: 1px solid #C7C7CC; height: 38px; padding: 0 12px; cursor: pointer; display: inline-flex; align-items: center; gap: 5px;",
                  span("👤"), span("Iniciar Sesión"))
    } else {
      es_admin <- (u$rol == "Administrador")
      badge_icon <- if (es_admin) {
        tags$span(style = "display: inline-flex; align-items: center; justify-content: center; width: 22px; height: 22px; border-radius: 6px; background: rgba(0, 122, 255, 0.12); color: #007AFF;",
                  HTML("<svg width='13' height='13' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2.3' stroke-linecap='round' stroke-linejoin='round'><path d='M12 22s8-4 8-10V5l-8-3-8 3v7c0 6 8 10 8 10z'></path></svg>"))
      } else {
        tags$span(style = "display: inline-flex; align-items: center; justify-content: center; width: 22px; height: 22px; border-radius: 6px; background: rgba(52, 199, 89, 0.12); color: #34C759;",
                  span(style = "font-size: 13px;", "🚜"))
      }
      
      div(style = "display: flex; align-items: center; gap: 8px; background: rgba(0, 122, 255, 0.08); border: 1px solid rgba(0, 122, 255, 0.22); border-radius: 10px; padding: 4px 12px; height: 38px; box-shadow: 0 1px 3px rgba(0,0,0,0.03);",
          badge_icon,
          div(style = "display: flex; flex-direction: column; text-align: left; line-height: 1.15;",
              span(style = "font-size: 11.5px; font-weight: 700; color: #1C1C1E;", u$nombre),
              span(style = "font-size: 10px; color: #007AFF; font-weight: 600;", 
                   if (es_admin) "Administrador • Todos los módulos" else paste("Módulos:", paste(u$modulos[[1]], collapse = ",")))
          ),
          actionButton("btn_logout", label = "Salir", 
                       style = "border: none; background: rgba(0,0,0,0.06); border-radius: 6px; font-size: 10.5px; font-weight: 600; color: #636366; padding: 3px 8px; height: 24px; cursor: pointer; margin-left: 4px;")
      )
    }
  })
  
  mostrar_modal_cambios_semana <- function(sem_ant, sem_nue) {
    sem_ant_str <- as.character(sem_ant)
    sem_nue_str <- as.character(sem_nue)
    semana_pendiente_modal_rv(sem_nue_str)
    showModal(modalDialog(
      title = "⚠️ Cambios sin guardar en la Semana actual",
      p(sprintf("Tienes trazos de cosecha o cambios de kilos en la Semana %s que aún no han sido guardados.", sem_ant_str)),
      p(sprintf("Si cambias a la Semana %s sin guardar, esas modificaciones se descartarán.", sem_nue_str)),
      footer = tagList(
        modalButton("Permanecer y Guardar"),
        actionButton("btn_confirmar_descartar_cambios", "Descartar y Cambiar de Semana",
                     style = "background: #FF3B30; color: #FFFFFF; font-weight: 600; border: none; border-radius: 8px; padding: 8px 16px;")
      ),
      easyClose = FALSE
    ))
  }
  
  observeEvent(input$hay_cambios_pendientes, {
    cambios_pendientes_rv(isTRUE(input$hay_cambios_pendientes))
  })
  
  modo_activo <- reactive({
    if (is.null(input$modo_registro_activo)) "CosechaCampo" else {
      m <- as.character(input$modo_registro_activo)
      if (m == "Cosecha") "CosechaCampo" else m
    }
  })
  
  pasada_activa <- reactive({
    if (is.null(input$pasada_activa)) 1 else as.numeric(input$pasada_activa)
  })
  
  # Cargar el Excel de Proyección Semanal (Hoja Cálculo, skip = 3) para la semana seleccionada
  proyeccion_excel_semana <- reactive({
    sem_act <- semana_activa_rv()
    req(!is.null(sem_act), !is.na(sem_act))
    cargar_proyeccion(ruta_raiz_arandano, campana = 2026, semana = as.numeric(sem_act))
  })
  
  # Catálogo de área física total por Turno para prorratear la proyección del Excel
  area_fisica_turnos <- df_valvulas %>%
    group_by(Módulo, Turno, Variedad) %>%
    summarise(Area_Turno = sum(Área, na.rm = TRUE), .groups = "drop")
  
  esquema_base_semana <- function() {
    expand_grid(
      Tipo_Registro = c("Proyeccion", "CosechaCampo", "CosechaOficial"),
      Num_Pasada    = c(1, 2),
      id            = df_valvulas$id
    ) %>%
      mutate(
        Lunes = 0, Martes = 0, Miércoles = 0, Jueves = 0, Viernes = 0, Sábado = 0,
        Kg_Lunes = 0, Kg_Martes = 0, Kg_Miércoles = 0, Kg_Jueves = 0, Kg_Viernes = 0, Kg_Sábado = 0,
        cells_str = ""
      )
  }
  
  programacion_semana <- reactiveVal(esquema_base_semana())
  
  # Función que recalcula automáticamente los Kilos Proyectados de cada válvula desde la Hoja Cálculo
  recalcular_kg_proyeccion <- function(df_prog) {
    df_excel <- proyeccion_excel_semana()
    if (is.null(df_excel) || nrow(df_excel) == 0) return(df_prog)
    
    # Calcular hectáreas pintadas totales en la semana por turno en modo Proyección
    ha_prog_turno <- df_prog %>%
      filter(Tipo_Registro == "Proyeccion") %>%
      select(-any_of(c("Área", "Área.x", "Área.y", "Módulo", "Turno", "Variedad"))) %>%
      left_join(df_valvulas %>% select(id, Módulo, Turno, Variedad, Área), by = "id") %>%
      mutate(
        Área    = coalesce(as.numeric(Área), 0),
        Ha_Valv = Área * ((coalesce(as.numeric(Lunes), 0) + coalesce(as.numeric(Martes), 0) + 
                          coalesce(as.numeric(Miércoles), 0) + coalesce(as.numeric(Jueves), 0) + 
                          coalesce(as.numeric(Viernes), 0) + coalesce(as.numeric(Sábado), 0)) / 100)
      ) %>%
      group_by(Módulo, Turno, Variedad) %>%
      summarise(Ha_Pintada_Semana = sum(Ha_Valv, na.rm = TRUE), .groups = "drop")
    
    # 1. Cruce exacto por Módulo + Turno + Variedad
    # 2. Si el Excel trae alguna variedad no registrada en las válvulas de ese (Módulo, Turno),
    #    repartir ÚNICAMENTE ese remanente no cruzado proporcional al área del turno, sin duplicar variedades ya cruzadas
    remanente_mt <- df_excel %>%
      anti_join(area_fisica_turnos, by = c("Módulo", "Turno", "Variedad")) %>%
      group_by(Módulo, Turno) %>%
      summarise(Kg_Remanente_MT = sum(Kg_Proy_Turno, na.rm = TRUE), .groups = "drop")
    
    area_mt <- area_fisica_turnos %>%
      group_by(Módulo, Turno) %>%
      summarise(Area_Total_MT = sum(Area_Turno, na.rm = TRUE), .groups = "drop")
    
    tasas_turno <- area_fisica_turnos %>%
      left_join(df_excel, by = c("Módulo", "Turno", "Variedad")) %>%
      left_join(remanente_mt, by = c("Módulo", "Turno")) %>%
      left_join(area_mt, by = c("Módulo", "Turno")) %>%
      mutate(
        Kg_Exacto     = coalesce(Kg_Proy_Turno, 0),
        Kg_Extra_Prop = ifelse(!is.na(Kg_Remanente_MT) & Area_Total_MT > 0, Kg_Remanente_MT * (Area_Turno / Area_Total_MT), 0),
        Kg_Proy_Final = Kg_Exacto + Kg_Extra_Prop
      ) %>%
      filter(!is.na(Kg_Proy_Final), Kg_Proy_Final > 0) %>%
      left_join(ha_prog_turno, by = c("Módulo", "Turno", "Variedad")) %>%
      mutate(
        Denom_Ha  = pmax(Area_Turno, coalesce(Ha_Pintada_Semana, 0), 0.01),
        Kg_Por_Ha = Kg_Proy_Final / Denom_Ha
      ) %>%
      select(Módulo, Turno, Variedad, Kg_Por_Ha)
    
    df_prog %>%
      select(-any_of(c("Área", "Área.x", "Área.y", "Módulo", "Turno", "Variedad"))) %>%
      left_join(df_valvulas %>% select(id, Módulo, Turno, Variedad, Área), by = "id") %>%
      mutate(Área = coalesce(as.numeric(Área), 0)) %>%
      left_join(tasas_turno, by = c("Módulo", "Turno", "Variedad")) %>%
      mutate(
        Kg_Lunes     = ifelse(Tipo_Registro == "Proyeccion" & !is.na(Kg_Por_Ha), round(Área * (coalesce(as.numeric(Lunes), 0) / 100) * Kg_Por_Ha, 2), Kg_Lunes),
        Kg_Martes    = ifelse(Tipo_Registro == "Proyeccion" & !is.na(Kg_Por_Ha), round(Área * (coalesce(as.numeric(Martes), 0) / 100) * Kg_Por_Ha, 2), Kg_Martes),
        Kg_Miércoles = ifelse(Tipo_Registro == "Proyeccion" & !is.na(Kg_Por_Ha), round(Área * (coalesce(as.numeric(Miércoles), 0) / 100) * Kg_Por_Ha, 2), Kg_Miércoles),
        Kg_Jueves    = ifelse(Tipo_Registro == "Proyeccion" & !is.na(Kg_Por_Ha), round(Área * (coalesce(as.numeric(Jueves), 0) / 100) * Kg_Por_Ha, 2), Kg_Jueves),
        Kg_Viernes   = ifelse(Tipo_Registro == "Proyeccion" & !is.na(Kg_Por_Ha), round(Área * (coalesce(as.numeric(Viernes), 0) / 100) * Kg_Por_Ha, 2), Kg_Viernes),
        Kg_Sábado    = ifelse(Tipo_Registro == "Proyeccion" & !is.na(Kg_Por_Ha), round(Área * (coalesce(as.numeric(Sábado), 0) / 100) * Kg_Por_Ha, 2), Kg_Sábado)
      ) %>%
      select(any_of(names(df_prog)))
  }
  
  # Función que prorratea los Kilos de Cosecha Campo entre las válvulas pintadas del filtro activo proporcional a su área cosechada
  recalcular_kg_cosecha_campo <- function(df_prog, kgs_dias = NULL, modo = "CosechaCampo", pasada = NULL, ids_alcance = NULL) {
    dias <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
    pasadas_proc <- if (!is.null(pasada)) pasada else c(1, 2)
    
    for (d in dias) {
      col_kg <- paste0("Kg_", d)
      if (!col_kg %in% names(df_prog)) df_prog[[col_kg]] <- 0
      df_prog[[col_kg]] <- coalesce(as.numeric(df_prog[[col_kg]]), 0)
    }
    
    for (p in pasadas_proc) {
      idx_filtro <- which(
        df_prog$Tipo_Registro == modo &
        df_prog$Num_Pasada == p &
        (if (!is.null(ids_alcance)) df_prog$id %in% ids_alcance else TRUE)
      )
      if (length(idx_filtro) == 0) next
      
      sub_prog <- df_prog[idx_filtro, ] %>%
        select(-any_of(c("Área", "Área.x", "Área.y"))) %>%
        left_join(df_valvulas %>% select(id, Área), by = "id") %>%
        mutate(Área = coalesce(as.numeric(Área), 0))
      
      for (d in dias) {
        if (!is.null(kgs_dias) && is.null(kgs_dias[[d]])) next
        
        col_pct <- d
        col_kg  <- paste0("Kg_", d)
        
        ha_valv_dia <- sub_prog$Área * (coalesce(as.numeric(sub_prog[[col_pct]]), 0) / 100)
        tot_ha_dia  <- sum(ha_valv_dia, na.rm = TRUE)
        
        tot_kg_dia <- if (!is.null(kgs_dias) && !is.null(kgs_dias[[d]])) {
          max(0, suppressWarnings(as.numeric(kgs_dias[[d]])), na.rm = TRUE)
        } else {
          sum(sub_prog[[col_kg]], na.rm = TRUE)
        }
        
        if (tot_ha_dia > 0 && tot_kg_dia > 0) {
          df_prog[[col_kg]][idx_filtro] <- round(tot_kg_dia * (ha_valv_dia / tot_ha_dia), 2)
        } else if (tot_ha_dia == 0 || tot_kg_dia == 0) {
          df_prog[[col_kg]][idx_filtro] <- 0
        }
      }
    }
    
    df_prog
  }
  
  semanas_sincronizadas_sesion <- reactiveVal(numeric(0))
  
  # Cargar datos cuando cambia la semana seleccionada
  observeEvent(input$sel_semana, {
    req(input$sel_semana)
    sem_num <- suppressWarnings(as.numeric(input$sel_semana))
    req(!is.na(sem_num))
    
    sem_prev <- isolate(semana_activa_rv())
    # Protección del lado del servidor: si hay cambios sin guardar y no se ha confirmado descartarlos,
    # bloquear el cambio de semana, revertir el selector y mostrar el modal sin tocar los trazos en pantalla
    if (isTRUE(isolate(cambios_pendientes_rv())) &&
        !isTRUE(isolate(forzando_cambio_semana_rv())) &&
        !is.null(sem_prev) && !is.na(sem_prev) && sem_num != sem_prev) {
      session$sendCustomMessage("revertir_selector_semana", list(semana = as.character(sem_prev)))
      mostrar_modal_cambios_semana(sem_prev, sem_num)
      return()
    }
    
    forzando_cambio_semana_rv(FALSE)
    semana_activa_rv(sem_num)
    
    session$sendCustomMessage("mostrar_loader", list(
      titulo = sprintf("Cargando Semana %02d...", sem_num),
      subtitulo = "Calculando proyección, histórico y distribución de kilos"
    ))
    
    # 1. Rutas de campo/proyección guardadas en disco/nube
    df_hist <- rutas_disco_rv() %>%
      filter(Campaña == "2026", Semana == sem_num) %>%
      mutate(Tipo_Registro = as.character(ifelse(Tipo_Registro == "Cosecha", "CosechaCampo", Tipo_Registro)))
    
    # En shinyapps.io (o en la primera visita a esta semana en la sesión), consultar siempre Google Sheets
    # para no quedarse con datos viejos empaquetados en rutas_cosecha_mapa.rds
    debe_consultar_sheets <- es_entorno_nube || !(sem_num %in% isolate(semanas_sincronizadas_sesion())) || nrow(df_hist) == 0
    if (debe_consultar_sheets) {
      df_sheets <- obtener_rutas_google_sheets(sem_num)
      if (!is.null(df_sheets)) {
        semanas_sincronizadas_sesion(unique(c(isolate(semanas_sincronizadas_sesion()), sem_num)))
        if (nrow(df_sheets) > 0 && "Semana" %in% names(df_sheets)) {
          df_hist <- df_sheets %>% filter(Semana == sem_num)
        } else if (es_entorno_nube) {
          df_hist <- esquema_vacio_rutas()
        }
        if (nrow(df_sheets) > 0 || es_entorno_nube) {
          df_total <- rutas_disco_rv() %>%
            filter(!(Campaña == "2026" & Semana == sem_num & Tipo_Registro %in% c("Proyeccion", "CosechaCampo", "Cosecha"))) %>%
            bind_rows(df_hist)
          guardar_rutas_disco(df_total)
          rutas_disco_rv(df_total)
        }
      }
    }
    
    # 2. Cosecha Oficial desde histórico de balanza de packing
    df_oficial <- obtener_cosecha_oficial_semana(sem_num)
    if (!is.null(df_oficial) && nrow(df_oficial) > 0) {
      df_oficial <- df_oficial %>% mutate(Tipo_Registro = as.character(Tipo_Registro))
    }
    rv_mtime_rdata(Sys.time())
    
    df_combinado <- df_hist
    if (!is.null(df_oficial) && nrow(df_oficial) > 0) {
      df_hist_sub <- if (nrow(df_hist) > 0 && "Tipo_Registro" %in% names(df_hist)) {
        df_hist %>%
          filter(Tipo_Registro != "CosechaOficial") %>%
          mutate(Tipo_Registro = as.character(Tipo_Registro))
      } else {
        esquema_vacio_rutas()
      }
      
      if (nrow(df_hist_sub) > 0) {
        df_combinado <- bind_rows(df_hist_sub, df_oficial)
      } else {
        df_combinado <- df_oficial
      }
    }
    
    base <- esquema_base_semana()
    if (nrow(df_combinado) > 0) {
      base <- base %>%
        select(Tipo_Registro, Num_Pasada, id) %>%
        left_join(
          df_combinado %>% select(Tipo_Registro, Num_Pasada, id, Lunes, Martes, Miércoles, Jueves, Viernes, Sábado,
                                  Kg_Lunes, Kg_Martes, Kg_Miércoles, Kg_Jueves, Kg_Viernes, Kg_Sábado, cells_str),
          by = c("Tipo_Registro", "Num_Pasada", "id")
        ) %>%
        mutate(
          across(c(Lunes, Martes, Miércoles, Jueves, Viernes, Sábado,
                   Kg_Lunes, Kg_Martes, Kg_Miércoles, Kg_Jueves, Kg_Viernes, Kg_Sábado), ~ coalesce(as.numeric(.), 0)),
          cells_str = coalesce(as.character(cells_str), "")
        )
    }
    # Respetar los kilos guardados para Proyección y Cosecha Campo sin sobreescribir automáticamente
    programacion_semana(base)
    
    filas_activas <- base %>%
      filter((Lunes + Martes + Miércoles + Jueves + Viernes + Sábado) > 0) %>%
      select(id, Tipo_Registro, Num_Pasada, Lunes, Martes, Miércoles, Jueves, Viernes, Sábado, cells_str, Kg_Lunes, Kg_Martes, Kg_Miércoles, Kg_Jueves, Kg_Viernes, Kg_Sábado)
    
    session$sendCustomMessage("cargar_estado_semana", list(
      rows = purrr::transpose(filas_activas)
    ))
    cambios_pendientes_rv(FALSE)
    session$sendCustomMessage("limpiar_cambios_pendientes", list())
  }, ignoreInit = FALSE)
  
  persistir_semana_actual <- function(df_prog_semana, modo = NULL, pasada = NULL, mostrar_notif = FALSE, sincronizar_sheets = FALSE) {
    sem_num <- as.numeric(isolate(semana_activa_rv()))
    if (is.na(sem_num)) sem_num <- suppressWarnings(as.numeric(isolate(input$sel_semana)))
    if (is.na(sem_num)) return(FALSE)
    
    m_act <- if (!is.null(modo)) modo else modo_activo()
    p_act <- if (!is.null(pasada)) as.numeric(pasada) else pasada_activa()
    
    usr <- usuario_autenticado_rv()
    es_admin <- if (!is.null(usr)) usr$rol == "Administrador" else TRUE
    modulos_usr <- if (!is.null(usr) && !es_admin) usr$modulos[[1]] else 1:14
    
    ids_alcance <- isolate(catalogo_filtrado()$id)
    if (!es_admin) {
      ids_usuario <- df_valvulas %>% filter(suppressWarnings(as.numeric(Módulo)) %in% modulos_usr) %>% pull(id)
      ids_alcance <- if (length(ids_alcance) > 0 && length(ids_alcance) < nrow(df_valvulas)) {
        intersect(ids_alcance, ids_usuario)
      } else {
        ids_usuario
      }
      hay_filtro_activo <- TRUE
    } else {
      hay_filtro_activo <- length(ids_alcance) > 0 && length(ids_alcance) < nrow(df_valvulas)
    }
    
    # Solo persistir rutas editables (Proyección y Cosecha Campo), sin sobreescribir Cosecha Oficial
    df_guardar_sem <- df_prog_semana %>%
      filter(Tipo_Registro %in% c("Proyeccion", "CosechaCampo", "Cosecha")) %>%
      mutate(Tipo_Registro = as.character(ifelse(Tipo_Registro == "Cosecha", "CosechaCampo", Tipo_Registro))) %>%
      filter((Lunes + Martes + Miércoles + Jueves + Viernes + Sábado) > 0) %>%
      select(-any_of(c("Área", "Área.x", "Área.y", "codigo", "Grupo", "Sector", "Módulo", "Turno", "Variedad"))) %>%
      left_join(df_valvulas %>% select(id, codigo, Grupo, Sector, Módulo, Turno, Variedad, Área), by = "id") %>%
      mutate(
        Campaña             = "2026",
        Semana              = sem_num,
        Ha_Lunes            = round(Área * (Lunes / 100), 2),
        Ha_Martes           = round(Área * (Martes / 100), 2),
        Ha_Miércoles        = round(Área * (Miércoles / 100), 2),
        Ha_Jueves           = round(Área * (Jueves / 100), 2),
        Ha_Viernes          = round(Área * (Viernes / 100), 2),
        Ha_Sábado           = round(Área * (Sábado / 100), 2),
        Ha_Total_Prog       = round(Ha_Lunes + Ha_Martes + Ha_Miércoles + Ha_Jueves + Ha_Viernes + Ha_Sábado, 2),
        Kg_Total_Prog       = round(Kg_Lunes + Kg_Martes + Kg_Miércoles + Kg_Jueves + Kg_Viernes + Kg_Sábado, 2),
        Fecha_Actualizacion = Sys.time()
      ) %>%
      select(
        Campaña, Semana, Tipo_Registro, id, codigo, Grupo, Sector, Módulo, Turno, Variedad, Área, Num_Pasada,
        Lunes, Martes, Miércoles, Jueves, Viernes, Sábado,
        Ha_Lunes, Ha_Martes, Ha_Miércoles, Ha_Jueves, Ha_Viernes, Ha_Sábado, Ha_Total_Prog,
        Kg_Lunes, Kg_Martes, Kg_Miércoles, Kg_Jueves, Kg_Viernes, Kg_Sábado, Kg_Total_Prog,
        cells_str, Fecha_Actualizacion
      )
    
    # ALCANCE DE ENVÍO (solo lo propio): válvulas que este usuario puede/ha modificado. Se limita siempre a sus módulos
    # permitidos, de modo que NUNCA se envíen ni se pisen válvulas de otros usuarios (aunque su sesión tenga copias antiguas).
    if (!exists("ids_usuario", inherits = FALSE)) ids_usuario <- df_valvulas$id
    ids_scope_envio <- NULL
    ids_protegidos_locales <- ids_alcance
    if (sincronizar_sheets && hay_filtro_activo) {
      prev_disco <- isolate(rutas_disco_rv()) %>%
        filter(Campaña == "2026", Semana == sem_num, Tipo_Registro == m_act, Num_Pasada == p_act) %>%
        select(id, Lunes_p = Lunes, Martes_p = Martes, Miercoles_p = Miércoles, Jueves_p = Jueves, Viernes_p = Viernes, Sabado_p = Sábado,
               Kg_Lun_p = Kg_Lunes, Kg_Mar_p = Kg_Martes, Kg_Mie_p = Kg_Miércoles, Kg_Jue_p = Kg_Jueves, Kg_Vie_p = Kg_Viernes, Kg_Sab_p = Kg_Sábado)
      act_modo <- df_prog_semana %>%
        filter(Tipo_Registro == m_act, Num_Pasada == p_act) %>%
        left_join(prev_disco, by = "id") %>%
        mutate(across(ends_with("_p"), ~ coalesce(as.numeric(.), 0))) %>%
        filter(
          abs(Lunes - Lunes_p) > 0.01 | abs(Martes - Martes_p) > 0.01 | abs(Miércoles - Miercoles_p) > 0.01 |
          abs(Jueves - Jueves_p) > 0.01 | abs(Viernes - Viernes_p) > 0.01 | abs(Sábado - Sabado_p) > 0.01 |
          abs(Kg_Lunes - Kg_Lun_p) > 0.01 | abs(Kg_Martes - Kg_Mar_p) > 0.01 | abs(Kg_Miércoles - Kg_Mie_p) > 0.01 |
          abs(Kg_Jueves - Kg_Jue_p) > 0.01 | abs(Kg_Viernes - Kg_Vie_p) > 0.01 | abs(Kg_Sábado - Kg_Sab_p) > 0.01
        )
      # MERGE MULTI-USUARIO COOPERATIVO (2 o más personas en el mismo grupo/sector):
      # Si el usuario no aplicó un subfiltro específico, protegemos solo las válvulas que realmente
      # modificó en esta sesión (act_modo$id) para no pisar las que su compañero del mismo sector guardó.
      filtro_es_especifico <- length(ids_alcance) < length(ids_usuario)
      ids_protegidos_locales <- if (filtro_es_especifico) {
        unique(c(ids_alcance, act_modo$id))
      } else {
        if (length(act_modo$id) > 0) act_modo$id else ids_alcance
      }
      ids_protegidos_locales <- intersect(ids_protegidos_locales, ids_usuario)
      ids_scope_envio <- ids_protegidos_locales
    }
    
    # MERGE MULTI-USUARIO: Si vamos a sincronizar con Google Sheets y el usuario tiene un filtro activo
    # (ej. solo su Grupo/Módulo/Turno/Válvula), descargar el estado remoto actual de la semana para no pisar otros grupos
    # que otro usuario haya guardado hace unos minutos, pero conservando SIEMPRE tanto el filtro activo como
    # cualquier válvula que este usuario haya editado previamente en la sesión actual (ej. al prorratear kilos por grupos de válvulas).
    if (sincronizar_sheets) {
      df_remoto <- obtener_rutas_google_sheets(sem_num)
      if (!is.null(df_remoto) && nrow(df_remoto) > 0) {
        df_remoto <- df_remoto %>% filter(Semana == sem_num)
        if (hay_filtro_activo) {
          # Conservar de Google Sheets: (a) otros modos/pasadas, y (b) en el mismo modo/pasada, las válvulas que este usuario NO tocó
          remoto_conservar <- df_remoto %>%
            filter(!(Tipo_Registro == m_act & Num_Pasada == p_act & id %in% ids_protegidos_locales))
          local_alcance <- df_guardar_sem %>%
            filter(Tipo_Registro == m_act, Num_Pasada == p_act, id %in% ids_protegidos_locales)
          df_guardar_sem <- bind_rows(remoto_conservar, local_alcance)
        } else {
          # Sin filtro activo: conservar otros modos/pasadas de Google Sheets y actualizar este modo/pasada completo
          remoto_otros <- df_remoto %>%
            filter(!(Tipo_Registro == m_act & Num_Pasada == p_act))
          local_modo <- df_guardar_sem %>%
            filter(Tipo_Registro == m_act, Num_Pasada == p_act)
          df_guardar_sem <- bind_rows(remoto_otros, local_modo)
        }
      }
    }
    
    df_total <- rutas_disco_rv() %>%
      filter(!(Campaña == "2026" & Semana == sem_num & Tipo_Registro %in% c("Proyeccion", "CosechaCampo", "Cosecha"))) %>%
      bind_rows(df_guardar_sem)
    
    ok <- guardar_rutas_disco(df_total)
    if (ok) {
      rutas_disco_rv(df_total)
      ultimo_guardado_rv(formatear_hora_peru(Sys.time(), "%H:%M:%S"))
      if (mostrar_notif) {
        showNotification(sprintf("✓ Ruta de Semana %02d guardada en disco.", sem_num), type = "message", duration = 3)
      }
    }
    
    # Sincronización con Google Sheets Webhook (sincroniza inserciones, modificaciones y eliminaciones)
    if (sincronizar_sheets) {
      rutas_modo <- if (nrow(df_guardar_sem) > 0) {
        df_guardar_sem %>% filter(Tipo_Registro == m_act, Num_Pasada == p_act)
      } else {
        tibble()
      }
      
      modo_label_sheet <- if (m_act == "Proyeccion") "Proyección" else "Cosecha Campo"
      
      construir_rutas_json <- function(df_r) {
      lista_rutas_json <- vector("list", nrow(df_r))
      if (nrow(df_r) > 0) {
        for (i in seq_len(nrow(df_r))) {
          rm <- df_r[i, ]
          lista_rutas_json[[i]] <- list(
            id          = coalesce(as.character(rm$id), ""),
            grupo       = coalesce(as.character(rm$Grupo), ""),
            sector      = coalesce(as.character(rm$Sector), ""),
            modulo      = coalesce(as.character(rm$Módulo), ""),
            turno       = coalesce(as.character(rm$Turno), ""),
            codigo      = coalesce(as.character(rm$codigo), ""),
            variedad    = coalesce(as.character(rm$Variedad), ""),
            area_fisica = coalesce(as.numeric(rm$Área), 0),
            pct_lun     = coalesce(as.numeric(rm$Lunes), 0),
            ha_lun      = coalesce(as.numeric(rm$Ha_Lunes), 0),
            kg_lun      = coalesce(as.numeric(rm$Kg_Lunes), 0),
            pct_mar     = coalesce(as.numeric(rm$Martes), 0),
            ha_mar      = coalesce(as.numeric(rm$Ha_Martes), 0),
            kg_mar      = coalesce(as.numeric(rm$Kg_Martes), 0),
            pct_mie     = coalesce(as.numeric(rm$Miércoles), 0),
            ha_mie      = coalesce(as.numeric(rm$Ha_Miércoles), 0),
            kg_mie      = coalesce(as.numeric(rm$Kg_Miércoles), 0),
            pct_jue     = coalesce(as.numeric(rm$Jueves), 0),
            ha_jue      = coalesce(as.numeric(rm$Ha_Jueves), 0),
            kg_jue      = coalesce(as.numeric(rm$Kg_Jueves), 0),
            pct_vie     = coalesce(as.numeric(rm$Viernes), 0),
            ha_vie      = coalesce(as.numeric(rm$Ha_Viernes), 0),
            kg_vie      = coalesce(as.numeric(rm$Kg_Viernes), 0),
            pct_sab     = coalesce(as.numeric(rm$Sábado), 0),
            ha_sab      = coalesce(as.numeric(rm$Ha_Sábado), 0),
            kg_sab      = coalesce(as.numeric(rm$Kg_Sábado), 0),
            pct_total   = coalesce(as.numeric(rm$Lunes + rm$Martes + rm$Miércoles + rm$Jueves + rm$Viernes + rm$Sábado), 0),
            ha_total    = coalesce(as.numeric(rm$Ha_Total_Prog), 0),
            kg_total    = coalesce(as.numeric(rm$Kg_Total_Prog), 0)
          )
        }
      }
      lista_rutas_json
      }
      
      # Estado combinado (compatibilidad con la versión antigua del script de Google)
      lista_rutas_json <- construir_rutas_json(rutas_modo)
      
      # ENVÍO PARCIAL: cuando hay filtro/permisos, el webhook recibe además SOLO las válvulas de este usuario
      # (rutas_usuario) y el alcance que puede reemplazar (alcance_codigos). Así Google Sheets conserva intactas
      # (con su evaluador, versión y hora originales) las filas de otros usuarios y el historial solo registra lo propio.
      es_parcial <- !is.null(ids_scope_envio)
      rutas_usuario_df <- rutas_modo
      if (es_parcial && nrow(rutas_modo) > 0) {
        rutas_usuario_df <- rutas_modo %>% filter(id %in% ids_scope_envio)
      }
      n_envio <- if (es_parcial) nrow(rutas_usuario_df) else nrow(rutas_modo)
      
      payload <- list(
        campana   = "2026",
        semana    = sem_num,
        modo      = modo_label_sheet,
        pasada    = p_act,
        evaluador = if (!is.null(usr)) usr$nombre else Sys.getenv("USERNAME", "Evaluador"),
        rutas     = lista_rutas_json
      )
      if (es_parcial) {
        payload$parcial         <- TRUE
        payload$alcance_codigos <- as.list(unique(as.character(df_valvulas$codigo[df_valvulas$id %in% ids_scope_envio])))
        payload$rutas_usuario   <- construir_rutas_json(rutas_usuario_df)
      }
      
      tryCatch({
        res <- httr::POST(
          url_webhook_sheets,
          body = jsonlite::toJSON(payload, auto_unbox = TRUE),
          httr::content_type_json(),
          httr::timeout(35)
        )
        if (httr::status_code(res) == 200) {
          resp_txt <- httr::content(res, as = "text", encoding = "UTF-8")
          resp_json <- tryCatch(jsonlite::fromJSON(resp_txt), error = function(e) NULL)
          v_tag <- if (!is.null(resp_json$version)) resp_json$version else "OK"
          if (n_envio > 0) {
            showNotification(sprintf("☁️ Google Sheets actualizado (%s - %s - %d válvulas%s).", modo_label_sheet, v_tag, n_envio, if (es_parcial) " propias" else ""), type = "message", duration = 4)
          } else {
            showNotification(sprintf("☁️ Google Sheets actualizado: ruta eliminada (%s - %dª Pasada).", modo_label_sheet, p_act), type = "message", duration = 4)
          }
        } else {
          showNotification(sprintf("⚠️ Error Sheets (HTTP %d)", httr::status_code(res)), type = "warning", duration = 4)
        }
      }, error = function(e) {
        if (mostrar_notif) {
          showNotification("⚠️ No se pudo conectar con Google Sheets (se guardó en disco local).", type = "warning", duration = 4)
        }
      })
    }
    
    ok
  }
  
  # Recibir trazos desde el Canvas JS y conservar/recalcular Kilos
  observeEvent(input$mapa_programacion_json, {
    req(input$mapa_programacion_json)
    raw_json <- trimws(as.character(input$mapa_programacion_json))
    
    prev_state <- programacion_semana()
    base <- esquema_base_semana() %>% select(Tipo_Registro, Num_Pasada, id)
    
    if (nzchar(raw_json) && raw_json != "[]" && raw_json != "null") {
      parsed <- tryCatch(jsonlite::fromJSON(raw_json), error = function(e) NULL)
      if (is.data.frame(parsed) && nrow(parsed) > 0) {
        clean_items <- tibble(
          id            = as.character(parsed$id),
          Tipo_Registro = as.character(parsed$Tipo_Registro),
          Num_Pasada    = as.numeric(parsed$Num_Pasada),
          Lunes         = coalesce(as.numeric(parsed$lunes), 0),
          Martes        = coalesce(as.numeric(parsed$martes), 0),
          Miércoles     = coalesce(as.numeric(parsed$miercoles), 0),
          Jueves        = coalesce(as.numeric(parsed$jueves), 0),
          Viernes       = coalesce(as.numeric(parsed$viernes), 0),
          Sábado        = coalesce(as.numeric(parsed$sabado), 0),
          cells_str     = coalesce(as.character(parsed$cells_str), "")
        )
        base <- base %>%
          left_join(clean_items, by = c("Tipo_Registro", "Num_Pasada", "id"))
      }
    }
    
    for (col_d in c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")) {
      if (!col_d %in% names(base)) base[[col_d]] <- 0
      base[[col_d]] <- coalesce(as.numeric(base[[col_d]]), 0)
    }
    if (!"cells_str" %in% names(base)) base$cells_str <- ""
    base$cells_str <- coalesce(as.character(base$cells_str), "")
    
    # Conservar Kg previos de cada válvula (si una celda fue borrada a 0%, poner su Kg en 0;
    # si el área disminuyó, reducir Kg proporcionalmente; las nuevas válvulas pintadas inician en 0 kg)
    base <- base %>%
      left_join(
        prev_state %>% select(
          Tipo_Registro, Num_Pasada, id,
          Prev_Lun = Lunes, Prev_Mar = Martes, Prev_Mie = Miércoles,
          Prev_Jue = Jueves, Prev_Vie = Viernes, Prev_Sab = Sábado,
          Kg_Lunes, Kg_Martes, Kg_Miércoles, Kg_Jueves, Kg_Viernes, Kg_Sábado
        ),
        by = c("Tipo_Registro", "Num_Pasada", "id")
      ) %>%
      mutate(
        Kg_Lunes = case_when(
          Lunes <= 0 ~ 0,
          !is.na(Prev_Lun) & Prev_Lun > 0 & Lunes < Prev_Lun ~ round(coalesce(Kg_Lunes, 0) * (Lunes / Prev_Lun), 2),
          TRUE ~ coalesce(Kg_Lunes, 0)
        ),
        Kg_Martes = case_when(
          Martes <= 0 ~ 0,
          !is.na(Prev_Mar) & Prev_Mar > 0 & Martes < Prev_Mar ~ round(coalesce(Kg_Martes, 0) * (Martes / Prev_Mar), 2),
          TRUE ~ coalesce(Kg_Martes, 0)
        ),
        Kg_Miércoles = case_when(
          Miércoles <= 0 ~ 0,
          !is.na(Prev_Mie) & Prev_Mie > 0 & Miércoles < Prev_Mie ~ round(coalesce(Kg_Miércoles, 0) * (Miércoles / Prev_Mie), 2),
          TRUE ~ coalesce(Kg_Miércoles, 0)
        ),
        Kg_Jueves = case_when(
          Jueves <= 0 ~ 0,
          !is.na(Prev_Jue) & Prev_Jue > 0 & Jueves < Prev_Jue ~ round(coalesce(Kg_Jueves, 0) * (Jueves / Prev_Jue), 2),
          TRUE ~ coalesce(Kg_Jueves, 0)
        ),
        Kg_Viernes = case_when(
          Viernes <= 0 ~ 0,
          !is.na(Prev_Vie) & Prev_Vie > 0 & Viernes < Prev_Vie ~ round(coalesce(Kg_Viernes, 0) * (Viernes / Prev_Vie), 2),
          TRUE ~ coalesce(Kg_Viernes, 0)
        ),
        Kg_Sábado = case_when(
          Sábado <= 0 ~ 0,
          !is.na(Prev_Sab) & Prev_Sab > 0 & Sábado < Prev_Sab ~ round(coalesce(Kg_Sábado, 0) * (Sábado / Prev_Sab), 2),
          TRUE ~ coalesce(Kg_Sábado, 0)
        )
      ) %>%
      select(-starts_with("Prev_"))
    
    # Cosecha Oficial es inmutable desde el mapa (solo lectura, proveniente de balanza packing).
    # La preservamos intacta desde el estado previo para que el canvas de dibujo nunca la sobreescriba a cero.
    oficial_prev <- prev_state %>% filter(Tipo_Registro == "CosechaOficial")
    
    base_editables <- base %>%
      filter(Tipo_Registro %in% c("Proyeccion", "CosechaCampo"))
    
    # Asegurar que base_completa tenga exactamente las mismas columnas limpias sin residuos de Área
    base_completa <- bind_rows(
      base_editables,
      oficial_prev %>% select(any_of(names(base_editables)))
    )
    # En Proyección y Cosecha Campo el trazado en canvas define el avance/hectáreas,
    # mientras que los kilos se asignan o prorratean al ingresar el valor por día y dar Enter.
    programacion_semana(base_completa)
  })
  
  # ============================================================================
  # DISTRIBUCIÓN AUTOMÁTICA DE KILOS POR HA AL INGRESAR KILOS EN UN DÍA
  # (Aplica exclusivamente sobre las válvulas de la selección/filtro activo)
  # ============================================================================
  observeEvent(input$ingresar_kg_dia, {
    info <- input$ingresar_kg_dia
    req(!is.null(info$dia))
    dia_col <- as.character(info$dia)
    kg_val  <- max(0, suppressWarnings(as.numeric(info$kg)), na.rm = TRUE)
    
    m_act   <- modo_activo()
    if (m_act == "CosechaOficial") {
      showNotification("🔒 En Cosecha Oficial los kilos son oficiales de balanza y no se pueden modificar.", type = "warning", duration = 4)
      return()
    }
    p_act      <- pasada_activa()
    df_prog    <- programacion_semana()
    ids_filtro <- catalogo_filtrado()$id
    
    sub_dia <- df_prog %>%
      filter(Tipo_Registro == m_act, Num_Pasada == p_act, id %in% ids_filtro) %>%
      select(-any_of(c("Área", "Área.x", "Área.y"))) %>%
      left_join(df_valvulas %>% select(id, Área), by = "id") %>%
      mutate(
        Área   = coalesce(as.numeric(Área), 0),
        Ha_Dia = Área * (coalesce(as.numeric(.data[[dia_col]]), 0) / 100)
      )
    
    tot_ha_dia <- sum(sub_dia$Ha_Dia, na.rm = TRUE)
    if (tot_ha_dia <= 0 && kg_val > 0) {
      showNotification(
        sprintf("⚠️ Primero pinta al menos una válvula en %s dentro del filtro actual (%s - %dª Pasada) para repartir los kilos.",
                dia_col, if (m_act == "Proyeccion") "Proyección" else "Cosecha Campo", p_act),
        type = "warning", duration = 4
      )
      return()
    }
    
    kgs_lista <- list()
    kgs_lista[[dia_col]] <- kg_val
    
    if (m_act %in% c("CosechaCampo", "Proyeccion")) {
      df_prog <- recalcular_kg_cosecha_campo(df_prog, kgs_dias = kgs_lista, modo = m_act, pasada = p_act, ids_alcance = ids_filtro)
    }
    
    programacion_semana(df_prog)
    cambios_pendientes_rv(TRUE)
    
    n_valvs_pintadas <- sum(sub_dia$Ha_Dia > 0, na.rm = TRUE)
    showNotification(
      sprintf("✓ %s kg prorrateados en %s entre %d válvula(s) del filtro activo (%s ha).",
              format(round(kg_val), big.mark = ","), dia_col, n_valvs_pintadas, round(tot_ha_dia, 2)),
      type = "message", duration = 3
    )
  })
  
  ejecutar_guardado_con_kilos <- function(info_kg = NULL) {
    session$sendCustomMessage("mostrar_loader", list(
      titulo = "Guardando Ruta...",
      subtitulo = "Sincronizando con Google Sheets y Disco"
    ))
    showNotification("⏳ Guardando y sincronizando con Google Sheets...", type = "message", duration = 2)
    
    df_prog <- programacion_semana()
    m_act   <- if (!is.null(info_kg$modo)) {
      m <- as.character(info_kg$modo)
      if (m == "Cosecha") "CosechaCampo" else m
    } else {
      modo_activo()
    }
    p_act   <- if (!is.null(info_kg$pasada)) as.numeric(info_kg$pasada) else pasada_activa()
    
    # Si estamos en Cosecha Campo o Proyección y el usuario modificó algún input diario antes de dar Guardar,
    # prorratear únicamente los días cuyo valor cambió (o donde haya válvulas pintadas con 0 kg) dentro del filtro activo
    if (m_act %in% c("CosechaCampo", "Proyeccion") && !is.null(info_kg) && !is.null(info_kg$kgs)) {
      ids_filtro <- catalogo_filtrado()$id
      sub_f <- df_prog %>%
        filter(Tipo_Registro == m_act, Num_Pasada == p_act, id %in% ids_filtro)
      
      kgs_cambiados <- list()
      for (d in c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")) {
        if (!is.null(info_kg$kgs[[d]])) {
          val_in   <- max(0, suppressWarnings(as.numeric(info_kg$kgs[[d]])), na.rm = TRUE)
          col_pct  <- d
          col_kg   <- paste0("Kg_", d)
          val_act  <- sum(sub_f[[col_kg]], na.rm = TRUE)
          hay_valvs_sin_kg <- sum(coalesce(as.numeric(sub_f[[col_pct]]), 0) > 0 & coalesce(as.numeric(sub_f[[col_kg]]), 0) == 0) > 0
          
          if (abs(val_in - round(val_act)) >= 1 || (val_in > 0 && hay_valvs_sin_kg)) {
            kgs_cambiados[[d]] <- val_in
          }
        }
      }
      if (length(kgs_cambiados) > 0) {
        df_prog <- recalcular_kg_cosecha_campo(df_prog, kgs_dias = kgs_cambiados, modo = m_act, pasada = p_act, ids_alcance = ids_filtro)
        programacion_semana(df_prog)
      }
    }
    
    persistir_semana_actual(df_prog, modo = m_act, pasada = p_act, mostrar_notif = TRUE, sincronizar_sheets = TRUE)
    cambios_pendientes_rv(FALSE)
    session$sendCustomMessage("limpiar_cambios_pendientes", list())
    session$sendCustomMessage("ocultar_loader", list())
  }
  
  observeEvent(input$guardar_ruta_con_kilos, {
    ejecutar_guardado_con_kilos(input$guardar_ruta_con_kilos)
  })
  
  # Sincronización manual bajo demanda desde Google Sheets y Google Drive (botón 🔄 Sincronizar)
  observeEvent(input$btn_sincronizar_sheets, {
    sem_num <- as.numeric(semana_activa_rv())
    if (is.na(sem_num)) sem_num <- suppressWarnings(as.numeric(input$sel_semana))
    if (is.na(sem_num)) {
      session$sendCustomMessage("ocultar_loader", list())
      return()
    }
    
    tryCatch({
      df_sheets <- obtener_rutas_google_sheets(sem_num)
      
      # Rutas oficiales de balanza de packing (inmutables) + recarga en caliente de RData desde Drive
      df_oficial <- obtener_cosecha_oficial_semana(sem_num, forzar_nube = TRUE)
      if (!is.null(df_oficial) && nrow(df_oficial) > 0) {
        df_oficial <- df_oficial %>% mutate(Tipo_Registro = as.character(Tipo_Registro))
      }
      rv_mtime_rdata(Sys.time())
      
      if (!is.null(df_sheets) && nrow(df_sheets) > 0) {
        df_sheets <- df_sheets %>% filter(Semana == sem_num)
        
        # Reemplazar las rutas de esa semana en disco y memoria
        df_total <- rutas_disco_rv() %>%
          filter(!(Campaña == "2026" & Semana == sem_num & Tipo_Registro %in% c("Proyeccion", "CosechaCampo", "Cosecha"))) %>%
          bind_rows(df_sheets)
        
        guardar_rutas_disco(df_total)
        rutas_disco_rv(df_total)
        
        df_combinado <- df_sheets
        if (!is.null(df_oficial) && nrow(df_oficial) > 0) {
          df_combinado <- bind_rows(df_sheets, df_oficial)
        }
        
        base <- esquema_base_semana() %>%
          select(Tipo_Registro, Num_Pasada, id) %>%
          left_join(
            df_combinado %>% select(Tipo_Registro, Num_Pasada, id, Lunes, Martes, Miércoles, Jueves, Viernes, Sábado,
                                    Kg_Lunes, Kg_Martes, Kg_Miércoles, Kg_Jueves, Kg_Viernes, Kg_Sábado, cells_str),
            by = c("Tipo_Registro", "Num_Pasada", "id")
          ) %>%
          mutate(
            across(c(Lunes, Martes, Miércoles, Jueves, Viernes, Sábado,
                     Kg_Lunes, Kg_Martes, Kg_Miércoles, Kg_Jueves, Kg_Viernes, Kg_Sábado), ~ coalesce(as.numeric(.), 0)),
            cells_str = coalesce(as.character(cells_str), "")
          )
        
        # Respetar los kilos descargados desde Google Sheets sin sobreescribir automáticamente
        programacion_semana(base)
        
        filas_activas <- base %>%
          filter((Lunes + Martes + Miércoles + Jueves + Viernes + Sábado) > 0) %>%
          select(id, Tipo_Registro, Num_Pasada, Lunes, Martes, Miércoles, Jueves, Viernes, Sábado, cells_str, Kg_Lunes, Kg_Martes, Kg_Miércoles, Kg_Jueves, Kg_Viernes, Kg_Sábado)
        
        session$sendCustomMessage("cargar_estado_semana", list(
          rows = purrr::transpose(filas_activas)
        ))
        
        ultimo_guardado_rv(formatear_hora_peru(Sys.time(), "%H:%M:%S"))
        cambios_pendientes_rv(FALSE)
        session$sendCustomMessage("limpiar_cambios_pendientes", list())
        session$sendCustomMessage("ocultar_loader", list())
        showNotification(sprintf("☁️ Sincronización exitosa: %d registros cargados desde Google Sheets.", nrow(df_sheets)), type = "message", duration = 4)
      } else {
        # Si Google Sheets está vacío para esta semana (ej. se borró la ruta en Sheets):
        df_total <- rutas_disco_rv() %>%
          filter(!(Campaña == "2026" & Semana == sem_num & Tipo_Registro %in% c("Proyeccion", "CosechaCampo", "Cosecha")))
        
        guardar_rutas_disco(df_total)
        rutas_disco_rv(df_total)
        
        base <- esquema_base_semana()
        if (!is.null(df_oficial) && nrow(df_oficial) > 0) {
          base <- base %>%
            select(Tipo_Registro, Num_Pasada, id) %>%
            left_join(
              df_oficial %>% select(Tipo_Registro, Num_Pasada, id, Lunes, Martes, Miércoles, Jueves, Viernes, Sábado,
                                    Kg_Lunes, Kg_Martes, Kg_Miércoles, Kg_Jueves, Kg_Viernes, Kg_Sábado, cells_str),
              by = c("Tipo_Registro", "Num_Pasada", "id")
            ) %>%
            mutate(
              across(c(Lunes, Martes, Miércoles, Jueves, Viernes, Sábado,
                       Kg_Lunes, Kg_Martes, Kg_Miércoles, Kg_Jueves, Kg_Viernes, Kg_Sábado), ~ coalesce(as.numeric(.), 0)),
              cells_str = coalesce(as.character(cells_str), "")
            )
        }
        
        programacion_semana(base)
        
        filas_activas <- base %>%
          filter((Lunes + Martes + Miércoles + Jueves + Viernes + Sábado) > 0) %>%
          select(id, Tipo_Registro, Num_Pasada, Lunes, Martes, Miércoles, Jueves, Viernes, Sábado, cells_str, Kg_Lunes, Kg_Martes, Kg_Miércoles, Kg_Jueves, Kg_Viernes, Kg_Sábado)
        
        session$sendCustomMessage("cargar_estado_semana", list(
          rows = purrr::transpose(filas_activas)
        ))
        
        ultimo_guardado_rv(formatear_hora_peru(Sys.time(), "%H:%M:%S"))
        cambios_pendientes_rv(FALSE)
        session$sendCustomMessage("limpiar_cambios_pendientes", list())
        session$sendCustomMessage("ocultar_loader", list())
        showNotification(sprintf("☁️ Google Sheets no contiene rutas activas para Semana %02d (mapa actualizado).", sem_num), type = "message", duration = 4)
      }
    }, error = function(e) {
      session$sendCustomMessage("ocultar_loader", list())
      showNotification(sprintf("❌ Error al sincronizar: %s", conditionMessage(e)), type = "error", duration = 5)
    })
  })
  
  output$badge_estado_guardado <- renderUI({
    pendientes <- cambios_pendientes_rv()
    hora <- ultimo_guardado_rv()
    
    if (pendientes) {
      span(style = "font-size: 12px; background: rgba(255, 149, 0, 0.14); padding: 7px 13px; border-radius: 20px; font-weight: 600; color: #C25E00; border: 1px solid rgba(255, 149, 0, 0.35);",
           "● Cambios sin guardar")
    } else if (is.null(hora)) {
      NULL
    } else {
      span(style = "font-size: 12px; background: rgba(22, 163, 74, 0.12); padding: 7px 13px; border-radius: 20px; font-weight: 600; color: #15803D;",
           sprintf("✓ Guardado %s", hora))
    }
  })
  
  output$badge_origen_datos <- renderUI({
    # Dependencia reactiva para refrescar cuando cambie el archivo RData
    rv_mtime_rdata()
    
    ruta_usada <- if (exists("ruta_rdata_master") && nzchar(ruta_rdata_master)) ruta_rdata_master else "datos_maestros_cosecha_2026.RData"
    origen_act <- if (exists("origen_rdata_actual") && nzchar(origen_rdata_actual)) origen_rdata_actual else {
      if (grepl("^[Gg]:", ruta_usada)) "DRIVE_LOCAL_G" else if (es_entorno_nube) "NUBE_DRIVE" else "DISCO_LOCAL_D"
    }
    
    # Priorizar la fecha/hora real en que actualizar_data_maestra.R generó el RData (en hora Perú UTC-5)
    fecha_fmt <- if (exists("metadata_actualizacion") && !is.null(metadata_actualizacion$Fecha_Hora_Actualizacion_Str) && nzchar(metadata_actualizacion$Fecha_Hora_Actualizacion_Str)) {
      metadata_actualizacion$Fecha_Hora_Actualizacion_Str
    } else {
      ts_meta  <- if (exists("metadata_actualizacion") && !is.null(metadata_actualizacion$Fecha_Hora_Actualizacion)) {
        tryCatch(as.POSIXct(metadata_actualizacion$Fecha_Hora_Actualizacion), error = function(e) NULL)
      } else NULL
      ts_mtime <- if (!es_entorno_nube && file.exists(ruta_usada)) file.mtime(ruta_usada) else NULL
      ts_final <- if (!is.null(ts_meta) && !is.na(ts_meta[1])) ts_meta[1] else if (!is.null(ts_mtime) && !is.na(ts_mtime[1])) ts_mtime[1] else NULL
      if (!is.null(ts_final)) formatear_hora_peru(ts_final, "%d/%m/%Y %H:%M:%S") else "No disponible"
    }
    
    es_conectado_drive <- origen_act %in% c("NUBE_DRIVE", "DRIVE_LOCAL_G")
    texto_origen <- dplyr::case_when(
      origen_act == "NUBE_DRIVE"       ~ "☁️ Conectado a Google Drive (Nube)",
      origen_act == "DRIVE_LOCAL_G"    ~ "☁️ Conectado a Google Drive",
      origen_act == "EMPAQUETADO_NUBE" ~ "📦 Conectado a Respaldo en Servidor",
      TRUE                             ~ "💻 Conectado a Respaldo Local"
    )
    
    if (es_conectado_drive) {
      div(
        style = "margin-top: 6px; display: inline-flex; align-items: center; gap: 8px; background: rgba(34, 197, 94, 0.12); color: #15803D; padding: 4px 12px; border-radius: 12px; font-size: 11.5px; font-weight: 600; border: 1px solid rgba(34, 197, 94, 0.28);",
        span(style = "width: 8px; height: 8px; border-radius: 50%; background: #22C55E; display: inline-block; box-shadow: 0 0 6px rgba(34, 197, 94, 0.8);"),
        span(texto_origen),
        span(style = "color: #9CA3AF; font-weight: normal;", "•"),
        span(style = "color: #1F2937; font-weight: 500;", sprintf("Última actualización: %s", fecha_fmt))
      )
    } else {
      div(
        style = "margin-top: 6px; display: inline-flex; align-items: center; gap: 8px; background: rgba(245, 158, 11, 0.12); color: #B45309; padding: 4px 12px; border-radius: 12px; font-size: 11.5px; font-weight: 600; border: 1px solid rgba(245, 158, 11, 0.32);",
        span(style = "width: 8px; height: 8px; border-radius: 50%; background: #F59E0B; display: inline-block; box-shadow: 0 0 6px rgba(245, 158, 11, 0.8);"),
        span(texto_origen),
        span(style = "color: #9CA3AF; font-weight: normal;", "•"),
        span(style = "color: #1F2937; font-weight: 500;", sprintf("Última actualización: %s", fecha_fmt))
      )
    }
  })
  
  observeEvent(input$intentar_cambiar_semana, {
    info <- input$intentar_cambiar_semana
    req(!is.null(info))
    sem_ant_raw <- if (!is.null(info$semana_anterior) && nzchar(as.character(info$semana_anterior))) {
      as.character(info$semana_anterior)
    } else {
      as.character(semana_activa_rv())
    }
    sem_nue_raw <- if (!is.null(info$semana_nueva) && nzchar(as.character(info$semana_nueva))) {
      as.character(info$semana_nueva)
    } else {
      ""
    }
    req(nzchar(sem_nue_raw), !is.na(suppressWarnings(as.numeric(sem_nue_raw))))
    mostrar_modal_cambios_semana(sem_ant_raw, sem_nue_raw)
  })
  
  observeEvent(input$btn_confirmar_descartar_cambios, {
    removeModal()
    sem_nue <- semana_pendiente_modal_rv()
    if (is.null(sem_nue) || !nzchar(as.character(sem_nue))) {
      info <- input$intentar_cambiar_semana
      sem_nue <- if (!is.null(info$semana_nueva)) as.character(info$semana_nueva) else NULL
    }
    req(!is.null(sem_nue), nzchar(as.character(sem_nue)))
    cambios_pendientes_rv(FALSE)
    forzando_cambio_semana_rv(TRUE)
    session$sendCustomMessage("forzar_cambio_semana", list(semana = as.character(sem_nue)))
  })
  
  # ============================================================================
  # FILTROS EN CASCADA: GRUPO -> MÓDULO -> TURNO -> VÁLVULA -> VARIEDAD
  # ============================================================================
  observeEvent(input$filtro_grupo, {
    df_u <- df_valvulas
    if (!is.null(input$filtro_grupo) && length(input$filtro_grupo) > 0) {
      mods_sel <- unlist(grupos_modulos[input$filtro_grupo], use.names = FALSE)
      df_u <- df_u %>% filter(suppressWarnings(as.numeric(Módulo)) %in% mods_sel)
    }
    mods_disp <- sort(unique(na.omit(as.character(df_u$Módulo))))
    mods_disp <- mods_disp[order(suppressWarnings(as.numeric(mods_disp)))]
    usr_f <- isolate(usuario_autenticado_rv())
    if (!is.null(usr_f) && usr_f$rol != "Administrador") {
      mods_disp <- intersect(mods_disp, as.character(unlist(usr_f$modulos)))
    }
    sel_prev <- isolate(input$filtro_modulo)
    updateSelectizeInput(session, "filtro_modulo", choices = mods_disp, selected = intersect(sel_prev, mods_disp), server = FALSE)
  }, ignoreInit = FALSE, ignoreNULL = FALSE)
  
  observeEvent(list(input$filtro_grupo, input$filtro_modulo), {
    df_u <- df_valvulas
    if (!is.null(input$filtro_grupo) && length(input$filtro_grupo) > 0) {
      mods_sel <- unlist(grupos_modulos[input$filtro_grupo], use.names = FALSE)
      df_u <- df_u %>% filter(suppressWarnings(as.numeric(Módulo)) %in% mods_sel)
    }
    if (!is.null(input$filtro_modulo) && length(input$filtro_modulo) > 0) {
      df_u <- df_u %>% filter(as.character(Módulo) %in% as.character(input$filtro_modulo))
    }
    turnos_disp <- sort(unique(na.omit(as.character(df_u$Turno))))
    turnos_disp <- turnos_disp[order(suppressWarnings(as.numeric(turnos_disp)))]
    
    sel_prev <- isolate(input$filtro_turno)
    updateSelectizeInput(session, "filtro_turno", choices = turnos_disp, selected = intersect(sel_prev, turnos_disp), server = FALSE)
  }, ignoreInit = FALSE, ignoreNULL = FALSE)
  
  observeEvent(list(input$filtro_grupo, input$filtro_modulo, input$filtro_turno), {
    df_u <- df_valvulas
    if (!is.null(input$filtro_grupo) && length(input$filtro_grupo) > 0) {
      mods_sel <- unlist(grupos_modulos[input$filtro_grupo], use.names = FALSE)
      df_u <- df_u %>% filter(suppressWarnings(as.numeric(Módulo)) %in% mods_sel)
    }
    if (!is.null(input$filtro_modulo) && length(input$filtro_modulo) > 0) {
      df_u <- df_u %>% filter(as.character(Módulo) %in% as.character(input$filtro_modulo))
    }
    if (!is.null(input$filtro_turno) && length(input$filtro_turno) > 0) {
      df_u <- df_u %>% filter(as.character(Turno) %in% as.character(input$filtro_turno))
    }
    valvs_disp <- sort(unique(na.omit(as.character(df_u$Válvula))))
    valvs_disp <- valvs_disp[valvs_disp != ""]
    valvs_disp <- valvs_disp[order(suppressWarnings(as.numeric(valvs_disp)))]
    
    sel_prev <- isolate(input$filtro_valvula)
    updateSelectizeInput(session, "filtro_valvula", choices = valvs_disp, selected = intersect(sel_prev, valvs_disp), server = FALSE)
  }, ignoreInit = FALSE, ignoreNULL = FALSE)
  
  observeEvent(list(input$filtro_grupo, input$filtro_modulo, input$filtro_turno, input$filtro_valvula), {
    df_u <- df_valvulas
    if (!is.null(input$filtro_grupo) && length(input$filtro_grupo) > 0) {
      mods_sel <- unlist(grupos_modulos[input$filtro_grupo], use.names = FALSE)
      df_u <- df_u %>% filter(suppressWarnings(as.numeric(Módulo)) %in% mods_sel)
    }
    if (!is.null(input$filtro_modulo) && length(input$filtro_modulo) > 0) {
      df_u <- df_u %>% filter(as.character(Módulo) %in% as.character(input$filtro_modulo))
    }
    if (!is.null(input$filtro_turno) && length(input$filtro_turno) > 0) {
      df_u <- df_u %>% filter(as.character(Turno) %in% as.character(input$filtro_turno))
    }
    if (!is.null(input$filtro_valvula) && length(input$filtro_valvula) > 0) {
      df_u <- df_u %>% filter(as.character(Válvula) %in% as.character(input$filtro_valvula))
    }
    vars_disp <- sort(unique(na.omit(as.character(df_u$Variedad))))
    vars_disp <- vars_disp[vars_disp != ""]
    
    sel_prev <- isolate(input$filtro_variedad)
    updateSelectizeInput(session, "filtro_variedad", choices = vars_disp, selected = intersect(sel_prev, vars_disp), server = FALSE)
  }, ignoreInit = FALSE, ignoreNULL = FALSE)
  
  observeEvent(input$btn_reset_filtros, {
    updateSelectizeInput(session, "filtro_grupo", selected = character(0))
    updateSelectizeInput(session, "filtro_modulo", choices = mods_iniciales, selected = character(0), server = FALSE)
    updateSelectizeInput(session, "filtro_turno", choices = turnos_iniciales, selected = character(0), server = FALSE)
    updateSelectizeInput(session, "filtro_valvula", choices = valvs_iniciales, selected = character(0), server = FALSE)
    updateSelectizeInput(session, "filtro_variedad", choices = vars_iniciales, selected = character(0), server = FALSE)
  })
  
  catalogo_filtrado <- reactive({
    df_res <- df_valvulas
    if (!is.null(input$filtro_grupo) && length(input$filtro_grupo) > 0) {
      mods_sel <- unlist(grupos_modulos[input$filtro_grupo], use.names = FALSE)
      df_res <- df_res %>% filter(suppressWarnings(as.numeric(Módulo)) %in% mods_sel)
    }
    if (!is.null(input$filtro_modulo) && length(input$filtro_modulo) > 0) {
      df_res <- df_res %>% filter(as.character(Módulo) %in% as.character(input$filtro_modulo))
    }
    if (!is.null(input$filtro_turno) && length(input$filtro_turno) > 0) {
      df_res <- df_res %>% filter(as.character(Turno) %in% as.character(input$filtro_turno))
    }
    if (!is.null(input$filtro_valvula) && length(input$filtro_valvula) > 0) {
      df_res <- df_res %>% filter(as.character(Válvula) %in% as.character(input$filtro_valvula))
    }
    if (!is.null(input$filtro_variedad) && length(input$filtro_variedad) > 0) {
      df_res <- df_res %>% filter(as.character(Variedad) %in% as.character(input$filtro_variedad))
    }
    # Restricción obligatoria para usuarios no administradores: solo sus módulos asignados
    usr_c <- usuario_autenticado_rv()
    if (!is.null(usr_c) && usr_c$rol != "Administrador") {
      df_res <- df_res %>% filter(suppressWarnings(as.numeric(Módulo)) %in% as.numeric(unlist(usr_c$modulos)))
    }
    df_res
  })
  
  # Proyección teórica exacta en Kg según el catálogo filtrado activo y la Hoja Cálculo
  proyeccion_kg_filtro_activo <- reactive({
    cat_f <- catalogo_filtrado() %>%
      group_by(Módulo, Turno, Variedad) %>%
      summarise(Area_Filtro_MTV = sum(Área, na.rm = TRUE), .groups = "drop")
    df_ex <- proyeccion_excel_semana()
    if (is.null(df_ex) || nrow(df_ex) == 0 || nrow(cat_f) == 0) return(0)
    
    # 1. Cruce exacto por Módulo + Turno + Variedad (proporcional al área física de las válvulas filtradas)
    cruce_exacto <- cat_f %>%
      left_join(df_ex, by = c("Módulo", "Turno", "Variedad")) %>%
      left_join(area_fisica_turnos, by = c("Módulo", "Turno", "Variedad")) %>%
      mutate(
        Kg_Proy_Filtro = ifelse(!is.na(Area_Turno) & Area_Turno > 0,
                                Kg_Proy_Turno * (Area_Filtro_MTV / Area_Turno),
                                Kg_Proy_Turno)
      )
    kg_exacto <- sum(cruce_exacto$Kg_Proy_Filtro, na.rm = TRUE)
    
    # 2. Si el Excel trae alguna variedad no registrada en las válvulas del mapa para un (Módulo, Turno)
    #    y el usuario no está filtrando por una variedad específica, incluir ese remanente proporcional al área filtrada
    hay_filtro_var <- !is.null(input$filtro_variedad) && length(input$filtro_variedad) > 0
    kg_remanente <- 0
    if (!hay_filtro_var) {
      remanente_mt <- df_ex %>%
        anti_join(area_fisica_turnos, by = c("Módulo", "Turno", "Variedad")) %>%
        group_by(Módulo, Turno) %>%
        summarise(Kg_Rem_MT = sum(Kg_Proy_Turno, na.rm = TRUE), .groups = "drop")
      if (nrow(remanente_mt) > 0) {
        area_mt_total <- area_fisica_turnos %>%
          group_by(Módulo, Turno) %>%
          summarise(Area_Total_MT = sum(Area_Turno, na.rm = TRUE), .groups = "drop")
        cruce_rem <- cat_f %>%
          group_by(Módulo, Turno) %>%
          summarise(Area_Filtro_MT = sum(Area_Filtro_MTV, na.rm = TRUE), .groups = "drop") %>%
          inner_join(remanente_mt, by = c("Módulo", "Turno")) %>%
          left_join(area_mt_total, by = c("Módulo", "Turno")) %>%
          mutate(Kg_Prop = ifelse(!is.na(Area_Total_MT) & Area_Total_MT > 0, Kg_Rem_MT * (Area_Filtro_MT / Area_Total_MT), Kg_Rem_MT))
        kg_remanente <- sum(cruce_rem$Kg_Prop, na.rm = TRUE)
      }
    }
    kg_exacto + kg_remanente
  })
  
  # Actualizar los badges de proyección sugerida en las 6 tarjetas de día según el filtro activo
  observe({
    kg_teo <- proyeccion_kg_filtro_activo()
    session$sendCustomMessage("actualizar_proy_sugerido", list(
      kg_proy_filtro = round(kg_teo)
    ))
  })
  
  observe({
    usr_m <- usuario_autenticado_rv()
    es_restringido <- !is.null(usr_m) && usr_m$rol != "Administrador"
    hay_filtro <- es_restringido ||
      (!is.null(input$filtro_grupo) && length(input$filtro_grupo) > 0) ||
      (!is.null(input$filtro_modulo) && length(input$filtro_modulo) > 0) ||
      (!is.null(input$filtro_turno) && length(input$filtro_turno) > 0) ||
      (!is.null(input$filtro_valvula) && length(input$filtro_valvula) > 0) ||
      (!is.null(input$filtro_variedad) && length(input$filtro_variedad) > 0)
    
    if (hay_filtro) {
      session$sendCustomMessage("actualizar_filtro_mapa", list(ids = as.list(catalogo_filtrado()$id)))
    } else {
      session$sendCustomMessage("actualizar_filtro_mapa", list(ids = list()))
    }
  })
  
  # Válvulas filtradas del MODO ACTIVO y PASADA ACTIVA
  valvulas_pasada_activa <- reactive({
    cat_f <- catalogo_filtrado()
    m_act <- modo_activo()
    p_act <- pasada_activa()
    
    ep_p <- programacion_semana() %>%
      filter(Tipo_Registro == m_act, Num_Pasada == p_act) %>%
      select(id, Lunes, Martes, Miércoles, Jueves, Viernes, Sábado,
             Kg_Lunes, Kg_Martes, Kg_Miércoles, Kg_Jueves, Kg_Viernes, Kg_Sábado)
    
    ep_all <- programacion_semana() %>%
      filter(Tipo_Registro == m_act) %>%
      group_by(id) %>%
      summarise(
        Pct_Ambas = sum(Lunes + Martes + Miércoles + Jueves + Viernes + Sábado, na.rm = TRUE),
        Kg_Ambas  = sum(Kg_Lunes + Kg_Martes + Kg_Miércoles + Kg_Jueves + Kg_Viernes + Kg_Sábado, na.rm = TRUE),
        .groups   = "drop"
      )
    
    cat_f %>%
      select(-any_of(c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado",
                       "Kg_Lunes", "Kg_Martes", "Kg_Miércoles", "Kg_Jueves", "Kg_Viernes", "Kg_Sábado",
                       "Pct_Ambas", "Kg_Ambas"))) %>%
      left_join(ep_p, by = "id") %>%
      left_join(ep_all, by = "id") %>%
      mutate(
        across(c(Lunes, Martes, Miércoles, Jueves, Viernes, Sábado,
                 Kg_Lunes, Kg_Martes, Kg_Miércoles, Kg_Jueves, Kg_Viernes, Kg_Sábado,
                 Pct_Ambas, Kg_Ambas), ~ coalesce(as.numeric(.), 0)),
        Ha_Lunes       = round(Área * (Lunes / 100), 2),
        Ha_Martes      = round(Área * (Martes / 100), 2),
        Ha_Miércoles   = round(Área * (Miércoles / 100), 2),
        Ha_Jueves      = round(Área * (Jueves / 100), 2),
        Ha_Viernes     = round(Área * (Viernes / 100), 2),
        Ha_Sábado      = round(Área * (Sábado / 100), 2),
        Ha_Total_Prog  = round(Ha_Lunes + Ha_Martes + Ha_Miércoles + Ha_Jueves + Ha_Viernes + Ha_Sábado, 2),
        Kg_Total_Prog  = round(Kg_Lunes + Kg_Martes + Kg_Miércoles + Kg_Jueves + Kg_Viernes + Kg_Sábado, 2),
        Ha_Ambas_Prog  = round(Área * (Pct_Ambas / 100), 2)
      )
  })
  
  # Sincronizar los inputs de Kilos en las 6 tarjetas de día y las etiquetas de válvulas en Canvas
  observe({
    vf <- valvulas_pasada_activa()
    day_totals <- list(
      Lunes     = sum(vf$Kg_Lunes, na.rm = TRUE),
      Martes    = sum(vf$Kg_Martes, na.rm = TRUE),
      Miércoles = sum(vf$Kg_Miércoles, na.rm = TRUE),
      Jueves    = sum(vf$Kg_Jueves, na.rm = TRUE),
      Viernes   = sum(vf$Kg_Viernes, na.rm = TRUE),
      Sábado    = sum(vf$Kg_Sábado, na.rm = TRUE)
    )
    
    filas_kg <- programacion_semana() %>%
      filter((Lunes + Martes + Miércoles + Jueves + Viernes + Sábado) > 0) %>%
      select(id, Tipo_Registro, Num_Pasada, Kg_Lunes, Kg_Martes, Kg_Miércoles, Kg_Jueves, Kg_Viernes, Kg_Sábado)
    
    session$sendCustomMessage("actualizar_kilos_ui", list(
      day_totals  = day_totals,
      valve_kilos = purrr::transpose(filas_kg)
    ))
  })
  
  # Limpiar exclusivamente el DÍA ACTIVO dentro del filtro activo
  observeEvent(input$btn_confirmar_limpiar_dia, {
    info <- input$btn_confirmar_limpiar_dia
    req(info$dia_key, info$dia_nombre)
    dia_k <- as.character(info$dia_key)
    dia_n <- as.character(info$dia_nombre)
    
    m_val <- modo_activo()
    if (m_val == "CosechaOficial") {
      showNotification("🔒 En Cosecha Oficial los datos son oficiales de balanza y no se pueden modificar.", type = "warning", duration = 4)
      return()
    }
    m_act <- if (m_val == "Proyeccion") "Proyección" else "Cosecha Campo"
    p_act <- pasada_activa()
    n_valvs_f <- nrow(catalogo_filtrado())
    
    showModal(modalDialog(
      title = sprintf("🗑️ Borrar %s (%s • %dª Pasada)", dia_n, m_act, p_act),
      p(HTML(sprintf("¿Deseas borrar la programación del día <b>%s</b> en las <b>%d válvula(s)</b> de tu filtro actual?", dia_n, n_valvs_f))),
      div(style = "background: rgba(0, 122, 255, 0.08); border-left: 3px solid #007AFF; padding: 8px 12px; border-radius: 6px; margin: 10px 0; font-size: 12.5px; color: #004085;",
          HTML(sprintf("✓ <b>Protección activa:</b> Solo se borrarán las celdas y kilos de <b>%s</b> en la selección actual. Los demás días (Lunes, Miércoles, etc.) y los módulos fuera de tu filtro quedarán <b>100%% intactos</b>.", dia_n))),
      p(style = "font-size: 11.5px; color: #8E8E93;", "Nota: Puedes usar '↩️ Deshacer' (Ctrl+Z) para revertir este cambio si lo necesitas."),
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("btn_ejecutar_limpiar_dia", sprintf("Sí, borrar solo %s", dia_n), 
                     style = "background: #FF3B30; color: white; border: none; border-radius: 8px; font-weight: 600; padding: 8px 16px;")
      ),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$btn_ejecutar_limpiar_dia, {
    removeModal()
    info <- input$btn_confirmar_limpiar_dia
    req(info$dia_key, info$dia_nombre)
    dia_k <- as.character(info$dia_key)
    dia_n <- as.character(info$dia_nombre)
    p_act <- pasada_activa()
    
    cambios_pendientes_rv(TRUE)
    session$sendCustomMessage("limpiar_dia_activo", list(
      dia_key    = dia_k,
      dia_nombre = dia_n
    ))
    showNotification(sprintf("✓ Se borró la programación de %s (%dª Pasada) en la selección actual. Recuerda guardar los cambios.", dia_n, p_act), type = "message", duration = 3)
  })
  
  observeEvent(input$btn_confirmar_limpiar, {
    m_val <- modo_activo()
    if (m_val == "CosechaOficial") {
      showNotification("🔒 No se puede reiniciar la Cosecha Oficial (datos oficiales de packing).", type = "warning", duration = 4)
      return()
    }
    m_act <- if (m_val == "Proyeccion") "Proyección" else "Cosecha Campo"
    p_act <- pasada_activa()
    showModal(modalDialog(
      title = sprintf("⚠️ Confirmar reinicio (%s • %dª Pasada)", m_act, p_act),
      p(sprintf("¿Estás seguro de que deseas borrar la programación de %s (%dª Pasada) en las válvulas visibles?", m_act, p_act)),
      p(style = "font-size: 12px; color: #636366;", "Nota: Los demás modos/pasadas no se verán afectados y siempre puedes usar '↩️ Deshacer'."),
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("btn_ejecutar_limpiar", "Sí, borrar pasada", 
                     style = "background: #FF3B30; color: white; border: none; border-radius: 8px; font-weight: 600;")
      ),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$btn_ejecutar_limpiar, {
    removeModal()
    cambios_pendientes_rv(TRUE)
    session$sendCustomMessage("limpiar_pasada_activa", list())
    showNotification(sprintf("Se limpió la %dª Pasada en la selección actual. Recuerda guardar los cambios.", pasada_activa()), type = "message", duration = 3)
  })
  
  # ============================================================================
  # KPIs EN TIEMPO REAL
  # ============================================================================
  output$kpi_ha_filtradas <- renderText({
    vf <- valvulas_pasada_activa()
    paste0(format(round(sum(vf$Área, na.rm = TRUE), 2), big.mark = ","), " ha")
  })
  
  output$kpi_num_valvulas <- renderText({
    vf <- valvulas_pasada_activa()
    sprintf("%d válvulas", nrow(vf))
  })
  
  output$kpi_kg_excel_filtro <- renderText({
    paste0(format(round(proyeccion_kg_filtro_activo()), big.mark = ","), " kg")
  })
  
  output$kpi_titulo_pasada_activa <- renderText({
    m_lbl <- case_when(
      modo_activo() == "Proyeccion"     ~ "Proy.",
      modo_activo() == "CosechaCampo"   ~ "Cosecha Campo",
      modo_activo() == "CosechaOficial" ~ "Cosecha Oficial 🔒",
      TRUE ~ "Proy."
    )
    sprintf("%s (%dª Pasada)", m_lbl, pasada_activa())
  })
  
  output$kpi_ha_total_prog <- renderText({
    vf <- valvulas_pasada_activa()
    paste0(format(round(sum(vf$Ha_Total_Prog, na.rm = TRUE), 2), big.mark = ","), " ha")
  })
  
  output$kpi_pct_avance <- renderText({
    vf <- valvulas_pasada_activa()
    tot_ha  <- sum(vf$Área, na.rm = TRUE)
    prog_ha <- sum(vf$Ha_Total_Prog, na.rm = TRUE)
    paste0(if (tot_ha > 0) round((prog_ha / tot_ha) * 100, 1) else 0, "% del área")
  })
  
  output$kpi_kg_pasada_activa <- renderText({
    vf <- valvulas_pasada_activa()
    kg_prog <- sum(vf$Kg_Total_Prog, na.rm = TRUE)
    if (modo_activo() == "Proyeccion" && kg_prog == 0) {
      kg_teo <- proyeccion_kg_filtro_activo()
      if (kg_teo > 0) {
        return(sprintf("%s kg (Meta)", format(round(kg_teo), big.mark = ",")))
      }
    }
    paste0(format(round(kg_prog), big.mark = ","), " kg")
  })
  
  output$kpi_ha_ambas_pasadas <- renderText({
    vf <- valvulas_pasada_activa()
    paste0(format(round(sum(vf$Ha_Ambas_Prog, na.rm = TRUE), 2), big.mark = ","), " ha")
  })
  
  output$kpi_pct_ambas_pasadas <- renderText({
    vf <- valvulas_pasada_activa()
    tot_ha <- sum(vf$Área, na.rm = TRUE)
    amb_ha <- sum(vf$Ha_Ambas_Prog, na.rm = TRUE)
    paste0(if (tot_ha > 0) round((amb_ha / tot_ha) * 100, 1) else 0, "% barrido")
  })
  
  output$kpi_kg_ambas_pasadas <- renderText({
    vf <- valvulas_pasada_activa()
    paste0(format(round(sum(vf$Kg_Ambas, na.rm = TRUE)), big.mark = ","), " kg")
  })
  
  output$kpi_ha_lunes <- renderText({
    vf <- valvulas_pasada_activa()
    paste0(format(round(sum(vf$Ha_Lunes, na.rm = TRUE), 2), big.mark = ","), " ha")
  })
  output$kpi_pct_lunes <- renderText({
    vf <- valvulas_pasada_activa()
    tot_ha <- sum(vf$Área, na.rm = TRUE)
    paste0(if (tot_ha > 0) round((sum(vf$Ha_Lunes, na.rm = TRUE) / tot_ha) * 100, 1) else 0, "% del filtro")
  })
  
  output$kpi_ha_martes <- renderText({
    vf <- valvulas_pasada_activa()
    paste0(format(round(sum(vf$Ha_Martes, na.rm = TRUE), 2), big.mark = ","), " ha")
  })
  output$kpi_pct_martes <- renderText({
    vf <- valvulas_pasada_activa()
    tot_ha <- sum(vf$Área, na.rm = TRUE)
    paste0(if (tot_ha > 0) round((sum(vf$Ha_Martes, na.rm = TRUE) / tot_ha) * 100, 1) else 0, "% del filtro")
  })
  
  output$kpi_ha_miercoles <- renderText({
    vf <- valvulas_pasada_activa()
    paste0(format(round(sum(vf$Ha_Miércoles, na.rm = TRUE), 2), big.mark = ","), " ha")
  })
  output$kpi_pct_miercoles <- renderText({
    vf <- valvulas_pasada_activa()
    tot_ha <- sum(vf$Área, na.rm = TRUE)
    paste0(if (tot_ha > 0) round((sum(vf$Ha_Miércoles, na.rm = TRUE) / tot_ha) * 100, 1) else 0, "% del filtro")
  })
  
  output$kpi_ha_jueves <- renderText({
    vf <- valvulas_pasada_activa()
    paste0(format(round(sum(vf$Ha_Jueves, na.rm = TRUE), 2), big.mark = ","), " ha")
  })
  output$kpi_pct_jueves <- renderText({
    vf <- valvulas_pasada_activa()
    tot_ha <- sum(vf$Área, na.rm = TRUE)
    paste0(if (tot_ha > 0) round((sum(vf$Ha_Jueves, na.rm = TRUE) / tot_ha) * 100, 1) else 0, "% del filtro")
  })
  
  output$kpi_ha_viernes <- renderText({
    vf <- valvulas_pasada_activa()
    paste0(format(round(sum(vf$Ha_Viernes, na.rm = TRUE), 2), big.mark = ","), " ha")
  })
  output$kpi_pct_viernes <- renderText({
    vf <- valvulas_pasada_activa()
    tot_ha <- sum(vf$Área, na.rm = TRUE)
    paste0(if (tot_ha > 0) round((sum(vf$Ha_Viernes, na.rm = TRUE) / tot_ha) * 100, 1) else 0, "% del filtro")
  })
  
  output$kpi_ha_sabado <- renderText({
    vf <- valvulas_pasada_activa()
    paste0(format(round(sum(vf$Ha_Sábado, na.rm = TRUE), 2), big.mark = ","), " ha")
  })
  output$kpi_pct_sabado <- renderText({
    vf <- valvulas_pasada_activa()
    tot_ha <- sum(vf$Área, na.rm = TRUE)
    paste0(if (tot_ha > 0) round((sum(vf$Ha_Sábado, na.rm = TRUE) / tot_ha) * 100, 1) else 0, "% del filtro")
  })
  
  # ============================================================================
  # TABLA RESUMEN CON HECTÁREAS Y KILOS POR TURNO + PIE DE TOTALES (tfoot)
  # ============================================================================
  output$tabla_resumen <- renderDT({
    vf <- valvulas_pasada_activa()
    m_act <- case_when(
      modo_activo() == "Proyeccion"     ~ "Proyección",
      modo_activo() == "CosechaCampo"   ~ "Cosecha Campo",
      modo_activo() == "CosechaOficial" ~ "Cosecha Oficial",
      TRUE ~ "Proyección"
    )
    p_act <- pasada_activa()
    
    resumen <- vf %>%
      group_by(Grupo, Módulo, Turno, Variedad) %>%
      summarise(
        Modo                = sprintf("%s (%dªP)", m_act, p_act),
        `N° Válv`           = n(),
        `Área Física (ha)`  = round(sum(Área, na.rm = TRUE), 2),
        `Lun (ha)`          = round(sum(Ha_Lunes, na.rm = TRUE), 2),
        `Mar (ha)`          = round(sum(Ha_Martes, na.rm = TRUE), 2),
        `Mié (ha)`          = round(sum(Ha_Miércoles, na.rm = TRUE), 2),
        `Jue (ha)`          = round(sum(Ha_Jueves, na.rm = TRUE), 2),
        `Vie (ha)`          = round(sum(Ha_Viernes, na.rm = TRUE), 2),
        `Sáb (ha)`          = round(sum(Ha_Sábado, na.rm = TRUE), 2),
        `Total Pasada (ha)` = round(sum(Ha_Total_Prog, na.rm = TRUE), 2),
        `Kilos Pasada (kg)` = round(sum(Kg_Total_Prog, na.rm = TRUE), 0),
        `Kilos 1ª+2ª (kg)`  = round(sum(Kg_Ambas, na.rm = TRUE), 0),
        `% Avance`          = round(sum(Ha_Total_Prog, na.rm = TRUE) / pmax(sum(Área, na.rm = TRUE), 0.001), 4),
        .groups             = "drop"
      ) %>%
      arrange(suppressWarnings(as.numeric(Módulo)), suppressWarnings(as.numeric(Turno)))
    
    sketch_footer <- htmltools::withTags(table(
      class = "display compact stripe hover",
      thead(
        tr(lapply(names(resumen), th))
      ),
      tfoot(
        tr(
          th(colspan = "5", style = "text-align: right; padding-right: 12px;", "TOTAL SELECCIÓN:"),
          th(""), th(""), th(""), th(""), th(""), th(""), th(""), th(""), th(""), th(""), th(""), th("")
        )
      )
    ))
    
    datatable(
      resumen,
      container = sketch_footer,
      options = list(
        pageLength = 15,
        dom = "tp",
        language = list(
          paginate = list(previous = "‹ Anterior", `next` = "Siguiente ›"),
          info = "Mostrando _START_ a _END_ de _TOTAL_ turnos"
        ),
        columnDefs = list(
          list(className = "dt-center", targets = c(1, 2, 3, 4, 5, 16)),
          list(className = "dt-right", targets = 6:15)
        ),
        footerCallback = JS("
          function(tfoot, data, start, end, display) {
            var api = this.api();
            var numVal = function(i) {
              if (typeof i === 'number') return i;
              if (typeof i === 'string') return parseFloat(i.replace(/[^0-9.-]+/g, '')) || 0;
              return 0;
            };
            var sumCol = function(idx) {
              return api.column(idx, { page: 'all' }).data().reduce(function(a, b) {
                return numVal(a) + numVal(b);
              }, 0);
            };
            
            var totValv = sumCol(5);
            var totArea = sumCol(6);
            var totPas  = sumCol(13);
            
            $(api.column(5).footer()).html(totValv);
            for (var c = 6; c <= 13; c++) {
              $(api.column(c).footer()).html(sumCol(c).toFixed(2));
            }
            $(api.column(14).footer()).html(Math.round(sumCol(14)).toLocaleString('en-US'));
            $(api.column(15).footer()).html(Math.round(sumCol(15)).toLocaleString('en-US'));
            
            var pctGlobal = totArea > 0 ? ((totPas / totArea) * 100).toFixed(1) + '%' : '0.0%';
            $(api.column(16).footer()).html(pctGlobal);
          }
        ")
      ),
      rownames = FALSE
    ) %>%
      formatPercentage("% Avance", digits = 1) %>%
      formatRound(c("Área Física (ha)", "Lun (ha)", "Mar (ha)", "Mié (ha)", 
                    "Jue (ha)", "Vie (ha)", "Sáb (ha)", "Total Pasada (ha)"), digits = 2) %>%
      formatRound(c("Kilos Pasada (kg)", "Kilos 1ª+2ª (kg)"), digits = 0)
  })
}

shinyApp(ui = ui, server = server)