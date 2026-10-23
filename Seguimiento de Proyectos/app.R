# ==============================================================================
# TABLERO EJECUTIVO DE CRONOGRAMA DE PROYECTOS (ACP)
# Estilo: MS Project Executive Gantt · Alta Dirección
# Exportación: PNG (Alta Resolución) y PDF
# ==============================================================================

paquetes <- c("shiny", "readxl", "dplyr", "tidyr", "lubridate", "writexl")
nuevos <- paquetes[!(paquetes %in% installed.packages()[, "Package"])]
if (length(nuevos) > 0) install.packages(nuevos, repos = "https://cloud.r-project.org")

library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(writexl)
library(base64enc)

Sys.setenv(TZ = "America/Lima")

# Función para cargar y codificar el logo de Cerro Prieto
obtener_logo_cerro_prieto <- function() {
  # 1. Si existe logo_cerro_prieto_b64.R en el proyecto
  for (f_b64 in c("logo_cerro_prieto_b64.R", "www/logo_cerro_prieto_b64.R", "D:/ACP/Seguimiento de Proyectos/logo_cerro_prieto_b64.R")) {
    if (file.exists(f_b64)) {
      env <- new.env()
      tryCatch({
        source(f_b64, local = env)
        if (exists("logo_cerro_prieto_b64", envir = env)) return(env$logo_cerro_prieto_b64)
      }, error = function(e) NULL)
    }
  }
  # 2. Si el usuario pega logo_cerro_prieto.png en la raíz del proyecto o en www
  posibles <- c(
    "logo_cerro_prieto.png", "www/logo_cerro_prieto.png",
    "logo.png", "www/logo.png",
    "D:/ACP/Seguimiento de Proyectos/logo_cerro_prieto.png",
    "D:/ACP/Seguimiento de Proyectos/www/logo_cerro_prieto.png",
    "D:/ACP/Rutas de Cosecha/www/logo_cerro_prieto.png"
  )
  for (p in posibles) {
    if (file.exists(p)) {
      res <- tryCatch({
        paste0("data:image/png;base64,", base64encode(p))
      }, error = function(e) p)
      return(res)
    }
  }
  return(NULL)
}

# 1. Función de Procesamiento y Normalización de Datos
procesar_datos_gantt <- function(ruta_archivo) {
  if (is.null(ruta_archivo) || !file.exists(ruta_archivo)) return(NULL)
  
  df_raw <- tryCatch({
    if (grepl("\\.csv$", ruta_archivo, ignore.case = TRUE)) {
      read.csv(ruta_archivo, stringsAsFactors = FALSE, check.names = FALSE)
    } else {
      suppressMessages(read_excel(ruta_archivo, sheet = 1))
    }
  }, error = function(e) return(NULL))
  
  if (is.null(df_raw) || nrow(df_raw) == 0) return(NULL)
  nms <- tolower(trimws(names(df_raw)))
  
  # Detección de columnas
  col_id     <- names(df_raw)[grep("^identifier$", nms)][1]
  col_name   <- names(df_raw)[grep("^name$", nms)][1]
  col_state  <- names(df_raw)[grep("state", nms)][1]
  col_start  <- names(df_raw)[grep("start.*date", nms)][1]
  col_due    <- names(df_raw)[grep("due.*date", nms)][1]
  col_assign <- names(df_raw)[grep("assignee", nms)][1]
  col_mod    <- names(df_raw)[grep("module", nms)][1]
  col_rel    <- names(df_raw)[grep("relations", nms)][1]
  col_proj   <- names(df_raw)[grep("project name", nms)][1]
  
  # Compatibilidad con plantillas clásicas Gantt
  if (is.na(col_name)) col_name <- names(df_raw)[grep("tarea|actividad|nombre", nms)][1]
  if (is.na(col_start)) col_start <- names(df_raw)[grep("fecha.*ini", nms)][1]
  if (is.na(col_due)) col_due <- names(df_raw)[grep("fecha.*fin", nms)][1]
  if (is.na(col_assign)) col_assign <- names(df_raw)[grep("responsable|asignado|nombre", nms)][1]
  
  parse_d <- function(x) {
    if (inherits(x, "Date")) return(x)
    if (inherits(x, "POSIXt")) return(as.Date(x))
    if (is.numeric(x)) return(as.Date(x, origin = "1899-12-30"))
    suppressWarnings(as.Date(parse_date_time(as.character(x), orders = c("ymd", "dmy", "mdy", "Y-m-d", "d/m/Y", "Ymd"))))
  }
  
  df <- tibble(
    Raw_ID    = if(!is.na(col_id)) as.character(df_raw[[col_id]]) else paste0("TSK-", seq_len(nrow(df_raw))),
    Tarea     = if(!is.na(col_name)) as.character(df_raw[[col_name]]) else "Sin Título",
    Estado    = if(!is.na(col_state)) as.character(df_raw[[col_state]]) else "Backlog",
    Modulo    = if(!is.na(col_mod) && any(!is.na(df_raw[[col_mod]]))) as.character(df_raw[[col_mod]]) else "General",
    Assignees = if(!is.na(col_assign)) as.character(df_raw[[col_assign]]) else "",
    Start     = if(!is.na(col_start)) parse_d(df_raw[[col_start]]) else as.Date(NA),
    Due       = if(!is.na(col_due)) parse_d(df_raw[[col_due]]) else as.Date(NA),
    Relations = if(!is.na(col_rel)) as.character(df_raw[[col_rel]]) else "",
    Proyecto  = if(!is.na(col_proj)) as.character(df_raw[[col_proj]]) else "Automatización SixWeek"
  )
  
  # Fechas por defecto
  df <- df %>%
    mutate(
      Num_ID = suppressWarnings(as.numeric(gsub("[^0-9]", "", Raw_ID))),
      Display_ID = sprintf("%02d", ifelse(is.na(Num_ID), row_number(), Num_ID)),
      Start = ifelse(is.na(Start), Due, Start),
      Due   = ifelse(is.na(Due), Start, Due),
      Start = as.Date(Start, origin = "1970-01-01"),
      Due   = as.Date(Due, origin = "1970-01-01"),
      Start = if_else(is.na(Start), Sys.Date(), Start),
      Due   = if_else(is.na(Due), Start + 1, Due)
    )
  
  # Limpieza de nombre de módulo
  df <- df %>%
    mutate(
      Modulo_Limpio = toupper(trimws(gsub("^proyecto\\s*", "", Modulo, ignore.case = TRUE))),
      Modulo_Limpio = ifelse(Modulo_Limpio == "" | is.na(Modulo_Limpio), "GENERAL", Modulo_Limpio)
    )
  
  # Días laborables (Lunes a Viernes)
  calc_working_days <- function(d1, d2) {
    if (is.na(d1) || is.na(d2)) return(1)
    if (d1 > d2) { tmp <- d1; d1 <- d2; d2 <- tmp }
    all_days <- seq(d1, d2, by = "day")
    sum(!wday(all_days, week_start = 1) %in% c(6, 7))
  }
  df$Dias_Lab <- mapply(calc_working_days, df$Start, df$Due)
  df$Dias_Lab <- pmax(1, df$Dias_Lab)
  
  # Extraer predecesoras
  extract_preds <- function(rel_str) {
    if (is.na(rel_str) || trimws(rel_str) == "" || rel_str == "NA") return("—")
    matches <- regmatches(rel_str, gregexpr("PROYECTOAU-(\\d+)", rel_str))[[1]]
    if (length(matches) == 0) {
      nums <- regmatches(rel_str, gregexpr("\\b\\d+\\b", rel_str))[[1]]
      if (length(nums) > 0) return(paste(unique(nums), collapse = ","))
      return("—")
    }
    nums <- gsub("PROYECTOAU-", "", matches)
    paste(sort(as.numeric(unique(nums))), collapse = ",")
  }
  df$Predecesoras <- sapply(df$Relations, extract_preds)
  
  # Generar iniciales de responsables
  get_initials <- function(txt) {
    if (is.na(txt) || trimws(txt) == "") return("—")
    parts <- trimws(unlist(strsplit(txt, "[,;]\\s*")))
    inits <- sapply(parts, function(p) {
      usr <- gsub("@.*", "", tolower(p))
      if (usr == "omalqui") return("OM")
      if (usr == "trodriguez") return("TR")
      if (usr == "cmendozac") return("CM")
      toupper(substr(usr, 1, 2))
    })
    paste(unique(inits), collapse = " · ")
  }
  df$Resp_Initials <- sapply(df$Assignees, get_initials)
  
  # Orden lógico de módulos y tareas
  modulo_order <- df %>%
    group_by(Modulo_Limpio) %>%
    summarise(Min_ID = min(Num_ID, na.rm = TRUE), Min_Start = min(Start, na.rm = TRUE), .groups = "drop") %>%
    arrange(Min_ID, Min_Start) %>%
    mutate(Mod_Num = sprintf("%02d", row_number()))
  
  df <- df %>%
    left_join(modulo_order %>% select(Modulo_Limpio, Mod_Num), by = "Modulo_Limpio") %>%
    arrange(Mod_Num, Start, Num_ID)
  
  return(df)
}

# 2. UI con Barra de Herramientas y Contenedor Imprimible
ui <- fluidPage(
  title = "Cronograma Ejecutivo · Automatización SixWeek | ACP",
  
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&display=swap"),
    tags$script(src = "html2canvas.min.js"),
    tags$script(src = "jspdf.umd.min.js"),
    tags$style(HTML("
      /* Reseteo y Base */
      * { box-sizing: border-box; }
      body {
        background-color: #E6EBED;
        font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
        color: #1A3038;
        margin: 0;
        padding: 0;
        -webkit-font-smoothing: antialiased;
      }
      
      /* Barra Superior de Control Flotante */
      .top-control-bar {
        background: rgba(255, 255, 255, 0.92);
        backdrop-filter: blur(15px);
        -webkit-backdrop-filter: blur(15px);
        border-bottom: 1px solid rgba(0, 0, 0, 0.08);
        padding: 12px 28px;
        position: sticky;
        top: 0;
        z-index: 9999;
        display: flex;
        justify-content: space-between;
        align-items: center;
        box-shadow: 0 4px 20px rgba(0, 0, 0, 0.04);
      }
      
      .btn-action-ios {
        background: #FFFFFF;
        color: #1A3038 !important;
        font-size: 13px;
        font-weight: 600;
        border: 1px solid rgba(0, 0, 0, 0.12);
        border-radius: 9px;
        padding: 8px 16px;
        cursor: pointer;
        transition: all 0.2s ease;
        display: inline-flex;
        align-items: center;
        gap: 7px;
        box-shadow: 0 1px 3px rgba(0,0,0,0.04);
      }
      .btn-action-ios:hover {
        background: #F4F7F8;
        border-color: #1A5964;
        color: #1A5964 !important;
        transform: translateY(-1px);
      }
      .btn-primary-ios {
        background: #1A5964;
        color: #FFFFFF !important;
        border: none;
      }
      .btn-primary-ios:hover {
        background: #13454E;
        color: #FFFFFF !important;
      }
      
      /* Selector de Archivos Estilo iOS */
      .top-control-bar .shiny-input-container {
        margin: 0 !important;
        margin-bottom: 0 !important;
        padding: 0 !important;
        width: 320px !important;
        display: inline-block !important;
        vertical-align: middle !important;
      }
      .top-control-bar .form-group {
        margin-bottom: 0 !important;
      }
      .top-control-bar .input-group {
        margin: 0 !important;
        height: 38px !important;
        width: 100% !important;
      }
      .top-control-bar .input-group-btn {
        vertical-align: middle !important;
        width: 1% !important;
      }
      .top-control-bar .btn-file {
        background: #FFFFFF !important;
        color: #1A3038 !important;
        font-size: 13px !important;
        font-weight: 600 !important;
        border: 1px solid rgba(0, 0, 0, 0.14) !important;
        border-radius: 9px 0 0 9px !important;
        height: 38px !important;
        padding: 8px 16px !important;
        box-shadow: 0 1px 3px rgba(0,0,0,0.04) !important;
        transition: all 0.2s ease !important;
        cursor: pointer !important;
        margin: 0 !important;
        white-space: nowrap !important;
        display: inline-block !important;
      }
      .top-control-bar .btn-file:hover {
        background: #F4F7F8 !important;
        border-color: #1A5964 !important;
        color: #1A5964 !important;
      }
      .top-control-bar input.form-control {
        border-radius: 0 9px 9px 0 !important;
        border: 1px solid rgba(0, 0, 0, 0.14) !important;
        border-left: none !important;
        height: 38px !important;
        font-size: 12.5px !important;
        background-color: #FAFCFC !important;
        color: #556B73 !important;
        box-shadow: none !important;
        padding: 8px 12px !important;
        cursor: pointer !important;
        margin: 0 !important;
      }
      .top-control-bar .progress {
        display: none !important;
      }
      
      /* Contenedor del Tablero Ejecutivo (Exportable) */
      .executive-page-wrapper {
        padding: 24px 30px 60px 30px;
        max-width: 1560px;
        margin: 0 auto;
      }
      
      .executive-board {
        background: #FFFFFF;
        border-radius: 14px;
        box-shadow: 0 8px 32px rgba(20, 45, 55, 0.07);
        padding: 34px 40px;
        border: 1px solid rgba(0, 0, 0, 0.05);
      }
      
      /* Header Ejecutivo */
      .board-header {
        display: flex;
        justify-content: space-between;
        align-items: flex-start;
        margin-bottom: 24px;
      }
      
      .eyebrow-wrapper {
        display: flex;
        align-items: center;
        gap: 8px;
        margin-bottom: 4px;
      }
      .eyebrow-bar {
        width: 3.5px;
        height: 14px;
        background: #1D5C67;
        border-radius: 2px;
      }
      .eyebrow-text {
        font-size: 11px;
        font-weight: 700;
        letter-spacing: 0.8px;
        color: #55727B;
        text-transform: uppercase;
      }
      .board-main-title {
        font-size: 30px;
        font-weight: 800;
        letter-spacing: -0.6px;
        color: #132E35;
        margin: 0 0 4px 0;
      }
      .board-subtitle {
        font-size: 13px;
        color: #6C8188;
        font-weight: 500;
        margin: 0;
      }
      
      .board-header-right {
        text-align: right;
      }
      .header-meta-plan {
        font-size: 11.5px;
        font-weight: 700;
        color: #55727B;
        letter-spacing: 0.6px;
        text-transform: uppercase;
      }
      .header-meta-style {
        font-size: 10.5px;
        font-weight: 600;
        color: #93A4AA;
        text-transform: uppercase;
        letter-spacing: 0.4px;
        margin-top: 3px;
      }
      
      /* Fila de Tarjetas KPI */
      .kpi-row {
        display: grid;
        grid-template-columns: repeat(4, 1fr);
        gap: 16px;
        margin-bottom: 24px;
      }
      .kpi-stat-card {
        background: #FFFFFF;
        border: 1px solid #E5EBEB;
        border-radius: 10px;
        padding: 14px 18px;
        display: flex;
        align-items: center;
        gap: 14px;
        box-shadow: 0 2px 6px rgba(0,0,0,0.02);
      }
      .kpi-accent-line {
        width: 3.5px;
        height: 38px;
        border-radius: 2px;
        flex-shrink: 0;
      }
      .kpi-num {
        font-size: 25px;
        font-weight: 800;
        color: #132E35;
        line-height: 1.1;
        letter-spacing: -0.5px;
      }
      .kpi-lbl {
        font-size: 10.5px;
        font-weight: 700;
        color: #6C8188;
        text-transform: uppercase;
        letter-spacing: 0.5px;
        margin-top: 3px;
      }
      
      /* Contenedor de la Tabla Gantt */
      .gantt-matrix-container {
        border: 1px solid #DFE6E8;
        border-radius: 10px;
        overflow: hidden;
        background: #FFFFFF;
      }
      
      .gantt-main-table {
        width: 100%;
        border-collapse: collapse;
        font-size: 11px;
      }
      
      /* Encabezados de Tabla */
      .th-group-left {
        background: #EEF4F4;
        padding: 9px 14px;
        border-bottom: 1px solid #DFE6E8;
        border-right: 1px solid #DFE6E8;
      }
      .th-group-title {
        font-size: 11px;
        font-weight: 700;
        color: #173840;
        text-transform: uppercase;
        letter-spacing: 0.5px;
      }
      .th-group-sub {
        float: right;
        font-size: 9.5px;
        font-weight: 600;
        color: #798E95;
        text-transform: uppercase;
        letter-spacing: 0.5px;
      }
      
      .th-month {
        background: #EEF4F4;
        font-size: 10.5px;
        font-weight: 700;
        color: #1F424B;
        text-transform: uppercase;
        letter-spacing: 0.5px;
        text-align: center;
        padding: 7px 4px;
        border-bottom: 1px solid #DFE6E8;
        border-right: 1px solid #DFE6E8;
      }
      .th-week {
        background: #F7FAFA;
        font-size: 9.5px;
        font-weight: 700;
        color: #6C8188;
        text-transform: uppercase;
        letter-spacing: 0.4px;
        text-align: center;
        padding: 5px 2px;
        border-bottom: 1px solid #DFE6E8;
        border-right: 1px solid #DFE6E8;
      }
      .th-col-header {
        background: #F7FAFA;
        font-size: 9.5px;
        font-weight: 700;
        color: #6C8188;
        text-transform: uppercase;
        letter-spacing: 0.5px;
        padding: 6px 8px;
        border-bottom: 1px solid #DFE6E8;
        border-right: 1px solid #DFE6E8;
      }
      
      .th-day-col {
        background: #FFFFFF;
        text-align: center;
        padding: 4px 1px;
        border-bottom: 1px solid #DFE6E8;
        border-right: 1px solid #EDF2F4;
        min-width: 34px;
      }
      .th-day-col.weekend {
        background: #EBF1F2;
      }
      .th-day-name {
        font-size: 9px;
        font-weight: 600;
        color: #7B8F97;
      }
      .th-day-num {
        font-size: 11px;
        font-weight: 700;
        color: #16363E;
        line-height: 1.1;
      }
      
      /* Filas de Módulo (Separador de Bloque) */
      .tr-modulo-header td {
        padding: 8px 14px;
        border-bottom: 1px solid #DFE6E8;
        border-top: 1px solid #DFE6E8;
      }
      .mod-title-text {
        font-size: 11px;
        font-weight: 700;
        letter-spacing: 0.4px;
        text-transform: uppercase;
        display: flex;
        align-items: center;
        gap: 6px;
      }
      .mod-count-pill {
        font-size: 9.5px;
        font-weight: 700;
        letter-spacing: 0.5px;
        text-transform: uppercase;
        float: right;
      }
      
      /* Filas de Tareas */
      .tr-task td {
        padding: 8px 10px;
        border-bottom: 1px solid #EEF3F5;
        vertical-align: middle;
      }
      .tr-task:hover {
        background: #FAFCFC;
      }
      
      .cell-id {
        width: 52px;
        color: #6C8188;
        font-weight: 600;
        border-right: 1px solid #EDF2F4;
      }
      .cell-status-circle {
        display: inline-flex;
        align-items: center;
        justify-content: center;
        width: 14px;
        height: 14px;
        border-radius: 50%;
        border: 1.5px solid #A8B8BD;
        color: transparent;
        font-size: 9px;
        margin-right: 5px;
        vertical-align: middle;
      }
      .cell-status-circle.done {
        background: #1D6750;
        border-color: #1D6750;
        color: #FFFFFF;
        font-weight: 700;
      }
      
      .cell-task-name {
        font-size: 11.5px;
        color: #1E3942;
        border-right: 1px solid #EDF2F4;
      }
      .cell-task-name.done {
        font-weight: 700;
      }
      
      .cell-center {
        text-align: center;
        border-right: 1px solid #EDF2F4;
        color: #556B73;
        font-size: 10.5px;
      }
      .cell-center.bold {
        font-weight: 700;
        color: #1E3942;
      }
      
      /* Grid del Cronograma (Lado Derecho) */
      .timeline-grid-cell {
        padding: 0 !important;
        position: relative;
        border-bottom: 1px solid #EEF3F5;
      }
      .timeline-grid-wrapper {
        display: grid;
        height: 34px;
        position: relative;
        align-items: center;
        width: 100%;
      }
      .timeline-bg-col {
        height: 100%;
        border-right: 1px solid #EEF2F4;
      }
      .timeline-bg-col.weekend {
        background: #F4F7F8;
      }
      
      /* Barras de Gantt */
      .gantt-bar-element {
        position: relative;
        z-index: 3;
        height: 13px;
        border-radius: 5px;
        box-shadow: 0 1px 3px rgba(0,0,0,0.12);
        display: flex;
        align-items: center;
        justify-content: flex-end;
      }
      
      .gantt-bar-mod1 { background: #3B626A; }
      .gantt-bar-mod2 { background: #486E96; }
      .gantt-bar-mod3 { background: #9C6138; }
      .gantt-bar-done { background: #1D6750; }
      
      /* Indicador de hito de producción (Chevron al final de tarea 08) */
      .milestone-marker {
        width: 0;
        height: 0;
        border-top: 6px solid transparent;
        border-bottom: 6px solid transparent;
        border-left: 7px solid #FFFFFF;
        margin-right: 3px;
        opacity: 0.9;
      }
      
      /* Línea Vertical de Fecha de Corte */
      .cutoff-line {
        position: absolute;
        top: 0;
        bottom: 0;
        width: 0;
        border-right: 2px dashed #B89C58;
        z-index: 5;
        pointer-events: none;
      }
      
      /* Footer y Leyenda */
      .board-footer {
        margin-top: 22px;
        display: flex;
        justify-content: space-between;
        align-items: flex-end;
        font-size: 10px;
        color: #6C8188;
      }
      .legend-title {
        font-weight: 700;
        color: #173840;
        text-transform: uppercase;
        letter-spacing: 0.5px;
        margin-bottom: 6px;
      }
      .legend-items {
        display: flex;
        gap: 20px;
        align-items: center;
        margin-bottom: 8px;
      }
      .legend-pill {
        width: 16px;
        height: 9px;
        border-radius: 3px;
        display: inline-block;
        margin-right: 5px;
        vertical-align: middle;
      }
      .legend-dashed-line {
        width: 14px;
        height: 0;
        border-top: 2px dashed #B89C58;
        display: inline-block;
        margin-right: 5px;
        vertical-align: middle;
      }
      .footer-notes {
        line-height: 1.6;
        color: #798E95;
      }
      .footer-source {
        text-align: right;
        color: #8E9FA5;
      }
      
      /* Reglas para Impresión y Exportación Limpia */
      @media print {
        .top-control-bar { display: none !important; }
        body { background: #FFFFFF; }
        .executive-page-wrapper { padding: 0; max-width: 100%; }
        .executive-board { box-shadow: none; border: none; padding: 10px; }
      }
    "))
  ),
  
  # Barra de Herramientas Flotante
  div(class = "top-control-bar",
    div(style = "display: flex; align-items: center; gap: 16px;",
      tags$div(style = "display: flex; align-items: center;",
        if (!is.null(obtener_logo_cerro_prieto())) {
          tags$img(src = obtener_logo_cerro_prieto(), style = "height: 48px; width: auto; object-fit: contain;")
        } else {
          tags$span(style = "font-weight: 800; font-size: 16px; color: #1A3038;",
            tags$span(style = "color: #1A5964;", "●"), " ACP Proyecciones")
        }
      ),
      tags$span(style = "color: #D2DADB; font-size: 18px;", "|"),
      fileInput("archivo_excel_input", label = NULL, 
                buttonLabel = "Subir Excel...", 
                placeholder = "Seleccionar .xlsx o .csv", 
                accept = c(".xlsx", ".xls", ".csv")),
      actionButton("btn_restaurar_muestra", "Restaurar Muestra SixWeek", class = "btn-action-ios", icon = icon("sync-alt"))
    ),
    div(style = "display: flex; align-items: center; gap: 10px;",
      # Botones de Exportación
      actionButton("btn_exportar_png", "Exportar Imagen (PNG)", class = "btn-action-ios btn-primary-ios", icon = icon("camera"), onclick = "descargarImagenPNG()"),
      actionButton("btn_exportar_pdf", "Exportar en PDF", class = "btn-action-ios btn-primary-ios", icon = icon("file-pdf"), onclick = "descargarPDF()"),
      tags$button(class = "btn-action-ios", onclick = "window.print()", icon("print"), "Imprimir")
    )
  ),
  
  # Contenedor del Tablero Ejecutivo
  div(class = "executive-page-wrapper",
    div(id = "tablero-cronograma-imprimible", class = "executive-board",
      uiOutput("render_tablero_ejecutivo")
    )
  ),
  
  # Scripts JavaScript para Exportación de Imagen (html2canvas) y PDF (jsPDF)
  tags$script(HTML("
    function descargarImagenPNG() {
      const elemento = document.getElementById('tablero-cronograma-imprimible');
      if (!elemento) return;
      
      const btn = document.getElementById('btn_exportar_png');
      if (btn) btn.innerHTML = '<i class=\"fa fa-spinner fa-spin\"></i> Generando PNG...';
      
      html2canvas(elemento, {
        scale: 2.2, // Alta definición ejecutiva
        useCORS: true,
        backgroundColor: '#FFFFFF',
        logging: false
      }).then(canvas => {
        const enlace = document.createElement('a');
        enlace.download = 'Cronograma_Ejecutivo_SixWeek.png';
        enlace.href = canvas.toDataURL('image/png');
        enlace.click();
        if (btn) btn.innerHTML = '<i class=\"fa fa-camera\"></i> Exportar Imagen (PNG)';
      }).catch(err => {
        alert('Error al generar imagen: ' + err);
        if (btn) btn.innerHTML = '<i class=\"fa fa-camera\"></i> Exportar Imagen (PNG)';
      });
    }

    function descargarPDF() {
      const elemento = document.getElementById('tablero-cronograma-imprimible');
      if (!elemento) return;
      
      const btn = document.getElementById('btn_exportar_pdf');
      if (btn) btn.innerHTML = '<i class=\"fa fa-spinner fa-spin\"></i> Generando PDF...';
      
      html2canvas(elemento, {
        scale: 2.2,
        useCORS: true,
        backgroundColor: '#FFFFFF',
        logging: false
      }).then(canvas => {
        const imgData = canvas.toDataURL('image/png');
        const { jsPDF } = window.jspdf;
        
        // Crear PDF Horizontal (Landscape)
        const pdf = new jsPDF({
          orientation: 'landscape',
          unit: 'mm',
          format: 'a4'
        });
        
        const pageWidth = pdf.internal.pageSize.getWidth();
        const pageHeight = pdf.internal.pageSize.getHeight();
        
        const margin = 8;
        const printWidth = pageWidth - (margin * 2);
        const printHeight = (canvas.height * printWidth) / canvas.width;
        
        pdf.addImage(imgData, 'PNG', margin, margin, printWidth, Math.min(printHeight, pageHeight - (margin * 2)));
        pdf.save('Cronograma_Ejecutivo_SixWeek.pdf');
        if (btn) btn.innerHTML = '<i class=\"fa fa-file-pdf\"></i> Exportar en PDF';
      }).catch(err => {
        alert('Error al generar PDF: ' + err);
        if (btn) btn.innerHTML = '<i class=\"fa fa-file-pdf\"></i> Exportar en PDF';
      });
    }

    // Permitir hacer clic tanto en el botón como en la caja de texto para abrir el selector de archivos
    $(document).on('click', '.top-control-bar input.form-control', function() {
      $(this).closest('.input-group').find('input[type=file]').click();
    });
  "))
)

# 3. Lógica del Servidor
server <- function(input, output, session) {
  
  # Archivo de muestra inicial
  ruta_muestra <- "datos_muestra.xlsx"
  if (!file.exists(ruta_muestra)) {
    ruta_muestra <- "D:/ACP/Seguimiento de Proyectos/datos_muestra.xlsx"
  }
  
  datos_actuales <- reactiveVal(
    if (file.exists(ruta_muestra)) procesar_datos_gantt(ruta_muestra) else NULL
  )
  
  # Evento: Subida de archivo por el usuario
  observeEvent(input$archivo_excel_input, {
    req(input$archivo_excel_input)
    tryCatch({
      df_nuevo <- procesar_datos_gantt(input$archivo_excel_input$datapath)
      if (!is.null(df_nuevo) && nrow(df_nuevo) > 0) {
        datos_actuales(df_nuevo)
        showNotification(sprintf("✔ Archivo cargado con éxito (%d actividades detectadas)", nrow(df_nuevo)), type = "message", duration = 3)
      } else {
        showNotification("El archivo subido no contiene columnas válidas de cronograma.", type = "error", duration = 4)
      }
    }, error = function(e) {
      showNotification(paste("Error al leer el archivo:", e$message), type = "error", duration = 4)
    })
  })
  
  # Evento: Restaurar archivo de muestra
  observeEvent(input$btn_restaurar_muestra, {
    if (file.exists(ruta_muestra)) {
      datos_actuales(procesar_datos_gantt(ruta_muestra))
      showNotification("Datos de muestra de SixWeek restaurados.", type = "default", duration = 2)
    }
  })
  
  # Renderizado del Tablero Ejecutivo
  output$render_tablero_ejecutivo <- renderUI({
    df <- datos_actuales()
    req(!is.null(df) && nrow(df) > 0)
    
    # 1. Rango de Fechas
    logo_src  <- obtener_logo_cerro_prieto()
    min_fecha <- min(df$Start, na.rm = TRUE)
    max_fecha <- max(df$Due, na.rm = TRUE)
    dias_secuencia <- seq(min_fecha, max_fecha, by = "day")
    n_dias <- length(dias_secuencia)
    
    # Fecha de corte (23 Oct 2026 si está en rango, o fecha de corte proporcional)
    fecha_corte <- as.Date("2026-10-23")
    if (fecha_corte < min_fecha || fecha_corte > max_fecha) {
      fecha_corte <- min_fecha + 1
    }
    col_corte_idx <- which(dias_secuencia == fecha_corte)
    
    # Textos de Fechas
    meses_es <- c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "set", "oct", "nov", "dic")
    meses_es_caps <- toupper(c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Setiembre", "Octubre", "Noviembre", "Diciembre"))
    dias_es <- c("LUN", "MAR", "MIÉ", "JUE", "VIE", "SÁB", "DOM")
    
    fmt_fecha_es <- function(d) paste0(day(d), " ", meses_es[month(d)])
    subtitulo_rango <- sprintf("Cronograma técnico de implementación · %s – %s %d", 
                               fmt_fecha_es(min_fecha), fmt_fecha_es(max_fecha), year(max_fecha))
    
    # 2. KPIs
    tot_actividades <- nrow(df)
    finalizadas     <- sum(grepl("done|complet|terminad", tolower(df$Estado)))
    modulos_unicos  <- unique(df$Modulo_Limpio)
    frentes_trabajo <- length(modulos_unicos)
    puesta_prod_str <- toupper(fmt_fecha_es(max_fecha))
    
    # 3. Estructura de Encabezados de Calendario
    df_dias <- tibble(
      Fecha = dias_secuencia,
      Col_Idx = seq_len(n_dias),
      Dia_Num = sprintf("%02d", day(dias_secuencia)),
      Dia_Sem = dias_es[wday(dias_secuencia, week_start = 1)],
      Es_Finde = wday(dias_secuencia, week_start = 1) %in% c(6, 7),
      Mes_Num = month(dias_secuencia),
      Anio = year(dias_secuencia),
      Semana_ISO = isoweek(dias_secuencia)
    )
    
    meses_span <- df_dias %>%
      group_by(Anio, Mes_Num) %>%
      summarise(Nombre_Mes = paste(meses_es_caps[first(Mes_Num)], first(Anio)), Span = n(), .groups = "drop")
    
    semanas_span <- df_dias %>%
      group_by(Semana_ISO) %>%
      summarise(Nombre_Sem = paste("SEMANA", first(Semana_ISO)), Span = n(), .groups = "drop")
    
    # Paleta de módulos para banners
    colores_modulos <- list(
      "1" = list(bg = "#EEF5F5", text = "#1D545D", bar = "gantt-bar-mod1", count = "#65828A", accent = "#1D545D"),
      "2" = list(bg = "#EEF3F8", text = "#24476E", bar = "gantt-bar-mod2", count = "#6882A0", accent = "#3F6895"),
      "3" = list(bg = "#F9F4EE", text = "#85512B", bar = "gantt-bar-mod3", count = "#9E7B62", accent = "#9E5F38"),
      "default" = list(bg = "#F5F5F7", text = "#4A4A4C", bar = "gantt-bar-mod1", count = "#8E8E93", accent = "#8E8E93")
    )
    
    # Generar HTML del Tablero
    tagList(
      # Encabezado Principal
      div(class = "board-header",
        div(
          div(class = "eyebrow-wrapper",
            div(class = "eyebrow-bar"),
            span(class = "eyebrow-text", "Agrícola Cerro Prieto / Equipo de Proyecciones")
          ),
          h1(class = "board-main-title", unique(df$Proyecto)[1]),
          p(class = "board-subtitle", subtitulo_rango)
        ),
        div(class = "board-header-right",
          if (!is.null(logo_src)) {
            tags$img(src = logo_src, style = "height: 60px; width: auto; object-fit: contain; display: block; margin-left: auto;")
          } else NULL
        )
      ),
      
      # 4 Tarjetas de Métricas KPI
      div(class = "kpi-row",
        div(class = "kpi-stat-card",
          div(class = "kpi-accent-line", style = "background: #1D545D;"),
          div(
            div(class = "kpi-num", tot_actividades),
            div(class = "kpi-lbl", "Actividades Programadas")
          )
        ),
        div(class = "kpi-stat-card",
          div(class = "kpi-accent-line", style = "background: #1D545D;"),
          div(
            div(class = "kpi-num", sprintf("%d / %d", finalizadas, tot_actividades)),
            div(class = "kpi-lbl", "Actividades Finalizadas")
          )
        ),
        div(class = "kpi-stat-card",
          div(class = "kpi-accent-line", style = "background: #3F6895;"),
          div(
            div(class = "kpi-num", frentes_trabajo),
            div(class = "kpi-lbl", "Frentes de Trabajo")
          )
        ),
        div(class = "kpi-stat-card",
          div(class = "kpi-accent-line", style = "background: #9E5F38;"),
          div(
            div(class = "kpi-num", puesta_prod_str),
            div(class = "kpi-lbl", "Puesta en Producción Prevista")
          )
        )
      ),
      
      # Matriz de Cronograma Gantt
      div(class = "gantt-matrix-container",
        tags$table(class = "gantt-main-table",
          tags$thead(
            # Fila 1: Título desglose + Meses
            tags$tr(
              tags$th(colspan = 7, class = "th-group-left",
                span(class = "th-group-title", "Desglose de Actividades"),
                span(class = "th-group-sub", "Estructura + Predecesoras")
              ),
              lapply(seq_len(nrow(meses_span)), function(i) {
                tags$th(colspan = meses_span$Span[i], class = "th-month", meses_span$Nombre_Mes[i])
              })
            ),
            # Fila 2: Sub-encabezado izquierdo + Semanas
            tags$tr(
              tags$th(colspan = 7, style = "background: #F7FAFA; border-bottom: 1px solid #DFE6E8; border-right: 1px solid #DFE6E8;"),
              lapply(seq_len(nrow(semanas_span)), function(i) {
                tags$th(colspan = semanas_span$Span[i], class = "th-week", semanas_span$Nombre_Sem[i])
              })
            ),
            # Fila 3: Columnas de datos + Días del Calendario
            tags$tr(
              tags$th(class = "th-col-header", style = "width: 52px;", "ID"),
              tags$th(class = "th-col-header", "Actividad"),
              tags$th(class = "th-col-header", style = "text-align: center; width: 90px;", "Resp."),
              tags$th(class = "th-col-header", style = "text-align: center; width: 60px;", "Inicio"),
              tags$th(class = "th-col-header", style = "text-align: center; width: 60px;", "Fin"),
              tags$th(class = "th-col-header", style = "text-align: center; width: 44px;", "Días"),
              tags$th(class = "th-col-header", style = "text-align: center; width: 50px;", "Pred."),
              lapply(seq_len(nrow(df_dias)), function(i) {
                es_corte <- (!is.na(col_corte_idx) && i == col_corte_idx)
                tags$th(
                  class = paste("th-day-col", ifelse(df_dias$Es_Finde[i], "weekend", "")),
                  style = if(es_corte) "border-left: 2px dashed #B89C58;" else "",
                  div(class = "th-day-name", df_dias$Dia_Sem[i]),
                  div(class = "th-day-num", df_dias$Dia_Num[i])
                )
              })
            )
          ),
          
          tags$tbody(
            # Recorrer por Módulo
            lapply(unique(df$Mod_Num), function(m_num) {
              sub_mod <- df %>% filter(Mod_Num == m_num)
              mod_nombre <- unique(sub_mod$Modulo_Limpio)[1]
              n_acts <- nrow(sub_mod)
              act_txt <- sprintf("%d ACTIVIDAD%s", n_acts, ifelse(n_acts == 1, "", "ES"))
              
              mod_style <- if (as.character(as.numeric(m_num)) %in% names(colores_modulos)) {
                colores_modulos[[as.character(as.numeric(m_num))]]
              } else {
                colores_modulos[["default"]]
              }
              
              tagList(
                # Fila de Encabezado de Módulo
                tags$tr(class = "tr-modulo-header", style = paste0("background: ", mod_style$bg, ";"),
                  tags$td(colspan = 7, style = "border-right: 1px solid #DFE6E8;",
                    div(class = "mod-title-text", style = paste0("color: ", mod_style$text, ";"),
                      tags$span(style = paste0("color: ", mod_style$accent, "; font-weight: 800; font-size: 13px;"), "|"),
                      paste(m_num, mod_nombre),
                      span(class = "mod-count-pill", style = paste0("color: ", mod_style$count, ";"), act_txt)
                    )
                  ),
                  tags$td(colspan = n_dias, style = paste0("background: ", mod_style$bg, "; border-right: none;"))
                ),
                
                # Filas de Tareas dentro del Módulo
                lapply(seq_len(nrow(sub_mod)), function(r) {
                  fila <- sub_mod[r, ]
                  es_done <- grepl("done|complet|terminad", tolower(fila$Estado))
                  
                  # Columnas en el grid del timeline
                  c_start <- which(dias_secuencia == fila$Start)
                  c_end   <- which(dias_secuencia == fila$Due)
                  
                  if (length(c_start) == 0) c_start <- 1
                  if (length(c_end) == 0) c_end <- c_start
                  if (c_end < c_start) c_end <- c_start
                  
                  # Clase de color de la barra
                  bar_class <- if (es_done) "gantt-bar-done" else mod_style$bar
                  
                  # ¿Tiene hito de producción final? (Si es la última tarea o contiene producción)
                  es_puesta_prod <- grepl("producci[oó]n|despliegue", tolower(fila$Tarea))
                  
                  tags$tr(class = "tr-task",
                    # Columna ID con círculo de estado
                    tags$td(class = "cell-id",
                      tags$span(class = paste("cell-status-circle", ifelse(es_done, "done", "")), 
                                if(es_done) "✔" else ""),
                      fila$Display_ID
                    ),
                    # Nombre de Actividad
                    tags$td(class = paste("cell-task-name", ifelse(es_done, "done", "")), fila$Tarea),
                    # Responsables
                    tags$td(class = "cell-center", fila$Resp_Initials),
                    # Inicio
                    tags$td(class = "cell-center", toupper(fmt_fecha_es(fila$Start))),
                    # Fin
                    tags$td(class = "cell-center", toupper(fmt_fecha_es(fila$Due))),
                    # Días Laborables
                    tags$td(class = "cell-center bold", fila$Dias_Lab),
                    # Predecesoras
                    tags$td(class = "cell-center", fila$Predecesoras),
                    
                    # Celda de Timeline con CSS Grid de 15 columnas
                    tags$td(colspan = n_dias, class = "timeline-grid-cell",
                      div(class = "timeline-grid-wrapper", style = sprintf("grid-template-columns: repeat(%d, 1fr);", n_dias),
                        # Columnas de fondo del grid
                        lapply(seq_len(n_dias), function(col_i) {
                          div(class = paste("timeline-bg-col", ifelse(df_dias$Es_Finde[col_i], "weekend", "")),
                              style = sprintf("grid-column: %d; grid-row: 1;", col_i))
                        }),
                        
                        # Línea vertical punteada de Fecha de Corte
                        if (!is.na(col_corte_idx) && length(col_corte_idx) > 0) {
                          div(class = "cutoff-line", style = sprintf("grid-column: %d; grid-row: 1; left: 0;", col_corte_idx))
                        } else NULL,
                        
                        # Barra de Cronograma
                        div(class = paste("gantt-bar-element", bar_class),
                            style = sprintf("grid-column: %d / %d; grid-row: 1;", c_start, c_end + 1),
                            if (es_puesta_prod) div(class = "milestone-marker") else NULL
                        )
                      )
                    )
                  )
                })
              )
            })
          )
        )
      ),
      
      # Pie de Página y Leyenda del Cronograma
      div(class = "board-footer",
        div(
          div(class = "legend-title", "Lectura del Cronograma"),
          div(class = "legend-items",
            div(tags$span(class = "legend-pill", style = "background: #3B626A;"), "Actividad programada"),
            div(tags$span(class = "legend-pill", style = "background: #1D6750;"), "Finalizada"),
            div(tags$span(class = "legend-pill", style = "background: #EBF1F2; border: 1px solid #D8E0E2;"), "Fin de semana"),
            div(tags$span(class = "legend-dashed-line"), sprintf("Fecha de corte: %s", fmt_fecha_es(fecha_corte)))
          ),
          div(class = "footer-notes",
            div("Barras = intervalo de fechas programado · Días = lunes a viernes · Pred. = ID de tareas predecesoras registradas"),
            div(style = "font-weight: 600; margin-top: 3px; color: #556B73;",
                "RESPONSABLES OM: omalqui · TR: trodriguez · CM: cmendozac")
          )
        ),
        div(class = "footer-source",
          sprintf("Fuente: plan de proyecto exportado · corte %s %d", fmt_fecha_es(fecha_corte), year(fecha_corte))
        )
      )
    )
  })
}

shinyApp(ui = ui, server = server)
