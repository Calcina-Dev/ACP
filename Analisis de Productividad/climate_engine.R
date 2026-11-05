# ==============================================================================
# Analisis de Productividad - Climate Engine (Variables Meteorológicas F7 & F9)
# Agrícola Cerro Prieto (ACP) • Emil Kowalski Craft
# 1. Modulos 1, 2, 3, 4, 5, 13, 14 pertenecen a Filtrado 7; el resto a Filtrado 9
# 2. Variable externa renombrada a 'Temperatura promedio' (activa por defecto)
# 3. Variables adicionales disponibles para activación dinámica
# 4. Cálculo de anomalías (Δ vs media histórica semanal) y valores absolutos
# ==============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(lubridate)
})

PATH_CLIMATE_EXCEL <- if (file.exists("d:/ACP/Analisis de Productividad/Variables_Filtrados_7_9.xlsx")) {
  "d:/ACP/Analisis de Productividad/Variables_Filtrados_7_9.xlsx"
} else {
  "d:/ACP/Variables_Filtrados_7_9.xlsx"
}

# Definition of available climate variables
CLIMATE_VAR_DEFS <- list(
  "Temperatura promedio" = list(
    id = "temp_prom",
    name = "Temperatura promedio",
    short_label = "T. Promedio",
    badge = "ΔTprom (°C)",
    unit = "°C",
    type = "temperature",
    default = TRUE
  ),
  "Temperatura Mínima (°C)" = list(
    id = "temp_min",
    name = "Temperatura Mínima (°C)",
    short_label = "T. Mínima",
    badge = "ΔTmin (°C)",
    unit = "°C",
    type = "temperature",
    default = FALSE
  ),
  "Temperatura Máxima (°C)" = list(
    id = "temp_max",
    name = "Temperatura Máxima (°C)",
    short_label = "T. Máxima",
    badge = "ΔTmax (°C)",
    unit = "°C",
    type = "temperature",
    default = FALSE
  ),
  "Humedad Relativa (%)" = list(
    id = "humedad",
    name = "Humedad Relativa (%)",
    short_label = "Humedad Rel.",
    badge = "ΔHR (%)",
    unit = "%",
    type = "humidity",
    default = FALSE
  ),
  "Radiación (W/m²)" = list(
    id = "radiacion",
    name = "Radiación (W/m²)",
    short_label = "Radiación",
    badge = "ΔRad (W/m²)",
    unit = "W/m²",
    type = "radiation",
    default = FALSE
  ),
  "Precipitación (mm)" = list(
    id = "precip",
    name = "Precipitación (mm)",
    short_label = "Precipitación",
    badge = "Lluvia (mm)",
    unit = "mm",
    type = "rain",
    default = FALSE
  ),
  "Evapotranspiración (mm)" = list(
    id = "evapo",
    name = "Evapotranspiración (mm)",
    short_label = "Evapotransp.",
    badge = "ETo (mm)",
    unit = "mm",
    type = "et",
    default = FALSE
  ),
  "Punto Rocío (°C)" = list(
    id = "rocio",
    name = "Punto Rocío (°C)",
    short_label = "Punto Rocío",
    badge = "ΔTd (°C)",
    unit = "°C",
    type = "temperature",
    default = FALSE
  )
)

# Map module string to station (Filtrado 7 vs Filtrado 9)
# Rule: Modulos 1, 2, 3, 4, 5, 13, 14 -> Filtrado 7; others -> Filtrado 9
get_modulo_filtrado <- function(modulo_str) {
  if (is.null(modulo_str) || is.na(modulo_str) || !nzchar(trimws(as.character(modulo_str)))) return("Filtrado 9")
  clean_str <- gsub("^[Mm]", "", trimws(as.character(modulo_str)))
  digits_only <- gsub("[^0-9.]", "", clean_str)
  if (!nzchar(digits_only)) return("Filtrado 9")
  num <- suppressWarnings(as.numeric(digits_only))
  if (is.na(num)) return("Filtrado 9")
  
  base_mod <- floor(num)
  if (base_mod %in% c(1, 2, 3, 4, 5, 13, 14)) {
    "Filtrado 7"
  } else {
    "Filtrado 9"
  }
}

# Load and precompute climate metrics for ultra-fast lookup
load_climate_engine <- function(path = PATH_CLIMATE_EXCEL) {
  if (!file.exists(path)) {
    warning("Variables_Filtrados_7_9.xlsx not found at: ", path)
    return(NULL)
  }
  
  # Read Sheet 1: Filtrado 7
  s7 <- tryCatch(read_excel(path, sheet = "Variables_Meteo_Filtrado7")[, 1:6], error = function(e) NULL)
  if (is.null(s7)) return(NULL)
  
  d7_col <- s7[[1]] # Día-Mes
  s7_clean <- s7 %>%
    mutate(
      Fecha = make_date(as.numeric(Año), as.numeric(format(d7_col, "%m")), as.numeric(format(d7_col, "%d"))),
      Semana = pmin(isoweek(Fecha), 52), # Estandarización canónica de semana 53 en semana 52
      AñoISO = isoyear(Fecha),
      Zona = "Filtrado 7",
      Variable = ifelse(Variable == "Temperatura Exterior (°C)", "Temperatura promedio", trimws(as.character(Variable)))
    ) %>%
    filter(!is.na(Fecha), !is.na(Valor)) %>%
    select(Zona, Variable, AñoISO, Semana, Fecha, Valor)
  
  # Read Sheet 2: Filtrado 9
  s9 <- tryCatch(read_excel(path, sheet = "Variables_Meteo_Filtrado9")[, 1:6], error = function(e) NULL)
  if (is.null(s9)) return(NULL)
  
  d9_col <- s9[[2]] # Día
  s9_clean <- s9 %>%
    mutate(
      Fecha = make_date(as.numeric(Año), as.numeric(format(d9_col, "%m")), as.numeric(format(d9_col, "%d"))),
      Semana = pmin(isoweek(Fecha), 52), # Estandarización canónica de semana 53 en semana 52
      AñoISO = isoyear(Fecha),
      Zona = "Filtrado 9",
      Variable = ifelse(Variable == "Temperatura Exterior (°C)", "Temperatura promedio", trimws(as.character(Variable)))
    ) %>%
    filter(!is.na(Fecha), !is.na(Valor)) %>%
    select(Zona, Variable, AñoISO, Semana, Fecha, Valor)
  
  df_all <- bind_rows(s7_clean, s9_clean)
  
  # 1. Weekly actuals
  df_weekly <- df_all %>%
    group_by(Zona, Variable, AñoISO, Semana) %>%
    summarise(
      ValorSem = ifelse(first(Variable) == "Precipitación (mm)", sum(Valor, na.rm = TRUE), mean(Valor, na.rm = TRUE)),
      .groups = "drop"
    )
  
  # 2. Historical baseline (all campaigns)
  df_base_all <- df_all %>%
    group_by(Zona, Variable, Semana) %>%
    summarise(
      MediaAll = ifelse(first(Variable) == "Precipitación (mm)", mean(Valor, na.rm = TRUE) * 7, mean(Valor, na.rm = TRUE)),
      SumAll = sum(Valor, na.rm = TRUE),
      NAll = n(),
      .groups = "drop"
    )
  
  # 3. Year sums for leave-one-year-out baseline
  df_yr_sum <- df_all %>%
    group_by(Zona, Variable, Semana, AñoISO) %>%
    summarise(
      SumYr = sum(Valor, na.rm = TRUE),
      NYr = n(),
      .groups = "drop"
    )
  
  df_merged <- df_weekly %>%
    left_join(df_base_all, by = c("Zona", "Variable", "Semana")) %>%
    left_join(df_yr_sum, by = c("Zona", "Variable", "Semana", "AñoISO")) %>%
    mutate(
      MediaExcl = ifelse(NAll > NYr, (SumAll - SumYr) / (NAll - NYr), MediaAll),
      DeltaAll = round(ValorSem - MediaAll, 1),
      DeltaExcl = round(ValorSem - MediaExcl, 1),
      ValorSem = round(ValorSem, 1),
      MediaAll = round(MediaAll, 1),
      MediaExcl = round(MediaExcl, 1)
    )
  
  # Create fast hash lookup environment for O(1) cell access
  lookup_env <- new.env(hash = TRUE, size = nrow(df_merged) * 2)
  for (i in 1:nrow(df_merged)) {
    row_i <- df_merged[i, ]
    key <- sprintf("%s|%s|%d|%d", row_i$Zona, row_i$Variable, row_i$AñoISO, row_i$Semana)
    lookup_env[[key]] <- list(
      valor = row_i$ValorSem,
      media_all = row_i$MediaAll,
      delta_all = row_i$DeltaAll,
      media_excl = row_i$MediaExcl,
      delta_excl = row_i$DeltaExcl
    )
  }
  
  list(
    data = df_merged,
    lookup = lookup_env
  )
}

# Heatmap styling based on anomaly
get_anomaly_style <- function(delta, var_name = "Temperatura promedio") {
  if (is.null(delta) || is.na(delta)) {
    return(list(bg = "transparent", text = "#94A3B8", formatted = "-"))
  }
  
  # Diverging palette for temperature-like variables
  is_temp <- grepl("Temperatura|Rocío", var_name, ignore.case = TRUE)
  
  fmt <- sprintf("%+.1f", delta)
  
  if (is_temp) {
    # Temperature scale: [-2.5, +2.5]
    d <- pmax(pmin(delta, 2.5), -2.5)
    if (d < 0) {
      ratio <- abs(d) / 2.5
      r <- round(255 - ratio * (255 - 29))
      g <- round(255 - ratio * (255 - 78))
      b <- round(255 - ratio * (255 - 216))
      bg <- sprintf("#%02X%02X%02X", r, g, b)
      txt <- if (ratio > 0.45) "#FFFFFF" else "#1E293B"
    } else if (d > 0) {
      ratio <- d / 2.5
      r <- round(255 - ratio * (255 - 220))
      g <- round(255 - ratio * (255 - 38))
      b <- round(255 - ratio * (255 - 38))
      bg <- sprintf("#%02X%02X%02X", r, g, b)
      txt <- if (ratio > 0.45) "#FFFFFF" else "#1E293B"
    } else {
      bg <- "#FFFFFF"
      txt <- "#64748B"
    }
  } else if (grepl("Humedad", var_name, ignore.case = TRUE)) {
    # Humidity scale: [-15, +15]
    d <- pmax(pmin(delta, 15), -15)
    ratio <- abs(d) / 15
    if (d < 0) {
      bg <- sprintf("rgba(217, 119, 6, %.2f)", 0.15 + ratio * 0.7)
      txt <- if (ratio > 0.5) "#FFFFFF" else "#78350F"
    } else {
      bg <- sprintf("rgba(13, 148, 136, %.2f)", 0.15 + ratio * 0.7)
      txt <- if (ratio > 0.5) "#FFFFFF" else "#134E4A"
    }
  } else if (grepl("Precipitación", var_name, ignore.case = TRUE)) {
    # Rain (blue scale)
    if (delta > 0) {
      bg <- "#DBEAFE"
      txt <- "#1E40AF"
    } else {
      bg <- "transparent"
      txt <- "#94A3B8"
    }
  } else {
    # General diverging scale
    d <- pmax(pmin(delta, 20), -20)
    ratio <- abs(d) / 20
    if (d < 0) {
      bg <- sprintf("rgba(59, 130, 246, %.2f)", 0.12 + ratio * 0.6)
      txt <- if (ratio > 0.5) "#FFFFFF" else "#1E3A8A"
    } else if (d > 0) {
      bg <- sprintf("rgba(239, 68, 68, %.2f)", 0.12 + ratio * 0.6)
      txt <- if (ratio > 0.5) "#FFFFFF" else "#7F1D1D"
    } else {
      bg <- "#FFFFFF"
      txt <- "#64748B"
    }
  }
  
  list(bg = bg, text = txt, formatted = fmt)
}
