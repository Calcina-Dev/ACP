# ==============================================================================
# Analisis de Productividad - Scenario Curve & Yield Engine
# Emil Kowalski Craft Guidelines: High Usability, Frictionless UX, Real-Time Reactivity
# ==============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
})

SCENARIO_COLORS <- c(
  "Presupuesto"      = "#F59E0B",
  "Presupuesto 27/28"= "#F59E0B",
  "ESC. PROY."       = "#EF4444",
  "ESC. CAMPO MODIF" = "#2563EB",
  "ESC. CAMPO"       = "#10B981",
  "Wish List 27/28"  = "#1E293B",
  "2024/2025"        = "#8B5CF6",
  "2025/2026"        = "#D97706",
  "2026/2027"        = "#06B6D4"
)

HIST_CAMPAIGN_COLORS <- c(
  "2021/2022" = "#EC4899",
  "2022/2023" = "#14B8A6",
  "2023/2024" = "#F97316",
  "2024/2025" = "#8B5CF6",
  "2025/2026" = "#D97706",
  "2026/2027" = "#06B6D4"
)

PATH_HISTORICAL_YIELDS <- "G:/Unidades compartidas/Compartido Proyecciones/3.- Arándano/Data Histórica/Productividad/KgPlanta por Campaña.xlsx"
PATH_HISTORICAL_COSECHA <- "D:/ACP/Actualizacion data maestra/datos_historicos_arandano.RData"
PATH_HISTORICAL_COSECHA_DRIVE <- "G:/Unidades compartidas/Compartido Proyecciones/3.- Arándano/datos_historicos_arandano.RData"

# Load Historical Cosecha from Consolidated Master Data
load_historical_cosecha <- function() {
  path <- PATH_HISTORICAL_COSECHA
  if (!file.exists(path)) path <- PATH_HISTORICAL_COSECHA_DRIVE
  if (!file.exists(path)) {
    path_alt <- "G:/Unidades compartidas/Compartido Proyecciones/3.- Arándano/datos_maestros_cosecha_2026.RData"
    if (file.exists(path_alt)) path <- path_alt else return(NULL)
  }
  
  env <- new.env()
  tryCatch({
    load(path, envir = env)
    if (exists("df_cosecha", envir = env)) {
      return(env$df_cosecha)
    }
    return(NULL)
  }, error = function(e) {
    warning("Error al cargar df_cosecha: ", e$message)
    return(NULL)
  })
}

# Compute weekly harvest curve from df_cosecha for a representative module in a given campaign
compute_representative_campaign_curve <- function(
  df_cosecha,
  rep_unit_row,
  variedad_str,
  df_historical_yields = NULL
) {
  if (is.null(df_cosecha) || nrow(df_cosecha) == 0 || is.null(rep_unit_row) || nrow(rep_unit_row) == 0) {
    return(data.frame())
  }
  
  # Fallback si variedad_str es nulo, vacío o "Todas"
  if (is.null(variedad_str) || variedad_str == "Todas" || !nzchar(trimws(as.character(variedad_str)))) {
    if (!is.null(rep_unit_row$Variedad) && !is.na(rep_unit_row$Variedad[1]) && nzchar(trimws(as.character(rep_unit_row$Variedad[1])))) {
      variedad_str <- as.character(rep_unit_row$Variedad[1])
    } else if (!is.null(rep_unit_row$Unidad) && !is.na(rep_unit_row$Unidad[1])) {
      variedad_str <- sub("^[0-9]+ [^ ]+ ", "", as.character(rep_unit_row$Unidad[1]))
    }
  }
  
  camp_yr <- as.numeric(rep_unit_row$YearCamp[1])
  mod_str <- as.character(rep_unit_row$Modulo[1])
  clean_m <- gsub("^M", "", mod_str)
  
  camp_label <- sprintf("%d/%d", camp_yr, camp_yr + 1)
  
  sub_c <- df_cosecha %>%
    filter(
      Campaña == camp_yr,
      toupper(trimws(Módulo)) %in% toupper(c(clean_m, paste0("M", clean_m), mod_str)),
      toupper(trimws(Variedad)) == toupper(trimws(variedad_str))
    )
  
  if (nrow(sub_c) == 0) return(data.frame())
  
  # Estandarización canónica de Semana 53 en Semana 52 (evita desfase al año siguiente)
  sub_c <- sub_c %>%
    mutate(Semana = pmin(as.numeric(Semana), 52))
  
  tot_kg <- sum(sub_c[["Kg Total"]], na.rm = TRUE)
  
  # Lookup official historical yield in Kg/Planta
  hist_yield <- NA_real_
  camp_col_short <- sprintf("%02d/%02d", camp_yr %% 100, (camp_yr + 1) %% 100)
  
  if (!is.null(df_historical_yields) && nrow(df_historical_yields) > 0) {
    if (camp_col_short %in% colnames(df_historical_yields)) {
      m_target <- paste0("M", clean_m)
      match_row <- df_historical_yields %>%
        filter(
          toupper(trimws(ModuloStr)) == toupper(trimws(m_target)),
          toupper(trimws(Variedad)) == toupper(trimws(variedad_str))
        )
      if (nrow(match_row) > 0) {
        val <- as.numeric(match_row[[camp_col_short]][1])
        if (!is.na(val) && val > 0) hist_yield <- val
      }
    }
  }
  
  # Fallback: check Plantas column in df_cosecha
  if (is.na(hist_yield)) {
    valves <- sub_c %>%
      distinct(Sector, Turno, Válvula, Plantas) %>%
      filter(!is.na(Plantas) & Plantas > 0)
    if (nrow(valves) > 0) {
      sum_p <- sum(valves$Plantas, na.rm = TRUE)
      if (sum_p > 0) hist_yield <- tot_kg / sum_p
    }
  }
  
  agg <- sub_c %>%
    group_by(Semana) %>%
    summarise(Kg_Semanal = sum(.data[["Kg Total"]], na.rm = TRUE), .groups = "drop") %>%
    mutate(
      TimelineIndex = ifelse(Semana >= 18, Semana, Semana + 52),
      CalWeek = Semana,
      CalYear = ifelse(Semana >= 18, 2027, 2028),
      WeekLabel = sprintf("Sem %d", CalWeek),
      Escenario = camp_label,
      Tipo_Anio = "Histórico",
      CurveLabel = camp_label,
      Total_Kg = tot_kg,
      Kg_Planta_Prom = hist_yield,
      Total_Plantas = ifelse(!is.na(hist_yield) && hist_yield > 0, tot_kg / hist_yield, NA_real_)
    ) %>%
    arrange(TimelineIndex) %>%
    mutate(
      Kg_Acumulado = cumsum(Kg_Semanal),
      Kg_Planta_Semanal = if (!is.na(hist_yield) && tot_kg > 0) (Kg_Semanal / tot_kg) * hist_yield else NA_real_,
      Kg_Planta_Acumulado = if (!is.na(hist_yield) && tot_kg > 0) (Kg_Acumulado / tot_kg) * hist_yield else NA_real_
    )
  
  return(agg)
}

# Load Scenarios Excel
load_escenarios_excel <- function(path = "d:/ACP/Analisis de Productividad/Datos_Escenarios_27-28_2026-11-02.xlsx") {
  if (!file.exists(path)) {
    warning("Datos_Escenarios Excel not found at: ", path)
    return(NULL)
  }
  
  df_datos <- read_excel(path, sheet = "Datos", skip = 3)
  colnames(df_datos) <- trimws(colnames(df_datos))
  
  df_curva <- read_excel(path, sheet = "Curva proyectada")
  df_poda <- tryCatch(read_excel(path, sheet = "Matriz de poda", skip = 2), error = function(e) NULL)
  df_ajustes <- tryCatch(read_excel(path, sheet = "Ajustes", skip = 2), error = function(e) NULL)
  
  # Filtrar estrictamente datos y curvas pertenecientes a ACP
  df_datos <- df_datos %>%
    filter(toupper(trimws(Empresa)) == "ACP") %>%
    mutate(
      Variedad = trimws(as.character(Variedad)),
      Breeder = trimws(as.character(Breeder)),
      MóduloStr = ifelse(is.na(Módulo), "Test Block", paste0("M", Módulo)),
      FundoKey = toupper(trimws(as.character(Empresa))),
      Sem_Poda = as.numeric(`Semana de poda`),
      Escenario = trimws(as.character(Escenario)),
      Tipo_Anio = trimws(as.character(`Tipo de año`)),
      Plantas = as.numeric(Plantas),
      Ha_Prod = as.numeric(`Ha productivas`),
      Kg_Total = as.numeric(kg),
      Kg_Planta = as.numeric(`kg/planta`),
      T_Formacion = as.numeric(`Tiempo de formación (sem.)`),
      T_Formacion = ifelse(is.na(T_Formacion),
        case_when(
          Variedad == "Imperial" ~ 21,
          Variedad == "Rocio" ~ 22,
          TRUE ~ 23
        ),
        T_Formacion
      ),
      Semanas_Cosecha = as.numeric(`Semanas con cosecha`)
    )
  
  df_curva <- df_curva %>%
    filter(toupper(trimws(Fundo)) == "ACP") %>%
    mutate(
      Variedad = trimws(as.character(Variedad)),
      FundoKey = toupper(trimws(as.character(Fundo))),
      Sem_Poda = as.numeric(Sem_Poda),
      Item = as.numeric(Item),
      Curva = as.numeric(Curva)
    )
  
  list(
    datos = df_datos,
    curva = df_curva,
    matriz_poda = df_poda,
    ajustes = df_ajustes
  )
}

# Load Historical Campaign Yields (2022 to 2026)
load_historical_yields <- function(path = PATH_HISTORICAL_YIELDS) {
  if (!file.exists(path)) return(data.frame())
  
  df_hist <- tryCatch(read_excel(path), error = function(e) NULL)
  if (is.null(df_hist)) return(data.frame())
  
  colnames(df_hist) <- make.names(colnames(df_hist))
  
  hist_clean <- df_hist %>%
    filter(Campaign %in% 2022:2026) %>%
    filter(Entregable %in% c("REAL", "FINAL", "PRESUPUESTO")) %>%
    mutate(
      CampLabel = paste0(substr(as.character(Campaign), 3, 4), "/", as.numeric(substr(as.character(Campaign), 3, 4)) + 1),
      ModuloStr = paste0("M", Modulo),
      Variedad = trimws(Variedad),
      Kg_Plt = round(Kg.Planta, 2)
    ) %>%
    group_by(ModuloStr, Variedad, CampLabel) %>%
    summarise(Kg_Plt = first(Kg_Plt), .groups = "drop") %>%
    pivot_wider(names_from = CampLabel, values_from = Kg_Plt)
  
  return(hist_clean)
}

# Build Campaign Yield Table (Modulo - Variedad en filas, Campañas históricas, 27/28 Neutral y 27/28 Caliente ambas editables)
build_campaign_yield_table <- function(
  engine_data,
  hist_yields,
  selected_modulo = "Todos",
  selected_breeder = "Todos",
  selected_variedad = "Todas",
  active_scenario_edit = "ESC. CAMPO"
) {
  df_d <- engine_data$datos
  if (is.null(df_d)) return(data.frame())
  
  # Current 27/28 data for both Año neutral and Año caliente
  sub_neutral <- df_d %>%
    filter(Tipo_Anio == "Año neutral", Escenario == active_scenario_edit)
  sub_caliente <- df_d %>%
    filter(Tipo_Anio == "Año caliente", Escenario == active_scenario_edit)
  
  if (!is.null(selected_modulo) && selected_modulo != "Todos") {
    clean_mod <- gsub("^M", "", selected_modulo)
    sub_neutral <- sub_neutral %>% filter(MóduloStr == selected_modulo | Módulo == clean_mod)
    sub_caliente <- sub_caliente %>% filter(MóduloStr == selected_modulo | Módulo == clean_mod)
  }
  if (!is.null(selected_breeder) && selected_breeder != "Todos") {
    sub_neutral <- sub_neutral %>% filter(toupper(trimws(Breeder)) == toupper(trimws(selected_breeder)))
    sub_caliente <- sub_caliente %>% filter(toupper(trimws(Breeder)) == toupper(trimws(selected_breeder)))
  }
  if (!is.null(selected_variedad) && selected_variedad != "Todas") {
    sub_neutral <- sub_neutral %>% filter(tolower(Variedad) == tolower(selected_variedad))
    sub_caliente <- sub_caliente %>% filter(tolower(Variedad) == tolower(selected_variedad))
  }
  
  if (nrow(sub_neutral) == 0 && nrow(sub_caliente) == 0) return(data.frame())
  
  base_neutral <- sub_neutral %>%
    mutate(`27/28 Neutral` = round(Kg_Planta, 2)) %>%
    select(Módulo = MóduloStr, Variedad, `27/28 Neutral`)
  
  base_caliente <- sub_caliente %>%
    mutate(`27/28 Caliente` = round(Kg_Planta, 2)) %>%
    select(Módulo = MóduloStr, Variedad, `27/28 Caliente`)
  
  base_27 <- full_join(base_neutral, base_caliente, by = c("Módulo", "Variedad"))
  
  # Join with historical yields if available
  if (!is.null(hist_yields) && nrow(hist_yields) > 0) {
    merged <- base_27 %>%
      left_join(hist_yields, by = c("Módulo" = "ModuloStr", "Variedad" = "Variedad"))
    
    # Ordered historical campaigns: 22/23, 23/24, 24/25, 25/26, 26/27
    camp_cols <- c("22/23", "23/24", "24/25", "25/26", "26/27")
    avail_camps <- intersect(camp_cols, colnames(merged))
    
    # Final Columns: Modulo, Variedad, historical campaigns, 27/28 Neutral, 27/28 Caliente
    final_cols <- c("Módulo", "Variedad", avail_camps, "27/28 Neutral", "27/28 Caliente")
    merged <- merged %>% select(any_of(final_cols))
    return(merged)
  }
  
  return(base_27)
}

# Update 27/28 Yield and Recalculate Kilos
update_campaign_yield <- function(
  engine_data,
  selected_tipo_anio,
  modulo_str,
  variedad,
  escenario_target,
  new_kg_planta,
  empresa = NULL,
  condicion = NULL
) {
  df_d <- engine_data$datos
  new_val <- as.numeric(new_kg_planta)
  if (is.na(new_val) || new_val < 0) return(engine_data)
  
  cond <- df_d$Tipo_Anio == selected_tipo_anio &
          df_d$MóduloStr == modulo_str &
          tolower(trimws(df_d$Variedad)) == tolower(trimws(variedad)) &
          df_d$Escenario == escenario_target
          
  if (!is.null(empresa)) cond <- cond & (df_d$Empresa == empresa)
  if (!is.null(condicion)) cond <- cond & (df_d$Condición == condicion)
  
  match_idx <- which(cond)
  if (length(match_idx) > 0) {
    df_d$Kg_Planta[match_idx] <- new_val
    df_d$`kg/planta`[match_idx] <- new_val
    # Recalculate kg = kg/planta * Plantas (plants are fixed)
    df_d$Kg_Total[match_idx] <- round(new_val * df_d$Plantas[match_idx], 2)
    df_d$kg[match_idx] <- df_d$Kg_Total[match_idx]
  }
  
  engine_data$datos <- df_d
  return(engine_data)
}

# Circular distance between calendar weeks (1 to 52)
circ_dist_weeks <- function(w1, w2) {
  d <- abs(w1 - w2) %% 52
  pmin(d, 52 - d)
}

# Calculate Weekly Curves (Con asignación inteligente por proximidad fenológica)
calculate_scenario_weekly_curves <- function(
  engine_data,
  selected_tipo_anio = c("Año neutral"),
  selected_modulo = "Todos",
  selected_breeder = "Todos",
  selected_variedad = "Todas",
  selected_escenarios = NULL
) {
  df_d <- engine_data$datos
  df_c <- engine_data$curva
  
  if (is.null(df_d) || is.null(df_c)) {
    return(list(curves = data.frame(), summary = data.frame(), notifications = data.frame()))
  }
  
  filtered_d <- df_d %>% filter(Tipo_Anio %in% selected_tipo_anio)
  
  if (!is.null(selected_modulo) && selected_modulo != "Todos") {
    clean_mod <- gsub("^M", "", selected_modulo)
    filtered_d <- filtered_d %>% filter(MóduloStr == selected_modulo | Módulo == clean_mod)
  }
  
  if (!is.null(selected_breeder) && selected_breeder != "Todos") {
    filtered_d <- filtered_d %>% filter(toupper(trimws(Breeder)) == toupper(trimws(selected_breeder)))
  }
  
  if (!is.null(selected_variedad) && selected_variedad != "Todas") {
    filtered_d <- filtered_d %>% filter(tolower(Variedad) == tolower(selected_variedad))
  }
  
  if (!is.null(selected_escenarios) && length(selected_escenarios) > 0) {
    filtered_d <- filtered_d %>% filter(Escenario %in% selected_escenarios)
  }
  
  if (nrow(filtered_d) == 0) {
    return(list(curves = data.frame(), summary = data.frame(), notifications = data.frame()))
  }
  
  # --- Mapeo de Curvas por Proximidad Fenológica (Nearest Neighbor) ---
  combos <- filtered_d %>% distinct(Variedad, Sem_Poda, FundoKey)
  map_list <- vector("list", nrow(combos))
  
  for (i in seq_len(nrow(combos))) {
    v <- combos$Variedad[i]
    sp <- combos$Sem_Poda[i]
    fk <- combos$FundoKey[i]
    
    exact <- df_c %>% filter(Variedad == v, Sem_Poda == sp, FundoKey == fk)
    if (nrow(exact) > 0) {
      map_list[[i]] <- data.frame(
        Variedad = v,
        Sem_Poda = sp,
        FundoKey = fk,
        Sem_Poda_Curva = sp,
        Is_Fallback = FALSE,
        Dist_Sem = 0,
        stringsAsFactors = FALSE
      )
    } else {
      # Buscar curva más cercana de la misma variedad
      cand <- df_c %>% filter(Variedad == v)
      if (nrow(cand %>% filter(FundoKey == fk)) > 0) {
        cand <- cand %>% filter(FundoKey == fk)
      }
      
      if (nrow(cand) > 0) {
        avail <- unique(cand$Sem_Poda)
        best_sp <- avail[which.min(circ_dist_weeks(avail, sp))]
        d_val <- circ_dist_weeks(best_sp, sp)
        map_list[[i]] <- data.frame(
          Variedad = v,
          Sem_Poda = sp,
          FundoKey = fk,
          Sem_Poda_Curva = best_sp,
          Is_Fallback = TRUE,
          Dist_Sem = d_val,
          stringsAsFactors = FALSE
        )
      } else {
        # Si la variedad no existiera en curvas, fallback general
        avail <- unique(df_c$Sem_Poda)
        best_sp <- avail[which.min(circ_dist_weeks(avail, sp))]
        d_val <- circ_dist_weeks(best_sp, sp)
        map_list[[i]] <- data.frame(
          Variedad = v,
          Sem_Poda = sp,
          FundoKey = fk,
          Sem_Poda_Curva = best_sp,
          Is_Fallback = TRUE,
          Dist_Sem = d_val,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  map_df <- bind_rows(map_list)
  
  # Generar resumen de notificaciones discretas
  filtered_mapped <- filtered_d %>%
    left_join(map_df, by = c("Variedad", "Sem_Poda", "FundoKey"))
  
  notifications_df <- data.frame()
  if (any(filtered_mapped$Is_Fallback)) {
    notifications_df <- filtered_mapped %>%
      filter(Is_Fallback == TRUE) %>%
      group_by(MóduloStr, Variedad, Sem_Poda, Sem_Poda_Curva, Dist_Sem) %>%
      summarise(
        Total_Kg = sum(Kg_Total, na.rm = TRUE),
        Total_Plantas = sum(Plantas, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(MóduloStr, Variedad, Sem_Poda)
  }
  
  # Cruce con la curva porcentual asignada
  joined <- filtered_mapped %>%
    inner_join(
      df_c,
      by = c("Variedad" = "Variedad", "Sem_Poda_Curva" = "Sem_Poda", "FundoKey" = "FundoKey"),
      relationship = "many-to-many"
    ) %>%
    mutate(
      # El inicio de cosecha se ancla a la semana de poda real del campo + tiempo de formación
      # Si la poda es tardía/diciembre (>= 45, Año 0), la cosecha inicia en el año de campaña (Sem_Poda + T_Formacion - 52)
      raw_start = Sem_Poda + T_Formacion,
      StartSem = ifelse(Sem_Poda >= 45, raw_start - 52, raw_start),
      AbsWeek = StartSem + Item - 1,
      CalYear = ifelse(AbsWeek > 52, 2028, 2027),
      CalWeek = ifelse(AbsWeek > 52, AbsWeek - 52, AbsWeek),
      Kg_Semanal = Kg_Total * Curva,
      TimelineIndex = AbsWeek,
      WeekLabel = sprintf("Sem %d", CalWeek)
    )
  
  num_tipos <- length(unique(joined$Tipo_Anio))
  
  weekly_agg <- joined %>%
    group_by(Escenario, Tipo_Anio, CalYear, CalWeek, TimelineIndex, WeekLabel) %>%
    summarise(
      Kg_Semanal = sum(Kg_Semanal, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(Escenario, Tipo_Anio) %>%
    arrange(TimelineIndex) %>%
    mutate(
      Kg_Acumulado = cumsum(Kg_Semanal),
      CurveLabel = ifelse(
        num_tipos > 1,
        paste0(Escenario, " (", gsub("Año ", "", Tipo_Anio), ")"),
        Escenario
      )
    ) %>%
    ungroup()
  
  summary_kpis <- filtered_d %>%
    group_by(Escenario, Tipo_Anio) %>%
    summarise(
      Total_Kg = sum(Kg_Total, na.rm = TRUE),
      Total_Plantas = sum(Plantas, na.rm = TRUE),
      Total_Ha = sum(Ha_Prod, na.rm = TRUE),
      Kg_Planta_Prom = ifelse(sum(Plantas, na.rm = TRUE) > 0, sum(Kg_Total, na.rm = TRUE) / sum(Plantas, na.rm = TRUE), 0),
      Ton_Ha_Prom = ifelse(sum(Ha_Prod, na.rm = TRUE) > 0, (sum(Kg_Total, na.rm = TRUE) / 1000) / sum(Ha_Prod, na.rm = TRUE), 0),
      .groups = "drop"
    )
  
  weekly_agg <- weekly_agg %>%
    left_join(
      summary_kpis %>% select(Escenario, Tipo_Anio, Total_Plantas, Total_Kg, Kg_Planta_Prom),
      by = c("Escenario", "Tipo_Anio")
    ) %>%
    mutate(
      Kg_Planta_Semanal = ifelse(!is.na(Total_Plantas) & Total_Plantas > 0, Kg_Semanal / Total_Plantas, 0),
      Kg_Planta_Acumulado = ifelse(!is.na(Total_Plantas) & Total_Plantas > 0, Kg_Acumulado / Total_Plantas, 0)
    )
  
  peak_info <- weekly_agg %>%
    group_by(Escenario, Tipo_Anio) %>%
    arrange(desc(Kg_Semanal)) %>%
    slice(1) %>%
    select(Escenario, Tipo_Anio, Sem_Pico = CalWeek, Anio_Pico = CalYear, Kg_Pico = Kg_Semanal)
  
  summary_kpis <- summary_kpis %>%
    left_join(peak_info, by = c("Escenario", "Tipo_Anio"))
  
  list(curves = weekly_agg, summary = summary_kpis, notifications = notifications_df)
}
