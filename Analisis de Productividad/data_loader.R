# ==============================================================================
# Analisis de Productividad - Data Loader & Engine
# Emil Kowalski Craft Guidelines: Precision, Clarity, Reactivity
# ==============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(writexl)
  library(dplyr)
  library(tidyr)
})

# Palette configuration as specified
STAGE_COLORS <- c(
  "Inducción Floral" = "#FDE68A",
  "Floración"        = "#FACC15",
  "Formación Fruta"  = "#60A5FA",
  "Cosecha"          = "#1E3A8A",
  "Vegetativo"       = "#22C55E",
  "Punta Verde"      = "#BBF7D0",
  "Siembra"          = "#14B8A6",
  "Poda"             = "#FF7C80",
  "Eliminación"      = "#FF0000"
)

STAGE_TEXT_COLORS <- c(
  "Inducción Floral" = "#78350F",
  "Floración"        = "#713F12",
  "Formación Fruta"  = "#FFFFFF",
  "Cosecha"          = "#FFFFFF",
  "Vegetativo"       = "#FFFFFF",
  "Punta Verde"      = "#065F46",
  "Siembra"          = "#FFFFFF",
  "Poda"             = "#FFFFFF",
  "Eliminación"      = "#FFFFFF"
)

# Canonical 56 columns definition
MONTH_DEFINITIONS <- list(
  list(year = 0, name = "Dic", weeks = 49:52),
  list(year = 1, name = "Ene", weeks = 1:5),
  list(year = 1, name = "Feb", weeks = 6:9),
  list(year = 1, name = "Mar", weeks = 10:13),
  list(year = 1, name = "Abr", weeks = 14:18),
  list(year = 1, name = "May", weeks = 19:22),
  list(year = 1, name = "Jun", weeks = 23:26),
  list(year = 1, name = "Jul", weeks = 27:31),
  list(year = 1, name = "Ago", weeks = 32:35),
  list(year = 1, name = "Set", weeks = 36:39),
  list(year = 1, name = "Oct", weeks = 40:44),
  list(year = 1, name = "Nov", weeks = 45:48),
  list(year = 1, name = "Dic", weeks = 49:52)
)

# Build ordered column list
build_column_metadata <- function() {
  cols <- list()
  idx <- 1
  for (m in MONTH_DEFINITIONS) {
    for (w in m$weeks) {
      key <- sprintf("Y%d_W%02d", m$year, w)
      cols[[idx]] <- data.frame(
        col_index = idx,
        col_key = key,
        year_label = ifelse(m$year == 0, "Año 0", "Año 1"),
        year_num = m$year,
        month_name = m$name,
        week_num = w,
        header_label = as.character(w),
        stringsAsFactors = FALSE
      )
      idx <- idx + 1
    }
  }
  bind_rows(cols)
}

COL_METADATA <- build_column_metadata()

# Vectorized stage mapping
stage_lookup <- c(
  "siembra"          = "Siembra",
  "vegetativo"       = "Vegetativo",
  "inducción floral" = "Inducción Floral",
  "induccion floral" = "Inducción Floral",
  "floración"        = "Floración",
  "floracion"        = "Floración",
  "formación fruta"  = "Formación Fruta",
  "formacion fruta"  = "Formación Fruta",
  "cosecha"          = "Cosecha",
  "poda"             = "Poda",
  "punta verde"      = "Punta Verde",
  "eliminación"      = "Eliminación",
  "eliminacion"      = "Eliminación"
)

clean_stage_name <- function(s) {
  s_char <- as.character(s)
  s_low <- tolower(trimws(s_char))
  res <- stage_lookup[s_low]
  ifelse(is.na(res), s_char, res)
}

# Vectorized variety mapping
variety_lookup <- c(
  "sekoya pop"     = "Sekoya Pop",
  "sekoya beauty"  = "Sekoya Beauty",
  "eureka gold"    = "Eureka Gold",
  "eureka maxx"    = "Eureka Maxx",
  "eureka sunrise" = "Eureka Sunrise",
  "eureka sunset"  = "Eureka Sunset",
  "atlas blue"     = "Atlas Blue",
  "abril blue"     = "Abril Blue",
  "alessia blue"   = "Alessia Blue",
  "mega crisp"     = "Mega Crisp",
  "mega early"     = "Mega Early",
  "mega gem"       = "Mega Gem",
  "mega grand"     = "Mega Grand",
  "mega one"       = "Mega One",
  "mega star"      = "Mega Star"
)

variety_breeder_lookup <- c(
  "sekoya pop"     = "NORTH BAY",
  "sekoya beauty"  = "NORTH BAY",
  "imperial"       = "HORTIFRUT",
  "rocio"          = "HORTIFRUT",
  "milagro"        = "HORTIFRUT",
  "atlas blue"     = "LIBRE",
  "biloxi"         = "LIBRE",
  "madeira"        = "LIBRE",
  "malibu"         = "LIBRE",
  "manila"         = "LIBRE",
  "ventura"        = "LIBRE",
  "test block"     = "LIBRE",
  "andrea"         = "OZ BLUE",
  "magica"         = "OZ BLUE",
  "eureka gold"    = "OZ BLUE",
  "eureka maxx"    = "OZ BLUE",
  "eureka sunrise" = "OZ BLUE",
  "eureka sunset"  = "OZ BLUE"
)

clean_breeder_name <- function(v) {
  v_low <- tolower(trimws(as.character(v)))
  res <- variety_breeder_lookup[v_low]
  ifelse(is.na(res), "OTRO", res)
}

clean_variety_name <- function(v) {
  v_trim <- trimws(as.character(v))
  v_low <- tolower(v_trim)
  res <- variety_lookup[v_low]
  ifelse(is.na(res), v_trim, res)
}

# Main Excel loader
load_fenologia_excel <- function(path = "G:/Unidades compartidas/Compartido Proyecciones/3.- Arándano/Data Histórica/Etapas Fenologicas/Historico_Etapas_Fenologicas.xlsx") {
  if (!file.exists(path)) {
    warning("Excel file not found at: ", path)
    return(NULL)
  }
  
  df_raw <- read_excel(path, sheet = "Hoja1")
  
  df_processed <- df_raw %>%
    mutate(
      Stage = clean_stage_name(Stage),
      Variedad = clean_variety_name(Variedad),
      Breeder = clean_breeder_name(Variedad),
      Campaña = trimws(as.character(Campaña)),
      ModuloNum = as.numeric(Modulo),
      Modulo = ifelse(is.na(ModuloNum), "Test Block", paste0("M", ModuloNum)),
      YearCamp = as.numeric(substr(Campaña, 1, 2)) + 2000,
      AñoNum = as.numeric(Año),
      SemNumRaw = as.numeric(Semana),
      # Estandarización de Semana 53 a Semana 52 (cierre canónico de diciembre)
      SemNum = pmin(SemNumRaw, 52),
      # Construct canonical Unit name matching Image 2
      # e.g. "2025 M1 Sekoya Pop"
      CleanRow = gsub(" · ", " ", RowLabel),
      Unidad = paste0("20", substr(Campaña, 1, 2), " ", CleanRow),
      # Canonical column key matching the 56 week grid
      ColKey = case_when(
        AñoNum == YearCamp - 1 & SemNum >= 49 ~ sprintf("Y0_W%02d", SemNum),
        AñoNum == YearCamp & SemNum >= 1 ~ sprintf("Y1_W%02d", SemNum),
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(ColKey))
  
  return(df_processed)
}

# Circular distance between calendar weeks (1 to 52)
circ_dist_weeks <- function(w1, w2) {
  if (length(w1) == 0 || length(w2) == 0) return(numeric(0))
  d <- abs(w1 - w2) %% 52
  res <- pmin(d, 52 - d)
  res[is.na(w1) | is.na(w2)] <- 999
  res
}

# Advanced Comparative Phenology Filter (Emil Kowalski / ACP Specification)
# 1. Default (is_historial = FALSE):
#    - When Modulo & Variedad are selected: Find target's latest campaign (e.g. 2026 M10 Rocio).
#      Detect poda/start week. Compare with all modules of the same variety pruned in nearby weeks (+-2 sem) from 2024 onwards.
#    - When Modulo = 'Todos': Show the latest campaign (>= 2024) per module for the variety.
# 2. Historial (is_historial = TRUE):
#    - Show all historical campaigns for the selected module and variety (2021 to 2026).
filter_fenologia_comparativa <- function(
  df_records,
  selected_modulo = "Todos",
  selected_breeder = "Todos",
  selected_variedad = "Todas",
  is_historial = FALSE
) {
  if (is.null(df_records) || nrow(df_records) == 0) {
    return(list(
      data = data.frame(),
      target_unit = NULL,
      target_sem = NULL,
      representative_units = NULL,
      is_comparative = FALSE,
      mode = "empty"
    ))
  }
  
  # Base filter by Breeder if not "Todos"
  df_base <- df_records
  if (!is.null(selected_breeder) && selected_breeder != "Todos") {
    df_base <- df_base %>% filter(toupper(Breeder) == toupper(selected_breeder))
  }
  
  # Mode 1: Historical Switch is ON
  if (isTRUE(is_historial)) {
    df_hist <- df_base
    if (!is.null(selected_modulo) && selected_modulo != "Todos") {
      df_hist <- df_hist %>% filter(Modulo == selected_modulo)
      # Si variedad es "Todas", resolver automáticamente a la variedad más reciente del módulo
      if (is.null(selected_variedad) || selected_variedad == "Todas") {
        if (nrow(df_hist) > 0) {
          latest_y <- max(df_hist$YearCamp, na.rm = TRUE)
          auto_var <- df_hist %>% filter(YearCamp == latest_y) %>% pull(Variedad) %>% .[1]
          df_hist <- df_hist %>% filter(Variedad == auto_var)
        }
      }
    }
    if (!is.null(selected_variedad) && selected_variedad != "Todas") {
      df_hist <- df_hist %>% filter(Variedad == selected_variedad)
    }
    
    # Chronological order by YearCamp and Unidad
    ordered_units <- df_hist %>%
      distinct(Unidad, YearCamp) %>%
      arrange(YearCamp, Unidad) %>%
      pull(Unidad)
    
    df_hist <- df_hist %>%
      mutate(Unidad = factor(Unidad, levels = ordered_units)) %>%
      arrange(Unidad)
    
    # Representative units in Historical mode are the historical campaigns of this module/variety
    rep_units_hist <- df_hist %>%
      group_by(Unidad, Campaña, YearCamp, Modulo, Variedad) %>%
      summarise(
        poda_sem = if (any(Stage == "Poda")) min(SemNum[Stage == "Poda"], na.rm = TRUE) else min(SemNum, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        dist = 0,
        is_same_mod = TRUE,
        is_target = (YearCamp == max(YearCamp, na.rm = TRUE))
      ) %>%
      arrange(YearCamp)
    
    latest_yr <- max(df_hist$YearCamp, na.rm = TRUE)
    target_row <- rep_units_hist %>% filter(YearCamp == latest_yr)
    target_unit <- if (nrow(target_row) > 0) as.character(target_row$Unidad[1]) else NULL
    target_sem <- if (nrow(target_row) > 0) target_row$poda_sem[1] else NULL
    
    return(list(
      data = df_hist,
      target_unit = target_unit,
      target_sem = target_sem,
      representative_units = rep_units_hist,
      is_comparative = FALSE,
      mode = "historial"
    ))
  }
  
  # Si se seleccionó un módulo específico pero variedad quedó como "Todas" o NULL,
  # resolver automáticamente a la variedad más reciente del módulo para garantizar congruencia
  if (!is.null(selected_modulo) && selected_modulo != "Todos" &&
      (is.null(selected_variedad) || selected_variedad == "Todas")) {
    cand_m <- df_records %>% filter(Modulo == selected_modulo, YearCamp >= 2024)
    if (nrow(cand_m) > 0) {
      latest_y <- max(cand_m$YearCamp, na.rm = TRUE)
      auto_v <- cand_m %>% filter(YearCamp == latest_y) %>% pull(Variedad) %>% .[1]
      if (!is.null(auto_v) && !is.na(auto_v) && nzchar(trimws(auto_v))) {
        selected_variedad <- auto_v
      }
    }
  }
  
  # Mode 2: Comparative Mode (Default, is_historial == FALSE)
  # Case A: Specific Modulo AND Specific Variedad
  # Rule: Exactly 1 module per year (>= 2024). Priority to consulted modulo;
  # if not present in that year, select the most proximal module (closest poda week).
  # Chronologically ordered by year.
  if (!is.null(selected_modulo) && selected_modulo != "Todos" &&
      !is.null(selected_variedad) && selected_variedad != "Todas") {
    
    sub_target <- df_records %>%
      filter(Modulo == selected_modulo, Variedad == selected_variedad)
    
    if (nrow(sub_target) == 0) {
      return(list(
        data = data.frame(),
        target_unit = NULL,
        target_sem = NULL,
        representative_units = NULL,
        is_comparative = FALSE,
        mode = "no_match"
      ))
    }
    
    latest_year <- max(sub_target$YearCamp, na.rm = TRUE)
    target_rows <- sub_target %>% filter(YearCamp == latest_year)
    target_unit <- target_rows$Unidad[1]
    target_sem <- if (any(target_rows$Stage == "Poda")) {
      min(target_rows$SemNum[target_rows$Stage == "Poda"], na.rm = TRUE)
    } else {
      min(target_rows$SemNum, na.rm = TRUE)
    }
    
    # Candidate units: same variety, campaigns >= 2024
    cand_rows <- df_records %>%
      filter(Variedad == selected_variedad, YearCamp >= 2024)
    
    if (nrow(cand_rows) == 0) {
      return(list(
        data = data.frame(),
        target_unit = NULL,
        target_sem = NULL,
        representative_units = NULL,
        is_comparative = FALSE,
        mode = "no_match"
      ))
    }
    
    unit_info <- cand_rows %>%
      group_by(Unidad, Campaña, YearCamp, Modulo, Variedad) %>%
      summarise(
        poda_sem = if (any(Stage == "Poda")) {
          min(SemNum[Stage == "Poda"], na.rm = TRUE)
        } else {
          min(SemNum, na.rm = TRUE)
        },
        .groups = "drop"
      ) %>%
      mutate(
        dist = circ_dist_weeks(poda_sem, target_sem),
        is_same_mod = (Modulo == selected_modulo),
        is_target = (Unidad == target_unit)
      )
    
    # Filter candidates: "si en un año se separa mas de 3 semanas no considerar el año"
    unit_info_valid <- unit_info %>%
      filter(dist <= 3 | is_target)
    
    # 1 module per year:
    # 1st priority: same module as consulted
    # 2nd priority: closest poda week to target
    # 3rd priority: target unit
    # Tie-break: Modulo alphabetical
    selected_u_df <- unit_info_valid %>%
      group_by(YearCamp) %>%
      arrange(desc(is_same_mod), dist, desc(is_target), Modulo) %>%
      slice_head(n = 1) %>%
      ungroup() %>%
      arrange(YearCamp)
    
    ordered_units <- selected_u_df$Unidad
    
    df_out <- df_records %>%
      filter(Unidad %in% ordered_units) %>%
      mutate(Unidad = factor(Unidad, levels = ordered_units)) %>%
      arrange(Unidad)
    
    return(list(
      data = df_out,
      target_unit = target_unit,
      target_sem = target_sem,
      representative_units = selected_u_df,
      is_comparative = TRUE,
      mode = "comparative_target"
    ))
  }
  
  # Case B: Modulo == "Todos" AND Specific Variedad
  if ((is.null(selected_modulo) || selected_modulo == "Todos") &&
      !is.null(selected_variedad) && selected_variedad != "Todas") {
    
    cand_rows <- df_base %>%
      filter(Variedad == selected_variedad, YearCamp >= 2024)
    
    if (nrow(cand_rows) == 0) {
      return(list(data = data.frame(), target_unit = NULL, target_sem = NULL, is_comparative = FALSE, mode = "no_match"))
    }
    
    # Keep latest campaign per module
    latest_per_mod <- cand_rows %>%
      group_by(Modulo) %>%
      filter(YearCamp == max(YearCamp, na.rm = TRUE)) %>%
      ungroup()
    
    u_poda <- latest_per_mod %>%
      group_by(Unidad, Modulo) %>%
      summarise(
        poda_sem = if (any(Stage == "Poda")) min(SemNum[Stage == "Poda"], na.rm = TRUE) else min(SemNum, na.rm = TRUE),
        year_camp = max(YearCamp, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(poda_sem, Modulo)
    
    ordered_units <- u_poda$Unidad
    
    df_out <- latest_per_mod %>%
      mutate(Unidad = factor(Unidad, levels = ordered_units)) %>%
      arrange(Unidad)
    
    return(list(
      data = df_out,
      target_unit = NULL,
      target_sem = NULL,
      is_comparative = TRUE,
      mode = "latest_per_module"
    ))
  }
  
  # Case C: Specific Modulo AND Variedad == "Todas"
  if (!is.null(selected_modulo) && selected_modulo != "Todos" &&
      (is.null(selected_variedad) || selected_variedad == "Todas")) {
    
    cand_rows <- df_base %>%
      filter(Modulo == selected_modulo, YearCamp >= 2024)
    
    if (nrow(cand_rows) == 0) {
      return(list(data = data.frame(), target_unit = NULL, target_sem = NULL, is_comparative = FALSE, mode = "no_match"))
    }
    
    latest_per_var <- cand_rows %>%
      group_by(Variedad) %>%
      filter(YearCamp == max(YearCamp, na.rm = TRUE)) %>%
      ungroup()
    
    ordered_units <- unique(latest_per_var$Unidad)
    df_out <- latest_per_var %>%
      mutate(Unidad = factor(Unidad, levels = ordered_units)) %>%
      arrange(Unidad)
    
    return(list(
      data = df_out,
      target_unit = NULL,
      target_sem = NULL,
      is_comparative = TRUE,
      mode = "latest_per_variety"
    ))
  }
  
  # Case D: Modulo == "Todos" AND Variedad == "Todas"
  cand_rows <- df_base %>%
    filter(YearCamp >= 2024)
  
  if (nrow(cand_rows) == 0) {
    return(list(data = data.frame(), target_unit = NULL, target_sem = NULL, is_comparative = FALSE, mode = "no_match"))
  }
  
  latest_per_mv <- cand_rows %>%
    group_by(Modulo, Variedad) %>%
    filter(YearCamp == max(YearCamp, na.rm = TRUE)) %>%
    ungroup()
  
  u_poda <- latest_per_mv %>%
    group_by(Unidad, Modulo, Variedad) %>%
    summarise(
      poda_sem = if (any(Stage == "Poda")) min(SemNum[Stage == "Poda"], na.rm = TRUE) else min(SemNum, na.rm = TRUE),
      year_camp = max(YearCamp, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(poda_sem, Modulo)
  
  ordered_units <- u_poda$Unidad
  df_out <- latest_per_mv %>%
    mutate(Unidad = factor(Unidad, levels = ordered_units)) %>%
    arrange(Unidad)
  
  return(list(
    data = df_out,
    target_unit = NULL,
    target_sem = NULL,
    is_comparative = TRUE,
    mode = "latest_all"
  ))
}

# Build Matrix for Gantt
build_gantt_matrix <- function(df_records, selected_modulo = "Todos", selected_breeder = "Todos", selected_variedad = "Todas") {
  df_filtered <- df_records
  
  if (!is.null(selected_modulo) && selected_modulo != "Todos") {
    df_filtered <- df_filtered %>% filter(Modulo == selected_modulo)
  }
  if (!is.null(selected_breeder) && selected_breeder != "Todos") {
    df_filtered <- df_filtered %>% filter(toupper(Breeder) == toupper(selected_breeder))
  }
  if (!is.null(selected_variedad) && selected_variedad != "Todas") {
    df_filtered <- df_filtered %>% filter(Variedad == selected_variedad)
  }
  
  if (nrow(df_filtered) == 0) {
    return(data.frame())
  }
  
  # Group by Unidad and ColKey
  cells <- df_filtered %>%
    distinct(Unidad, Campaña, Modulo, Variedad, ColKey, .keep_all = TRUE) %>%
    select(Unidad, Campaña, Modulo, Variedad, ColKey, Stage)
  
  # Preserve explicit order of units
  ordered_units <- if (is.factor(cells$Unidad)) levels(cells$Unidad)[levels(cells$Unidad) %in% unique(cells$Unidad)] else unique(cells$Unidad)
  if (length(ordered_units) == 0) ordered_units <- unique(cells$Unidad)

  grid_template <- expand.grid(
    Unidad = ordered_units,
    ColKey = COL_METADATA$col_key,
    stringsAsFactors = FALSE
  )
  grid_template$Unidad <- factor(grid_template$Unidad, levels = ordered_units)
  
  mat_long <- grid_template %>%
    left_join(cells, by = c("Unidad", "ColKey")) %>%
    left_join(COL_METADATA, by = c("ColKey" = "col_key")) %>%
    arrange(Unidad, col_index)
  
  return(mat_long)
}

# Build summary block representation for easy editing
build_block_summary <- function(df_records, selected_modulo = "Todos", selected_variedad = "Sekoya Pop") {
  df_filtered <- df_records
  if (!is.null(selected_modulo) && selected_modulo != "Todos") {
    df_filtered <- df_filtered %>% filter(Modulo == selected_modulo)
  }
  if (!is.null(selected_variedad) && selected_variedad != "Todas") {
    df_filtered <- df_filtered %>% filter(Variedad == selected_variedad)
  }
  
  if (nrow(df_filtered) == 0) return(data.frame())
  
  df_ordered <- df_filtered %>%
    left_join(COL_METADATA %>% select(col_key, col_index, year_label, month_name, week_num), by = c("ColKey" = "col_key")) %>%
    arrange(Unidad, col_index)
  
  # Group consecutive stages per Unidad
  blocks_list <- list()
  b_idx <- 1
  
  for (u in unique(df_ordered$Unidad)) {
    sub_u <- df_ordered %>% filter(Unidad == u) %>% arrange(col_index)
    if (nrow(sub_u) == 0) next
    
    # Identify change points
    curr_stage <- sub_u$Stage[1]
    start_idx <- sub_u$col_index[1]
    start_sem <- sub_u$week_num[1]
    start_yr <- sub_u$year_label[1]
    start_mo <- sub_u$month_name[1]
    
    prev_idx <- start_idx
    count_sem <- 1
    
    camp <- sub_u$Campaña[1]
    mod <- sub_u$Modulo[1]
    var <- sub_u$Variedad[1]
    
    if (nrow(sub_u) > 1) {
      for (i in 2:nrow(sub_u)) {
        s <- sub_u$Stage[i]
        idx <- sub_u$col_index[i]
        
        # If same stage and consecutive index
        if (!is.na(s) && !is.na(curr_stage) && s == curr_stage && idx == (prev_idx + 1)) {
          prev_idx <- idx
          count_sem <- count_sem + 1
        } else {
          # Record previous block
          end_row <- sub_u[i - 1, ]
          blocks_list[[b_idx]] <- data.frame(
            Unidad = u,
            Campaña = camp,
            Modulo = mod,
            Variedad = var,
            Etapa = curr_stage,
            ColInicio = start_idx,
            ColFin = end_row$col_index,
            SemInicio = sprintf("%s Sem %d (%s)", start_yr, start_sem, start_mo),
            SemFin = sprintf("%s Sem %d (%s)", end_row$year_label, end_row$week_num, end_row$month_name),
            SemanaIniNum = start_sem,
            SemanaFinNum = end_row$week_num,
            Duracion_Semanas = count_sem,
            stringsAsFactors = FALSE
          )
          b_idx <- b_idx + 1
          
          # Start new block
          curr_stage <- s
          start_idx <- idx
          start_sem <- sub_u$week_num[i]
          start_yr <- sub_u$year_label[i]
          start_mo <- sub_u$month_name[i]
          prev_idx <- idx
          count_sem <- 1
        }
      }
    }
    
    # Last block
    last_row <- sub_u[nrow(sub_u), ]
    blocks_list[[b_idx]] <- data.frame(
      Unidad = u,
      Campaña = camp,
      Modulo = mod,
      Variedad = var,
      Etapa = curr_stage,
      ColInicio = start_idx,
      ColFin = last_row$col_index,
      SemInicio = sprintf("%s Sem %d (%s)", start_yr, start_sem, start_mo),
      SemFin = sprintf("%s Sem %d (%s)", last_row$year_label, last_row$week_num, last_row$month_name),
      SemanaIniNum = start_sem,
      SemanaFinNum = last_row$week_num,
      Duracion_Semanas = count_sem,
      stringsAsFactors = FALSE
    )
    b_idx <- b_idx + 1
  }
  
  bind_rows(blocks_list)
}

# Generate Productivity Curve Data (Generic Realistic Starter Data)
generate_productivity_data <- function(units, weeks = 1:52) {
  curves <- list()
  units <- as.character(unique(units))
  if (length(units) == 0) return(data.frame())
  
  for (u in units) {
    seed_val <- sum(as.numeric(charToRaw(u)))
    set.seed(seed_val)
    
    peak_w <- sample(30:33, 1)
    spread <- runif(1, 4.0, 5.5)
    target_total <- round(runif(1, 2.7, 3.8), 2)
    poda_week <- sample(c(2, 4, 7), 1)
    
    dens <- dnorm(weeks, mean = peak_w, sd = spread)
    dens[weeks < (peak_w - 9) | weeks > (peak_w + 12)] <- 0
    weekly_kg <- round((dens / sum(dens)) * target_total, 3)
    cum_kg <- round(cumsum(weekly_kg), 3)
    
    curves[[u]] <- data.frame(
      Unidad = u,
      Semana = weeks,
      Kg_Plt_Semanal = weekly_kg,
      Kg_Plt_Acumulado = cum_kg,
      Poda_Semana = poda_week,
      stringsAsFactors = FALSE
    )
  }
  
  bind_rows(curves)
}
