# ==============================================================================
# Gantt HTML Renderer - Emil Kowalski Craft
# Integrated Phenology Calendar + Climate Variables (F7 & F9)
# Pixel-perfect alignment matching Image 2 & Climate Mockup
# ==============================================================================

library(shiny)
library(htmltools)
source("d:/ACP/Analisis de Productividad/data_loader.R")
source("d:/ACP/Analisis de Productividad/climate_engine.R")

generate_gantt_html <- function(
  mat_long,
  highlighted_stage = NULL,
  cell_size = "standard",
  readonly = TRUE,
  target_unit = NULL,
  climate_engine = NULL,
  active_climate_vars = character(0),
  ref_media = "all"
) {
  if (is.null(mat_long) || nrow(mat_long) == 0) {
    return(tags$div(
      class = "empty-state-card",
      tags$div(class = "empty-icon", icon("magnifying-glass")),
      tags$h4("No hay datos disponibles"),
      tags$p("Ajusta los filtros de Módulo o Variedad para visualizar el calendario fenológico.")
    ))
  }
  
  # Width config per cell
  cell_w <- switch(cell_size,
    "compact" = 16,
    "standard" = 22,
    "spacious" = 28,
    22
  )
  
  # Group data by Unidad
  units <- unique(as.character(mat_long$Unidad))
  
  # Climate mode enabled if engine is present and at least one climate variable is selected
  has_climate <- !is.null(climate_engine) && length(active_climate_vars) > 0
  
  # Month column boundaries for visual separation
  month_end_cols <- c(4, 9, 13, 17, 22, 26, 30, 35, 39, 43, 48, 52, 56)
  
  # Month headers definition
  month_headers <- list(
    list(name = "Dic", colspan = 4, year = "Año 0"),
    list(name = "Ene", colspan = 5, year = "Año 1"),
    list(name = "Feb", colspan = 4, year = "Año 1"),
    list(name = "Mar", colspan = 4, year = "Año 1"),
    list(name = "Abr", colspan = 5, year = "Año 1"),
    list(name = "May", colspan = 4, year = "Año 1"),
    list(name = "Jun", colspan = 4, year = "Año 1"),
    list(name = "Jul", colspan = 5, year = "Año 1"),
    list(name = "Ago", colspan = 4, year = "Año 1"),
    list(name = "Set", colspan = 4, year = "Año 1"),
    list(name = "Oct", colspan = 5, year = "Año 1"),
    list(name = "Nov", colspan = 4, year = "Año 1"),
    list(name = "Dic", colspan = 4, year = "Año 1")
  )
  
  # Build Header Row 1
  h1_cells <- list(
    tags$th(
      class = "th-unidad-corner",
      rowspan = "3",
      tags$div(
        class = "unidad-corner-content",
        tags$span(class = "corner-title", "Campaña / Unidad"),
        tags$span(class = "corner-count", sprintf("%d campañas", length(units)))
      )
    )
  )
  
  if (has_climate) {
    h1_cells[[length(h1_cells) + 1]] <- tags$th(
      class = "th-var-corner",
      rowspan = "3",
      tags$div(
        class = "var-corner-content",
        tags$span(class = "corner-title", "Variable")
      )
    )
  }
  
  h1_cells[[length(h1_cells) + 1]] <- tags$th(class = "th-year year-0", colspan = "4", "Año 0")
  h1_cells[[length(h1_cells) + 1]] <- tags$th(class = "th-year year-1", colspan = "52", "Año 1")
  
  h_row1 <- tags$tr(class = "header-row-years", h1_cells)
  
  # Build Header Row 2: Months
  month_ths <- lapply(month_headers, function(m) {
    tags$th(
      class = paste0("th-month ", ifelse(m$year == "Año 0", "mo-y0", "mo-y1")),
      colspan = as.character(m$colspan),
      m$name
    )
  })
  h_row2 <- tags$tr(class = "header-row-months", month_ths)
  
  # Build Header Row 3: Week numbers
  week_ths <- lapply(1:nrow(COL_METADATA), function(i) {
    cm <- COL_METADATA[i, ]
    is_month_end <- i %in% month_end_cols
    cls <- paste0("th-week", ifelse(is_month_end, " month-border-right", ""))
    tags$th(
      class = cls,
      style = sprintf("width: %dpx; min-width: %dpx; max-width: %dpx;", cell_w, cell_w, cell_w),
      tags$span(class = "week-label", cm$week_num)
    )
  })
  h_row3 <- tags$tr(class = "header-row-weeks", week_ths)
  
  # Split mat_long by Unidad for fast row rendering
  mat_by_unit <- split(mat_long, mat_long$Unidad)
  
  # Build Body Rows
  all_body_rows <- list()
  
  for (u in units) {
    sub_u <- mat_by_unit[[u]]
    if (is.null(sub_u) || nrow(sub_u) == 0) next
    
    sub_u <- sub_u[order(sub_u$col_index), ]
    
    # Extract metadata for module, year and station
    u_mod <- sub_u$Modulo[1]
    if (is.null(u_mod) || is.na(u_mod) || !nzchar(as.character(u_mod))) {
      u_mod <- sub("^[0-9]+ ([^ ]+).*", "\\1", u)
    }
    
    u_year <- sub_u$YearCamp[1]
    if (is.null(u_year) || is.na(u_year)) {
      u_year <- as.numeric(substr(u, 1, 4))
    }
    
    zona <- get_modulo_filtrado(u_mod)
    station_badge_cls <- ifelse(zona == "Filtrado 7", "station-badge f7", "station-badge f9")
    station_label <- ifelse(zona == "Filtrado 7", "F7", "F9")
    
    is_target <- !is.null(target_unit) && (u == target_unit)
    
    n_subrows <- if (has_climate) (1 + length(active_climate_vars)) else 1
    
    # --- SUB-ROW 1: Phenological Stages (Etapa) ---
    cells_pheno <- lapply(1:nrow(sub_u), function(r_idx) {
      row_item <- sub_u[r_idx, ]
      stage <- row_item$Stage
      has_stage <- !is.na(stage) && nzchar(stage)
      is_month_end <- row_item$col_index %in% month_end_cols
      
      bg_color <- "transparent"
      opacity_style <- ""
      text_color <- "#000000"
      
      if (has_stage) {
        bg_color <- STAGE_COLORS[stage]
        if (is.na(bg_color)) bg_color <- "#94A3B8"
        text_color <- STAGE_TEXT_COLORS[stage]
        if (is.na(text_color)) text_color <- "#FFFFFF"
        
        if (!is.null(highlighted_stage) && highlighted_stage != "Todos") {
          if (stage != highlighted_stage) {
            opacity_style <- "opacity: 0.18; filter: grayscale(40%);"
          } else {
            opacity_style <- "opacity: 1; transform: scaleY(1.08); z-index: 5;"
          }
        }
      }
      
      cls <- paste0(
        "gantt-cell",
        ifelse(has_stage, " has-stage", " is-empty"),
        ifelse(is_month_end, " month-border-right", ""),
        ifelse(readonly, " is-readonly", " is-editable")
      )
      
      tooltip_txt <- if (has_stage) {
        sprintf("%s | %s Sem %d (%s) | %s", u, row_item$year_label, row_item$week_num, row_item$month_name, stage)
      } else {
        sprintf("%s | %s Sem %d (%s) | (Sin etapa)", u, row_item$year_label, row_item$week_num, row_item$month_name)
      }
      
      cell_attrs <- list(
        class = cls,
        style = sprintf(
          "width: %dpx; min-width: %dpx; max-width: %dpx; background-color: %s; %s",
          cell_w, cell_w, cell_w, bg_color, opacity_style
        ),
        title = tooltip_txt,
        `data-unidad` = u,
        `data-colkey` = row_item$ColKey,
        `data-week` = row_item$week_num,
        `data-year` = row_item$year_label,
        `data-stage` = ifelse(has_stage, stage, "")
      )
      
      if (!readonly) {
        cell_attrs$onclick <- sprintf("handleCellClick('%s', '%s', '%s')", u, row_item$ColKey, ifelse(has_stage, stage, ""))
      }
      
      do.call(tags$td, cell_attrs)
    })
    
    # Unit Label Cell (with rowspan for climate variables)
    u_td <- tags$td(
      class = paste0("td-unidad-label", ifelse(is_target, " is-target-unit", "")),
      rowspan = as.character(n_subrows),
      title = sprintf("%s • Estación: %s", u, zona),
      tags$div(
        class = "unidad-name-container",
        tags$span(class = "unidad-name-text", u),
        tags$span(class = station_badge_cls, title = sprintf("Estación meteorológica: %s", zona), station_label),
        if (is_target) tags$span(class = "badge-target-unit", "Módulo consultado")
      )
    )
    
    row_1_cells <- list(u_td)
    if (has_climate) {
      row_1_cells[[length(row_1_cells) + 1]] <- tags$td(
        class = "td-var-sublabel td-var-etapa",
        tags$span(class = "var-chip-etapa", "Etapa")
      )
    }
    row_1_cells <- c(row_1_cells, cells_pheno)
    
    row_1 <- tags$tr(
      class = paste0("gantt-row gantt-row-etapa", ifelse(is_target, " is-target-unit", "")),
      `data-unidad` = u,
      row_1_cells
    )
    all_body_rows[[length(all_body_rows) + 1]] <- row_1
    
    # --- SUB-ROWS: Climate Variables ---
    if (has_climate) {
      for (v_name in active_climate_vars) {
        v_def <- CLIMATE_VAR_DEFS[[v_name]]
        badge_txt <- if (!is.null(v_def)) v_def$badge else v_name
        unit_str <- if (!is.null(v_def)) v_def$unit else ""
        
        # Climate cells across 56 columns
        cells_climate <- lapply(1:nrow(COL_METADATA), function(i) {
          cm <- COL_METADATA[i, ]
          is_month_end <- i %in% month_end_cols
          target_year <- if (cm$year_num == 0) (u_year - 1) else u_year
          target_week <- cm$week_num
          
          key <- sprintf("%s|%s|%d|%d", zona, v_name, target_year, target_week)
          c_info <- climate_engine$lookup[[key]]
          
          if (!is.null(c_info)) {
            delta_val <- if (ref_media == "excl") c_info$delta_excl else c_info$delta_all
            st <- get_anomaly_style(delta_val, v_name)
            med_val <- if (ref_media == "excl") c_info$media_excl else c_info$media_all
            
            tooltip_txt <- sprintf(
              "%s • %s\n%s Sem %d (%s) • %s\nValor Real: %.1f %s\nMedia Histórica: %.1f %s\nAnomalía: %s %s",
              u, v_name, cm$year_label, cm$week_num, cm$month_name, zona,
              c_info$valor, unit_str, med_val, unit_str, st$formatted, unit_str
            )
            
            cell_cls <- paste0("gantt-cell climate-cell", ifelse(is_month_end, " month-border-right", ""))
            tags$td(
              class = cell_cls,
              style = sprintf(
                "width: %dpx; min-width: %dpx; max-width: %dpx; background-color: %s; color: %s;",
                cell_w, cell_w, cell_w, st$bg, st$text
              ),
              title = tooltip_txt,
              tags$span(class = "climate-val-txt", st$formatted)
            )
          } else {
            cell_cls <- paste0("gantt-cell climate-cell is-empty", ifelse(is_month_end, " month-border-right", ""))
            tags$td(
              class = cell_cls,
              style = sprintf("width: %dpx; min-width: %dpx; max-width: %dpx;", cell_w, cell_w, cell_w),
              title = sprintf("%s • %s\n%s Sem %d (%s) • (Sin datos climáticos)", u, v_name, cm$year_label, cm$week_num, cm$month_name),
              tags$span(class = "climate-val-txt", "-")
            )
          }
        })
        
        var_td <- tags$td(
          class = "td-var-sublabel td-var-climate",
          title = sprintf("%s (%s)", v_name, zona),
          tags$span(class = "var-chip-climate", badge_txt)
        )
        
        row_clim_cells <- c(list(var_td), cells_climate)
        row_clim <- tags$tr(
          class = paste0("gantt-row gantt-row-climate", ifelse(is_target, " is-target-unit", "")),
          `data-unidad` = u,
          `data-variable` = v_name,
          row_clim_cells
        )
        all_body_rows[[length(all_body_rows) + 1]] <- row_clim
      }
    }
  }
  
  # Wrap in responsive container
  tags$div(
    class = "gantt-scroll-wrapper",
    tags$table(
      class = paste0("gantt-matrix-table", ifelse(has_climate, " has-climate-rows", "")),
      tags$thead(h_row1, h_row2, h_row3),
      tags$tbody(all_body_rows)
    )
  )
}

# ==============================================================================
# Fenología Stage Duration Summary Table (Pixel-perfect matching Emil Kowalski)
# Columns: Campaña / Unidad, Etapas (duración en semanas, fecha mínima para poda)
# ==============================================================================
generate_fenologia_summary_html <- function(df_data, target_unit = NULL) {
  if (is.null(df_data) || nrow(df_data) == 0) return(NULL)
  
  ordered_units <- if (is.factor(df_data$Unidad)) {
    levels(df_data$Unidad)[levels(df_data$Unidad) %in% unique(df_data$Unidad)]
  } else {
    unique(df_data$Unidad)
  }
  
  canonical_stages <- c("Poda", "Siembra", "Punta Verde", "Vegetativo", "Inducción Floral", "Floración", "Formación Fruta", "Cosecha", "Eliminación")
  stages_present <- intersect(canonical_stages, unique(df_data$Stage))
  
  # Build Header
  th_list <- list(
    tags$th(
      class = "th-summary-unit",
      style = "padding: 10px 16px; font-weight: 700; color: #334155; font-size: 11px; text-transform: uppercase; letter-spacing: 0.04em; min-width: 220px; border-right: 1px solid #E2E8F0; background: #F8FAFC;",
      "Campaña / Unidad"
    )
  )
  for (st in stages_present) {
    clr <- STAGE_COLORS[st]
    if (is.na(clr)) clr <- "#64748B"
    sub_title <- if (st == "Poda") "Fecha mín." else "Duración"
    th_list[[length(th_list) + 1]] <- tags$th(
      class = "th-summary-stage",
      style = "padding: 8px 14px; font-weight: 600; color: #1E293B; text-align: center; border-right: 1px solid #E2E8F0; min-width: 110px; background: #F8FAFC;",
      tags$div(
        class = "summary-th-content",
        style = "display: flex; flex-direction: column; align-items: center; gap: 3px;",
        tags$span(class = "summary-stage-dot", style = paste0("width: 8px; height: 8px; border-radius: 50%; display: inline-block; background-color: ", clr, ";")),
        tags$span(class = "summary-stage-title", style = "font-size: 11.5px; font-weight: 700; color: #0F172A; white-space: nowrap;", st),
        tags$span(class = "summary-stage-sub", style = "font-size: 10px; font-weight: 500; color: #64748B;", sprintf("(%s)", sub_title))
      )
    )
    
    # Inserción de la sumatoria de Desarrollo Fenológico (desde post-poda hasta fin de formación fruta)
    if (st == "Formación Fruta") {
      th_list[[length(th_list) + 1]] <- tags$th(
        class = "th-summary-stage th-summary-dev",
        style = "padding: 8px 14px; font-weight: 600; color: #1E293B; text-align: center; border-right: 1px solid #E2E8F0; min-width: 130px; background: #F8FAFC;",
        tags$div(
          class = "summary-th-content",
          style = "display: flex; flex-direction: column; align-items: center; gap: 3px;",
          tags$span(class = "summary-stage-dot", style = "width: 8px; height: 8px; border-radius: 50%; display: inline-block; background-color: #6366F1;"),
          tags$span(class = "summary-stage-title", style = "font-size: 11.5px; font-weight: 700; color: #4338CA; white-space: nowrap;", "Desarrollo Fenológico"),
          tags$span(class = "summary-stage-sub", style = "font-size: 10px; font-weight: 500; color: #6366F1;", "(Post-Poda a Fruta)")
        )
      )
    }
  }
  
  # Build Body Rows
  tr_list <- list()
  for (u in ordered_units) {
    u_sub <- df_data[df_data$Unidad == u, ]
    is_target <- !is.null(target_unit) && (u == target_unit)
    
    u_mod <- u_sub$Modulo[1]
    if (is.null(u_mod) || is.na(u_mod) || !nzchar(as.character(u_mod))) {
      u_mod <- sub("^[0-9]+ ([^ ]+).*", "\\1", u)
    }
    zona <- get_modulo_filtrado(u_mod)
    station_badge_cls <- ifelse(zona == "Filtrado 7", "station-badge f7", "station-badge f9")
    station_label <- ifelse(zona == "Filtrado 7", "F7", "F9")
    station_bg <- ifelse(zona == "Filtrado 7", "#EFF6FF", "#FAF5FF")
    station_clr <- ifelse(zona == "Filtrado 7", "#1D4ED8", "#7E22CE")
    station_bdr <- ifelse(zona == "Filtrado 7", "#BFDBFE", "#E9D5FF")
    
    unit_content <- tags$div(
      class = "summary-unit-cell",
      style = "display: flex; align-items: center; gap: 8px; flex-wrap: wrap;",
      tags$span(class = "summary-unit-name", style = "font-weight: 600; color: #0F172A;", u),
      tags$span(
        class = station_badge_cls,
        style = sprintf("font-size: 9px; font-weight: 700; padding: 1px 5px; border-radius: 4px; background: %s; color: %s; border: 1px solid %s;", station_bg, station_clr, station_bdr),
        station_label
      ),
      if (is_target) tags$span(
        class = "target-badge-pill",
        style = "font-size: 10px; font-weight: 600; color: #059669; background: #ECFDF5; border: 1px solid #A7F3D0; border-radius: 12px; padding: 1px 8px; margin-left: 4px;",
        "Módulo consultado"
      ) else NULL
    )
    
    row_bg <- ifelse(is_target, "background: #F0FDF4;", "background: #FFFFFF;")
    
    td_list <- list(
      tags$td(
        class = paste0("td-summary-unit", ifelse(is_target, " is-target-unit", "")),
        style = paste0("padding: 9px 16px; border-right: 1px solid #E2E8F0; border-bottom: 1px solid #F1F5F9; ", row_bg),
        unit_content
      )
    )
    
    for (st in stages_present) {
      st_sub <- u_sub[u_sub$Stage == st, ]
      if (st == "Poda") {
        sem_vals <- suppressWarnings(as.numeric(st_sub$SemNum))
        valid_mask <- !is.na(sem_vals)
        if (any(valid_mask)) {
          min_sem <- min(sem_vals[valid_mask])
          valid_indices <- which(valid_mask)
          min_idx <- valid_indices[which.min(sem_vals[valid_mask])]
          per_lbl <- if (!is.na(min_idx) && length(st_sub$PeriodLabel) >= min_idx) st_sub$PeriodLabel[min_idx] else ""
          mes_anio <- sub(" Sem.*", "", per_lbl)
          val_txt <- sprintf("Sem %s (%s)", as.character(min_sem), as.character(mes_anio))
          td_list[[length(td_list) + 1]] <- tags$td(
            class = "td-summary-stage",
            style = paste0("padding: 8px 12px; text-align: center; border-right: 1px solid #E2E8F0; border-bottom: 1px solid #F1F5F9; ", row_bg),
            tags$span(
              class = "summary-badge-poda",
              style = "display: inline-block; padding: 2px 8px; border-radius: 6px; font-size: 11px; font-weight: 600; background: #FEF2F2; color: #DC2626; border: 1px solid #FECACA;",
              val_txt
            )
          )
        } else {
          td_list[[length(td_list) + 1]] <- tags$td(
            class = "td-summary-stage",
            style = paste0("padding: 8px 12px; text-align: center; border-right: 1px solid #E2E8F0; border-bottom: 1px solid #F1F5F9; ", row_bg),
            tags$span(class = "summary-dash", style = "color: #CBD5E1;", "—")
          )
        }
      } else {
        cnt <- nrow(st_sub)
        if (cnt > 0) {
          td_list[[length(td_list) + 1]] <- tags$td(
            class = "td-summary-stage",
            style = paste0("padding: 8px 12px; text-align: center; border-right: 1px solid #E2E8F0; border-bottom: 1px solid #F1F5F9; ", row_bg),
            tags$span(
              class = "summary-badge-duration",
              style = "display: inline-block; padding: 2px 8px; border-radius: 6px; font-size: 11px; font-weight: 600; background: #F1F5F9; color: #334155;",
              sprintf("%s sem", as.character(cnt))
            )
          )
        } else {
          td_list[[length(td_list) + 1]] <- tags$td(
            class = "td-summary-stage",
            style = paste0("padding: 8px 12px; text-align: center; border-right: 1px solid #E2E8F0; border-bottom: 1px solid #F1F5F9; ", row_bg),
            tags$span(class = "summary-dash", style = "color: #CBD5E1;", "—")
          )
        }
      }
      
      # Inserción de celda Desarrollo Fenológico tras Formación Fruta
      if (st == "Formación Fruta") {
        dev_stages <- c("Punta Verde", "Vegetativo", "Inducción Floral", "Floración", "Formación Fruta")
        tot_dev_sem <- sum(u_sub$Stage %in% dev_stages, na.rm = TRUE)
        
        dev_badge <- if (tot_dev_sem > 0) {
          tags$span(
            class = "summary-badge-dev",
            style = "display: inline-block; padding: 2px 8px; border-radius: 6px; font-size: 11px; font-weight: 700; background: #EEF2FF; color: #4338CA; border: 1px solid #C7D2FE;",
            sprintf("%s sem", as.character(tot_dev_sem))
          )
        } else {
          tags$span(class = "summary-dash", style = "color: #CBD5E1;", "—")
        }
        
        td_list[[length(td_list) + 1]] <- tags$td(
          class = "td-summary-stage td-summary-dev",
          style = paste0("padding: 8px 12px; text-align: center; border-right: 1px solid #E2E8F0; border-bottom: 1px solid #F1F5F9; ", ifelse(is_target, "background: #EEF2FF;", "background: #FAFAFE;")),
          dev_badge
        )
      }
    }
    
    tr_list[[length(tr_list) + 1]] <- tags$tr(
      class = paste0("summary-body-row", ifelse(is_target, " target-row", "")),
      td_list
    )
  }
  
  tags$div(
    class = "fenologia-summary-card",
    style = "margin-top: 22px; padding-top: 18px; border-top: 1px solid #E2E8F0;",
    tags$div(
      class = "fenologia-summary-header",
      style = "display: flex; align-items: center; justify-content: space-between; margin-bottom: 12px;",
      tags$div(
        class = "summary-title-group",
        tags$h4(class = "fenologia-summary-title", style = "font-size: 13.5px; font-weight: 700; color: #0F172A; margin: 0 0 2px 0;", "Resumen de Etapas Fenológicas por Campaña"),
        tags$p(class = "fenologia-summary-sub", style = "font-size: 11.5px; color: #64748B; margin: 0;", "Duración en semanas activas por etapa fenológica y semana mínima de inicio para poda.")
      )
    ),
    tags$div(
      class = "fenologia-summary-table-wrapper",
      style = "overflow-x: auto; border: 1px solid #E2E8F0; border-radius: 10px; background: #FFFFFF; box-shadow: 0 1px 3px rgba(0, 0, 0, 0.02);",
      tags$table(
        class = "fenologia-summary-table",
        style = "width: 100%; border-collapse: collapse; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; font-size: 12px; text-align: left;",
        tags$thead(tags$tr(th_list)),
        tags$tbody(tr_list)
      )
    )
  )
}

# ==============================================================================
# COMBINED EXPORT DATA FRAME BUILDERS (EXCEL COMPATIBLE)
# ==============================================================================

#' Build 56-week combined phenology and climate data frame for Excel export
#' Combines Stage row and active Climate temperature rows per campaign/unit
build_fenologia_climate_export_df <- function(
  mat_long,
  climate_engine = NULL,
  active_climate_vars = character(0),
  ref_media = "all"
) {
  if (is.null(mat_long) || nrow(mat_long) == 0) return(data.frame())
  
  has_climate <- !is.null(climate_engine) && length(active_climate_vars) > 0
  units <- unique(mat_long$Unidad)
  
  # Build 56 column headers matching the canonical calendar
  col_headers <- sapply(1:nrow(COL_METADATA), function(i) {
    cm <- COL_METADATA[i, ]
    sprintf("%s - Sem %02d (%s)", cm$year_label, cm$week_num, cm$month_name)
  })
  
  rows_list <- list()
  
  for (u in units) {
    sub_u <- mat_long %>% filter(Unidad == u) %>% arrange(col_index)
    
    # Safe module and year parsing
    u_str <- as.character(u)
    u_year_str <- sub("^([0-9]{4}).*", "\\1", u_str)
    u_year <- if (grepl("^[0-9]{4}$", u_year_str)) as.numeric(u_year_str) else 2027
    u_mod <- sub("^[0-9]+ ([^ ]+).*", "\\1", u_str)
    zona <- get_modulo_filtrado(u_mod)
    
    # 1. Phenology Stage Row
    stage_vals <- sapply(1:nrow(COL_METADATA), function(i) {
      match_col <- sub_u %>% filter(col_index == i)
      if (nrow(match_col) > 0 && !is.na(match_col$Stage[1]) && nzchar(trimws(match_col$Stage[1]))) {
        as.character(match_col$Stage[1])
      } else {
        ""
      }
    })
    
    row_stage <- c(
      list("Campaña / Unidad" = u_str, "Variable" = "Etapa"),
      as.list(setNames(stage_vals, col_headers))
    )
    rows_list[[length(rows_list) + 1]] <- as.data.frame(row_stage, stringsAsFactors = FALSE, check.names = FALSE)
    
    # 2. Climate Sub-Rows if active
    if (has_climate) {
      for (v_name in active_climate_vars) {
        v_def <- CLIMATE_VAR_DEFS[[v_name]]
        badge_txt <- if (!is.null(v_def)) v_def$badge else v_name
        var_label <- badge_txt
        
        clim_vals <- sapply(1:nrow(COL_METADATA), function(i) {
          cm <- COL_METADATA[i, ]
          target_year <- if (cm$year_num == 0) (u_year - 1) else u_year
          target_week <- cm$week_num
          key <- sprintf("%s|%s|%d|%d", zona, v_name, target_year, target_week)
          c_info <- climate_engine$lookup[[key]]
          
          if (!is.null(c_info)) {
            delta_val <- if (ref_media == "excl") c_info$delta_excl else c_info$delta_all
            if (!is.na(delta_val)) sprintf("%+.1f", delta_val) else "-"
          } else {
            "-"
          }
        })
        
        row_clim <- c(
          list("Campaña / Unidad" = u_str, "Variable" = var_label),
          as.list(setNames(clim_vals, col_headers))
        )
        rows_list[[length(rows_list) + 1]] <- as.data.frame(row_clim, stringsAsFactors = FALSE, check.names = FALSE)
      }
    }
  }
  
  df_export <- bind_rows(rows_list)
  if (!has_climate && "Variable" %in% colnames(df_export)) {
    df_export <- df_export %>% select(-Variable)
  }
  
  return(df_export)
}

#' Build summary dataframe matching the UI Resumen de Etapas table
build_fenologia_summary_export_df <- function(df_records, target_unit = NULL) {
  if (is.null(df_records) || nrow(df_records) == 0) return(data.frame())
  
  ordered_units <- rev(unique(df_records$Unidad))
  
  rows <- list()
  for (u in ordered_units) {
    sub_u <- df_records %>% filter(Unidad == u)
    if (nrow(sub_u) == 0) next
    
    u_mod <- sub("^[0-9]+ ([^ ]+).*", "\\1", u)
    zona <- get_modulo_filtrado(u_mod)
    estacion_label <- ifelse(zona == "Filtrado 7", "F7", "F9")
    
    # Calculate poda info
    poda_rows <- sub_u %>% filter(Stage == "Poda")
    poda_label <- "-"
    if (nrow(poda_rows) > 0) {
      min_sem <- min(poda_rows$SemNum, na.rm = TRUE)
      yr <- poda_rows$AñoNum[which.min(poda_rows$SemNum)]
      meses <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Set", "Oct", "Nov", "Dic")
      m_idx <- pmin(12, pmax(1, ceiling(min_sem / 4.33)))
      poda_label <- sprintf("Sem %d (%s-%02d)", min_sem, meses[m_idx], yr %% 100)
    }
    
    # Calculate durations (number of active weeks per stage)
    get_dur <- function(st_name) {
      st_rows <- sub_u %>% filter(Stage == st_name)
      if (nrow(st_rows) > 0) sprintf("%d sem", nrow(st_rows)) else "—"
    }
    get_dur_num <- function(st_name) {
      st_rows <- sub_u %>% filter(Stage == st_name)
      if (nrow(st_rows) > 0) nrow(st_rows) else 0
    }
    
    # Sum of phenological development stages (Punta Verde a Formación Fruta)
    dev_sum <- get_dur_num("Punta Verde") + 
               get_dur_num("Vegetativo") + 
               get_dur_num("Inducción Floral") + 
               get_dur_num("Floración") + 
               get_dur_num("Formación Fruta")
    
    dev_label <- if (dev_sum > 0) sprintf("%d sem", dev_sum) else "—"
    
    row_data <- data.frame(
      "Campaña / Unidad" = u,
      "Estación" = estacion_label,
      "Poda (Fecha mín.)" = poda_label,
      "Punta Verde (Duración)" = get_dur("Punta Verde"),
      "Vegetativo (Duración)" = get_dur("Vegetativo"),
      "Inducción Floral (Duración)" = get_dur("Inducción Floral"),
      "Floración (Duración)" = get_dur("Floración"),
      "Formación Fruta (Duración)" = get_dur("Formación Fruta"),
      "Desarrollo Fenológico (Semanas)" = dev_label,
      "Cosecha (Duración)" = get_dur("Cosecha"),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    rows[[length(rows) + 1]] <- row_data
  }
  
  bind_rows(rows)
}

