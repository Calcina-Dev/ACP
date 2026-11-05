# ==============================================================================
# PROYECTO: ANÁLISIS DE PRODUCTIVIDAD
# Agrícola Cerro Prieto (ACP) • Lineamientos Emil Kowalski & Diseño iOS
# 1. Logo Oficial de la Empresa
# 2. Cajas Seleccionadoras Formato iOS & Alineación Horizontal/Vertical Perfecta
# 3. Selección Simultánea de Año Neutral y Año Caliente (Comparación Dual)
# 4. Eliminación Completa de Íconos Emojis / IA Clichés en Mensajes y Acciones
# 5. Menú de 3 Puntos (⋯) con Switches iOS para Comparar Escenarios (Solo ESC. CAMPO inicial)
# 6. Calendario Fenológico Histórico (Primero) & Curvas con Tabla de Campañas (Segundo)
# ==============================================================================

library(shiny)
library(bslib)
library(DT)
library(plotly)
library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(htmltools)

# Source core engine modules
source("d:/ACP/Analisis de Productividad/data_loader.R")
source("d:/ACP/Analisis de Productividad/gantt_renderer.R")
source("d:/ACP/Analisis de Productividad/scenario_engine.R")
source("d:/ACP/Analisis de Productividad/climate_engine.R")

HIST_CAMPAIGN_COLORS <- c(
  "2024/2025" = "#8B5CF6",
  "2025/2026" = "#D97706",
  "2026/2027" = "#06B6D4"
)

# Base Excel paths
EXCEL_FENOLOGIA_PATH <- "G:/Unidades compartidas/Compartido Proyecciones/3.- Arándano/Data Histórica/Etapas Fenologicas/Historico_Etapas_Fenologicas.xlsx"
EXCEL_ESCENARIOS_PATH <- "d:/ACP/Analisis de Productividad/Datos_Escenarios_27-28_2026-11-02.xlsx"
PATH_HISTORICAL_YIELDS <- "G:/Unidades compartidas/Compartido Proyecciones/3.- Arándano/Data Histórica/Productividad/KgPlanta por Campaña.xlsx"

# ------------------------------------------------------------------------------
# UI DEFINITION
# ------------------------------------------------------------------------------

ui <- fluidPage(
  title = "Análisis de Productividad • Cerro Prieto",
  lang = "es",
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css?v=20261103_4"),
    tags$script(src = "app.js?v=20261103_4")
  ),
  
  tags$div(
    class = "app-container",
    
    # --- Top Navigation Bar ---
    tags$header(
      class = "top-nav",
      tags$div(
        class = "brand-section",
        tags$img(src = "logo_cerro_prieto.png", class = "company-header-logo", alt = "Agrícola Cerro Prieto"),
        tags$div(
          tags$h1(class = "brand-title", "Análisis de Productividad"),
          tags$p(class = "brand-subtitle", "Curvas de Producción por Escenario y Calendario Fenológico • Arándano")
        )
      ),
      tags$div(
        class = "header-actions",
        actionButton(
          "btn_undo",
          label = "Deshacer Rendimiento",
          icon = icon("rotate-left"),
          class = "btn-emil"
        ),
        actionButton(
          "btn_reset_all",
          label = "Restablecer Originales",
          icon = icon("arrow-rotate-right"),
          class = "btn-emil btn-emil-danger"
        ),
        downloadButton(
          "btn_export_escenarios",
          label = "Exportar Análisis (Excel)",
          class = "btn-emil btn-emil-primary"
        )
      )
    ),
    
    # --- Unified Filter Card (iOS Format & Pixel-Perfect Baseline Alignment) ---
    tags$div(
      class = "filter-card",
      tags$div(
        class = "filter-bar-ios",
        
        # 1. Breeder (Nivel 1 de la Jerarquía)
        tags$div(
          class = "filter-field filter-field-breeder",
          tags$label(class = "filter-label", "Breeder"),
          selectInput(
            "filtro_breeder",
            label = NULL,
            choices = c("Todos"),
            selected = "Todos",
            width = "100%"
          )
        ),
        
        # 2. Módulo (Nivel 2 de la Jerarquía - Filtrado por Breeder)
        tags$div(
          class = "filter-field filter-field-modulo",
          tags$label(class = "filter-label", "Módulo"),
          selectInput(
            "filtro_modulo",
            label = NULL,
            choices = c("Todos"),
            selected = "Todos",
            width = "100%"
          )
        ),
        
        # 3. Variedad (Nivel 3 de la Jerarquía - Filtrada por Breeder y Módulo)
        tags$div(
          class = "filter-field filter-field-variedad",
          tags$label(class = "filter-label", "Variedad"),
          selectInput(
            "filtro_variedad",
            label = NULL,
            choices = c("Todas"),
            selected = "Todas",
            width = "100%"
          )
        ),
        
        # 4. Action Buttons (Buscar y Limpiar) con label dummy para alineación exacta
        tags$div(
          class = "filter-field",
          tags$label(class = "filter-label", style = "visibility: hidden;", "Acción"),
          tags$div(
            class = "filter-buttons-cluster",
            actionButton(
              "btn_buscar",
              label = "Buscar",
              icon = icon("magnifying-glass"),
              class = "btn-ios-search"
            ),
            actionButton(
              "btn_filter_clear",
              label = "Limpiar",
              icon = icon("filter-circle-xmark"),
              class = "btn-ios-clear"
            )
          )
        ),
        
        # 5. Stats pill (Right aligned flush) con label dummy para alineación exacta
        tags$div(
          class = "filter-stat-container",
          tags$label(class = "filter-label", style = "visibility: hidden;", "Resumen"),
          uiOutput("filter_stat_pill_ui")
        )
      )
    ),
    
    # --------------------------------------------------------------------------
    # ESTADO INICIAL EN BLANCO (Muestra Empty State antes de Buscar)
    # --------------------------------------------------------------------------
    conditionalPanel(
      condition = "output.has_searched == false",
      tags$div(
        class = "empty-search-state",
        tags$div(class = "empty-search-icon-box", icon("magnifying-glass")),
        tags$h3(class = "empty-search-title", "Consulta de Análisis de Productividad"),
        tags$p(
          class = "empty-search-desc",
          "Selecciona los filtros de Módulo, Breeder o Variedad y haz clic en Buscar para consultar el calendario fenológico y las curvas de rendimiento."
        ),
        tags$div(
          class = "quick-filter-container",
          actionButton("btn_quick_sekoya", "Sekoya Pop (NORTH BAY)", class = "quick-filter-btn"),
          actionButton("btn_quick_imperial", "Imperial (HORTIFRUT)", class = "quick-filter-btn"),
          actionButton("btn_quick_biloxi", "Biloxi (LIBRE)", class = "quick-filter-btn"),
          actionButton("btn_quick_todos", "Ver Todos los Módulos", class = "quick-filter-btn")
        )
      )
    ),
    
    # --------------------------------------------------------------------------
    # CONTENEDOR PRINCIPAL DE RESULTADOS (Se activa tras pulsar Buscar)
    # --------------------------------------------------------------------------
    conditionalPanel(
      condition = "output.has_searched == true",
      
      # ------------------------------------------------------------------------
      # 1. CALENDARIO FENOLÓGICO (PRIMERO)
      # ------------------------------------------------------------------------
      tags$div(
        class = "content-card",
        tags$div(
          class = "card-header-flex",
          tags$div(
            class = "card-title-group",
            tags$h3(textOutput("fenologia_header_title", inline = TRUE)),
            uiOutput("fenologia_header_subtitle_ui")
          ),
          tags$div(
            class = "fenologia-header-controls",
            
            # Menú de 3 Puntos (⋯) con Switches iOS para Variables Climáticas (Desplegable idéntico a Comparar)
            tags$div(
              class = "dots-menu-wrapper",
              tags$button(
                id = "btn_toggle_climate",
                type = "button",
                class = "btn-dots-trigger",
                tags$span(class = "dots-icon", "⋯"),
                tags$span("Clima"),
                tags$span(id = "active_climate_badge", class = "dots-badge", textOutput("active_climate_count", inline = TRUE))
              ),
              tags$div(
                id = "popover_climate_menu",
                class = "ios-popover",
                style = "width: 275px; right: 0; left: auto;",
                tags$div(class = "ios-popover-header", "Variables Climáticas"),
                
                # 1. Temperatura promedio (Inactiva inicial por defecto)
                tags$label(
                  class = "ios-switch-row",
                  `for` = "switch_clim_temp_prom",
                  tags$span(
                    class = "ios-switch-label",
                    tags$span(class = "swatch-dot", style = "background-color: #2563EB;"),
                    tags$span("Temperatura promedio")
                  ),
                  tags$span(
                    class = "ios-toggle",
                    tags$input(type = "checkbox", id = "switch_clim_temp_prom", onchange = "handleSwitchToggle('switch_clim_temp_prom')"),
                    tags$span(class = "ios-slider")
                  )
                ),
                
                # 2. T. Mínima (Inactiva inicial)
                tags$label(
                  class = "ios-switch-row",
                  `for` = "switch_clim_temp_min",
                  tags$span(
                    class = "ios-switch-label",
                    tags$span(class = "swatch-dot", style = "background-color: #0284C7;"),
                    tags$span("T. Mínima")
                  ),
                  tags$span(
                    class = "ios-toggle",
                    tags$input(type = "checkbox", id = "switch_clim_temp_min", onchange = "handleSwitchToggle('switch_clim_temp_min')"),
                    tags$span(class = "ios-slider")
                  )
                ),
                
                # 3. T. Máxima (Inactiva inicial)
                tags$label(
                  class = "ios-switch-row",
                  `for` = "switch_clim_temp_max",
                  tags$span(
                    class = "ios-switch-label",
                    tags$span(class = "swatch-dot", style = "background-color: #DC2626;"),
                    tags$span("T. Máxima")
                  ),
                  tags$span(
                    class = "ios-toggle",
                    tags$input(type = "checkbox", id = "switch_clim_temp_max", onchange = "handleSwitchToggle('switch_clim_temp_max')"),
                    tags$span(class = "ios-slider")
                  )
                ),
                
                tags$div(class = "ios-popover-divider"),
                tags$div(class = "ios-popover-header", "Referencia de la Media"),
                
                # Switch Excluir Campaña
                tags$label(
                  class = "ios-switch-row",
                  `for` = "switch_clim_excl_camp",
                  tags$span(
                    class = "ios-switch-label",
                    tags$span("Excluir campaña de la media")
                  ),
                  tags$span(
                    class = "ios-toggle",
                    tags$input(type = "checkbox", id = "switch_clim_excl_camp", onchange = "handleSwitchToggle('switch_clim_excl_camp')"),
                    tags$span(class = "ios-slider")
                  )
                )
              )
            ),
            
            tags$div(class = "header-controls-divider"),
            
            # Switch Historial del módulo
            tags$label(
              class = "ios-switch-row-header",
              `for` = "switch_fenologia_historial",
              tags$span(class = "ios-switch-header-label", "Historial del módulo"),
              tags$span(
                class = "ios-toggle",
                tags$input(
                  type = "checkbox",
                  id = "switch_fenologia_historial",
                  onchange = "handleSwitchToggle('switch_fenologia_historial')"
                ),
                tags$span(class = "ios-slider")
              )
            ),
            tags$div(class = "header-controls-divider"),
            radioButtons(
              "cell_size_toggle",
              label = NULL,
              choices = c("Compacto" = "compact", "Estándar" = "standard", "Amplio" = "spacious"),
              selected = "standard",
              inline = TRUE
            )
          )
        ),
        uiOutput("legend_bar_ui"),
        uiOutput("gantt_container_ui"),
        uiOutput("fenologia_summary_table_ui")
      ),
      
      # ------------------------------------------------------------------------
      # 2. CURVAS DE ESCENARIOS Y PRODUCTIVIDAD + TABLA DE CAMPAÑAS (SEGUNDO)
      # ------------------------------------------------------------------------
      tags$div(
        class = "content-card",
        
        # Top Banner Alert matching Image 3 (Word 'Excel' removed)
        tags$div(
          class = "scenario-banner-alert",
          tags$div(
            tags$strong("Curva del presupuesto: "),
            tags$span("Distribución semanal de kilos calculada por variedad, semana de poda y fundo.")
          ),
          tags$span(style = "font-size: 11px; opacity: 0.8; font-weight: 600;", "Campaña 27/28")
        ),
        
        # Scenario Toolbar: Selección simultánea de años + Menú de 3 Puntos
        tags$div(
          class = "scenario-toolbar",
          
          # Left controls
          tags$div(
            class = "scenario-toolbar-left",
            
            # Tipo de Año (Ambos seleccionables simultáneamente, activos por defecto)
            tags$div(
              style = "display: flex; align-items: center; gap: 8px;",
              tags$span(style = "font-size: 11px; font-weight: 700; text-transform: uppercase; color: #64748B;", "Tipo:"),
              tags$div(
                class = "ios-pill-btn-group",
                actionButton("btn_year_neutral", "Año neutral", class = "ios-pill-btn active"),
                actionButton("btn_year_caliente", "Año caliente", class = "ios-pill-btn active")
              )
            ),
            
            # Unidad: Kg / Planta vs Kg Totales (Kg / Planta activo por defecto)
            tags$div(
              style = "display: flex; align-items: center; gap: 8px;",
              tags$span(style = "font-size: 11px; font-weight: 700; text-transform: uppercase; color: #64748B;", "Unidad:"),
              tags$div(
                class = "ios-seg-control",
                actionButton("btn_unit_kg_planta", "Kg / Planta", class = "ios-seg-btn active"),
                actionButton("btn_unit_kg_total", "Kg Totales", class = "ios-seg-btn")
              )
            ),
            
            # Vista: Semanal vs Acumulado (Semanal activo por defecto)
            tags$div(
              style = "display: flex; align-items: center; gap: 8px;",
              tags$span(style = "font-size: 11px; font-weight: 700; text-transform: uppercase; color: #64748B;", "Vista:"),
              tags$div(
                class = "ios-seg-control",
                actionButton("btn_metric_semanal", "Semanal", class = "ios-seg-btn active"),
                actionButton("btn_metric_acumulado", "Acumulado", class = "ios-seg-btn")
              )
            ),
            
            # Menú de 3 Puntos (⋯) con Switches iOS para Comparar Escenarios
            tags$div(
              class = "dots-menu-wrapper",
              tags$button(
                id = "btn_toggle_options",
                type = "button",
                class = "btn-dots-trigger",
                tags$span(class = "dots-icon", "⋯"),
                tags$span("Comparar"),
                tags$span(id = "active_scen_badge", class = "dots-badge", textOutput("active_scenarios_count", inline = TRUE))
              ),
              tags$div(
                id = "popover_options_menu",
                class = "ios-popover",
                tags$div(class = "ios-popover-header", "Escenarios a Comparar"),
                
                # Switch ESC. CAMPO (Único activo inicialmente)
                tags$label(
                  class = "ios-switch-row",
                  `for` = "switch_esc_campo",
                  tags$span(
                    class = "ios-switch-label",
                    tags$span(class = "swatch-dot", style = "background-color: #10B981;"),
                    tags$span("ESC. CAMPO")
                  ),
                  tags$span(
                    class = "ios-toggle",
                    tags$input(type = "checkbox", id = "switch_esc_campo", checked = "checked", onchange = "handleSwitchToggle('switch_esc_campo')"),
                    tags$span(class = "ios-slider")
                  )
                ),
                
                # Switch Presupuesto (Inactivo por defecto)
                tags$label(
                  class = "ios-switch-row",
                  `for` = "switch_presupuesto",
                  tags$span(
                    class = "ios-switch-label",
                    tags$span(class = "swatch-dot", style = "background-color: #F59E0B;"),
                    tags$span("Presupuesto")
                  ),
                  tags$span(
                    class = "ios-toggle",
                    tags$input(type = "checkbox", id = "switch_presupuesto", onchange = "handleSwitchToggle('switch_presupuesto')"),
                    tags$span(class = "ios-slider")
                  )
                ),
                
                tags$div(class = "ios-popover-divider"),
                tags$div(class = "ios-popover-header", "Campañas Históricas"),
                
                # Curvas históricas de los módulos representantes (ancladas por proximidad <= 3 sem)
                uiOutput("comparar_historical_switches_ui"),
                
                tags$div(class = "ios-popover-divider"),
                
                # Switch Wish List 27/28
                tags$label(
                  class = "ios-switch-row",
                  `for` = "switch_wish_list",
                  tags$span(
                    class = "ios-switch-label",
                    tags$span(class = "swatch-dot", style = "background-color: #1E293B;"),
                    tags$span("Wish List 27/28")
                  ),
                  tags$span(
                    class = "ios-toggle",
                    tags$input(type = "checkbox", id = "switch_wish_list", onchange = "handleSwitchToggle('switch_wish_list')"),
                    tags$span(class = "ios-slider")
                  )
                ),
                
                # Switch Mostrar Picos (Inactivo inicialmente)
                tags$label(
                  class = "ios-switch-row",
                  `for` = "switch_show_peak",
                  tags$span(
                    class = "ios-switch-label",
                    tags$span("Mostrar Picos")
                  ),
                  tags$span(
                    class = "ios-toggle",
                    tags$input(type = "checkbox", id = "switch_show_peak", onchange = "handleSwitchToggle('switch_show_peak')"),
                    tags$span(class = "ios-slider")
                  )
                )
              )
            )
          ),
          
          # Right controls: summary info & subtle notifications bell
          tags$div(
            class = "scenario-toolbar-right",
            uiOutput("active_scenarios_label_ui"),
            uiOutput("scenario_notifications_bell_ui")
          )
        ),
        
        # Graph & Side Campaign Yield Card
        tags$div(
          class = "productivity-layout",
          
          # Plotly Curve Chart (Left)
          tags$div(
            plotlyOutput("plot_escenarios_curva", height = "480px")
          ),
          
          # Right: Campaign Yield Table (Módulo - Variedad en filas, Campañas en columnas, solo 27/28 editable)
          tags$div(
            class = "campaign-side-card",
            tags$div(
              class = "campaign-side-header",
              tags$div(
                tags$h4(class = "campaign-side-title", "Rendimiento por Campaña (Kg/Planta)"),
                tags$p(class = "campaign-side-sub", "Solo la campaña 27/28 es editable. Los cambios recalculan la curva en tiempo real.")
              ),
              tags$div(
                style = "display: flex; align-items: center; gap: 8px;",
                tags$div(
                  style = "display: flex; align-items: center; gap: 4px;",
                  tags$label(style = "font-size: 11px; font-weight: 600; color: #475569; margin: 0;", "Escenario:"),
                  selectInput(
                    "scenario_edit_select",
                    label = NULL,
                    choices = c("ESC. CAMPO", "Presupuesto"),
                    selected = "ESC. CAMPO",
                    width = "140px"
                  )
                )
              )
            ),
            
            DTOutput("dt_campaign_yields"),
            uiOutput("campaign_table_footer_ui")
          )
        )
      )
    )
  )
)

# ------------------------------------------------------------------------------
# SERVER LOGIC
# ------------------------------------------------------------------------------

server <- function(input, output, session) {
  
  # --- State Flags & Applied Filters ---
  has_searched <- reactiveVal(FALSE)
  
  output$has_searched <- reactive({
    has_searched()
  })
  outputOptions(output, "has_searched", suspendWhenHidden = FALSE)
  
  applied_filters <- reactiveValues(
    modulo = "Todos",
    breeder = "Todos",
    variedad = "Todas"
  )
  
  # --- Active Scenarios (Inicialmente ESC. CAMPO + Campañas Históricas) ---
  active_scenarios <- reactiveVal(c("ESC. CAMPO"))
  active_historical_campaigns <- reactiveVal(c("2024/2025", "2025/2026", "2026/2027"))
  opt_show_peak <- reactiveVal(FALSE)
  opt_wish_list <- reactiveVal(FALSE)
  curve_unit_mode <- reactiveVal("kg_planta")
  curve_view_mode <- reactiveVal("semanal")
  
  # Count badge in 3-dots trigger button
  output$active_scenarios_count <- renderText({
    cnt <- length(active_scenarios())
    if (isTRUE(opt_wish_list())) cnt <- cnt + 1
    cnt <- cnt + length(active_historical_campaigns())
    as.character(cnt)
  })
  
  output$active_scenarios_label_ui <- renderUI({
    scens <- active_scenarios()
    hist_camps <- active_historical_campaigns()
    y_types <- selected_year_types()
    
    if (length(scens) == 0 && length(hist_camps) == 0) {
      return(tags$span(style = "font-size: 11.5px; color: #94A3B8;", "Ningún escenario activo"))
    }
    
    scen_tags <- lapply(scens, function(s) {
      clr <- SCENARIO_COLORS[s]
      if (is.na(clr)) clr <- "#475569"
      tags$span(
        style = sprintf("background: %s18; color: %s; border: 1px solid %s40; padding: 2px 8px; border-radius: 9999px; font-size: 11px; font-weight: 700;", clr, clr, clr),
        s
      )
    })
    
    hist_tags <- lapply(hist_camps, function(h) {
      clr <- HIST_CAMPAIGN_COLORS[h]
      if (is.na(clr)) clr <- "#8B5CF6"
      tags$span(
        style = sprintf("background: %s18; color: %s; border: 1px solid %s40; padding: 2px 8px; border-radius: 9999px; font-size: 11px; font-weight: 700;", clr, clr, clr),
        h
      )
    })
    
    year_tag <- tags$span(
      style = "background: #F1F5F9; color: #0F172A; border: 1px solid #CBD5E1; padding: 2px 8px; border-radius: 9999px; font-size: 11px; font-weight: 600;",
      paste(gsub("Año ", "", y_types), collapse = " + ")
    )
    
    tags$div(
      style = "display: flex; align-items: center; gap: 6px;",
      tags$span(style = "font-size: 11px; font-weight: 600; color: #64748B;", "Activo:"),
      scen_tags,
      hist_tags,
      year_tag
    )
  })
  
  # Subtle Notification Bell (Proyección de Curvas Referenciales)
  output$scenario_notifications_bell_ui <- renderUI({
    req(has_searched())
    req(scenario_results())
    res <- scenario_results()
    notifs <- res$notifications
    
    has_notifs <- !is.null(notifs) && nrow(notifs) > 0
    n_count <- if (has_notifs) nrow(notifs) else 0
    
    badge_tag <- if (has_notifs) {
      tags$span(class = "bell-badge-count", as.character(n_count))
    } else {
      NULL
    }
    
    btn_class <- if (has_notifs) "btn-bell-trigger has-notices" else "btn-bell-trigger"
    
    popover_content <- if (has_notifs) {
      items <- lapply(seq_len(nrow(notifs)), function(i) {
        row <- notifs[i, ]
        tags$div(
          class = "notif-item",
          tags$div(
            class = "notif-item-header",
            tags$span(class = "notif-item-title", sprintf("%s · %s", row$Variedad, row$MóduloStr)),
            tags$span(class = "notif-item-tag", sprintf("±%d sem", row$Dist_Sem))
          ),
          tags$div(
            class = "notif-item-body",
            sprintf("Poda real Sem %d sin curva directa en catálogo. Asignada curva de Sem %d.", row$Sem_Poda, row$Sem_Poda_Curva)
          ),
          tags$div(
            class = "notif-item-vol",
            sprintf("%s kg protegidos al 100%%", format(round(row$Total_Kg), big.mark = ","))
          )
        )
      })
      
      tagList(
        tags$div(
          class = "notif-popover-header",
          tags$div(
            style = "display: flex; align-items: center; justify-content: space-between;",
            tags$span(class = "notif-popover-title", "Avisos de Proyección"),
            tags$span(class = "notif-popover-badge", sprintf("%d asignaciones", n_count))
          ),
          tags$p(class = "notif-popover-sub", "Curvas porcentuales referenciales por proximidad fenológica")
        ),
        tags$div(
          class = "notif-list-container",
          items
        ),
        tags$div(
          class = "notif-popover-footer",
          tags$span(class = "notif-footer-check", "✓"),
          tags$span("El 100% del volumen proyectado ha sido distribuido.")
        )
      )
    } else {
      tagList(
        tags$div(
          class = "notif-popover-header",
          tags$span(class = "notif-popover-title", "Avisos de Proyección"),
          tags$p(class = "notif-popover-sub", "Estado del catálogo de curvas")
        ),
        tags$div(
          style = "padding: 16px 8px; text-align: center; color: #64748B; font-size: 11.5px;",
          tags$div(style = "font-size: 18px; color: #10B981; margin-bottom: 4px;", "✓"),
          tags$strong(style = "color: #0F172A; display: block; margin-bottom: 3px;", "Coincidencia Exacta al 100%"),
          "Todas las semanas de poda coinciden con curvas calibradas en el catálogo."
        )
      )
    }
    
    tags$div(
      class = "bell-menu-wrapper",
      tags$button(
        id = "btn_toggle_notifications",
        type = "button",
        class = btn_class,
        title = if (has_notifs) sprintf("%d curvas referenciales asignadas", n_count) else "Sin avisos de proyección",
        icon("bell"),
        badge_tag
      ),
      tags$div(
        id = "popover_notifications_menu",
        class = "notifications-popover",
        popover_content
      )
    )
  })
  
  # Observers for switches in 3-dots popover
  observeEvent(input$switch_esc_campo, {
    scens <- active_scenarios()
    if (isTRUE(input$switch_esc_campo)) {
      active_scenarios(unique(c(scens, "ESC. CAMPO")))
    } else {
      active_scenarios(setdiff(scens, "ESC. CAMPO"))
    }
  })
  
  observeEvent(input$switch_presupuesto, {
    scens <- active_scenarios()
    if (isTRUE(input$switch_presupuesto)) {
      active_scenarios(unique(c(scens, "Presupuesto")))
    } else {
      active_scenarios(setdiff(scens, "Presupuesto"))
    }
  })
  
  # Dedicated observer for Dynamic Historical Campaign Switches
  # Avoids nested reactive re-renders and ping-pong loops
  observeEvent(input$historical_campaign_toggled, {
    info <- input$historical_campaign_toggled
    req(info, info$campaign)
    curr <- active_historical_campaigns()
    if (isTRUE(info$active)) {
      active_historical_campaigns(unique(c(curr, info$campaign)))
    } else {
      active_historical_campaigns(setdiff(curr, info$campaign))
    }
  })
  
  # Dynamic UI for Historical Campaign Switches in Comparar Popover
  output$comparar_historical_switches_ui <- renderUI({
    req(has_searched())
    res <- fenologia_comparativa_res()
    reps <- res$representative_units
    if (is.null(reps) || nrow(reps) == 0) {
      return(tags$div(style = "padding: 6px 14px; font-size: 11px; color: #94A3B8;", "Selecciona un módulo para comparar campañas"))
    }
    
    # Filter for historical years (YearCamp < 2027)
    hist_reps <- reps %>% filter(YearCamp < 2027)
    if (nrow(hist_reps) == 0) {
      return(tags$div(style = "padding: 6px 14px; font-size: 11px; color: #94A3B8;", "Sin campañas dentro de ±3 semanas"))
    }
    
    # ISOLATE active_historical_campaigns so user toggle actions do NOT destroy
    # and re-create this UI container in a feedback loop!
    active_camps <- isolate(active_historical_campaigns())
    all_reps_camps <- unique(sprintf("%d/%d", hist_reps$YearCamp, hist_reps$YearCamp + 1))
    if (length(active_camps) == 0) {
      active_camps <- all_reps_camps
    }
    
    switches <- lapply(seq_len(nrow(hist_reps)), function(i) {
      row <- hist_reps[i, ]
      camp_lbl <- sprintf("%d/%d", row$YearCamp, row$YearCamp + 1)
      sw_id <- paste0("switch_hist_", row$YearCamp)
      clr <- HIST_CAMPAIGN_COLORS[camp_lbl]
      if (is.na(clr)) clr <- "#8B5CF6"
      is_checked <- camp_lbl %in% active_camps
      
      tags$label(
        class = "ios-switch-row",
        `for` = sw_id,
        tags$span(
          class = "ios-switch-label",
          tags$span(class = "swatch-dot", style = sprintf("background-color: %s;", clr)),
          tags$span(camp_lbl),
          tags$span(style = "font-size: 10px; color: #64748B; margin-left: 4px;", sprintf("(%s)", row$Modulo))
        ),
        tags$span(
          class = "ios-toggle",
          tags$input(
            type = "checkbox",
            id = sw_id,
            checked = if (is_checked) "checked" else NULL,
            onchange = sprintf("handleHistoricalSwitchToggle('%s', '%s')", sw_id, camp_lbl)
          ),
          tags$span(class = "ios-slider")
        )
      )
    })
    
    tagList(switches)
  })
  
  observeEvent(input$switch_wish_list, {
    opt_wish_list(isTRUE(input$switch_wish_list))
  })
  
  observeEvent(input$switch_show_peak, {
    opt_show_peak(isTRUE(input$switch_show_peak))
  })
  
  # Unit Segmented Controls (Kg / Planta vs Kg Totales)
  observeEvent(input$btn_unit_kg_planta, {
    curve_unit_mode("kg_planta")
    session$sendCustomMessage("active_unit_changed", "kg_planta")
    session$sendCustomMessage("show_toast", list(message = "Métrica: Rendimiento (Kg / Planta)", icon = ""))
  })
  
  observeEvent(input$btn_unit_kg_total, {
    curve_unit_mode("kg_total")
    session$sendCustomMessage("active_unit_changed", "kg_total")
    session$sendCustomMessage("show_toast", list(message = "Métrica: Volumen Total (Kg)", icon = ""))
  })
  
  # View Segmented Controls (Semanal vs Acumulado)
  observeEvent(input$btn_metric_semanal, {
    curve_view_mode("semanal")
    session$sendCustomMessage("active_view_changed", "semanal")
  })
  
  observeEvent(input$btn_metric_acumulado, {
    curve_view_mode("acumulado")
    session$sendCustomMessage("active_view_changed", "acumulado")
  })
  
  # Auto-activar campañas históricas representativas al actualizar consulta
  observeEvent(fenologia_comparativa_res(), {
    res <- fenologia_comparativa_res()
    if (!is.null(res) && !is.null(res$representative_units) && nrow(res$representative_units) > 0) {
      hist_reps <- res$representative_units %>% filter(YearCamp < 2027)
      if (nrow(hist_reps) > 0) {
        camps <- unique(sprintf("%d/%d", hist_reps$YearCamp, hist_reps$YearCamp + 1))
        active_historical_campaigns(camps)
      } else {
        active_historical_campaigns(character(0))
      }
    } else {
      active_historical_campaigns(character(0))
    }
  })
  
  # --- Active Year Type Reactive State: AMBOS ACTIVOS DESDE UN INICIO ---
  selected_year_types <- reactiveVal(c("Año neutral", "Año caliente"))
  
  observeEvent(input$btn_year_neutral, {
    curr <- selected_year_types()
    if ("Año neutral" %in% curr) {
      if (length(curr) > 1) {
        selected_year_types(setdiff(curr, "Año neutral"))
      }
    } else {
      selected_year_types(unique(c(curr, "Año neutral")))
    }
    session$sendCustomMessage("active_year_types_changed", selected_year_types())
    session$sendCustomMessage("show_toast", list(
      message = paste0("Tipo de año: ", paste(gsub("Año ", "", selected_year_types()), collapse = " + ")),
      icon = ""
    ))
  })
  
  observeEvent(input$btn_year_caliente, {
    curr <- selected_year_types()
    if ("Año caliente" %in% curr) {
      if (length(curr) > 1) {
        selected_year_types(setdiff(curr, "Año caliente"))
      }
    } else {
      selected_year_types(unique(c(curr, "Año caliente")))
    }
    session$sendCustomMessage("active_year_types_changed", selected_year_types())
    session$sendCustomMessage("show_toast", list(
      message = paste0("Tipo de año: ", paste(gsub("Año ", "", selected_year_types()), collapse = " + ")),
      icon = ""
    ))
  })
  
  # --- Data Stores ---
  master_fenologia <- reactiveVal(NULL)
  climate_engine_store <- reactiveVal(NULL)
  historical_yields_store <- reactiveVal(NULL)
  historical_cosecha_store <- reactiveVal(NULL)
  scenario_engine_store <- reactiveVal(NULL)
  original_scenario_store <- reactiveVal(NULL)
  yield_edit_history <- reactiveVal(list())
  
  highlighted_stage <- reactiveVal("Todos")
  
  # --- Initial Data Load (Background cache, without heavy calculations) ---
  observe({
    raw_fen <- load_fenologia_excel(EXCEL_FENOLOGIA_PATH)
    if (!is.null(raw_fen)) {
      master_fenologia(raw_fen)
    }
    
    c_eng <- load_climate_engine()
    if (!is.null(c_eng)) {
      climate_engine_store(c_eng)
    }
    
    raw_hist <- load_historical_yields(PATH_HISTORICAL_YIELDS)
    if (!is.null(raw_hist)) {
      historical_yields_store(raw_hist)
    }
    
    raw_cos <- load_historical_cosecha()
    if (!is.null(raw_cos)) {
      historical_cosecha_store(raw_cos)
    }
    
    raw_esc <- load_escenarios_excel(EXCEL_ESCENARIOS_PATH)
    if (!is.null(raw_esc)) {
      scenario_engine_store(raw_esc)
      original_scenario_store(raw_esc)
      
      df_d <- raw_esc$datos
      modulos <- c("Todos", sort(unique(df_d$MóduloStr)))
      breeders <- c("Todos", sort(unique(df_d$Breeder[nzchar(df_d$Breeder)])))
      variedades <- c("Todas", sort(unique(df_d$Variedad)))
      
      updateSelectInput(session, "filtro_modulo", choices = modulos, selected = "Todos")
      updateSelectInput(session, "filtro_breeder", choices = breeders, selected = "Todos")
      updateSelectInput(session, "filtro_variedad", choices = variedades, selected = "Todas")
    }
  })
  
  # ----------------------------------------------------------------------------
  # CASCADING HIERARCHICAL FILTERS (Jerarquía Estricta: Breeder -> Módulo -> Variedad)
  # ----------------------------------------------------------------------------
  
  # Nivel 1: Cambio en BREEDER -> actualiza Módulo y Variedad
  observeEvent(input$filtro_breeder, {
    raw_esc <- scenario_engine_store()
    req(raw_esc)
    df_d <- raw_esc$datos
    
    sel_br <- input$filtro_breeder
    cur_mod <- isolate(input$filtro_modulo)
    cur_var <- isolate(input$filtro_variedad)
    
    if (!is.null(sel_br) && sel_br != "Todos") {
      df_br <- df_d %>% filter(toupper(trimws(Breeder)) == toupper(trimws(sel_br)))
      avail_mods <- c("Todos", sort(unique(df_br$MóduloStr)))
      
      new_sel_mod <- if (!is.null(cur_mod) && cur_mod %in% avail_mods) cur_mod else "Todos"
      
      if (new_sel_mod != "Todos") {
        df_mod <- df_br %>% filter(MóduloStr == new_sel_mod)
        mod_vars <- sort(unique(df_mod$Variedad))
        mod_vars <- mod_vars[nzchar(trimws(mod_vars))]
        new_sel_var <- if (!is.null(cur_var) && cur_var %in% mod_vars && cur_var != "Todas") cur_var else if (length(mod_vars) > 0) mod_vars[1] else "Todas"
        avail_vars <- if (length(mod_vars) > 0) mod_vars else "Todas"
      } else {
        all_br_vars <- sort(unique(df_br$Variedad))
        all_br_vars <- all_br_vars[nzchar(trimws(all_br_vars))]
        avail_vars <- c("Todas", all_br_vars)
        new_sel_var <- if (!is.null(cur_var) && cur_var %in% avail_vars) cur_var else "Todas"
      }
      
      updateSelectInput(session, "filtro_modulo", choices = avail_mods, selected = new_sel_mod)
      updateSelectInput(session, "filtro_variedad", choices = avail_vars, selected = new_sel_var)
    } else {
      # Si Breeder vuelve a "Todos", restablecer opciones completas
      all_mods <- c("Todos", sort(unique(df_d$MóduloStr)))
      new_sel_mod <- if (!is.null(cur_mod) && cur_mod %in% all_mods) cur_mod else "Todos"
      
      if (new_sel_mod != "Todos") {
        df_mod <- df_d %>% filter(MóduloStr == new_sel_mod)
        mod_vars <- sort(unique(df_mod$Variedad))
        mod_vars <- mod_vars[nzchar(trimws(mod_vars))]
        new_sel_var <- if (!is.null(cur_var) && cur_var %in% mod_vars && cur_var != "Todas") cur_var else if (length(mod_vars) > 0) mod_vars[1] else "Todas"
        avail_vars <- if (length(mod_vars) > 0) mod_vars else "Todas"
      } else {
        all_vars <- sort(unique(df_d$Variedad))
        all_vars <- all_vars[nzchar(trimws(all_vars))]
        avail_vars <- c("Todas", all_vars)
        new_sel_var <- if (!is.null(cur_var) && cur_var %in% avail_vars) cur_var else "Todas"
      }
      
      updateSelectInput(session, "filtro_modulo", choices = all_mods, selected = new_sel_mod)
      updateSelectInput(session, "filtro_variedad", choices = avail_vars, selected = new_sel_var)
    }
  }, ignoreInit = TRUE)
  
  # Nivel 2: Cambio en MÓDULO -> actualiza Variedad (respetando el Breeder actual)
  observeEvent(input$filtro_modulo, {
    raw_esc <- scenario_engine_store()
    req(raw_esc)
    df_d <- raw_esc$datos
    
    sel_mod <- input$filtro_modulo
    sel_br <- isolate(input$filtro_breeder)
    cur_var <- isolate(input$filtro_variedad)
    
    df_sub <- df_d
    if (!is.null(sel_br) && sel_br != "Todos") {
      df_sub <- df_sub %>% filter(toupper(trimws(Breeder)) == toupper(trimws(sel_br)))
    }
    
    if (!is.null(sel_mod) && sel_mod != "Todos") {
      df_sub <- df_sub %>% filter(MóduloStr == sel_mod)
      
      mod_vars <- sort(unique(df_sub$Variedad))
      mod_vars <- mod_vars[nzchar(trimws(mod_vars))]
      
      # Si por discrepancia de datos no hay variedad en df_sub, buscar en fenología
      if (length(mod_vars) == 0) {
        df_fen <- fenologia_store()
        if (!is.null(df_fen)) {
          fen_sub <- df_fen %>% filter(Modulo == sel_mod)
          if (nrow(fen_sub) > 0) {
            latest_y <- max(fen_sub$YearCamp, na.rm = TRUE)
            mod_vars <- fen_sub %>% filter(YearCamp == latest_y) %>% pull(Variedad) %>% unique()
            mod_vars <- sort(mod_vars[nzchar(trimws(mod_vars))])
          }
        }
      }
      
      # Regla: Al seleccionar un módulo, se selecciona automáticamente su primera variedad
      new_sel_var <- if (!is.null(cur_var) && cur_var %in% mod_vars && cur_var != "Todas") {
        cur_var
      } else if (length(mod_vars) > 0) {
        mod_vars[1]
      } else {
        "Todas"
      }
      
      avail_vars <- if (length(mod_vars) > 0) mod_vars else "Todas"
    } else {
      # Módulo es "Todos"
      all_vars <- sort(unique(df_sub$Variedad))
      all_vars <- all_vars[nzchar(trimws(all_vars))]
      avail_vars <- c("Todas", all_vars)
      new_sel_var <- if (!is.null(cur_var) && cur_var %in% avail_vars) cur_var else "Todas"
    }
    
    updateSelectInput(session, "filtro_variedad", choices = avail_vars, selected = new_sel_var)
  }, ignoreInit = TRUE)
  
  # ----------------------------------------------------------------------------
  # BOTÓN BUSCAR & ACCIONES DE FILTRO
  # ----------------------------------------------------------------------------
  
  observeEvent(input$btn_buscar, {
    applied_filters$modulo <- input$filtro_modulo
    applied_filters$breeder <- input$filtro_breeder
    applied_filters$variedad <- input$filtro_variedad
    
    has_searched(TRUE)
    
    session$sendCustomMessage("show_toast", list(
      message = sprintf(
        "Consulta aplicada: %s | %s | %s",
        applied_filters$modulo, applied_filters$breeder, applied_filters$variedad
      ),
      icon = ""
    ))
  })
  
  observeEvent(input$btn_filter_clear, {
    raw_esc <- scenario_engine_store()
    if (!is.null(raw_esc)) {
      df_d <- raw_esc$datos
      updateSelectInput(session, "filtro_modulo", choices = c("Todos", sort(unique(df_d$MóduloStr))), selected = "Todos")
      updateSelectInput(session, "filtro_breeder", choices = c("Todos", sort(unique(df_d$Breeder[nzchar(df_d$Breeder)]))), selected = "Todos")
      updateSelectInput(session, "filtro_variedad", choices = c("Todas", sort(unique(df_d$Variedad))), selected = "Todas")
    }
    
    has_searched(FALSE)
    session$sendCustomMessage("show_toast", list(
      message = "Filtros restablecidos.",
      icon = ""
    ))
  })
  
  # Atajos Rápidos del Empty State (sin emojis de IA)
  observeEvent(input$btn_quick_sekoya, {
    updateSelectInput(session, "filtro_modulo", selected = "Todos")
    updateSelectInput(session, "filtro_breeder", selected = "NORTH BAY")
    updateSelectInput(session, "filtro_variedad", selected = "Sekoya Pop")
    applied_filters$modulo <- "Todos"
    applied_filters$breeder <- "NORTH BAY"
    applied_filters$variedad <- "Sekoya Pop"
    has_searched(TRUE)
  })
  
  observeEvent(input$btn_quick_imperial, {
    updateSelectInput(session, "filtro_modulo", selected = "Todos")
    updateSelectInput(session, "filtro_breeder", selected = "HORTIFRUT")
    updateSelectInput(session, "filtro_variedad", selected = "Imperial")
    applied_filters$modulo <- "Todos"
    applied_filters$breeder <- "HORTIFRUT"
    applied_filters$variedad <- "Imperial"
    has_searched(TRUE)
  })
  
  observeEvent(input$btn_quick_biloxi, {
    updateSelectInput(session, "filtro_modulo", selected = "Todos")
    updateSelectInput(session, "filtro_breeder", selected = "LIBRE")
    updateSelectInput(session, "filtro_variedad", selected = "Biloxi")
    applied_filters$modulo <- "Todos"
    applied_filters$breeder <- "LIBRE"
    applied_filters$variedad <- "Biloxi"
    has_searched(TRUE)
  })
  
  observeEvent(input$btn_quick_todos, {
    updateSelectInput(session, "filtro_modulo", selected = "Todos")
    updateSelectInput(session, "filtro_breeder", selected = "Todos")
    updateSelectInput(session, "filtro_variedad", selected = "Todas")
    applied_filters$modulo <- "Todos"
    applied_filters$breeder <- "Todos"
    applied_filters$variedad <- "Todas"
    has_searched(TRUE)
  })
  
  # Summary Pill in Filter Card
  output$filter_stat_pill_ui <- renderUI({
    if (!has_searched()) {
      return(tags$div(
        class = "stat-pill-badge",
        style = "color: #94A3B8;",
        "Esperando consulta..."
      ))
    }
    
    req(scenario_results())
    res <- scenario_results()
    summ <- res$summary
    
    total_vol <- sum(summ$Total_Kg, na.rm = TRUE)
    total_ha <- ifelse(nrow(summ) > 0, summ$Total_Ha[1], 0)
    
    tags$div(
      class = "stat-pill-badge",
      sprintf("Superficie: %.1f ha • Volumen: %s kg", total_ha, format(round(total_vol), big.mark = ","))
    )
  })
  
  # ----------------------------------------------------------------------------
  # SECTION 1: CALENDARIO FENOLÓGICO
  # ----------------------------------------------------------------------------
  
  fenologia_comparativa_res <- reactive({
    req(has_searched())
    req(master_fenologia())
    
    df <- master_fenologia()
    mod <- applied_filters$modulo
    br <- applied_filters$breeder
    var <- applied_filters$variedad
    is_hist <- isTRUE(input$switch_fenologia_historial)
    
    filter_fenologia_comparativa(
      df_records = df,
      selected_modulo = mod,
      selected_breeder = br,
      selected_variedad = var,
      is_historial = is_hist
    )
  })
  
  filtered_fenologia <- reactive({
    res <- fenologia_comparativa_res()
    res$data
  })
  
  output$fenologia_header_title <- renderText({
    is_hist <- isTRUE(input$switch_fenologia_historial)
    if (is_hist) {
      "Calendario Fenológico Histórico (Historial Completo)"
    } else {
      "Calendario Fenológico (Comparativa Última Campaña y Podas Similares)"
    }
  })
  
  output$fenologia_header_subtitle_ui <- renderUI({
    is_hist <- isTRUE(input$switch_fenologia_historial)
    res <- fenologia_comparativa_res()
    mod <- applied_filters$modulo
    var <- applied_filters$variedad
    
    if (is_hist) {
      txt <- if (!is.null(mod) && mod != "Todos") {
        sprintf("Historial del módulo %s: Visualizando todas las campañas históricas registradas.", mod)
      } else {
        "Historial completo: Visualizando todas las campañas históricas registradas."
      }
      tags$p(class = "brand-subtitle", txt)
    } else {
      if (res$mode == "comparative_target") {
        txt <- sprintf(
          "Comparativa anual (≥ 2024): 1 módulo por año priorizando %s y podas cercanas a %s (Semana %d).",
          mod, res$target_unit, res$target_sem
        )
        tags$p(class = "brand-subtitle", txt)
      } else if (res$mode == "latest_per_module") {
        txt <- sprintf("Última campaña registrada (≥ 2024) para cada módulo con variedad %s.", var)
        tags$p(class = "brand-subtitle", txt)
      } else {
        tags$p(class = "brand-subtitle", "Visualización de la última campaña registrada (≥ 2024) para cada módulo.")
      }
    }
  })
  
  gantt_matrix_data <- reactive({
    req(has_searched())
    req(filtered_fenologia())
    df <- filtered_fenologia()
    build_gantt_matrix(df, selected_modulo = "Todos", selected_breeder = "Todos", selected_variedad = "Todas")
  })
  
  stage_counts <- reactive({
    req(has_searched())
    req(filtered_fenologia())
    table(filtered_fenologia()$Stage)
  })
  
  # Climate variables reactive selection via iOS switches (solo temperatura prom, minima y maxima)
  active_climate_vars <- reactive({
    vars <- c()
    if (isTRUE(input$switch_clim_temp_prom)) {
      vars <- c(vars, "Temperatura promedio")
    }
    if (isTRUE(input$switch_clim_temp_min)) {
      vars <- c(vars, "Temperatura Mínima (°C)")
    }
    if (isTRUE(input$switch_clim_temp_max)) {
      vars <- c(vars, "Temperatura Máxima (°C)")
    }
    vars
  })
  
  output$active_climate_count <- renderText({
    as.character(length(active_climate_vars()))
  })
  
  ref_media_selected <- reactive({
    if (isTRUE(input$switch_clim_excl_camp)) "excl" else "all"
  })
  
  output$legend_bar_ui <- renderUI({
    req(has_searched())
    req(stage_counts())
    counts <- stage_counts()
    
    chips <- lapply(names(STAGE_COLORS), function(st) {
      cnt <- as.numeric(counts[st])
      if (is.na(cnt)) cnt <- 0
      color <- STAGE_COLORS[st]
      is_active <- highlighted_stage() == st
      
      tags$div(
        class = paste0("legend-chip", ifelse(is_active, " active", "")),
        onclick = sprintf("highlightStage('%s')", ifelse(is_active, "Todos", st)),
        tags$span(class = "legend-swatch", style = sprintf("background-color: %s;", color)),
        tags$span(st),
        tags$span(class = "legend-count", sprintf("(%d)", cnt))
      )
    })
    
    all_chip <- tags$div(
      class = paste0("legend-chip", ifelse(highlighted_stage() == "Todos", " active", "")),
      onclick = "highlightStage('Todos')",
      tags$span("Ver Todas")
    )
    
    pheno_legend <- tags$div(
      class = "legend-bar",
      tags$span(class = "legend-title", "Etapas:"),
      all_chip,
      chips
    )
    
    # Climate anomaly gradient scale box (Matching User Mockup)
    climate_legend <- if (length(active_climate_vars()) > 0) {
      tags$div(
        class = "climate-legend-box",
        tags$span(class = "climate-legend-title", "Anomalía de temperatura (ΔT, °C):"),
        tags$div(
          class = "climate-bar-wrapper",
          tags$div(class = "climate-color-bar"),
          tags$div(
            class = "climate-bar-ticks",
            tags$span("≤ -2.0"),
            tags$span("-1.0"),
            tags$span("-0.5"),
            tags$span("±0.0"),
            tags$span("+0.5"),
            tags$span("+1.0"),
            tags$span("≥ +2.0")
          )
        )
      )
    } else {
      NULL
    }
    
    tags$div(
      class = "legend-wrapper-flex",
      pheno_legend,
      climate_legend
    )
  })
  
  output$gantt_container_ui <- renderUI({
    req(has_searched())
    req(gantt_matrix_data())
    mat <- gantt_matrix_data()
    res <- fenologia_comparativa_res()
    
    generate_gantt_html(
      mat_long = mat,
      highlighted_stage = highlighted_stage(),
      cell_size = input$cell_size_toggle,
      readonly = TRUE,
      target_unit = res$target_unit,
      climate_engine = climate_engine_store(),
      active_climate_vars = active_climate_vars(),
      ref_media = ref_media_selected()
    )
  })
  
  # Resumen de Etapas Fenológicas por Campaña (Duración en semanas y fecha mínima para Poda)
  output$fenologia_summary_table_ui <- renderUI({
    req(has_searched())
    req(filtered_fenologia())
    res <- fenologia_comparativa_res()
    df_data <- filtered_fenologia()
    target_unit <- res$target_unit
    
    generate_fenologia_summary_html(df_data, target_unit)
  })
  
  observeEvent(input$legend_stage_selected, {
    highlighted_stage(input$legend_stage_selected)
  })
  
  # ----------------------------------------------------------------------------
  # SECTION 2: SCENARIO PRODUCTION CURVES & CAMPAIGN YIELD TABLE
  # ----------------------------------------------------------------------------
  
  scenario_results <- reactive({
    req(has_searched())
    req(scenario_engine_store())
    eng <- scenario_engine_store()
    
    mod <- applied_filters$modulo
    br <- applied_filters$breeder
    var <- applied_filters$variedad
    t_anios <- selected_year_types()
    scens <- active_scenarios()
    
    calculate_scenario_weekly_curves(
      engine_data = eng,
      selected_tipo_anio = t_anios,
      selected_modulo = mod,
      selected_breeder = br,
      selected_variedad = var,
      selected_escenarios = scens
    )
  })
  
  # Plotly Curve Chart (Dual Year Type Comparison + Scenario Curves)
  output$plot_escenarios_curva <- renderPlotly({
    req(has_searched())
    req(scenario_results())
    res <- scenario_results()
    curves_df <- res$curves
    
    if (nrow(curves_df) == 0 || length(active_scenarios()) == 0) {
      return(plot_ly() %>% layout(
        title = list(
          text = "Selecciona al menos un escenario en el menú ⋯ Comparar para visualizar curvas",
          font = list(family = "Inter, sans-serif", size = 13, color = "#64748B")
        ),
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE)
      ))
    }
    
    is_kg_planta <- (curve_unit_mode() == "kg_planta")
    is_acumulado <- (curve_view_mode() == "acumulado")
    
    metric_col <- if (is_kg_planta) {
      if (is_acumulado) "Kg_Planta_Acumulado" else "Kg_Planta_Semanal"
    } else {
      if (is_acumulado) "Kg_Acumulado" else "Kg_Semanal"
    }
    
    y_title <- if (is_kg_planta) {
      if (is_acumulado) "Kg / Planta Acumulado" else "Kg / Planta Semanal"
    } else {
      if (is_acumulado) "Kg Acumulado" else "Kg Semanal"
    }
    
    # Identify unique curves to plot (CurveLabel is e.g. 'ESC. CAMPO (neutral)' vs 'ESC. CAMPO (caliente)')
    curve_labels <- unique(curves_df$CurveLabel)
    multi_year <- length(unique(curves_df$Tipo_Anio)) > 1
    
    p <- plot_ly()
    
    for (cl in curve_labels) {
      sub_esc <- curves_df %>% filter(CurveLabel == cl) %>% arrange(TimelineIndex)
      base_scen <- sub_esc$Escenario[1]
      tipo <- sub_esc$Tipo_Anio[1]
      
      base_clr <- SCENARIO_COLORS[base_scen]
      if (is.na(base_clr)) base_clr <- "#64748B"
      
      # Distinctive styling for Caliente vs Neutral when both are selected
      is_caliente <- (tipo == "Año caliente")
      line_color <- if (multi_year && is_caliente) "#EA580C" else base_clr
      line_dash <- if (multi_year && is_caliente) "dash" else "solid"
      
      show_pks <- isTRUE(opt_show_peak())
      
      y_plt <- sub_esc$Kg_Planta_Prom[1]
      yield_suffix <- if (!is.null(y_plt) && !is.na(y_plt) && y_plt > 0) sprintf(" (%.2f kg/plt)", y_plt) else ""
      cl_display <- paste0(cl, yield_suffix)
      
      custom_data_col <- if (is_kg_planta) {
        if (is_acumulado) sub_esc$Kg_Acumulado else sub_esc$Kg_Semanal
      } else {
        if (is_acumulado) sub_esc$Kg_Planta_Acumulado else sub_esc$Kg_Planta_Semanal
      }
      
      hover_tmpl <- if (is_kg_planta) {
        paste0(
          "<b>", cl_display, "</b><br>",
          "%{text}<br>",
          if (is_acumulado) "Rendimiento acum.: <b>%{y:.2f} kg/plt</b>" else "Rendimiento: <b>%{y:.3f} kg/plt</b>",
          "<br><span style='color:#94A3B8;'>Volumen: %{customdata:,.0f} kg</span><extra></extra>"
        )
      } else {
        paste0(
          "<b>", cl_display, "</b><br>",
          "%{text}<br>",
          if (is_acumulado) "Volumen acum.: <b>%{y:,.0f} kg</b>" else "Volumen: <b>%{y:,.0f} kg</b>",
          "<br><span style='color:#94A3B8;'>Rendimiento: %{customdata:.2f} kg/plt</span><extra></extra>"
        )
      }
      
      p <- p %>% add_trace(
        data = sub_esc,
        x = ~TimelineIndex,
        y = as.formula(paste0("~", metric_col)),
        customdata = custom_data_col,
        name = cl_display,
        type = "scatter",
        mode = ifelse(show_pks, "lines+markers", "lines"),
        marker = if (show_pks) list(size = 4, color = line_color) else NULL,
        line = list(
          color = line_color,
          width = 2.8,
          dash = line_dash,
          shape = "spline",
          smoothing = 1.05
        ),
        hovertemplate = hover_tmpl,
        text = ~WeekLabel
      )
      
      # Peak annotations for each curve
      if (show_pks) {
        pk_idx <- which.max(sub_esc[[metric_col]])
        if (length(pk_idx) > 0 && !is.na(pk_idx)) {
          pk_row <- sub_esc[pk_idx, ]
          if (nrow(pk_row) > 0) {
            pk_val_txt <- if (is_kg_planta) {
              sprintf("%.2f kg/plt", pk_row[[metric_col]])
            } else {
              sprintf("%s kg", format(round(pk_row[[metric_col]]), big.mark = ","))
            }
            p <- p %>% add_annotations(
              x = pk_row$TimelineIndex,
              y = pk_row[[metric_col]],
              text = sprintf("<b>%s: %s (Sem %d)</b>", cl, pk_val_txt, pk_row$CalWeek),
              showarrow = TRUE,
              arrowhead = 2,
              arrowsize = 0.8,
              arrowcolor = line_color,
              ax = 0,
              ay = if (is_caliente) -38 else -22,
              font = list(family = "Inter, sans-serif", size = 10, color = "#0F172A"),
              bgcolor = "rgba(255, 255, 255, 0.92)",
              bordercolor = line_color,
              borderwidth = 1,
              borderpad = 3
            )
          }
        }
      }
    }
    
    # 2. Add Active Historical Campaign Curves (2024/2025, 2025/2026, 2026/2027)
    hist_camps <- active_historical_campaigns()
    all_max_y <- c(curves_df[[metric_col]])
    
    if (length(hist_camps) > 0) {
      res_fen <- fenologia_comparativa_res()
      reps <- res_fen$representative_units
      df_cos <- historical_cosecha_store()
      raw_hist <- historical_yields_store()
      var_curr <- applied_filters$variedad
      show_pks <- isTRUE(opt_show_peak())
      
      if (!is.null(reps) && nrow(reps) > 0 && !is.null(df_cos)) {
        for (hc in hist_camps) {
          yr <- as.numeric(substr(hc, 1, 4))
          match_rep <- reps %>% filter(YearCamp == yr)
          if (nrow(match_rep) > 0) {
            var_to_use <- if (!is.null(match_rep$Variedad) && !is.na(match_rep$Variedad[1]) && nzchar(trimws(as.character(match_rep$Variedad[1])))) {
              as.character(match_rep$Variedad[1])
            } else if (!is.null(var_curr) && var_curr != "Todas") {
              var_curr
            } else {
              sub("^[0-9]+ [^ ]+ ", "", as.character(match_rep$Unidad[1]))
            }
            curve_hist <- compute_representative_campaign_curve(df_cos, match_rep[1, ], var_to_use, raw_hist)
            if (nrow(curve_hist) > 0) {
              h_clr <- HIST_CAMPAIGN_COLORS[hc]
              if (is.na(h_clr)) h_clr <- "#8B5CF6"
              
              h_y_plt <- curve_hist$Kg_Planta_Prom[1]
              h_yield_suffix <- if (!is.null(h_y_plt) && !is.na(h_y_plt) && h_y_plt > 0) sprintf(" (%.2f kg/plt)", h_y_plt) else ""
              h_name <- sprintf("%s (%s)%s", hc, match_rep$Modulo[1], h_yield_suffix)
              all_max_y <- c(all_max_y, curve_hist[[metric_col]])
              
              h_customdata <- if (is_kg_planta) {
                if (is_acumulado) curve_hist$Kg_Acumulado else curve_hist$Kg_Semanal
              } else {
                if (is_acumulado) curve_hist$Kg_Planta_Acumulado else curve_hist$Kg_Planta_Semanal
              }
              
              h_hover_tmpl <- if (is_kg_planta) {
                paste0(
                  "<b>", h_name, "</b><br>",
                  "%{text}<br>",
                  if (is_acumulado) "Rendimiento acum.: <b>%{y:.2f} kg/plt</b>" else "Rendimiento: <b>%{y:.3f} kg/plt</b>",
                  "<br><span style='color:#94A3B8;'>Cosecha: %{customdata:,.0f} kg</span><extra></extra>"
                )
              } else {
                paste0(
                  "<b>", h_name, "</b><br>",
                  "%{text}<br>",
                  if (is_acumulado) "Cosecha acum.: <b>%{y:,.0f} kg</b>" else "Cosecha: <b>%{y:,.0f} kg</b>",
                  if (!is.na(h_y_plt) && h_y_plt > 0) paste0("<br><span style='color:#94A3B8;'>Rendimiento: ", sprintf("%.2f", h_y_plt), " kg/plt</span>") else "",
                  "<extra></extra>"
                )
              }
              
              p <- p %>% add_trace(
                data = curve_hist,
                x = ~TimelineIndex,
                y = as.formula(paste0("~", metric_col)),
                customdata = h_customdata,
                name = h_name,
                type = "scatter",
                mode = ifelse(show_pks, "lines+markers", "lines"),
                marker = if (show_pks) list(size = 4, color = h_clr) else NULL,
                line = list(
                  color = h_clr,
                  width = 2.4,
                  dash = "solid",
                  shape = "spline",
                  smoothing = 1.05
                ),
                hovertemplate = h_hover_tmpl,
                text = ~WeekLabel
              )
              
              if (show_pks) {
                pk_idx <- which.max(curve_hist[[metric_col]])
                if (length(pk_idx) > 0 && !is.na(pk_idx)) {
                  pk_row <- curve_hist[pk_idx, ]
                  if (nrow(pk_row) > 0) {
                    pk_val_txt <- if (is_kg_planta) {
                      sprintf("%.2f kg/plt", pk_row[[metric_col]])
                    } else {
                      sprintf("%s kg", format(round(pk_row[[metric_col]]), big.mark = ","))
                    }
                    p <- p %>% add_annotations(
                      x = pk_row$TimelineIndex,
                      y = pk_row[[metric_col]],
                      text = sprintf("<b>%s (%s): %s (Sem %d)</b>", hc, match_rep$Modulo[1], pk_val_txt, pk_row$CalWeek),
                      showarrow = TRUE,
                      arrowhead = 2,
                      arrowsize = 0.8,
                      arrowcolor = h_clr,
                      ax = 0,
                      ay = -25,
                      font = list(family = "Inter, sans-serif", size = 10, color = "#0F172A"),
                      bgcolor = "rgba(255, 255, 255, 0.92)",
                      bordercolor = h_clr,
                      borderwidth = 1,
                      borderpad = 3
                    )
                  }
                }
              }
            }
          }
        }
      }
    }
    
    # 3. Add Wish List 27/28 (if active via switch)
    if (isTRUE(opt_wish_list())) {
      ref_sub <- curves_df %>% filter(CurveLabel == curve_labels[1]) %>% arrange(TimelineIndex)
      if (nrow(ref_sub) > 0) {
        t_idx <- 19:69
        dens_wish <- dnorm(t_idx, mean = 40, sd = 9.5)
        dens_wish[t_idx < 19 | t_idx > 69] <- 0
        total_kg_wish <- sum(ref_sub$Kg_Semanal) * 0.95
        wish_kg <- (dens_wish / sum(dens_wish)) * total_kg_wish
        tot_plt <- if (!is.na(ref_sub$Total_Plantas[1]) && ref_sub$Total_Plantas[1] > 0) ref_sub$Total_Plantas[1] else 1
        
        wish_df <- data.frame(
          TimelineIndex = t_idx,
          CalWeek = ifelse(t_idx > 52, t_idx - 52, t_idx),
          CalYear = ifelse(t_idx > 52, 2028, 2027),
          Kg_Semanal = wish_kg,
          Kg_Acumulado = cumsum(wish_kg),
          Kg_Planta_Semanal = wish_kg / tot_plt,
          Kg_Planta_Acumulado = cumsum(wish_kg) / tot_plt
        )
        wish_df$WeekLabel <- sprintf("Sem %d", wish_df$CalWeek)
        all_max_y <- c(all_max_y, wish_df[[metric_col]])
        
        wish_hover_tmpl <- if (is_kg_planta) {
          paste0(
            "<b>Wish List 27/28</b><br>",
            "%{text}<br>",
            if (is_acumulado) "Meta acum.: <b>%{y:.2f} kg/plt</b>" else "Meta: <b>%{y:.3f} kg/plt</b>",
            "<extra></extra>"
          )
        } else {
          paste0(
            "<b>Wish List 27/28</b><br>",
            "%{text}<br>",
            if (is_acumulado) "Meta acum.: <b>%{y:,.0f} kg</b>" else "Meta: <b>%{y:,.0f} kg</b>",
            "<extra></extra>"
          )
        }
        
        p <- p %>% add_trace(
          data = wish_df,
          x = ~TimelineIndex,
          y = as.formula(paste0("~", metric_col)),
          name = "Wish List 27/28",
          type = "scatter",
          mode = "lines",
          line = list(
            color = SCENARIO_COLORS["Wish List 27/28"],
            width = 2.8,
            dash = "dash",
            shape = "spline"
          ),
          hovertemplate = wish_hover_tmpl,
          text = ~WeekLabel
        )
      }
    }
    
    all_ticks <- c(seq(19, 52, by = 2), seq(53, 69, by = 2))
    all_tick_labels <- c(seq(19, 52, by = 2), seq(1, 17, by = 2))
    
    p %>% layout(
      xaxis = list(
        title = list(
          text = "<b>Semana de Cosecha</b>",
          font = list(family = "Inter, sans-serif", size = 12, color = "#0F172A")
        ),
        tickvals = all_ticks,
        ticktext = all_tick_labels,
        range = c(18.5, 70),
        gridcolor = "#F1F5F9",
        zeroline = FALSE,
        rangeslider = list(visible = TRUE, thickness = 0.04, bgcolor = "#F8FAFC")
      ),
      yaxis = list(
        title = list(
          text = y_title,
          font = list(family = "Inter, sans-serif", size = 11, color = "#475569")
        ),
        tickformat = if (is_kg_planta) (if (is_acumulado) ".1f" else ".2f") else ",d",
        gridcolor = "#F1F5F9",
        zerolinecolor = "#CBD5E1"
      ),
      legend = list(
        orientation = "h",
        y = -0.38,
        yanchor = "top",
        x = 0.5,
        xanchor = "center",
        font = list(family = "Inter, sans-serif", size = 10.5)
      ),
      hovermode = "x unified",
      margin = list(l = 65, r = 25, t = 20, b = 125),
      plot_bgcolor = "#FFFFFF",
      paper_bgcolor = "#FFFFFF"
    ) %>% config(displayModeBar = FALSE)
  })
  
  # Campaign Yield Data (Módulo - Variedad en filas, Campañas en columnas)
  campaign_yield_data <- reactive({
    req(has_searched())
    req(scenario_engine_store())
    eng <- scenario_engine_store()
    hist <- historical_yields_store()
    mod <- applied_filters$modulo
    br <- applied_filters$breeder
    var <- applied_filters$variedad
    scen_edit <- if (!is.null(input$scenario_edit_select)) input$scenario_edit_select else "ESC. CAMPO"
    
    build_campaign_yield_table(
      engine_data = eng,
      hist_yields = hist,
      selected_modulo = mod,
      selected_breeder = br,
      selected_variedad = var,
      active_scenario_edit = scen_edit
    )
  })
  
  # DT Output for Campaign Yield Table (Columnas 27/28 Neutral y 27/28 Caliente ambas editables con color ESC. CAMPO)
  output$dt_campaign_yields <- renderDT({
    req(has_searched())
    req(campaign_yield_data())
    df <- campaign_yield_data()
    
    if (nrow(df) == 0) return(datatable(data.frame(Mensaje = "Sin datos para los filtros seleccionados")))
    
    col_names <- colnames(df)
    idx_neutral <- which(col_names == "27/28 Neutral") - 1 # 0-indexed for DT
    idx_caliente <- which(col_names == "27/28 Caliente") - 1
    editable_cols <- c(idx_neutral, idx_caliente)
    
    all_cols_idx <- 0:(ncol(df) - 1)
    disabled_cols <- setdiff(all_cols_idx, editable_cols)
    
    camp_cols <- intersect(c("22/23", "23/24", "24/25", "25/26", "26/27", "27/28 Neutral", "27/28 Caliente"), col_names)
    
    datatable(
      df,
      editable = list(target = "cell", disable = list(columns = disabled_cols)),
      rownames = FALSE,
      options = list(
        pageLength = 8,
        scrollX = TRUE,
        dom = "tp",
        ordering = FALSE,
        language = list(
          paginate = list(previous = "‹", `next` = "›"),
          info = "Mostrando _START_ a _END_ de _TOTAL_"
        )
      )
    ) %>%
      formatRound(camp_cols, digits = 2) %>%
      formatStyle(
        c("27/28 Neutral", "27/28 Caliente"),
        backgroundColor = "#ECFDF5",
        fontWeight = "700",
        color = "#047857",
        borderLeft = "1.5px solid #10B981"
      )
  })
  
  # Footer summary below the campaign table
  output$campaign_table_footer_ui <- renderUI({
    req(has_searched())
    req(scenario_results())
    res <- scenario_results()
    summ <- res$summary
    
    if (nrow(summ) == 0) return(NULL)
    
    scen_edit <- if (!is.null(input$scenario_edit_select)) input$scenario_edit_select else "ESC. CAMPO"
    scen_rows <- summ %>% filter(Escenario == scen_edit)
    if (nrow(scen_rows) == 0) scen_rows <- summ
    
    tot_kg <- sum(scen_rows$Total_Kg, na.rm = TRUE)
    kg_plt <- mean(scen_rows$Kg_Planta_Prom, na.rm = TRUE)
    tot_plt <- max(scen_rows$Total_Plantas, na.rm = TRUE)
    
    tags$div(
      style = "margin-top: 6px; padding: 8px 12px; background: #F8FAFC; border-radius: 8px; font-size: 11.5px; color: #475569; display: flex; align-items: center; justify-content: space-between;",
      tags$div(
        tags$span(style = "font-weight: 600; color: #0F172A;", sprintf("Escenario %s: ", scen_edit)),
        tags$span(sprintf("%s kg • Promedio: %.2f kg/plt", format(round(tot_kg), big.mark = ","), kg_plt))
      ),
      tags$span(
        style = "background: #E2E8F0; padding: 2px 8px; border-radius: 9999px; font-weight: 600; font-size: 10.5px; color: #334155;",
        sprintf("%s plantas", format(round(tot_plt), big.mark = ","))
      )
    )
  })
  
  # Handle Real-time Edits in Campaign Yield Table
  observeEvent(input$dt_campaign_yields_cell_edit, {
    info <- input$dt_campaign_yields_cell_edit
    df <- campaign_yield_data()
    eng <- scenario_engine_store()
    
    row_idx <- info$row
    col_idx <- info$col # 0-indexed column
    new_val <- as.numeric(info$value)
    
    col_name <- colnames(df)[col_idx + 1]
    if (!(col_name %in% c("27/28 Neutral", "27/28 Caliente"))) {
      session$sendCustomMessage("show_toast", list(message = "Solo las columnas 27/28 (Neutral y Caliente) son editables", icon = ""))
      return()
    }
    
    if (is.na(new_val) || new_val < 0) {
      session$sendCustomMessage("show_toast", list(message = "Valor de rendimiento inválido", icon = ""))
      return()
    }
    
    target_row <- df[row_idx, ]
    scen_target <- if (!is.null(input$scenario_edit_select)) input$scenario_edit_select else "ESC. CAMPO"
    tipo_target <- if (col_name == "27/28 Neutral") "Año neutral" else "Año caliente"
    
    # Save snapshot for undo
    history <- yield_edit_history()
    history[[length(history) + 1]] <- eng
    yield_edit_history(history)
    
    # Update engine
    updated_eng <- update_campaign_yield(
      engine_data = eng,
      selected_tipo_anio = tipo_target,
      modulo_str = target_row$Módulo,
      variedad = target_row$Variedad,
      escenario_target = scen_target,
      new_kg_planta = new_val
    )
    
    scenario_engine_store(updated_eng)
    
    sub_d <- updated_eng$datos
    m_row <- sub_d[
      sub_d$Tipo_Anio == tipo_target &
      sub_d$MóduloStr == target_row$Módulo &
      tolower(sub_d$Variedad) == tolower(target_row$Variedad) &
      sub_d$Escenario == scen_target,
    ]
    new_kg_str <- if (nrow(m_row) > 0) format(round(m_row$Kg_Total[1]), big.mark = ",") else ""
    
    session$sendCustomMessage("show_toast", list(
      message = sprintf(
        "Campaña 27/28 [%s - %s] actualizada: %s %s → %.2f kg/plt (%s kg)",
        scen_target, gsub("Año ", "", tipo_target), target_row$Módulo, target_row$Variedad, new_val, new_kg_str
      ),
      icon = ""
    ))
  })
  
  # --- Undo Yield Edit ---
  observeEvent(input$btn_undo, {
    history <- yield_edit_history()
    if (length(history) > 0) {
      last_state <- history[[length(history)]]
      yield_edit_history(history[-length(history)])
      scenario_engine_store(last_state)
      session$sendCustomMessage("show_toast", list(message = "Rendimiento anterior restaurado", icon = ""))
    } else {
      session$sendCustomMessage("show_toast", list(message = "Sin cambios en el historial para deshacer", icon = ""))
    }
  })
  
  # --- Reset All Scenarios ---
  observeEvent(input$btn_reset_all, {
    scenario_engine_store(original_scenario_store())
    yield_edit_history(list())
    session$sendCustomMessage("show_toast", list(message = "Rendimientos restablecidos a valores originales del Excel", icon = ""))
  })
  
  # --- Export Multi-Sheet Analysis (Fenología, Clima, Resumen, Rendimientos, Escenarios) ---
  output$btn_export_escenarios <- downloadHandler(
    filename = function() {
      prefix <- if (isTRUE(has_searched())) "Analisis_Productividad_Fenologia_" else "Datos_Escenarios_Rendimiento_"
      paste0(prefix, format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      sheets <- list()
      
      # 1. Calendario Fenológico (con temperaturas combinadas si hay variables climáticas activas)
      if (isTRUE(has_searched()) && !is.null(gantt_matrix_data()) && nrow(gantt_matrix_data()) > 0) {
        clim_vars <- active_climate_vars()
        sheet_name <- if (length(clim_vars) > 0) "Fenologia_y_Clima" else "Calendario_Fenologico"
        
        df_fen_clim <- build_fenologia_climate_export_df(
          mat_long = gantt_matrix_data(),
          climate_engine = climate_engine_store(),
          active_climate_vars = clim_vars,
          ref_media = ref_media_selected()
        )
        if (nrow(df_fen_clim) > 0) {
          sheets[[sheet_name]] <- df_fen_clim
        }
      }
      
      # 2. Resumen de Etapas Fenológicas por Campaña (Duración y Desarrollo)
      if (isTRUE(has_searched()) && !is.null(filtered_fenologia()) && nrow(filtered_fenologia()) > 0) {
        res_comp <- fenologia_comparativa_res()
        target_u <- if (!is.null(res_comp)) res_comp$target_unit else NULL
        df_summary <- build_fenologia_summary_export_df(filtered_fenologia(), target_u)
        if (nrow(df_summary) > 0) {
          sheets[["Resumen_Fenologico"]] <- df_summary
        }
      }
      
      # 3. Rendimiento por Campaña (Kg/Planta)
      if (isTRUE(has_searched()) && !is.null(campaign_yield_data()) && nrow(campaign_yield_data()) > 0) {
        sheets[["Rendimiento_Campañas"]] <- campaign_yield_data()
      }
      
      # 4. Datos de Escenarios y Curva Proyectada
      if (!is.null(scenario_engine_store())) {
        eng <- scenario_engine_store()
        if (!is.null(eng$datos) && nrow(eng$datos) > 0) sheets[["Datos_Escenarios"]] <- eng$datos
        if (!is.null(eng$curva) && nrow(eng$curva) > 0) sheets[["Curva_Proyectada"]] <- eng$curva
      }
      
      # Fallback por seguridad si la lista quedase vacía
      if (length(sheets) == 0) {
        sheets[["Aviso"]] <- data.frame(Mensaje = "No hay datos para exportar con los filtros seleccionados.")
      }
      
      write_xlsx(sheets, path = file)
    }
  )
}

# ------------------------------------------------------------------------------
# APPLICATION LAUNCHER
# ------------------------------------------------------------------------------

shinyApp(ui = ui, server = server)
