# apply_emil_kowalski_ui.R
# Script para actualizar app.R implementando el modal "Agrupar por", 
# la barra de Detalle General rediseñada con Emil Kowalski,
# y la remoción de iconos tipo IA y emojis.

app_path <- "D:/ACP/Actualizacion Semanal/app.R"
lines <- readLines(app_path, encoding = "UTF-8", warn = FALSE)

# 1. ACTUALIZAR CSS
# Agregar reglas CSS para Emil Kowalski y nuevos controles
css_needle <- "/* Barra superior derecha a la altura de los selectores de pesta"
css_idx <- grep(css_needle, lines, fixed = FALSE)[1]
if (is.na(css_idx)) stop("No se encontro css_needle")

css_new_block <- c(
  "      /* Emil Kowalski Polish: Status dots y controles unificados */",
  "      .ek-dot {",
  "        display: inline-block;",
  "        width: 7px;",
  "        height: 7px;",
  "        border-radius: 50%;",
  "        margin-right: 5px;",
  "        vertical-align: middle;",
  "      }",
  "      .detalle-controls-toolbar {",
  "        display: flex;",
  "        align-items: center;",
  "        gap: 8px;",
  "        flex-wrap: wrap;",
  "        margin-bottom: 14px;",
  "      }",
  "      .controls-group-left {",
  "        display: inline-flex;",
  "        align-items: center;",
  "        gap: 6px;",
  "        flex-wrap: wrap;",
  "      }",
  "      .controls-separator {",
  "        width: 1px;",
  "        height: 22px;",
  "        background: rgba(0, 0, 0, 0.08);",
  "        margin: 0 2px;",
  "      }",
  "      .escenario-control-group {",
  "        display: inline-flex;",
  "        align-items: center;",
  "        height: 34px;",
  "        background: #FFFFFF;",
  "        border: 1px solid rgba(0, 0, 0, 0.12);",
  "        border-radius: 9px;",
  "        padding: 0 4px 0 10px;",
  "        box-shadow: 0 1px 2px rgba(0, 0, 0, 0.03);",
  "        box-sizing: border-box;",
  "        gap: 6px;",
  "        transition: all 0.2s cubic-bezier(0.16, 1, 0.3, 1);",
  "      }",
  "      .escenario-control-group:hover {",
  "        border-color: rgba(0, 0, 0, 0.22);",
  "      }",
  "      .escenario-control-group:focus-within {",
  "        border-color: #007AFF;",
  "        box-shadow: 0 0 0 3px rgba(0, 122, 255, 0.12);",
  "      }",
  "      .escenario-prefix {",
  "        display: inline-flex;",
  "        align-items: center;",
  "        gap: 4px;",
  "        font-size: 11px;",
  "        font-weight: 700;",
  "        color: #636366;",
  "        text-transform: uppercase;",
  "        letter-spacing: 0.03em;",
  "        white-space: nowrap;",
  "      }",
  "      .escenario-prefix i {",
  "        color: #8E8E93;",
  "        font-size: 11px;",
  "      }",
  "      .escenario-select-wrap {",
  "        width: 175px;",
  "      }",
  "      .escenario-select-wrap .form-group {",
  "        margin-bottom: 0 !important;",
  "      }",
  "      .escenario-select-wrap select {",
  "        height: 28px !important;",
  "        border: none !important;",
  "        background: transparent !important;",
  "        font-size: 12px !important;",
  "        font-weight: 500 !important;",
  "        color: #1C1C1E !important;",
  "        padding: 0 4px !important;",
  "        box-shadow: none !important;",
  "        cursor: pointer;",
  "      }",
  "      .btn-escenario-save {",
  "        height: 26px !important;",
  "        padding: 0 9px !important;",
  "        border-radius: 6px !important;",
  "        font-size: 11.5px !important;",
  "        font-weight: 600 !important;",
  "        background: rgba(0, 122, 255, 0.08) !important;",
  "        color: #007AFF !important;",
  "        border: 1px solid rgba(0, 122, 255, 0.20) !important;",
  "        display: inline-flex !important;",
  "        align-items: center !important;",
  "        gap: 4px !important;",
  "        cursor: pointer;",
  "        transition: all 0.15s ease;",
  "      }",
  "      .btn-escenario-save:hover {",
  "        background: #007AFF !important;",
  "        color: #FFFFFF !important;",
  "      }",
  "      .btn-escenario-manage {",
  "        width: 26px !important;",
  "        height: 26px !important;",
  "        border-radius: 6px !important;",
  "        font-size: 11px !important;",
  "        background: #F2F2F7 !important;",
  "        color: #636366 !important;",
  "        border: 1px solid rgba(0,0,0,0.08) !important;",
  "        display: inline-flex !important;",
  "        align-items: center !important;",
  "        justify-content: center !important;",
  "        padding: 0 !important;",
  "        cursor: pointer;",
  "        transition: all 0.15s ease;",
  "      }",
  "      .btn-escenario-manage:hover {",
  "        background: #E5E5EA !important;",
  "        color: #1C1C1E !important;",
  "      }",
  "      .controls-group-right {",
  "        display: flex;",
  "        align-items: center;",
  "        gap: 6px;",
  "        margin-left: auto;",
  "      }",
  "      .vista-prefix-label {",
  "        font-size: 11px;",
  "        font-weight: 700;",
  "        color: #8E8E93;",
  "        text-transform: uppercase;",
  "        letter-spacing: 0.03em;",
  "      }",
  "      .ek-modal-switch-btn {",
  "        position: relative !important;",
  "        width: 44px !important;",
  "        height: 26px !important;",
  "        border-radius: 13px !important;",
  "        border: none !important;",
  "        padding: 0 !important;",
  "        cursor: pointer !important;",
  "        transition: background 0.2s cubic-bezier(0.16, 1, 0.3, 1) !important;",
  "        outline: none !important;",
  "        box-shadow: none !important;",
  "        flex-shrink: 0 !important;",
  "      }",
  "      .ek-modal-switch-btn.switch-on {",
  "        background: #34C759 !important;",
  "      }",
  "      .ek-modal-switch-btn.switch-off {",
  "        background: #E5E5EA !important;",
  "      }",
  "      .ek-modal-switch-knob {",
  "        position: absolute;",
  "        top: 3px;",
  "        left: 3px;",
  "        width: 20px;",
  "        height: 20px;",
  "        border-radius: 50%;",
  "        background: #FFFFFF;",
  "        box-shadow: 0 1px 3px rgba(0, 0, 0, 0.25);",
  "        transition: transform 0.2s cubic-bezier(0.16, 1, 0.3, 1);",
  "        pointer-events: none;",
  "      }",
  "      .ek-modal-switch-btn.switch-on .ek-modal-switch-knob {",
  "        transform: translateX(18px);",
  "      }"
)

lines <- c(lines[1:(css_idx-1)], css_new_block, lines[css_idx:length(lines)])

# Ajustar nav-pills max-width para que nunca toque el boton de 3 puntos
nav_pills_idx <- grep("\\.nav-pills \\{", lines)[1]
if (!is.na(nav_pills_idx)) {
  # buscar margin-bottom: 16px !important;
  for (j in nav_pills_idx:(nav_pills_idx + 15)) {
    if (grepl("margin-bottom: 16px !important;", lines[j])) {
      lines[j] <- "        margin-bottom: 16px !important;\n        max-width: calc(100% - 54px) !important;\n        flex-wrap: wrap !important;"
      break
    }
  }
}

# Ajustar height de range-picker-connected, btn-ajustar-semana y btn-deshacer-ajuste a 34px
lines <- gsub("height: 38px;", "height: 34px;", lines)
lines <- gsub("height: 38px !important;", "height: 34px !important;", lines)

# 2. ELIMINAR agrupar-dropdown-wrapper de tabs-top-right-bar
# Buscar donde empieza agrupar-dropdown-wrapper y termina antes de more-options-dropdown-wrapper
start_agrupar <- grep('class = "agrupar-dropdown-wrapper"', lines)[1]
end_agrupar <- grep('class = "more-options-dropdown-wrapper"', lines)[1]

if (!is.na(start_agrupar) && !is.na(end_agrupar) && end_agrupar > start_agrupar) {
  # retroceder 1 o 2 lineas para el comentario "# Agrupar por"
  if (grepl("# Agrupar por", lines[start_agrupar - 1])) {
    start_agrupar <- start_agrupar - 1
  }
  # buscar la linea de cierre del div anterior a more-options
  cut_end <- end_agrupar - 1
  while (cut_end > start_agrupar && !grepl("^\\s*\\),?\\s*$", lines[cut_end])) {
    cut_end <- cut_end - 1
  }
  lines <- lines[-(start_agrupar:cut_end)]
}

# 3. INSERTAR "Agrupar por Dimensiones" en more-options-list
more_opts_idx <- grep('class = "more-options-list"', lines)[1]
if (!is.na(more_opts_idx)) {
  agrupar_menu_item <- c(
    "                          # 0. Agrupar por Dimensiones",
    "                          actionButton(",
    "                            \"btn_abrir_modal_agrupar\",",
    "                            label = tagList(",
    "                              div(class = \"menu-item-icon\", style = \"background: rgba(0, 122, 255, 0.10); color: #007AFF;\", icon(\"layer-group\")),",
    "                              div(class = \"menu-item-content\",",
    "                                  div(style = \"display: flex; align-items: center; justify-content: space-between; width: 100%;\",",
    "                                      tags$span(class = \"menu-item-title\", \"Agrupar por Dimensiones\"),",
    "                                      uiOutput(\"ui_badge_agrupar_menu\", inline = TRUE)",
    "                                  ),",
    "                                  tags$span(class = \"menu-item-desc\", \"Configurar consolidación de tabla y evolución\")",
    "                              )",
    "                            ),",
    "                            class = \"more-options-item-btn\"",
    "                          ),",
    "                          div(class = \"more-options-divider\"),"
  )
  lines <- c(lines[1:more_opts_idx], agrupar_menu_item, lines[(more_opts_idx + 1):length(lines)])
}

# 4. ACTUALIZAR DETALLE GENERAL: LEYENDA (QUITAR EMOJIS) Y TOOLBAR COMPLETA
legend_idx <- grep('class = "detalle-toolbar-legend"', lines)[1]
if (!is.na(legend_idx)) {
  # Reemplazar el bloque de la leyenda y la toolbar
  # Encontrar fin de la toolbar anterior: DTOutput("tabla_control_cosecha")
  dt_idx <- grep('DTOutput\\("tabla_control_cosecha"\\)', lines)[1]
  if (!is.na(dt_idx) && dt_idx > legend_idx) {
    # start replacement right at legend_idx
    # Find start of container for legend or legend div
    new_toolbar_block <- c(
      "                             div(class = \"detalle-toolbar-legend\",",
      "                                 span(class = \"legend-pill legend-manual\", span(class = \"ek-dot\", style = \"background: #AF52DE;\"), \"Manual\"),",
      "                                 span(class = \"legend-pill legend-ruta\", span(class = \"ek-dot\", style = \"background: #FF3B30;\"), \"Ruta (Sin Kg SAP)\"),",
      "                                 span(class = \"legend-pill legend-auto\", span(class = \"ek-dot\", style = \"background: #FF9500;\"), \"Auto (Ciclo)\"),",
      "                                 span(class = \"legend-pill legend-real\", span(class = \"ek-dot\", style = \"background: #007AFF;\"), \"Real SAP\"),",
      "                                 span(class = \"legend-pill legend-ajuste\", span(class = \"ek-dot\", style = \"background: #34C759;\"), \"Ajuste / kg/ha\")",
      "                             )",
      "                         ),",
      "                         # Fila de Acciones Emil Kowalski: Rango, Ajuste, Escenario y Vista",
      "                         div(class = \"detalle-controls-toolbar\",",
      "                             # Grupo 1: Selector de Rango Conectado y Botones de Cálculo",
      "                             div(class = \"controls-group-left\",",
      "                                 div(class = \"range-picker-connected\",",
      "                                     div(class = \"range-picker-icon\", icon(\"calendar-day\")),",
      "                                     div(class = \"range-picker-segment\",",
      "                                         tags$span(class = \"range-picker-prefix\", \"Desde:\"),",
      "                                         selectInput(\"ajustar_desde_dia\", label = NULL,",
      "                                                     choices = c(\"Lunes\", \"Martes\", \"Miércoles\", \"Jueves\", \"Viernes\", \"Sábado\"),",
      "                                                     selected = {",
      "                                                       w_num <- lubridate::wday(Sys.Date(), week_start = 1)",
      "                                                       if (w_num > 6) \"Lunes\" else c(\"Lunes\", \"Martes\", \"Miércoles\", \"Jueves\", \"Viernes\", \"Sábado\")[w_num]",
      "                                                     },",
      "                                                     selectize = FALSE,",
      "                                                     width = \"90px\")",
      "                                     ),",
      "                                     div(class = \"range-picker-divider\"),",
      "                                     div(class = \"range-picker-segment\",",
      "                                         tags$span(class = \"range-picker-prefix\", \"Hasta:\"),",
      "                                         selectInput(\"ajustar_hasta_dia\", label = NULL,",
      "                                                     choices = c(\"Lunes\", \"Martes\", \"Miércoles\", \"Jueves\", \"Viernes\", \"Sábado\"),",
      "                                                     selected = \"Sábado\",",
      "                                                     selectize = FALSE,",
      "                                                     width = \"90px\")",
      "                                     )",
      "                                 ),",
      "                                 actionButton(\"btn_ajustar_semana\", \"Ajustar Semana\", class = \"btn-ajustar-semana\", icon = icon(\"calculator\")),",
      "                                 actionButton(\"btn_deshacer_ajuste\", \"Deshacer\", class = \"btn-deshacer-ajuste\", icon = icon(\"rotate-left\")),",
      "                                 actionButton(\"btn_borrar_manuales\", \"Borrar Manual\", class = \"btn-deshacer-ajuste btn-borrar-manuales\", icon = icon(\"trash-can\"))",
      "                             ),",
      "                             # Separador sutil",
      "                             div(class = \"controls-separator\"),",
      "                             # Grupo 2: Control Integrado de Escenarios Emil Kowalski",
      "                             div(class = \"escenario-control-group\",",
      "                                 div(class = \"escenario-prefix\", icon(\"bookmark\"), \"Escenario:\"),",
      "                                 div(class = \"escenario-select-wrap\",",
      "                                     selectInput(\"selector_escenario_activo\", label = NULL,",
      "                                                 choices = c(\"Ajuste en Curso\" = \"__actual__\"),",
      "                                                 selected = \"__actual__\", selectize = FALSE, width = \"100%\")",
      "                                 ),",
      "                                 actionButton(\"btn_abrir_modal_guardar_escenario\", \"Guardar\",",
      "                                              icon = icon(\"floppy-disk\"), class = \"btn-escenario-save\",",
      "                                              title = \"Guardar este rango y proyección como un escenario\"),",
      "                                 actionButton(\"btn_abrir_modal_gestionar_escenarios\", NULL,",
      "                                              icon = icon(\"sliders\"), class = \"btn-escenario-manage\",",
      "                                              title = \"Gestionar escenarios (renombrar, eliminar)\")",
      "                             ),",
      "                             # Grupo 3: Selector de Vista alineado a la derecha",
      "                             div(class = \"controls-group-right\",",
      "                                 tags$span(class = \"vista-prefix-label\", \"Vista:\"),",
      "                                 div(class = \"ios-segmented-switch\",",
      "                                     radioButtons(",
      "                                         inputId  = \"filtro_modo_detalle\",",
      "                                         label    = NULL,",
      "                                         choices  = c(\"Real\" = \"real\", \"Ajustado\" = \"ajustado\"),",
      "                                         selected = \"real\",",
      "                                         inline   = TRUE",
      "                                     )",
      "                                 )",
      "                             )",
      "                         ),"
    )
    lines <- c(lines[1:(legend_idx - 1)], new_toolbar_block, lines[dt_idx:length(lines)])
  }
}

# 5. REMOVER EMOJIS EN RESUMEN Y COMPARATIVO
lines <- gsub('span\\(class = "legend-pill legend-ajuste", "📊 Consolidado Semanal"\\)',
              'span(class = "legend-pill legend-ajuste", span(class = "ek-dot", style = "background: #34C759;"), "Consolidado Semanal")',
              lines)

lines <- gsub('"🎯 Auditoría del Escenario (Rango)"', '"Auditoría del Escenario (Rango)"', lines, fixed = TRUE)
lines <- gsub('"📊 Semana: Proy vs Ajustado vs Real"', '"Semana: Proy vs Ajustado vs Real"', lines, fixed = TRUE)
lines <- gsub('"🔀 Entre 2 Escenarios"', '"Comparar entre 2 Escenarios"', lines, fixed = TRUE)
lines <- gsub('"🎯 Auditor.*del Escenario \\(Rango\\)"', '"Auditoría del Escenario (Rango)"', lines)

# 6. SERVER: ACTUALIZAR cols_agrupar Y AGREGAR MODAL "Agrupar por Dimensiones"
cols_idx <- grep('cols_agrupar <- reactive\\(\\{', lines)[1]
if (!is.na(cols_idx)) {
  # Encontrar fin del bloque cols_agrupar
  end_cols <- cols_idx
  while (!grepl("^\\s*\\}\\)\\s*$", lines[end_cols])) {
    end_cols <- end_cols + 1
  }
  
  # Buscar arriba donde estan dims_todas_agrupar
  dims_top <- cols_idx - 1
  while (dims_top > 1 && !grepl("dims_todas_agrupar", lines[dims_top])) {
    dims_top <- dims_top - 1
  }
  
  server_agrupar_block <- c(
    "  dims_todas_agrupar  <- c(\"Sector\", \"Módulo\", \"Turno\", \"Válvula\", \"Variedad\", \"Tipo\")",
    "  dims_tabla_agrupar  <- c(\"Sector\", \"Módulo\", \"Turno\", \"Variedad\", \"Tipo\")",
    "  dims_conteo_agrupar <- c(\"Sector\", \"Módulo\", \"Turno\", \"Válvula\", \"Variedad\")",
    "  ",
    "  rv_dims_agrupar <- reactiveVal(dims_tabla_agrupar)",
    "  ",
    "  cols_agrupar <- reactive({",
    "    sel <- rv_dims_agrupar()",
    "    if (is.null(sel) || length(sel) == 0) dims_tabla_agrupar else sel",
    "  })",
    "  ",
    "  output$ui_badge_agrupar_menu <- renderUI({",
    "    n_act <- length(intersect(dims_todas_agrupar, cols_agrupar()))",
    "    span(class = \"badge-agrupar-dims\", sprintf(\"%d/6\", n_act))",
    "  })",
    "  ",
    "  # Modal 'Agrupar por Dimensiones' Emil Kowalski",
    "  observeEvent(input$btn_abrir_modal_agrupar, {",
    "    showModal(modalDialog(",
    "      title = tagList(",
    "        div(",
    "          style = \"display: flex; align-items: center; gap: 10px;\",",
    "          div(style = \"width: 34px; height: 34px; border-radius: 9px; background: rgba(0, 122, 255, 0.10); color: #007AFF; display: flex; align-items: center; justify-content: center; font-size: 15px;\",",
    "              icon(\"layer-group\")),",
    "          div(",
    "            tags$div(style = \"font-size: 15px; font-weight: 700; color: #1C1C1E; line-height: 1.2;\", \"Agrupar por Dimensiones\"),",
    "            tags$div(style = \"font-size: 11.5px; color: #8E8E93; margin-top: 2px;\", \"Configura las dimensiones visibles para consolidar la tabla general\")",
    "          )",
    "        )",
    "      ),",
    "      size = \"m\",",
    "      easyClose = TRUE,",
    "      footer = tagList(",
    "        modalButton(\"Listo\")",
    "      ),",
    "      div(",
    "        style = \"padding: 4px 0;\",",
    "        div(",
    "          style = \"display: flex; align-items: center; justify-content: space-between; margin-bottom: 12px; padding: 0 4px;\",",
    "          tags$span(style = \"font-size: 12px; font-weight: 600; color: #8E8E93;\", \"Dimensiones activas:\"),",
    "          div(",
    "            style = \"display: flex; gap: 6px;\",",
    "            actionButton(\"btn_agrupar_marcar_todas\", \"Marcar todas\",",
    "                         class = \"ek-btn-mini-config\",",
    "                         style = \"font-size: 11px; padding: 4px 9px; border-radius: 6px; background: #F2F2F7; border: 1px solid rgba(0,0,0,0.08); font-weight: 600; color: #1C1C1E;\"),",
    "            actionButton(\"btn_agrupar_restablecer_default\", \"Vista estándar (5)\",",
    "                         class = \"ek-btn-mini-config\",",
    "                         style = \"font-size: 11px; padding: 4px 9px; border-radius: 6px; background: #F2F2F7; border: 1px solid rgba(0,0,0,0.08); font-weight: 600; color: #1C1C1E;\")",
    "          )",
    "        ),",
    "        uiOutput(\"ui_modal_agrupar_lista\")",
    "      )",
    "    ))",
    "  })",
    "  ",
    "  output$ui_modal_agrupar_lista <- renderUI({",
    "    cur <- cols_agrupar()",
    "    dims_info <- list(",
    "      list(id = \"Sector\",   label = \"Sector\",   icon = \"compass\",     desc = \"Sector agronómico del campo\"),",
    "      list(id = \"Módulo\",   label = \"Módulo\",   icon = \"cubes\",       desc = \"Módulo de producción y riego\"),",
    "      list(id = \"Turno\",    label = \"Turno\",    icon = \"clock\",       desc = \"Turno de riego específico\"),",
    "      list(id = \"Válvula\",  label = \"Válvula\",  icon = \"faucet-drip\", desc = \"Válvula individual (conteo y frecuencias)\", pill = \"Conteo / Frecuencia\"),",
    "      list(id = \"Variedad\", label = \"Variedad\", icon = \"seedling\",    desc = \"Variedad comercial\"),",
    "      list(id = \"Tipo\",     label = \"Tipo\",     icon = \"leaf\",        desc = \"Convencional, Orgánico o Testblock\")",
    "    )",
    "    ",
    "    div(",
    "      class = \"agrupar-modal-container\",",
    "      style = \"background: #F8F9FB; border-radius: 12px; border: 1px solid rgba(0,0,0,0.06); overflow: hidden;\",",
    "      lapply(seq_along(dims_info), function(i) {",
    "        dim <- dims_info[[i]]",
    "        is_active <- dim$id %in% cur",
    "        div(",
    "          style = paste0(\"display: flex; align-items: center; justify-content: space-between; padding: 11px 14px;\",",
    "                         if (i < length(dims_info)) \" border-bottom: 1px solid rgba(0,0,0,0.05);\" else \"\"),",
    "          div(",
    "            style = \"display: flex; align-items: center; gap: 10px;\",",
    "            div(style = \"width: 28px; height: 28px; border-radius: 6px; background: #FFFFFF; display: flex; align-items: center; justify-content: center; color: #636366; font-size: 13px; box-shadow: 0 1px 2px rgba(0,0,0,0.04);\",",
    "                icon(dim$icon)),",
    "            div(",
    "              div(style = \"display: flex; align-items: center; gap: 6px;\",",
    "                  span(style = \"font-size: 13px; font-weight: 600; color: #1C1C1E;\", dim$label),",
    "                  if (!is.null(dim$pill)) span(style = \"font-size: 9px; font-weight: 700; color: #0E7A68; background: rgba(14, 122, 104, 0.12); padding: 2px 6px; border-radius: 6px;\", dim$pill) else NULL",
    "              ),",
    "              div(style = \"font-size: 11px; color: #8E8E93;\", dim$desc)",
    "            )",
    "          ),",
    "          actionButton(",
    "            inputId = sprintf(\"btn_toggle_dim_%d\", i),",
    "            label = NULL,",
    "            class = paste0(\"ek-modal-switch-btn\", if (is_active) \" switch-on\" else \" switch-off\"),",
    "            title = if (is_active) \"Desactivar dimensión\" else \"Activar dimensión\",",
    "            tags$span(class = \"ek-modal-switch-knob\")",
    "          )",
    "        )",
    "      })",
    "    )",
    "  })",
    "  ",
    "  lapply(1:6, function(i) {",
    "    observeEvent(input[[sprintf(\"btn_toggle_dim_%d\", i)]], {",
    "      dim_name <- dims_todas_agrupar[i]",
    "      cur <- rv_dims_agrupar()",
    "      if (dim_name %in% cur) {",
    "        if (length(cur) <= 1) {",
    "          showNotification(\"Debe mantener al menos 1 dimensión activa en 'Agrupar por'.\", type = \"warning\", duration = 3)",
    "          return()",
    "        }",
    "        nuevo <- setdiff(cur, dim_name)",
    "      } else {",
    "        nuevo <- union(cur, dim_name)",
    "      }",
    "      nuevo <- intersect(dims_todas_agrupar, nuevo)",
    "      rv_dims_agrupar(nuevo)",
    "    }, ignoreInit = TRUE)",
    "  })",
    "  ",
    "  observeEvent(input$btn_agrupar_marcar_todas, {",
    "    rv_dims_agrupar(dims_todas_agrupar)",
    "  })",
    "  ",
    "  observeEvent(input$btn_agrupar_restablecer_default, {",
    "    rv_dims_agrupar(dims_tabla_agrupar)",
    "  })"
  )
  
  lines <- c(lines[1:(dims_top - 1)], server_agrupar_block, lines[(end_cols + 1):length(lines)])
}

# 7. REMOVER ICONOS TIPO IA Y EMOJIS DE ESCENARIOS Y FRECUENCIAS
# a) Frecuencias: bolt -> arrows-rotate
lines <- gsub('icon\\(if \\(es_auto\\) "bolt" else "sliders"\\)', 'icon(if (es_auto) "arrows-rotate" else "sliders")', lines)
lines <- gsub('label = tagList\\(icon\\("bolt"\\), "Automático"\\)', 'label = tagList(icon("arrows-rotate"), "Automático")', lines)

# b) Escenarios: Quitar estrella ⭐
lines <- gsub('lbl <- sprintf\\("⭐ %s \\(%s \\| %s kg\\)"', 'lbl <- sprintf("%s (%s | %s kg)"', lines)
lines <- gsub('sprintf\\("⭐ Escenario \'%s\' cargado exitosamente', 'sprintf("Escenario \'%s\' cargado exitosamente', lines)

# Escribir archivo actualizado
writeLines(lines, app_path, useBytes = TRUE)
cat("Archivo app.R actualizado correctamente con lineamientos Emil Kowalski.\n")
