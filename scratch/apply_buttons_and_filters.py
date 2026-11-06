# -*- coding: utf-8 -*-
import sys

src = "Actualizacion Semanal/app.R"
with open(src, "r", encoding="utf-8") as f:
    text = f.read()

# 1. Update CSS to style .btn-borrar-manuales
target_css = """.btn-deshacer-ajuste:active {
        transform: translateY(0) scale(0.98);
      }"""

replacement_css = """.btn-deshacer-ajuste:active {
        transform: translateY(0) scale(0.98);
      }
      
      .btn-borrar-manuales:hover, .btn-borrar-manuales:focus {
        background: #FFF1F0 !important;
        color: #FF3B30 !important;
        border-color: rgba(255, 59, 48, 0.35) !important;
        transform: translateY(-1px);
      }"""

assert target_css in text, "Target CSS not found"
text = text.replace(target_css, replacement_css, 1)

# 2. Update UI Toolbar to add btn_borrar_manuales
target_ui = """                             actionButton("btn_ajustar_semana", "Ajustar Semana", class = "btn-ajustar-semana", icon = icon("calculator")),
                             actionButton("btn_deshacer_ajuste", "Deshacer Ajuste", class = "btn-deshacer-ajuste", icon = icon("rotate-left")),"""

replacement_ui = """                             actionButton("btn_ajustar_semana", "Ajustar Semana", class = "btn-ajustar-semana", icon = icon("calculator")),
                             actionButton("btn_deshacer_ajuste", "Deshacer Ajuste", class = "btn-deshacer-ajuste", icon = icon("rotate-left")),
                             actionButton("btn_borrar_manuales", "Borrar Ajuste Manual", class = "btn-deshacer-ajuste btn-borrar-manuales", icon = icon("trash-can")),"""

assert target_ui in text, "Target UI not found"
text = text.replace(target_ui, replacement_ui, 1)

# 3. Add hay_filtro_tabla_activo and turnos_activos_filtrados right above datos_reactivos
target_dr = """  # Datos filtrados según las selecciones interactivas y agrupados según 'Agrupar por'
  datos_reactivos <- reactive({"""

replacement_dr = """  # Identifica si hay algún filtro específico de grupo, módulo, turno o variedad activo
  hay_filtro_tabla_activo <- reactive({
    (!is.null(input$filtro_grupo) && length(input$filtro_grupo) > 0) ||
    (!is.null(input$filtro_modulo) && length(input$filtro_modulo) > 0) ||
    (!is.null(input$filtro_turno) && length(input$filtro_turno) > 0) ||
    (!is.null(input$filtro_variedad) && length(input$filtro_variedad) > 0)
  })
  
  # Conjunto de turnos activos/visibles según los filtros actuales
  turnos_activos_filtrados <- reactive({
    u <- universo_completo()
    if (is.null(u) || nrow(u) == 0) return(tibble())
    
    if (!is.null(input$filtro_grupo) && length(input$filtro_grupo) > 0) {
      mods_permitidos <- unlist(grupos_modulos[input$filtro_grupo], use.names = FALSE)
      u <- u %>%
        filter(suppressWarnings(as.numeric(sub("\\\\..*", "", as.character(Módulo)))) %in% mods_permitidos)
    }
    if (!is.null(input$filtro_modulo) && length(input$filtro_modulo) > 0) {
      u <- u %>% filter(as.character(Módulo) %in% as.character(input$filtro_modulo))
    }
    if (!is.null(input$filtro_turno) && length(input$filtro_turno) > 0) {
      u <- u %>% filter(as.character(Turno) %in% as.character(input$filtro_turno))
    }
    if (!is.null(input$filtro_variedad) && length(input$filtro_variedad) > 0) {
      u <- u %>% filter(limpiar_variedad(Variedad) %in% limpiar_variedad(input$filtro_variedad))
    }
    
    u %>%
      distinct(
        Sector   = trimws(as.character(Sector)),
        Módulo   = trimws(as.character(Módulo)),
        Turno    = trimws(as.character(Turno)),
        Variedad = limpiar_variedad(Variedad)
      )
  })

  # Datos filtrados según las selecciones interactivas y agrupados según 'Agrupar por'
  datos_reactivos <- reactive({"""

assert target_dr in text, "Target DR not found"
text = text.replace(target_dr, replacement_dr, 1)

with open(src, "w", encoding="utf-8", newline="\r\n") as f:
    f.write(text)

print("Part 1 successfully applied!")
