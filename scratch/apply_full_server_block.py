# -*- coding: utf-8 -*-
import shutil

src = "Actualizacion Semanal/app.R"
bak = "Actualizacion Semanal/app.R.bak_borrar_ajuste_manual"
shutil.copy(bak, src)

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

lines = text.splitlines(keepends=True)

start_idx = None
end_idx = None

for i, line in enumerate(lines):
    if '# BOTÓN AJUSTAR SEMANA (COMBINATORIA RENDIMIENTO' in line:
        start_idx = i - 1
    if 'sprintf("↺ Se removieron %d registros automáticos' in line:
        end_idx = i + 4

print("start_idx:", start_idx, "end_idx:", end_idx)
assert start_idx is not None and end_idx is not None

obs_start = None
for i in range(start_idx, end_idx):
    if 'observeEvent(input$btn_ajustar_semana, {' in lines[i]:
        obs_start = i
        break

obs_ajustar_end = None
for i in range(obs_start, end_idx):
    if '# BOTÓN ↺ DESHACER AJUSTE AUTOMÁTICO' in lines[i]:
        obs_ajustar_end = i - 3  # stop right after withProgress '    })\n'
        break

print("Last line of ajustar_body_lines:", repr(lines[obs_ajustar_end]))
assert '})' in lines[obs_ajustar_end], "Expected withProgress closing line"

ajustar_body_lines = lines[obs_start+1 : obs_ajustar_end+1]
ajustar_body_text = "".join(ajustar_body_lines)

target_after_calc = """          Tiene_Ciclo_Previo_Valv = !is.na(Dia_7d) | (!is.na(Dia_Auto_Valv) & nzchar(Dia_Auto_Valv))
        )"""

replacement_after_calc = target_after_calc + """
      
      if (solo_filtrado) {
        turnos_filtro <- turnos_activos_filtrados()
        if (is.null(turnos_filtro) || nrow(turnos_filtro) == 0) {
          showNotification("No hay turnos visibles en el filtro actual para ajustar.", type = "warning", duration = 4)
          return()
        }
        claves_filtro <- paste(
          trimws(as.character(turnos_filtro$Módulo)),
          trimws(as.character(turnos_filtro$Turno)),
          limpiar_variedad(turnos_filtro$Variedad),
          sep = "___"
        )
        valvulas_calc <- valvulas_calc %>%
          filter(paste(trimws(as.character(Módulo)), trimws(as.character(Turno)), limpiar_variedad(Variedad), sep = "___") %in% claves_filtro)
        
        if (nrow(valvulas_calc) == 0) {
          showNotification("No se encontraron válvulas en el catálogo para los turnos filtrados.", type = "warning", duration = 4)
          return()
        }
      }"""

ajustar_body_text = ajustar_body_text.replace(target_after_calc, replacement_after_calc, 1)

target_valvs_save = """      valvs_anteriores <- rv_ajuste_valvulas()
      valvs_filtradas <- if (!is.null(valvs_anteriores) && is.data.frame(valvs_anteriores) && nrow(valvs_anteriores) > 0) {
        valvs_anteriores %>%
          filter(!(as.character(Campaña) == as.character(input$filtro_campana) & as.numeric(Semana) == as.numeric(sem_sel)))
      } else {
        tibble()
      }
      valvs_guardar <- bind_rows(valvs_filtradas, df_valvs_ajuste_nueva)
      rv_ajuste_valvulas(valvs_guardar)
      guardar_ajuste_valvulas_disco(valvs_guardar)"""

replacement_valvs_save = """      valvs_anteriores <- rv_ajuste_valvulas()
      valvs_filtradas <- if (!is.null(valvs_anteriores) && is.data.frame(valvs_anteriores) && nrow(valvs_anteriores) > 0) {
        if (solo_filtrado) {
          claves_valvs_ant <- paste(
            trimws(as.character(valvs_anteriores$Módulo)),
            trimws(as.character(valvs_anteriores$Turno)),
            limpiar_variedad(valvs_anteriores$Variedad),
            sep = "___"
          )
          valvs_anteriores %>%
            filter(!(as.character(Campaña) == as.character(input$filtro_campana) &
                     as.numeric(Semana)    == as.numeric(sem_sel) &
                     claves_valvs_ant %in% claves_filtro))
        } else {
          valvs_anteriores %>%
            filter(!(as.character(Campaña) == as.character(input$filtro_campana) & as.numeric(Semana) == as.numeric(sem_sel)))
        }
      } else {
        tibble()
      }
      valvs_guardar <- bind_rows(valvs_filtradas, df_valvs_ajuste_nueva)
      rv_ajuste_valvulas(valvs_guardar)
      guardar_ajuste_valvulas_disco(valvs_guardar)"""

ajustar_body_text = ajustar_body_text.replace(target_valvs_save, replacement_valvs_save, 1)

target_auto_validas = """      if (nrow(auto_validas) == 0) {
        showNotification("No hay válvulas o días pendientes para autocompletar en esta semana.", type = "warning", duration = 4)
        return()
      }"""

replacement_auto_validas = """      if (nrow(auto_validas) == 0) {
        msg_v <- if (solo_filtrado) "No hay válvulas o días pendientes para autocompletar en la selección filtrada." else "No hay válvulas o días pendientes para autocompletar en esta semana."
        showNotification(msg_v, type = "warning", duration = 4)
        return()
      }"""

ajustar_body_text = ajustar_body_text.replace(target_auto_validas, replacement_auto_validas, 1)

target_ed_save = """      ed_conservar <- ed_actuales %>%
        filter(!(
          as.character(Campaña) == as.character(input$filtro_campana) &
            as.numeric(Semana)    == sem_sel &
            (is.na(Tipo_Edicion) | Tipo_Edicion %in% c("auto", "ruta_sin_kg"))
        ))"""

replacement_ed_save = """      if (solo_filtrado) {
        claves_ed_act <- paste(
          trimws(as.character(ed_actuales$Módulo)),
          trimws(as.character(ed_actuales$Turno)),
          limpiar_variedad(ed_actuales$Variedad),
          sep = "___"
        )
        ed_conservar <- ed_actuales %>%
          filter(!(
            as.character(Campaña) == as.character(input$filtro_campana) &
              as.numeric(Semana)    == sem_sel &
              (is.na(Tipo_Edicion) | Tipo_Edicion %in% c("auto", "ruta_sin_kg")) &
              claves_ed_act %in% claves_filtro
          ))
      } else {
        ed_conservar <- ed_actuales %>%
          filter(!(
            as.character(Campaña) == as.character(input$filtro_campana) &
              as.numeric(Semana)    == sem_sel &
              (is.na(Tipo_Edicion) | Tipo_Edicion %in% c("auto", "ruta_sin_kg"))
          ))
      }"""

ajustar_body_text = ajustar_body_text.replace(target_ed_save, replacement_ed_save, 1)

target_notif = """      showNotification(
        sprintf("Se completaron automáticamente %d turnos proyectados de %s a %s (%s Kg)%s%s.", 
                nrow(auto_validas), dia_inicio_nom, dia_fin_nom, format(round(sum(auto_validas$Valor), 1), big.mark = ","), txt_floral_info, txt_frec_info),
        type = "message",
        duration = 6
      )"""

replacement_notif = """      txt_alcance <- if (solo_filtrado) "en la selección filtrada" else "en toda la semana"
      showNotification(
        sprintf("Se completaron automáticamente %d turnos proyectados %s de %s a %s (%s Kg)%s%s.", 
                nrow(auto_validas), txt_alcance, dia_inicio_nom, dia_fin_nom, format(round(sum(auto_validas$Valor), 1), big.mark = ","), txt_floral_info, txt_frec_info),
        type = "message",
        duration = 6
      )"""

ajustar_body_text = ajustar_body_text.replace(target_notif, replacement_notif, 1)

new_block = f"""  # ============================================================================== =
  # MOTOR DE AJUSTE AUTOMÁTICO (COMBINATORIA RENDIMIENTO PONDERADO + FRECUENCIAS)
  # ============================================================================== =
  ejecutar_ajuste_semana <- function(solo_filtrado = FALSE) {{
{ajustar_body_text}
  }}

  # BOTÓN AJUSTAR SEMANA
  observeEvent(input$btn_ajustar_semana, {{
    permitir_cualquier_sem <- is.null(input$switch_ajuste_cualquier_semana) || isTRUE(input$switch_ajuste_cualquier_semana)
    es_campana_act <- grepl("2026|^26", as.character(input$filtro_campana))
    sem_actual_num <- lubridate::isoweek(Sys.Date())
    es_semana_act  <- (as.numeric(input$filtro_semana) == sem_actual_num) || permitir_cualquier_sem
    
    if (!es_campana_act) {{
      showNotification("El ajuste automático solo está habilitado para la campaña 2026.", type = "warning", duration = 4)
      return()
    }}
    
    if (!es_semana_act) {{
      showNotification("El ajuste automático está restringido a la semana actual en curso. Activa 'Ajustar cualquier semana' en el menú (⋮) para habilitarlo.", type = "warning", duration = 5)
      return()
    }}
    
    if (hay_filtro_tabla_activo()) {{
      ejecutar_ajuste_semana(solo_filtrado = TRUE)
    }} else {{
      sem_sel  <- as.numeric(input$filtro_semana)
      camp_sel <- as.character(input$filtro_campana)
      showModal(modalDialog(
        title = div(
          style = "display: flex; align-items: center; gap: 10px;",
          div(
            style = "width: 36px; height: 36px; border-radius: 10px; background: rgba(255, 149, 0, 0.15); color: #FF9500; display: flex; align-items: center; justify-content: center; font-size: 16px;",
            icon("calculator")
          ),
          div(
            div(style = "font-size: 15px; font-weight: 600; color: #18181b;", "Ajustar Toda la Semana"),
            div(style = "font-size: 12px; color: #71717a;", sprintf("Semana %s • Campaña %s", sem_sel, camp_sel))
          )
        ),
        div(
          style = "font-size: 13px; color: #3f3f46; line-height: 1.5; padding: 10px 0;",
          tags$p(
            tags$b("No tienes ningún filtro aplicado."),
            " Esta acción calculará y proyectará el ajuste automático para ", tags$strong("todos los sectores y módulos de la semana"), "."
          ),
          tags$p(style = "color: #71717a; font-size: 12px; margin-bottom: 0;",
            "Las celdas con ediciones manuales existentes o cosecha confirmada en SAP se conservarán intactas. ¿Deseas continuar?"
          )
        ),
        footer = tagList(
          modalButton("Cancelar"),
          actionButton("btn_confirmar_ajustar_todo", "Sí, ajustar todo", class = "ek-btn-primary")
        ),
        size = "m",
        easyClose = TRUE
      ))
    }}
  }})

  observeEvent(input$btn_confirmar_ajustar_todo, {{
    removeModal()
    ejecutar_ajuste_semana(solo_filtrado = FALSE)
  }})

  # ============================================================================== =
  # BOTÓN ↺ DESHACER AJUSTE AUTOMÁTICO
  # ============================================================================== =
  ejecutar_deshacer_ajuste <- function(solo_filtrado = TRUE) {{
    sem_sel  <- as.numeric(input$filtro_semana)
    camp_sel <- as.character(input$filtro_campana)
    
    ed_actuales <- ediciones_manuales()
    if (is.null(ed_actuales) || nrow(ed_actuales) == 0) {{
      showNotification("No hay ajustes automáticos para remover.", type = "message", duration = 3)
      return()
    }}
    
    es_auto_sem <- as.character(ed_actuales$Campaña) == camp_sel &
                   as.numeric(ed_actuales$Semana) == sem_sel &
                   (is.na(ed_actuales$Tipo_Edicion) | ed_actuales$Tipo_Edicion %in% c("auto", "ruta_sin_kg"))
    
    if (solo_filtrado) {{
      turnos_filtro <- turnos_activos_filtrados()
      if (is.null(turnos_filtro) || nrow(turnos_filtro) == 0) {{
        showNotification("No hay turnos visibles en el filtro actual.", type = "warning", duration = 3)
        return()
      }}
      claves_filtro <- paste(
        trimws(as.character(turnos_filtro$Módulo)),
        trimws(as.character(turnos_filtro$Turno)),
        limpiar_variedad(turnos_filtro$Variedad),
        sep = "___"
      )
      claves_ed <- paste(
        trimws(as.character(ed_actuales$Módulo)),
        trimws(as.character(ed_actuales$Turno)),
        limpiar_variedad(ed_actuales$Variedad),
        sep = "___"
      )
      a_borrar <- es_auto_sem & (claves_ed %in% claves_filtro)
    }} else {{
      a_borrar <- es_auto_sem
    }}
    
    n_removidos <- sum(a_borrar)
    if (n_removidos == 0) {{
      showNotification("No se encontraron ajustes automáticos en la selección.", type = "message", duration = 3)
      return()
    }}
    
    ed_conservar <- ed_actuales[!a_borrar, ]
    ediciones_manuales(ed_conservar)
    guardar_ediciones_disco(ed_conservar)
    
    valvs_act <- rv_ajuste_valvulas()
    if (!is.null(valvs_act) && is.data.frame(valvs_act) && nrow(valvs_act) > 0) {{
      es_valv_sem <- as.character(valvs_act$Campaña) == camp_sel & as.numeric(valvs_act$Semana) == sem_sel
      if (solo_filtrado) {{
        claves_valvs <- paste(
          trimws(as.character(valvs_act$Módulo)),
          trimws(as.character(valvs_act$Turno)),
          limpiar_variedad(valvs_act$Variedad),
          sep = "___"
        )
        valvs_conservar <- valvs_act[!(es_valv_sem & (claves_valvs %in% claves_filtro)), ]
      }} else {{
        valvs_conservar <- valvs_act[!es_valv_sem, ]
      }}
      rv_ajuste_valvulas(valvs_conservar)
      guardar_ajuste_valvulas_disco(valvs_conservar)
    }}
    
    showNotification(
      sprintf("↺ Se removieron %d registros automáticos%s. Las ediciones manuales se conservaron intactas.",
              n_removidos,
              if (solo_filtrado) " de la selección filtrada" else " de toda la semana"),
      type = "message",
      duration = 4
    )
  }}

  observeEvent(input$btn_deshacer_ajuste, {{
    sem_sel  <- as.numeric(input$filtro_semana)
    camp_sel <- as.character(input$filtro_campana)
    
    ed_actuales <- ediciones_manuales()
    es_auto_sem <- if (!is.null(ed_actuales) && nrow(ed_actuales) > 0) {{
      as.character(ed_actuales$Campaña) == camp_sel &
      as.numeric(ed_actuales$Semana) == sem_sel &
      (is.na(ed_actuales$Tipo_Edicion) | ed_actuales$Tipo_Edicion %in% c("auto", "ruta_sin_kg"))
    }} else logical(0)
    
    if (!any(es_auto_sem)) {{
      showNotification("No hay ajustes automáticos para remover en esta semana.", type = "message", duration = 3)
      return()
    }}
    
    if (hay_filtro_tabla_activo()) {{
      ejecutar_deshacer_ajuste(solo_filtrado = TRUE)
    }} else {{
      showModal(modalDialog(
        title = div(
          style = "display: flex; align-items: center; gap: 10px;",
          div(
            style = "width: 36px; height: 36px; border-radius: 10px; background: rgba(225, 29, 72, 0.12); color: #e11d48; display: flex; align-items: center; justify-content: center; font-size: 16px;",
            icon("rotate-left")
          ),
          div(
            div(style = "font-size: 15px; font-weight: 600; color: #18181b;", "Deshacer Todos los Ajustes Automáticos"),
            div(style = "font-size: 12px; color: #71717a;", sprintf("Semana %s • Campaña %s", sem_sel, camp_sel))
          )
        ),
        div(
          style = "font-size: 13px; color: #3f3f46; line-height: 1.5; padding: 10px 0;",
          tags$p(
            tags$b("No tienes ningún filtro aplicado."),
            " Esta acción eliminará los ", tags$strong("ajustes automáticos de toda la semana"), sprintf(" (%d registros)", sum(es_auto_sem)), " para todos los sectores y módulos."
          ),
          tags$p(style = "color: #71717a; font-size: 12px; margin-bottom: 0;",
            "Las ediciones manuales que hayas ingresado se conservarán intactas. ¿Deseas continuar?"
          )
        ),
        footer = tagList(
          modalButton("Cancelar"),
          actionButton("btn_confirmar_deshacer_todo", "Sí, deshacer todos", class = "ek-btn-danger")
        ),
        size = "m",
        easyClose = TRUE
      ))
    }}
  }})

  observeEvent(input$btn_confirmar_deshacer_todo, {{
    removeModal()
    ejecutar_deshacer_ajuste(solo_filtrado = FALSE)
  }})

  # ============================================================================== =
  # BOTÓN 🗑️ BORRAR AJUSTES MANUALES
  # ============================================================================== =
  ejecutar_borrado_manuales <- function(solo_filtrado = TRUE) {{
    camp_sel <- as.character(input$filtro_campana)
    sem_sel  <- as.numeric(input$filtro_semana)
    
    ed_actuales <- ediciones_manuales()
    if (is.null(ed_actuales) || nrow(ed_actuales) == 0) {{
      showNotification("No hay ajustes manuales para remover en esta semana.", type = "message", duration = 3)
      return()
    }}
    
    es_manual_sem <- as.character(ed_actuales$Campaña) == camp_sel &
                     as.numeric(ed_actuales$Semana) == sem_sel &
                     coalesce(ed_actuales$Tipo_Edicion, "manual") == "manual"
    
    if (!any(es_manual_sem)) {{
      showNotification("No se encontraron ajustes manuales en esta semana.", type = "message", duration = 3)
      return()
    }}
    
    if (solo_filtrado) {{
      turnos_filtro <- turnos_activos_filtrados()
      if (is.null(turnos_filtro) || nrow(turnos_filtro) == 0) {{
        showNotification("No hay turnos visibles en el filtro actual.", type = "warning", duration = 3)
        return()
      }}
      claves_filtro <- paste(
        trimws(as.character(turnos_filtro$Módulo)),
        trimws(as.character(turnos_filtro$Turno)),
        limpiar_variedad(turnos_filtro$Variedad),
        sep = "___"
      )
      claves_ed <- paste(
        trimws(as.character(ed_actuales$Módulo)),
        trimws(as.character(ed_actuales$Turno)),
        limpiar_variedad(ed_actuales$Variedad),
        sep = "___"
      )
      a_borrar <- es_manual_sem & (claves_ed %in% claves_filtro)
    }} else {{
      a_borrar <- es_manual_sem
    }}
    
    n_a_borrar <- sum(a_borrar)
    if (n_a_borrar == 0) {{
      showNotification("No se encontraron ajustes manuales en la selección filtrada.", type = "message", duration = 3)
      return()
    }}
    
    ed_a_eliminar <- ed_actuales[a_borrar, ]
    ed_conservar  <- ed_actuales[!a_borrar, ]
    
    # Restaurar valores automáticos de rv_ajuste_valvulas si existían para las celdas eliminadas
    valvs_aj <- rv_ajuste_valvulas()
    reg_restaurados <- tibble()
    if (!is.null(valvs_aj) && is.data.frame(valvs_aj) && nrow(valvs_aj) > 0) {{
      valvs_sem <- valvs_aj %>%
        filter(as.character(Campaña) == camp_sel, as.numeric(Semana) == sem_sel)
      
      if (nrow(valvs_sem) > 0) {{
        claves_celdas_elim <- paste(
          trimws(as.character(ed_a_eliminar$Módulo)),
          trimws(as.character(ed_a_eliminar$Turno)),
          limpiar_variedad(ed_a_eliminar$Variedad),
          trimws(as.character(ed_a_eliminar$Campo)),
          sep = "___"
        )
        claves_valvs <- paste(
          trimws(as.character(valvs_sem$Módulo)),
          trimws(as.character(valvs_sem$Turno)),
          limpiar_variedad(valvs_sem$Variedad),
          trimws(as.character(valvs_sem$Dia_Asignado)),
          sep = "___"
        )
        valvs_restaurar <- valvs_sem[claves_valvs %in% claves_celdas_elim, ]
        if (nrow(valvs_restaurar) > 0) {{
          reg_restaurados <- valvs_restaurar %>%
            group_by(Sector, Módulo, Turno, Variedad, Campo = Dia_Asignado) %>%
            summarise(
              Valor           = round(sum(Kg_Asignado, na.rm = TRUE), 2),
              Ha_Asignada     = round(sum(Ha_Asignada, na.rm = TRUE), 2),
              Ha_Asignada_1ra = round(sum(ifelse(coalesce(Num_Pasada, 1L) == 1L, Ha_Asignada, 0), na.rm = TRUE), 2),
              Kg_Asignado_1ra = round(sum(ifelse(coalesce(Num_Pasada, 1L) == 1L, Kg_Asignado, 0), na.rm = TRUE), 2),
              Ha_Asignada_2da = round(sum(ifelse(coalesce(Num_Pasada, 1L) == 2L, Ha_Asignada, 0), na.rm = TRUE), 2),
              Kg_Asignado_2da = round(sum(ifelse(coalesce(Num_Pasada, 1L) == 2L, Kg_Asignado, 0), na.rm = TRUE), 2),
              Tipo_Edicion    = "auto",
              Motivo_Ajuste   = first(na.omit(Motivo_Ajuste)),
              .groups         = "drop"
            ) %>%
            mutate(
              Campaña       = camp_sel,
              Semana        = sem_sel,
              Fecha_Edicion = Sys.time()
            )
        }}
      }}
    }}
    
    ed_finales <- bind_rows(ed_conservar, reg_restaurados)
    ediciones_manuales(ed_finales)
    guardar_ediciones_disco(ed_finales)
    
    txt_rest <- if (nrow(reg_restaurados) > 0) sprintf(" (Se restauraron %d valores a su estado automático)", nrow(reg_restaurados)) else ""
    showNotification(
      sprintf("🗑️ Se eliminaron %d ajustes manuales%s.%s",
              n_a_borrar,
              if (solo_filtrado) " de la selección filtrada" else " de toda la semana",
              txt_rest),
      type = "message",
      duration = 4
    )
  }}

  observeEvent(input$btn_borrar_manuales, {{
    sem_sel  <- as.numeric(input$filtro_semana)
    camp_sel <- as.character(input$filtro_campana)
    
    ed_actuales <- ediciones_manuales()
    es_manual_sem <- if (!is.null(ed_actuales) && nrow(ed_actuales) > 0) {{
      as.character(ed_actuales$Campaña) == camp_sel &
      as.numeric(ed_actuales$Semana) == sem_sel &
      coalesce(ed_actuales$Tipo_Edicion, "manual") == "manual"
    }} else logical(0)
    
    if (!any(es_manual_sem)) {{
      showNotification("No hay ajustes manuales registrados en esta semana.", type = "message", duration = 3)
      return()
    }}
    
    if (hay_filtro_tabla_activo()) {{
      ejecutar_borrado_manuales(solo_filtrado = TRUE)
    }} else {{
      showModal(modalDialog(
        title = div(
          style = "display: flex; align-items: center; gap: 10px;",
          div(
            style = "width: 36px; height: 36px; border-radius: 10px; background: rgba(225, 29, 72, 0.12); color: #e11d48; display: flex; align-items: center; justify-content: center; font-size: 16px;",
            icon("trash-can")
          ),
          div(
            div(style = "font-size: 15px; font-weight: 600; color: #18181b;", "Borrar Todos los Ajustes Manuales"),
            div(style = "font-size: 12px; color: #71717a;", sprintf("Semana %s • Campaña %s", sem_sel, camp_sel))
          )
        ),
        div(
          style = "font-size: 13px; color: #3f3f46; line-height: 1.5; padding: 10px 0;",
          tags$p(
            tags$b("No tienes ningún filtro aplicado."),
            " Esta acción eliminará ", tags$strong("todos los ajustes manuales"), sprintf(" (%d registrados)", sum(es_manual_sem)), " de toda la semana."
          ),
          tags$p(style = "color: #71717a; font-size: 12px; margin-bottom: 0;",
            "Si algún turno contaba previamente con cálculo automático de válvulas, se restaurará su valor automático original. ¿Deseas continuar?"
          )
        ),
        footer = tagList(
          modalButton("Cancelar"),
          actionButton("btn_confirmar_borrar_manuales_todo", "Sí, borrar todos", class = "ek-btn-danger")
        ),
        size = "m",
        easyClose = TRUE
      ))
    }}
  }})

  observeEvent(input$btn_confirmar_borrar_manuales_todo, {{
    removeModal()
    ejecutar_borrado_manuales(solo_filtrado = FALSE)
  }})
"""

lines_before = lines[:start_idx]
lines_after  = lines[end_idx+1:]

new_content = "".join(lines_before) + new_block + "".join(lines_after)

with open(src, "w", encoding="utf-8", newline="\r\n") as f:
    f.write(new_content)

print("Fixed and rewritten successfully!")
