# -*- coding: utf-8 -*-
import sys

src = "Actualizacion Semanal/app.R"
with open(src, "r", encoding="utf-8") as f:
    lines = f.readlines()

start_idx = None
end_idx = None

for i, line in enumerate(lines):
    if '# BOTÓN AJUSTAR SEMANA (COMBINATORIA RENDIMIENTO' in line:
        start_idx = i - 1
    if 'sprintf("↺ Se removieron %d registros automáticos' in line:
        end_idx = i + 4

print("start_idx:", start_idx, "end_idx:", end_idx)
assert start_idx is not None and end_idx is not None

# Read lines of btn_ajustar_semana body from line where observeEvent(input$btn_ajustar_semana starts
obs_start = None
for i in range(start_idx, end_idx):
    if 'observeEvent(input$btn_ajustar_semana, {' in lines[i]:
        obs_start = i
        break

assert obs_start is not None

obs_ajustar_end = None
for i in range(obs_start, end_idx):
    if '# BOTÓN ↺ DESHACER AJUSTE AUTOMÁTICO' in lines[i]:
        obs_ajustar_end = i - 2
        break

assert obs_ajustar_end is not None

ajustar_body_lines = lines[obs_start+1 : obs_ajustar_end]
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

assert target_after_calc in ajustar_body_text, "target_after_calc not found"
ajustar_body_text = ajustar_body_text.replace(target_after_calc, replacement_after_calc, 1)

# Target for valvs_guardar
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

assert target_valvs_save in ajustar_body_text, "target_valvs_save not found"
ajustar_body_text = ajustar_body_text.replace(target_valvs_save, replacement_valvs_save, 1)

# Target for auto_validas empty
target_auto_validas = """      if (nrow(auto_validas) == 0) {
        showNotification("No hay válvulas o días pendientes para autocompletar en esta semana.", type = "warning", duration = 4)
        return()
      }"""

replacement_auto_validas = """      if (nrow(auto_validas) == 0) {
        msg_v <- if (solo_filtrado) "No hay válvulas o días pendientes para autocompletar en la selección filtrada." else "No hay válvulas o días pendientes para autocompletar en esta semana."
        showNotification(msg_v, type = "warning", duration = 4)
        return()
      }"""

assert target_auto_validas in ajustar_body_text, "target_auto_validas not found"
ajustar_body_text = ajustar_body_text.replace(target_auto_validas, replacement_auto_validas, 1)

# Target for ed_conservar
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

assert target_ed_save in ajustar_body_text, "target_ed_save not found"
ajustar_body_text = ajustar_body_text.replace(target_ed_save, replacement_ed_save, 1)

# Target for notification
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

assert target_notif in ajustar_body_text, "target_notif not found"
ajustar_body_text = ajustar_body_text.replace(target_notif, replacement_notif, 1)

print("All targets in ajustar_body_text matched perfectly!")
