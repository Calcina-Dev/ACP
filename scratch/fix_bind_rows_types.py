# -*- coding: utf-8 -*-
import sys

src = "Actualizacion Semanal/app.R"
with open(src, "r", encoding="utf-8") as f:
    text = f.read()

# 1. Fix df_valvs_ajuste_nueva and valvs_guardar
target_1 = """      # Guardar el detalle de válvulas asignadas para la vista de mapa de ruta proyectada
      df_valvs_ajuste_nueva <- registros_todos %>%
        filter(Kg_Asignado > 0, Es_Ruta_Sin_Kg | Dia_Asignado %in% dias_ajustables_nom) %>%
        mutate(
          Campaña         = as.character(input$filtro_campana),
          Semana          = as.numeric(sem_sel),
          Dia_Desde       = dia_inicio_nom,
          Dia_Hasta       = dia_fin_nom,
          Tipo_Asignacion = ifelse(Es_Ruta_Sin_Kg, "ruta_sin_kg", ifelse(coalesce(Num_Pasada, 1L) == 2L, "2da_pasada", "regular")),
          Fecha_Ajuste    = Sys.time()
        ) %>%
        select(Campaña, Semana, id, codigo, Sector, Módulo, Turno, Variedad, Dia_Asignado, Kg_Asignado, Ha_Asignada, Num_Pasada, Tipo_Asignacion, Dia_Desde, Dia_Hasta, Motivo_Ajuste, Fecha_Ajuste)

      valvs_anteriores <- rv_ajuste_valvulas()
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

replacement_1 = """      # Guardar el detalle de válvulas asignadas para la vista de mapa de ruta proyectada
      df_valvs_ajuste_nueva <- registros_todos %>%
        filter(Kg_Asignado > 0, Es_Ruta_Sin_Kg | Dia_Asignado %in% dias_ajustables_nom) %>%
        mutate(
          Campaña         = as.character(input$filtro_campana),
          Semana          = as.numeric(sem_sel),
          Dia_Desde       = as.character(dia_inicio_nom),
          Dia_Hasta       = as.character(dia_fin_nom),
          Tipo_Asignacion = as.character(case_when(
            coalesce(Es_Ruta_Sin_Kg, FALSE) ~ "ruta_sin_kg",
            coalesce(Num_Pasada, 1L) == 2L  ~ "2da_pasada",
            TRUE                            ~ "regular"
          )),
          Motivo_Ajuste   = as.character(Motivo_Ajuste),
          Fecha_Ajuste    = Sys.time()
        ) %>%
        select(Campaña, Semana, id, codigo, Sector, Módulo, Turno, Variedad, Dia_Asignado, Kg_Asignado, Ha_Asignada, Num_Pasada, Tipo_Asignacion, Dia_Desde, Dia_Hasta, Motivo_Ajuste, Fecha_Ajuste)

      valvs_anteriores <- rv_ajuste_valvulas()
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
      valvs_guardar <- if (nrow(df_valvs_ajuste_nueva) > 0 && nrow(valvs_filtradas) > 0) {
        bind_rows(valvs_filtradas, df_valvs_ajuste_nueva)
      } else if (nrow(df_valvs_ajuste_nueva) > 0) {
        df_valvs_ajuste_nueva
      } else {
        valvs_filtradas
      }
      rv_ajuste_valvulas(valvs_guardar)
      guardar_ajuste_valvulas_disco(valvs_guardar)"""

assert target_1 in text, "Target 1 not found"
text = text.replace(target_1, replacement_1, 1)

# 2. Fix auto_ediciones and ed_nuevas_totales
target_2 = """          Tipo_Edicion    = ifelse(any(Es_Ruta_Sin_Kg, na.rm = TRUE), "ruta_sin_kg", "auto"),
          Motivo_Ajuste   = first(na.omit(Motivo_Ajuste)),
          .groups         = "drop"
        ) %>%
        mutate(
          Campaña       = as.character(input$filtro_campana),
          Semana        = sem_sel,
          Fecha_Edicion = Sys.time()
        )"""

replacement_2 = """          Tipo_Edicion    = as.character(ifelse(any(Es_Ruta_Sin_Kg, na.rm = TRUE), "ruta_sin_kg", "auto")),
          Motivo_Ajuste   = as.character(first(na.omit(Motivo_Ajuste))),
          .groups         = "drop"
        ) %>%
        mutate(
          Campaña       = as.character(input$filtro_campana),
          Semana        = sem_sel,
          Fecha_Edicion = Sys.time()
        )"""

assert target_2 in text, "Target 2 not found"
text = text.replace(target_2, replacement_2, 1)

target_3 = """      ed_nuevas_totales <- bind_rows(ed_conservar, auto_validas)
      
      ediciones_manuales(ed_nuevas_totales)
      guardar_ediciones_disco(ed_nuevas_totales)"""

replacement_3 = """      ed_nuevas_totales <- if (nrow(auto_validas) > 0 && nrow(ed_conservar) > 0) {
        bind_rows(ed_conservar, auto_validas)
      } else if (nrow(auto_validas) > 0) {
        auto_validas
      } else {
        ed_conservar
      }
      
      ediciones_manuales(ed_nuevas_totales)
      guardar_ediciones_disco(ed_nuevas_totales)"""

assert target_3 in text, "Target 3 not found"
text = text.replace(target_3, replacement_3, 1)

# 4. Fix ed_finales in ejecutar_borrado_manuales
target_4 = """    ed_finales <- bind_rows(ed_conservar, reg_restaurados)
    ediciones_manuales(ed_finales)
    guardar_ediciones_disco(ed_finales)"""

replacement_4 = """    ed_finales <- if (nrow(reg_restaurados) > 0 && nrow(ed_conservar) > 0) {
      bind_rows(ed_conservar, reg_restaurados)
    } else if (nrow(reg_restaurados) > 0) {
      reg_restaurados
    } else {
      ed_conservar
    }
    ediciones_manuales(ed_finales)
    guardar_ediciones_disco(ed_finales)"""

assert target_4 in text, "Target 4 not found"
text = text.replace(target_4, replacement_4, 1)

with open(src, "w", encoding="utf-8", newline="\r\n") as f:
    f.write(text)

print("All bind_rows type fixes applied successfully!")
