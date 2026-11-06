import sys

with open("Actualizacion Semanal/app.R", "r", encoding="utf-8") as f:
    content = f.read()

target1 = """    rc_ha_turno <- if (nrow(rc_sem_u) > 0) {
      cols_ha_rc_exist_u <- intersect(cols_ha_prev_u, names(rc_sem_u))
      rc_sem_u %>%
        filter(!("Tipo_Registro" %in% names(.)) | Tipo_Registro != "Rectificacion") %>%
        mutate(
          Turno_num_rc = suppressWarnings(as.numeric(Turno)),
          Módulo_rc = case_when(
            es_campana_act_u & as.character(Módulo) %in% c("13.1", "13.2") ~ "13",
            es_campana_act_u & as.character(Módulo) %in% c("14.1", "14.2") ~ "14",
            as.character(Módulo) == "9" & between(Turno_num_rc, 1, 11)  ~ "9.2",
            as.character(Módulo) == "9" & Turno_num_rc >= 12            ~ "9.1",
            as.character(Módulo) == "11" & between(Turno_num_rc, 1, 12) ~ "11.1",
            as.character(Módulo) == "11" & Turno_num_rc >= 13           ~ "11.2",
            TRUE                                                         ~ as.character(Módulo)
          ),
          Turno_rc   = as.character(Turno),
          Var_rc     = limpiar_variedad(Variedad),
          Ha_Rc_Prev = if (length(cols_ha_rc_exist_u) > 0) {
            rowSums(across(all_of(cols_ha_rc_exist_u)), na.rm = TRUE)
          } else if ("Ha_Total_Prog" %in% names(.)) {
            coalesce(Ha_Total_Prog, 0)
          } else 0,
          kg_tot_rc  = if ("Kg_Total_Prog" %in% names(.)) coalesce(Kg_Total_Prog, 0) else 0,
          num_pas_rc = if ("Num_Pasada" %in% names(.)) suppressWarnings(as.numeric(Num_Pasada)) else 1
        ) %>%
        group_by(Módulo_rc, Turno_rc, Var_rc) %>%
        summarise(
          Ha_Rc          = sum(coalesce(Ha_Rc_Prev, 0), na.rm = TRUE),
          Ha_Rc_2da      = sum(ifelse(coalesce(num_pas_rc, 1) == 2, coalesce(Ha_Rc_Prev, 0), 0), na.rm = TRUE),
          Kg_Real_1ra_rc = sum(ifelse(num_pas_rc == 1, kg_tot_rc, 0), na.rm = TRUE),
          Kg_Real_2da_rc = sum(ifelse(num_pas_rc == 2, kg_tot_rc, 0), na.rm = TRUE),
          .groups        = "drop"
        )
    } else tibble(Módulo_rc = character(), Turno_rc = character(), Var_rc = character(), Ha_Rc = numeric(), Ha_Rc_2da = numeric(), Kg_Real_1ra_rc = numeric(), Kg_Real_2da_rc = numeric())"""

target1b = """        `Ha Recorrida` = case_when(
          Ha_Rc > 0        ~ round(Ha_Rc, 2),
          Ha_Valvs_SAP > 0 ~ round(pmin(Área, Ha_Valvs_SAP), 2),
          es_remanente_u   ~ round(pmin(Área, kg_sap_act / pmax(1, rend_proy_u)), 2),
          kg_sap_act > 0   ~ round(pmin(Área, kg_sap_act / pmax(1, rend_proy_u)), 2),
          TRUE             ~ 0
        ),"""

target2 = """      rc_valv_avance <- if (nrow(rc_sem) > 0 && "codigo" %in% names(rc_sem)) {
        rc_sem %>%
          mutate(
            codigo_norm  = norm_codigo_valvula(codigo),
            Ha_Prev_Valv = if (length(cols_ha_rc_exist) > 0) rowSums(across(all_of(cols_ha_rc_exist)), na.rm = TRUE) else 0,
            Kg_Prev_Valv = if (length(cols_kg_rc_exist) > 0) rowSums(across(all_of(cols_kg_rc_exist)), na.rm = TRUE) else 0
          ) %>%
          group_by(codigo_norm) %>%
          summarise(
            Ha_Prog      = sum(coalesce(Ha_Prev_Valv, 0), na.rm = TRUE),
            Kg_Prog      = sum(coalesce(Kg_Prev_Valv, 0), na.rm = TRUE),
            Ha_Lunes_Rc  = sum(coalesce(Ha_Lunes, 0), na.rm = TRUE),
            Kg_Lunes_Rc  = if ("Kg_Lunes" %in% names(.)) sum(coalesce(Kg_Lunes, 0), na.rm = TRUE) else 0,
            Ha_Martes_Rc = sum(coalesce(Ha_Martes, 0), na.rm = TRUE),
            Kg_Martes_Rc = if ("Kg_Martes" %in% names(.)) sum(coalesce(Kg_Martes, 0), na.rm = TRUE) else 0,
            .groups      = "drop"
          )
      } else tibble(codigo_norm = character(), Ha_Prog = numeric(), Kg_Prog = numeric(), Ha_Lunes_Rc = numeric(), Kg_Lunes_Rc = numeric(), Ha_Martes_Rc = numeric(), Kg_Martes_Rc = numeric())"""

target3 = """        left_join(rc_valv_avance, by = "codigo_norm") %>%
        left_join(sap_valv_avance, by = "codigo_norm") %>%
        mutate(
          Ha_Prog      = case_when(
            coalesce(Ha_Prog, 0) > 0     ~ Ha_Prog,
            coalesce(Ha_Prog_Sap, 0) > 0 ~ pmin(Área, Ha_Prog_Sap),
            TRUE                         ~ 0
          ),
          Kg_Prog      = case_when(
            coalesce(Kg_Prog, 0) > 0     ~ Kg_Prog,
            coalesce(Kg_Prog_Sap, 0) > 0 ~ Kg_Prog_Sap,
            TRUE                         ~ 0
          ),
          Ha_Lunes_Rc   = pmax(coalesce(Ha_Lunes_Rc, 0), coalesce(Ha_Lunes_Sap, 0)),
          Ha_Martes_Rc  = pmax(coalesce(Ha_Martes_Rc, 0), coalesce(Ha_Martes_Sap, 0)),
          Kg_Lunes_Valv = coalesce(Kg_Lunes_Sap, 0) + ifelse(coalesce(Kg_Lunes_Sap, 0) == 0, coalesce(Kg_Lunes_Rc, 0), 0),
          Kg_Martes_Valv = coalesce(Kg_Martes_Sap, 0) + ifelse(coalesce(Kg_Martes_Sap, 0) == 0, coalesce(Kg_Martes_Rc, 0), 0),
          Ha_Faltante   = pmax(0, Área - Ha_Prog)
        ) %>%
        left_join(valv_7d_map, by = "codigo_norm") %>%
        left_join(turno_dia_habitual, by = c("Módulo", "Turno")) %>%
        mutate(
          Kg_Total_Ant            = coalesce(Kg_Total_Ant, 0),
          Ha_Total_Ant            = coalesce(Ha_Total_Ant, 0),
          Dia_Base_Ant            = coalesce(Dia_7d, Dia_Habitual),
          Dia_Habitual_Calc       = case_when(
            !is.na(Dia_Rect_Ant) & nzchar(Dia_Rect_Ant) ~ desplazar_dia_frecuencia(Dia_Rect_Ant, Frecuencia_Dias, dias_nombres, dia_inicio_idx, dia_inicio_nom, dia_fin_idx, dia_fin_nom),
            !is.na(Dia_Auto_Valv) & nzchar(Dia_Auto_Valv) ~ {
              idx_auto <- match(Dia_Auto_Valv, dias_nombres)
              case_when(
                !is.na(idx_auto) & idx_auto < dia_inicio_idx ~ dia_inicio_nom,
                !is.na(idx_auto) & idx_auto > dia_fin_idx    ~ dia_fin_nom,
                TRUE                                         ~ Dia_Auto_Valv
              )
            },
            TRUE ~ desplazar_dia_frecuencia(Dia_Base_Ant, Frecuencia_Dias, dias_nombres, dia_inicio_idx, dia_inicio_nom, dia_fin_idx, dia_fin_nom)
          ),
          Tiene_Ciclo_Previo_Valv = !is.na(Dia_7d) | (!is.na(Dia_Auto_Valv) & nzchar(Dia_Auto_Valv))
        )"""

# Normalize CRLF
content_norm = content.replace("\r\n", "\n")
t1_norm = target1.replace("\r\n", "\n")
t1b_norm = target1b.replace("\r\n", "\n")
t2_norm = target2.replace("\r\n", "\n")
t3_norm = target3.replace("\r\n", "\n")

print("Target 1 found:", t1_norm in content_norm)
print("Target 1b found:", t1b_norm in content_norm)
print("Target 2 found:", t2_norm in content_norm)
print("Target 3 found:", t3_norm in content_norm)
