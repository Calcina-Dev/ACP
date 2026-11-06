import sys

file_path = "Actualizacion Semanal/app.R"
with open(file_path, "r", encoding="utf-8") as f:
    content = f.read()

# Make backup first
with open(file_path + ".bak_range_dist", "w", encoding="utf-8") as f:
    f.write(content)

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

replacement1 = """    rc_ha_turno <- if (nrow(rc_sem_u) > 0) {
      rc_temp_u <- rc_sem_u %>%
        filter(!("Tipo_Registro" %in% names(.)) | Tipo_Registro != "Rectificacion")
      
      # Acumular avance ejecutado previo a dia_inicio_u (solo días con kilos o Lunes/Martes en curso)
      ha_prev_vec_u <- numeric(nrow(rc_temp_u))
      for (d_prev in dias_previos_u) {
        col_h <- paste0("Ha_", d_prev)
        col_k <- paste0("Kg_", d_prev)
        v_h   <- if (col_h %in% names(rc_temp_u)) coalesce(rc_temp_u[[col_h]], 0) else 0
        v_k   <- if (col_k %in% names(rc_temp_u)) coalesce(rc_temp_u[[col_k]], 0) else 0
        es_ejec <- (d_prev %in% c("Lunes", "Martes")) | (v_k > 0)
        ha_prev_vec_u <- ha_prev_vec_u + ifelse(es_ejec, v_h, 0)
      }
      rc_temp_u$Ha_Rc_Prev <- ha_prev_vec_u
      
      rc_temp_u %>%
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

replacement1b = """        `Ha Recorrida` = case_when(
          Ha_Valvs_SAP > 0 ~ round(pmin(Área, Ha_Valvs_SAP), 2),
          Ha_Rc > 0        ~ round(pmin(Área, Ha_Rc), 2),
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

replacement2 = """      rc_valv_avance <- if (nrow(rc_sem) > 0 && "codigo" %in% names(rc_sem)) {
        rc_temp <- rc_sem %>% mutate(codigo_norm = norm_codigo_valvula(codigo))
        
        # Identificar día planificado en la ruta de la semana actual
        dias_cols_all <- paste0("Ha_", dias_nombres)
        dias_cols_ex  <- intersect(dias_cols_all, names(rc_temp))
        if (length(dias_cols_ex) > 0) {
          mat_ha <- as.matrix(rc_temp[, dias_cols_ex])
          has_any <- rowSums(mat_ha > 0, na.rm = TRUE) > 0
          max_idx <- max.col(mat_ha, ties.method = "first")
          dias_noms_sub <- sub("^Ha_", "", dias_cols_ex)
          rc_temp$Dia_Ruta_Sem <- ifelse(has_any, dias_noms_sub[max_idx], NA_character_)
        } else {
          rc_temp$Dia_Ruta_Sem <- NA_character_
        }
        
        # Acumular avance ejecutado previo a dia_inicio_nom (solo días con kilos o Lunes/Martes en curso)
        ha_prev_vec <- numeric(nrow(rc_temp))
        kg_prev_vec <- numeric(nrow(rc_temp))
        for (d in dias_previos_nom) {
          col_h <- paste0("Ha_", d)
          col_k <- paste0("Kg_", d)
          val_h <- if (col_h %in% names(rc_temp)) coalesce(rc_temp[[col_h]], 0) else 0
          val_k <- if (col_k %in% names(rc_temp)) coalesce(rc_temp[[col_k]], 0) else 0
          es_ejecutado <- (d %in% c("Lunes", "Martes")) | (val_k > 0)
          ha_prev_vec <- ha_prev_vec + ifelse(es_ejecutado, val_h, 0)
          kg_prev_vec <- kg_prev_vec + val_k
        }
        rc_temp$Ha_Prev_Valv <- ha_prev_vec
        rc_temp$Kg_Prev_Valv <- kg_prev_vec
        
        rc_temp %>%
          group_by(codigo_norm) %>%
          summarise(
            Ha_Prog      = sum(coalesce(Ha_Prev_Valv, 0), na.rm = TRUE),
            Kg_Prog      = sum(coalesce(Kg_Prev_Valv, 0), na.rm = TRUE),
            Ha_Lunes_Rc  = sum(coalesce(Ha_Lunes, 0), na.rm = TRUE),
            Kg_Lunes_Rc  = if ("Kg_Lunes" %in% names(.)) sum(coalesce(Kg_Lunes, 0), na.rm = TRUE) else 0,
            Ha_Martes_Rc = sum(coalesce(Ha_Martes, 0), na.rm = TRUE),
            Kg_Martes_Rc = if ("Kg_Martes" %in% names(.)) sum(coalesce(Kg_Martes, 0), na.rm = TRUE) else 0,
            Dia_Ruta_Sem = first(na.omit(Dia_Ruta_Sem)),
            .groups      = "drop"
          )
      } else tibble(codigo_norm = character(), Ha_Prog = numeric(), Kg_Prog = numeric(), Ha_Lunes_Rc = numeric(), Kg_Lunes_Rc = numeric(), Ha_Martes_Rc = numeric(), Kg_Martes_Rc = numeric(), Dia_Ruta_Sem = character())"""

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

replacement3 = """        left_join(rc_valv_avance, by = "codigo_norm") %>%
        left_join(sap_valv_avance, by = "codigo_norm") %>%
        mutate(
          Ha_Prog      = case_when(
            coalesce(Ha_Prog_Sap, 0) > 0                        ~ pmin(Área, Ha_Prog_Sap),
            coalesce(Kg_Prog_Sap, 0) > 0                        ~ Área,
            (dia_inicio_idx > 1 & coalesce(Ha_Lunes_Rc, 0) > 0) ~ pmin(Área, Ha_Lunes_Rc),
            (dia_inicio_idx > 2 & coalesce(Ha_Martes_Rc, 0) > 0) ~ pmin(Área, Ha_Martes_Rc),
            coalesce(Ha_Prog, 0) > 0 & coalesce(Kg_Prog, 0) > 0 ~ pmin(Área, Ha_Prog),
            TRUE                                                ~ 0
          ),
          Kg_Prog      = case_when(
            coalesce(Kg_Prog_Sap, 0) > 0 ~ Kg_Prog_Sap,
            coalesce(Kg_Prog, 0) > 0     ~ Kg_Prog,
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
          Dia_Natural_Valv        = case_when(
            !is.na(Dia_Ruta_Sem) & nzchar(Dia_Ruta_Sem)   ~ Dia_Ruta_Sem,
            !is.na(Dia_Rect_Ant) & nzchar(Dia_Rect_Ant)   ~ desplazar_dia_frecuencia(Dia_Rect_Ant, Frecuencia_Dias, dias_nombres, 1L, "Lunes", 6L, "Sábado"),
            !is.na(Dia_Auto_Valv) & nzchar(Dia_Auto_Valv) ~ Dia_Auto_Valv,
            !is.na(Dia_Base_Ant) & nzchar(Dia_Base_Ant)   ~ desplazar_dia_frecuencia(Dia_Base_Ant, Frecuencia_Dias, dias_nombres, 1L, "Lunes", 6L, "Sábado"),
            TRUE                                          ~ dia_inicio_nom
          ),
          idx_natural             = match(Dia_Natural_Valv, dias_nombres),
          idx_natural             = ifelse(is.na(idx_natural), dia_inicio_idx, idx_natural),
          # REGLA DE POSICIONAMIENTO EN RANGO [DESDE, HASTA]:
          # Si el día natural está dentro del rango disponible, se respeta.
          # Si cae antes (ej. Martes cuando rango empieza Miércoles), se posiciona en dia_inicio (fecha más próxima).
          # Si cae después (ej. Sábado cuando rango termina Viernes), se posiciona en dia_fin (fecha más próxima).
          idx_asig_rango          = pmin(dia_fin_idx, pmax(dia_inicio_idx, idx_natural)),
          Dia_Habitual_Calc       = dias_nombres[idx_asig_rango],
          Tiene_Ciclo_Previo_Valv = !is.na(Dia_7d) | (!is.na(Dia_Auto_Valv) & nzchar(Dia_Auto_Valv)) | (!is.na(Dia_Ruta_Sem) & nzchar(Dia_Ruta_Sem))
        )"""

content_norm = content.replace("\r\n", "\n")
t1_norm = target1.replace("\r\n", "\n")
r1_norm = replacement1.replace("\r\n", "\n")

t1b_norm = target1b.replace("\r\n", "\n")
r1b_norm = replacement1b.replace("\r\n", "\n")

t2_norm = target2.replace("\r\n", "\n")
r2_norm = replacement2.replace("\r\n", "\n")

t3_norm = target3.replace("\r\n", "\n")
r3_norm = replacement3.replace("\r\n", "\n")

if t1_norm not in content_norm:
    print("Error: Target 1 not found!")
    sys.exit(1)
content_norm = content_norm.replace(t1_norm, r1_norm)

if t1b_norm not in content_norm:
    print("Error: Target 1b not found!")
    sys.exit(1)
content_norm = content_norm.replace(t1b_norm, r1b_norm)

if t2_norm not in content_norm:
    print("Error: Target 2 not found!")
    sys.exit(1)
content_norm = content_norm.replace(t2_norm, r2_norm)

if t3_norm not in content_norm:
    print("Error: Target 3 not found!")
    sys.exit(1)
content_norm = content_norm.replace(t3_norm, r3_norm)

# Restore CRLF
content_final = content_norm.replace("\n", "\r\n")
with open(file_path, "w", encoding="utf-8") as f:
    f.write(content_final)

print("SUCCESS: Range distribution and pending valve detection applied successfully!")
