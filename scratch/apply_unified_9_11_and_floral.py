import shutil

src = 'D:/ACP/Actualizacion Semanal/app.R'
bak = 'D:/ACP/Actualizacion Semanal/app.R.bak_9_11_floral'
shutil.copyfile(bak, src)
print("[INFO] Restored clean state from:", bak)

with open(src, 'r', encoding='utf-8', newline='') as f:
    text = f.read()

# 1. df_valvulas initialization
old_1 = '  if (exists("df_valvulas") && is.data.frame(df_valvulas)) {\r\n    df_valvulas <- df_valvulas %>%\r\n      mutate(\r\n        Sector = case_when(\r\n          as.character(Módulo) == "12" ~ "9",\r\n          TRUE ~ as.character(Sector)\r\n        )\r\n      )\r\n    if (!"Grupo" %in% names(df_valvulas)) {'
new_1 = '''  if (exists("df_valvulas") && is.data.frame(df_valvulas)) {\r
    df_valvulas <- df_valvulas %>%\r
      mutate(\r
        Sector = case_when(\r
          as.character(Módulo) %in% c("9", "11", "12") ~ "9",\r
          TRUE ~ as.character(Sector)\r
        ),\r
        Módulo = case_when(\r
          as.character(Módulo) %in% c("9.1", "9.2")   ~ "9",\r
          as.character(Módulo) %in% c("11.1", "11.2") ~ "11",\r
          as.character(Módulo) %in% c("13.1", "13.2") ~ "13",\r
          as.character(Módulo) %in% c("14.1", "14.2") ~ "14",\r
          TRUE ~ as.character(Módulo)\r
        )\r
      )\r
    if (!"Grupo" %in% names(df_valvulas)) {'''
assert old_1 in text, "old_1 not found!"
text = text.replace(old_1, new_1, 1)
print("[OK] Update 1: df_valvulas initialization")

# 2. catastro in obtener_cosecha_conciliada_semana
idx_cat = text.find('catastro <- if (exists("df_valvulas") && is.data.frame(df_valvulas)')
idx_close_cat = text.find('} else {', idx_cat)
new_2 = '''catastro <- if (exists("df_valvulas") && is.data.frame(df_valvulas) && nrow(df_valvulas) > 0) {\r
      df_valvulas %>%\r
        mutate(\r
          codigo_norm = norm_codigo_valvula(codigo),\r
          Sector      = case_when(as.character(Módulo) %in% c("9", "11", "12") ~ "9", TRUE ~ as.character(Sector)),\r
          Módulo      = case_when(\r
            as.character(Módulo) %in% c("9.1", "9.2")   ~ "9",\r
            as.character(Módulo) %in% c("11.1", "11.2") ~ "11",\r
            as.character(Módulo) %in% c("13.1", "13.2") ~ "13",\r
            as.character(Módulo) %in% c("14.1", "14.2") ~ "14",\r
            TRUE ~ as.character(Módulo)\r
          )\r
        ) %>%\r
        distinct(codigo_norm, .keep_all = TRUE) %>%\r
        select(codigo_norm, Sector, Módulo, Turno, Válvula, Variedad_cat = Variedad, Area_ha = Área)\r
    } else {'''

text = text[:idx_cat] + new_2 + text[idx_close_cat + len('} else {'):]
print("[OK] Update 2: catastro in obtener_cosecha_conciliada_semana")

# 3. cosecha_preparada Branch 1
old_3a = '      df_concil %>%\r\n        filter(Es_Valido_Operativo) %>%\r\n        mutate(\r\n          Sector   = case_when(as.character(Módulo) == "12" ~ "9", TRUE ~ trimws(as.character(Sector))),\r\n          Módulo   = trimws(as.character(Módulo)),\r\n          Módulo   = case_when(\r\n            es_campana_2026 & Módulo %in% c("13.1", "13.2") ~ "13",\r\n            es_campana_2026 & Módulo %in% c("14.1", "14.2") ~ "14",\r\n            TRUE ~ Módulo\r\n          ),'
new_3a = '''      df_concil %>%\r
        filter(Es_Valido_Operativo) %>%\r
        mutate(\r
          Sector   = case_when(as.character(Módulo) %in% c("9", "11", "12") ~ "9", TRUE ~ trimws(as.character(Sector))),\r
          Módulo   = trimws(as.character(Módulo)),\r
          Módulo   = case_when(\r
            es_campana_2026 & Módulo %in% c("9.1", "9.2")   ~ "9",\r
            es_campana_2026 & Módulo %in% c("11.1", "11.2") ~ "11",\r
            es_campana_2026 & Módulo %in% c("13.1", "13.2") ~ "13",\r
            es_campana_2026 & Módulo %in% c("14.1", "14.2") ~ "14",\r
            TRUE ~ Módulo\r
          ),'''
assert old_3a in text, "old_3a not found!"
text = text.replace(old_3a, new_3a, 1)
print("[OK] Update 3A: cosecha_preparada Branch 1")

# 4. cosecha_preparada Branch 2
old_3b = '        mutate(\r\n          Dia      = factor(dias_semana[wday(Fecha, week_start = 1)], levels = dias_semana),\r\n          Sector   = case_when(as.character(Módulo) == "12" ~ "9", TRUE ~ trimws(as.character(Sector))),\r\n          Módulo   = trimws(as.character(Módulo)),\r\n          Módulo   = case_when(\r\n            es_campana_2026 & Módulo %in% c("13.1", "13.2") ~ "13",\r\n            es_campana_2026 & Módulo %in% c("14.1", "14.2") ~ "14",\r\n            TRUE ~ Módulo\r\n          ),'
new_3b = '''        mutate(\r\n          Dia      = factor(dias_semana[wday(Fecha, week_start = 1)], levels = dias_semana),\r\n          Sector   = case_when(as.character(Módulo) %in% c("9", "11", "12") ~ "9", TRUE ~ trimws(as.character(Sector))),\r\n          Módulo   = trimws(as.character(Módulo)),\r\n          Módulo   = case_when(\r
            es_campana_2026 & Módulo %in% c("9.1", "9.2")   ~ "9",\r
            es_campana_2026 & Módulo %in% c("11.1", "11.2") ~ "11",\r
            es_campana_2026 & Módulo %in% c("13.1", "13.2") ~ "13",\r
            es_campana_2026 & Módulo %in% c("14.1", "14.2") ~ "14",\r
            TRUE ~ Módulo\r
          ),'''
assert old_3b in text, "old_3b not found!"
text = text.replace(old_3b, new_3b, 1)
print("[OK] Update 3B: cosecha_preparada Branch 2")

# 5. areas_cat & tratt_cat in universo_base
idx_areas = text.find('areas_cat <- areas_base %>%')
idx_areas_close = text.find('summarise(Área_cat = sum(Área, na.rm = TRUE)', idx_areas)
new_areas = '''areas_cat <- areas_base %>%\r
      filter(as.character(Campaña) == as.character(input$filtro_campana)) %>%\r
      mutate(\r
        Módulo = case_when(\r
          es_campana_2026 & as.character(Módulo) %in% c("9.1", "9.2")   ~ "9",\r
          es_campana_2026 & as.character(Módulo) %in% c("11.1", "11.2") ~ "11",\r
          es_campana_2026 & as.character(Módulo) %in% c("13.1", "13.2") ~ "13",\r
          es_campana_2026 & as.character(Módulo) %in% c("14.1", "14.2") ~ "14",\r
          TRUE ~ as.character(Módulo)\r
        )\r
      ) %>%\r
      group_by(\r
        Sector_cat = trimws(as.character(Sector)), \r
        Módulo_cat = trimws(as.character(Módulo)), \r
        Turno_cat  = trimws(as.character(Turno)), \r
        Var_cat    = limpiar_variedad(Variedad)\r
      ) %>%\r
      '''
text = text[:idx_areas] + new_areas + text[idx_areas_close:]

idx_tratt = text.find('tratt_cat <- tratamientos_base %>%')
idx_tratt_close = text.find('distinct(Módulo_unif, .keep_all = TRUE)', idx_tratt)
new_tratt = '''tratt_cat <- tratamientos_base %>%\r
      filter(as.character(Campaña) == as.character(input$filtro_campana)) %>%\r
      mutate(\r
        Módulo_unif = case_when(\r
          es_campana_2026 & Módulo %in% c("9.1", "9.2")   ~ "9",\r
          es_campana_2026 & Módulo %in% c("11.1", "11.2") ~ "11",\r
          es_campana_2026 & Módulo %in% c("13.1", "13.2") ~ "13",\r
          es_campana_2026 & Módulo %in% c("14.1", "14.2") ~ "14",\r
          TRUE ~ Módulo\r
        )\r
      ) %>%\r
      '''
text = text[:idx_tratt] + new_tratt + text[idx_tratt_close:]
print("[OK] Update 5: areas_cat and tratt_cat")

# 6. rc_ha_turno & sap_ha_valvs_turno in universo_base
idx_rc = text.find('Turno_num_rc = suppressWarnings(as.numeric(Turno)),')
idx_rc_close = text.find('Turno_rc   = as.character(Turno),', idx_rc)
new_rc = '''Turno_num_rc = suppressWarnings(as.numeric(Turno)),\r
          Módulo_rc = case_when(\r
            es_campana_act_u & as.character(Módulo) %in% c("9.1", "9.2")   ~ "9",\r
            es_campana_act_u & as.character(Módulo) %in% c("11.1", "11.2") ~ "11",\r
            es_campana_act_u & as.character(Módulo) %in% c("13.1", "13.2") ~ "13",\r
            es_campana_act_u & as.character(Módulo) %in% c("14.1", "14.2") ~ "14",\r
            !es_campana_act_u & as.character(Módulo) == "9" & between(Turno_num_rc, 1, 11)  ~ "9.2",\r
            !es_campana_act_u & as.character(Módulo) == "9" & Turno_num_rc >= 12            ~ "9.1",\r
            !es_campana_act_u & as.character(Módulo) == "11" & between(Turno_num_rc, 1, 12) ~ "11.1",\r
            !es_campana_act_u & as.character(Módulo) == "11" & Turno_num_rc >= 13           ~ "11.2",\r
            TRUE                                                                            ~ as.character(Módulo)\r
          ),\r
          '''
text = text[:idx_rc] + new_rc + text[idx_rc_close:]

idx_sap = text.find('Turno_num_s = suppressWarnings(as.numeric(Turno_Norm)),')
idx_sap_close = text.find('Turno_s   = as.character(Turno_Norm),', idx_sap)
new_sap = '''Turno_num_s = suppressWarnings(as.numeric(Turno_Norm)),\r
          Módulo_s = case_when(\r
            es_campana_2026 & Módulo_Norm %in% c("9.1", "9.2")   ~ "9",\r
            es_campana_2026 & Módulo_Norm %in% c("11.1", "11.2") ~ "11",\r
            es_campana_2026 & Módulo_Norm %in% c("13.1", "13.2") ~ "13",\r
            es_campana_2026 & Módulo_Norm %in% c("14.1", "14.2") ~ "14",\r
            !es_campana_2026 & as.character(Módulo_Norm) == "9" & between(Turno_num_s, 1, 11)  ~ "9.2",\r
            !es_campana_2026 & as.character(Módulo_Norm) == "9" & Turno_num_s >= 12            ~ "9.1",\r
            !es_campana_2026 & as.character(Módulo_Norm) == "11" & between(Turno_num_s, 1, 12) ~ "11.1",\r
            !es_campana_2026 & as.character(Módulo_Norm) == "11" & Turno_num_s >= 13           ~ "11.2",\r
            TRUE                                                                                ~ as.character(Módulo_Norm)\r
          ),\r
          '''
text = text[:idx_sap] + new_sap + text[idx_sap_close:]
print("[OK] Update 6: rc_ha_turno and sap_ha_valvs_turno")

# 7. btn_ajustar_semana: df_ant_valvs, sap_valv_avance, valvulas_calc
idx_ant = text.find('Turno_num = suppressWarnings(as.numeric(Turno_Norm)),')
idx_ant_close = text.find('Turno = trimws(as.character(Turno_Norm)),', idx_ant)
new_ant = '''Turno_num = suppressWarnings(as.numeric(Turno_Norm)),\r
              Módulo = case_when(\r
                es_campana_act & Módulo %in% c("9.1", "9.2")   ~ "9",\r
                es_campana_act & Módulo %in% c("11.1", "11.2") ~ "11",\r
                es_campana_act & Módulo %in% c("13.1", "13.2") ~ "13",\r
                es_campana_act & Módulo %in% c("14.1", "14.2") ~ "14",\r
                !es_campana_act & Módulo == "9" & between(Turno_num, 1, 11)  ~ "9.2",\r
                !es_campana_act & Módulo == "9" & Turno_num >= 12            ~ "9.1",\r
                !es_campana_act & Módulo == "11" & between(Turno_num, 1, 12) ~ "11.1",\r
                !es_campana_act & Módulo == "11" & Turno_num >= 13           ~ "11.2",\r
                TRUE                                                         ~ trimws(as.character(Módulo))\r
              ),\r
              '''
text = text[:idx_ant] + new_ant + text[idx_ant_close:]

idx_sap_av = text.find('Turno_num   = suppressWarnings(as.numeric(Turno_Norm)),')
idx_sap_av_close = text.find('Turno       = trimws(as.character(Turno_Norm)),', idx_sap_av)
new_sap_av = '''Turno_num   = suppressWarnings(as.numeric(Turno_Norm)),\r
          Módulo = case_when(\r
            es_campana_act & Módulo %in% c("9.1", "9.2")   ~ "9",\r
            es_campana_act & Módulo %in% c("11.1", "11.2") ~ "11",\r
            es_campana_act & Módulo %in% c("13.1", "13.2") ~ "13",\r
            es_campana_act & Módulo %in% c("14.1", "14.2") ~ "14",\r
            !es_campana_act & Módulo == "9" & between(Turno_num, 1, 11)  ~ "9.2",\r
            !es_campana_act & Módulo == "9" & Turno_num >= 12            ~ "9.1",\r
            !es_campana_act & Módulo == "11" & between(Turno_num, 1, 12) ~ "11.1",\r
            !es_campana_act & Módulo == "11" & Turno_num >= 13           ~ "11.2",\r
            TRUE                                                         ~ trimws(as.character(Módulo))\r
          ),\r
          '''
text = text[:idx_sap_av] + new_sap_av + text[idx_sap_av_close:]

idx_vc = text.find('valvulas_calc <- df_valvulas %>%')
idx_vc_turn = text.find('Turno       = trimws(as.character(Turno)),', idx_vc)
new_vc = '''valvulas_calc <- df_valvulas %>%\r
        mutate(\r
          codigo_norm = norm_codigo_valvula(coalesce(codigo_norm, codigo)),\r
          Sector      = case_when(as.character(Módulo) %in% c("9", "11", "12") ~ "9", TRUE ~ trimws(as.character(Sector))),\r
          Turno_num   = suppressWarnings(as.numeric(Turno)),\r
          Módulo      = case_when(\r
            es_campana_act & Módulo %in% c("9.1", "9.2")   ~ "9",\r
            es_campana_act & Módulo %in% c("11.1", "11.2") ~ "11",\r
            es_campana_act & Módulo %in% c("13.1", "13.2") ~ "13",\r
            es_campana_act & Módulo %in% c("14.1", "14.2") ~ "14",\r
            !es_campana_act & Módulo == "9" & between(Turno_num, 1, 11)  ~ "9.2",\r
            !es_campana_act & Módulo == "9" & Turno_num >= 12            ~ "9.1",\r
            !es_campana_act & Módulo == "11" & between(Turno_num, 1, 12) ~ "11.1",\r
            !es_campana_act & Módulo == "11" & Turno_num >= 13           ~ "11.2",\r
            TRUE                                                         ~ trimws(as.character(Módulo))\r
          ),\r
          '''
text = text[:idx_vc] + new_vc + text[idx_vc_turn:]
print("[OK] Update 7: btn_ajustar_semana (df_ant_valvs, sap_valv_avance, valvulas_calc)")

# 8. Request 2: Rend_Base_Con_Floral and Detalle_Floral when avance > 30%
old_rend_flor = '          Rend_Base_Con_Floral = round(Rend_Base_Turno * Factor_Floral, 2),\r\n'
new_rend_flor = '''          # REGLA: Cuando el avance es > 30% del turno, NO se aplica factor floral\r
          # porque el rendimiento de la semana actual ya viene afectado por ese factor.\r
          Rend_Base_Con_Floral = case_when(\r
            Tiene_Cosecha_Act & Prop_Avance_Turno > 0.30 ~ round(Rend_Base_Turno, 2),\r
            TRUE                                         ~ round(Rend_Base_Turno * Factor_Floral, 2)\r
          ),\r\n'''

assert old_rend_flor in text, "old_rend_flor not found!"
text = text.replace(old_rend_flor, new_rend_flor, 1)

old_det_flor = '            !Esta_Proyectado | (Turno_Completo_Lun_Mar & !Es_Ciclo_Corto) | (Turno_Completo_Lun_Mar & Es_Ciclo_Corto) ~ "",\r\n'
new_det_flor = '''            !Esta_Proyectado | (Turno_Completo_Lun_Mar & !Es_Ciclo_Corto) | (Turno_Completo_Lun_Mar & Es_Ciclo_Corto) ~ "",\r
            Tiene_Cosecha_Act & Prop_Avance_Turno > 0.30 ~ "",\r\n'''
assert old_det_flor in text, "old_det_flor not found!"
text = text.replace(old_det_flor, new_det_flor, 1)
print("[OK] Update 8: Request 2 implemented (Avance >30% without Factor_Floral and without floral label)")

# 9. n_premios / n_castigos counts excluding avance > 30%
old_counts = '''      n_premios  <- sum(rend_turno$Tipo_Efecto_Floral == "Premio" & rend_turno$Esta_Proyectado, na.rm = TRUE)\r
      n_castigos <- sum(rend_turno$Tipo_Efecto_Floral == "Castigo" & rend_turno$Esta_Proyectado, na.rm = TRUE)'''

new_counts = '''      n_premios  <- sum(rend_turno$Tipo_Efecto_Floral == "Premio" & rend_turno$Esta_Proyectado & !(rend_turno$Tiene_Cosecha_Act & rend_turno$Prop_Avance_Turno > 0.30), na.rm = TRUE)\r
      n_castigos <- sum(rend_turno$Tipo_Efecto_Floral == "Castigo" & rend_turno$Esta_Proyectado & !(rend_turno$Tiene_Cosecha_Act & rend_turno$Prop_Avance_Turno > 0.30), na.rm = TRUE)'''

assert old_counts in text, "old_counts not found!"
text = text.replace(old_counts, new_counts, 1)
print("[OK] Update 9: n_premios / n_castigos counts")

# 10. deshacer_ajuste
old_deshacer = '''        Módulo = case_when(\r
          es_campana_2026 & Módulo %in% c("13.1", "13.2") ~ "13",\r
          es_campana_2026 & Módulo %in% c("14.1", "14.2") ~ "14",\r
          TRUE ~ Módulo\r
        ),'''

new_deshacer = '''        Módulo = case_when(\r
          es_campana_2026 & Módulo %in% c("9.1", "9.2")   ~ "9",\r
          es_campana_2026 & Módulo %in% c("11.1", "11.2") ~ "11",\r
          es_campana_2026 & Módulo %in% c("13.1", "13.2") ~ "13",\r
          es_campana_2026 & Módulo %in% c("14.1", "14.2") ~ "14",\r
          TRUE ~ Módulo\r
        ),'''

assert old_deshacer in text, "old_deshacer not found!"
text = text.replace(old_deshacer, new_deshacer, 1)
print("[OK] Update 10: deshacer_ajuste")

# 11. Replace "11.2" with "11" in lines 634 and 8797
text = text.replace('c("8", "9", "10", "11.2", "12", "13", "14")', 'c("8", "9", "10", "11", "12", "13", "14")')
print("[OK] Update 11: fallback lists 11.2 -> 11")

with open(src, 'w', encoding='utf-8', newline='') as f:
    f.write(text)

print("[SUCCESS] All updates applied to app.R successfully!")
