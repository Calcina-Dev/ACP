import shutil

src = 'D:/ACP/Actualizacion Semanal/app.R'
bak = 'D:/ACP/Actualizacion Semanal/app.R.bak_9_11_floral'
shutil.copyfile(bak, src)

with open(src, 'r', encoding='utf-8', newline='') as f:
    text = f.read()

def check(step):
    idx = text.find('Rend_Base_Con_Floral')
    print(f"Step {step}: 'Rend_Base_Con_Floral' at {idx}")

check("initial")

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
assert old_1 in text
text = text.replace(old_1, new_1, 1)
check(1)

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
check(2)

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
assert old_3a in text
text = text.replace(old_3a, new_3a, 1)
check("3A")

old_3b = '        mutate(\r\n          Dia      = factor(dias_semana[wday(Fecha, week_start = 1)], levels = dias_semana),\r\n          Sector   = case_when(as.character(Módulo) == "12" ~ "9", TRUE ~ trimws(as.character(Sector))),\r\n          Módulo   = trimws(as.character(Módulo)),\r\n          Módulo   = case_when(\r\n            es_campana_2026 & Módulo %in% c("13.1", "13.2") ~ "13",\r\n            es_campana_2026 & Módulo %in% c("14.1", "14.2") ~ "14",\r\n            TRUE ~ Módulo\r\n          ),'
new_3b = '''        mutate(\r\n          Dia      = factor(dias_semana[wday(Fecha, week_start = 1)], levels = dias_semana),\r\n          Sector   = case_when(as.character(Módulo) %in% c("9", "11", "12") ~ "9", TRUE ~ trimws(as.character(Sector))),\r\n          Módulo   = trimws(as.character(Módulo)),\r\n          Módulo   = case_when(\r
            es_campana_2026 & Módulo %in% c("9.1", "9.2")   ~ "9",\r
            es_campana_2026 & Módulo %in% c("11.1", "11.2") ~ "11",\r
            es_campana_2026 & Módulo %in% c("13.1", "13.2") ~ "13",\r
            es_campana_2026 & Módulo %in% c("14.1", "14.2") ~ "14",\r
            TRUE ~ Módulo\r
          ),'''
assert old_3b in text
text = text.replace(old_3b, new_3b, 1)
check("3B")

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
check("5 areas")

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
check("5 tratt")

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
check("6 rc")

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
check("6 sap")

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
check("7 ant")

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
check("7 sap av")

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
check("7 vc")
