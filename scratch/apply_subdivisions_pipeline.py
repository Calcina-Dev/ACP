import shutil

src = 'D:/ACP/Actualizacion Semanal/app.R'
with open(src, 'r', encoding='utf-8') as f:
    text = f.read()

replacements = []

# 1. df_valvulas initialization
old_1 = '''  if (exists("df_valvulas") && is.data.frame(df_valvulas)) {
    df_valvulas <- df_valvulas %>%
      mutate(
        Sector = case_when(
          as.character(Módulo) %in% c("9", "11", "12") ~ "9",
          TRUE ~ as.character(Sector)
        ),
        Módulo = case_when(
          as.character(Módulo) %in% c("9.1", "9.2")   ~ "9",
          as.character(Módulo) %in% c("11.1", "11.2") ~ "11",
          as.character(Módulo) %in% c("13.1", "13.2") ~ "13",
          as.character(Módulo) %in% c("14.1", "14.2") ~ "14",
          TRUE ~ as.character(Módulo)
        )
      )
    if (!"Grupo" %in% names(df_valvulas)) {'''

new_1 = '''  if (exists("df_valvulas") && is.data.frame(df_valvulas)) {
    df_valvulas <- df_valvulas %>%
      mutate(
        Turno_num = suppressWarnings(as.numeric(Turno)),
        Sector = case_when(
          as.character(Módulo) %in% c("9", "9.1", "9.2", "11", "11.1", "11.2", "12") ~ "9",
          TRUE ~ as.character(Sector)
        ),
        Módulo = case_when(
          as.character(Módulo) %in% c("13.1", "13.2") ~ "13",
          as.character(Módulo) %in% c("14.1", "14.2") ~ "14",
          as.character(Módulo) == "9" & between(Turno_num, 1, 11)  ~ "9.2",
          as.character(Módulo) == "9" & Turno_num >= 12            ~ "9.1",
          as.character(Módulo) == "11" & between(Turno_num, 1, 12) ~ "11.1",
          as.character(Módulo) == "11" & Turno_num >= 13           ~ "11.2",
          TRUE ~ as.character(Módulo)
        )
      ) %>%
      select(-any_of("Turno_num"))
    if (!"Grupo" %in% names(df_valvulas)) {'''
replacements.append(("1. df_valvulas init", old_1, new_1))

# 2. catastro in obtener_cosecha_conciliada_semana
old_2 = '''    catastro <- if (exists("df_valvulas") && is.data.frame(df_valvulas) && nrow(df_valvulas) > 0) {
      df_valvulas %>%
        mutate(
          codigo_norm = norm_codigo_valvula(codigo),
          Sector      = case_when(as.character(Módulo) %in% c("9", "11", "12") ~ "9", TRUE ~ as.character(Sector)),
          Módulo      = case_when(
            as.character(Módulo) %in% c("9.1", "9.2")   ~ "9",
            as.character(Módulo) %in% c("11.1", "11.2") ~ "11",
            as.character(Módulo) %in% c("13.1", "13.2") ~ "13",
            as.character(Módulo) %in% c("14.1", "14.2") ~ "14",
            TRUE ~ as.character(Módulo)
          )
        ) %>%
        distinct(codigo_norm, .keep_all = TRUE) %>%
        select(codigo_norm, Sector, Módulo, Turno, Válvula, Variedad_cat = Variedad, Area_ha = Área)
    } else {'''

new_2 = '''    catastro <- if (exists("df_valvulas") && is.data.frame(df_valvulas) && nrow(df_valvulas) > 0) {
      df_valvulas %>%
        mutate(
          codigo_norm = norm_codigo_valvula(codigo),
          Turno_num   = suppressWarnings(as.numeric(Turno)),
          Sector      = case_when(as.character(Módulo) %in% c("9", "9.1", "9.2", "11", "11.1", "11.2", "12") ~ "9", TRUE ~ as.character(Sector)),
          Módulo      = case_when(
            as.character(Módulo) %in% c("13.1", "13.2") ~ "13",
            as.character(Módulo) %in% c("14.1", "14.2") ~ "14",
            as.character(Módulo) == "9" & between(Turno_num, 1, 11)  ~ "9.2",
            as.character(Módulo) == "9" & Turno_num >= 12            ~ "9.1",
            as.character(Módulo) == "11" & between(Turno_num, 1, 12) ~ "11.1",
            as.character(Módulo) == "11" & Turno_num >= 13           ~ "11.2",
            TRUE ~ as.character(Módulo)
          )
        ) %>%
        distinct(codigo_norm, .keep_all = TRUE) %>%
        select(codigo_norm, Sector, Módulo, Turno, Válvula, Variedad_cat = Variedad, Area_ha = Área)
    } else {'''
replacements.append(("2. catastro", old_2, new_2))

# 3A. cosecha_preparada Branch 1
old_3a = '''      df_concil %>%
        filter(Es_Valido_Operativo) %>%
        mutate(
          Sector   = case_when(as.character(Módulo) %in% c("9", "11", "12") ~ "9", TRUE ~ trimws(as.character(Sector))),
          Módulo   = trimws(as.character(Módulo)),
          Módulo   = case_when(
            es_campana_2026 & Módulo %in% c("9.1", "9.2")   ~ "9",
            es_campana_2026 & Módulo %in% c("11.1", "11.2") ~ "11",
            es_campana_2026 & Módulo %in% c("13.1", "13.2") ~ "13",
            es_campana_2026 & Módulo %in% c("14.1", "14.2") ~ "14",
            TRUE ~ Módulo
          ),'''

new_3a = '''      df_concil %>%
        filter(Es_Valido_Operativo) %>%
        mutate(
          Turno_num = suppressWarnings(as.numeric(Turno)),
          Sector    = case_when(as.character(Módulo) %in% c("9", "9.1", "9.2", "11", "11.1", "11.2", "12") ~ "9", TRUE ~ trimws(as.character(Sector))),
          Módulo    = trimws(as.character(Módulo)),
          Módulo    = case_when(
            es_campana_2026 & Módulo %in% c("13.1", "13.2") ~ "13",
            es_campana_2026 & Módulo %in% c("14.1", "14.2") ~ "14",
            as.character(Módulo) == "9" & between(Turno_num, 1, 11)  ~ "9.2",
            as.character(Módulo) == "9" & Turno_num >= 12            ~ "9.1",
            as.character(Módulo) == "11" & between(Turno_num, 1, 12) ~ "11.1",
            as.character(Módulo) == "11" & Turno_num >= 13           ~ "11.2",
            TRUE ~ Módulo
          ),'''
replacements.append(("3A. cosecha_prep branch 1", old_3a, new_3a))

# 3B. cosecha_preparada Branch 2
old_3b = '''        mutate(
          Dia      = factor(dias_semana[wday(Fecha, week_start = 1)], levels = dias_semana),
          Sector   = case_when(as.character(Módulo) %in% c("9", "11", "12") ~ "9", TRUE ~ trimws(as.character(Sector))),
          Módulo   = trimws(as.character(Módulo)),
          Módulo   = case_when(
            es_campana_2026 & Módulo %in% c("9.1", "9.2")   ~ "9",
            es_campana_2026 & Módulo %in% c("11.1", "11.2") ~ "11",
            es_campana_2026 & Módulo %in% c("13.1", "13.2") ~ "13",
            es_campana_2026 & Módulo %in% c("14.1", "14.2") ~ "14",
            TRUE ~ Módulo
          ),'''

new_3b = '''        mutate(
          Dia       = factor(dias_semana[wday(Fecha, week_start = 1)], levels = dias_semana),
          Turno_num = suppressWarnings(as.numeric(Turno)),
          Sector    = case_when(as.character(Módulo) %in% c("9", "9.1", "9.2", "11", "11.1", "11.2", "12") ~ "9", TRUE ~ trimws(as.character(Sector))),
          Módulo    = trimws(as.character(Módulo)),
          Módulo    = case_when(
            es_campana_2026 & Módulo %in% c("13.1", "13.2") ~ "13",
            es_campana_2026 & Módulo %in% c("14.1", "14.2") ~ "14",
            as.character(Módulo) == "9" & between(Turno_num, 1, 11)  ~ "9.2",
            as.character(Módulo) == "9" & Turno_num >= 12            ~ "9.1",
            as.character(Módulo) == "11" & between(Turno_num, 1, 12) ~ "11.1",
            as.character(Módulo) == "11" & Turno_num >= 13           ~ "11.2",
            TRUE ~ Módulo
          ),'''
replacements.append(("3B. cosecha_prep branch 2", old_3b, new_3b))

# 4. areas_por_turno
old_4 = '''    areas_base %>%
      filter(as.character(Campaña) == as.character(input$filtro_campana)) %>%
      mutate(
        Módulo = case_when(
          es_campana_2026 & Módulo %in% c("9.1", "9.2")   ~ "9",
          es_campana_2026 & Módulo %in% c("11.1", "11.2") ~ "11",
          es_campana_2026 & Módulo %in% c("13.1", "13.2") ~ "13",
          es_campana_2026 & Módulo %in% c("14.1", "14.2") ~ "14",
          TRUE ~ Módulo
        ),'''

new_4 = '''    areas_base %>%
      filter(as.character(Campaña) == as.character(input$filtro_campana)) %>%
      mutate(
        Turno_num = suppressWarnings(as.numeric(Turno)),
        Módulo = case_when(
          es_campana_2026 & Módulo %in% c("13.1", "13.2") ~ "13",
          es_campana_2026 & Módulo %in% c("14.1", "14.2") ~ "14",
          as.character(Módulo) == "9" & between(Turno_num, 1, 11)  ~ "9.2",
          as.character(Módulo) == "9" & Turno_num >= 12            ~ "9.1",
          as.character(Módulo) == "11" & between(Turno_num, 1, 12) ~ "11.1",
          as.character(Módulo) == "11" & Turno_num >= 13           ~ "11.2",
          TRUE ~ Módulo
        ),'''
replacements.append(("4. areas_por_turno", old_4, new_4))

# 5. tratt_cat in universo_base
old_5 = '''    tratt_cat <- tratamientos_base %>%
      filter(as.character(Campaña) == as.character(input$filtro_campana)) %>%
      mutate(
        Módulo_unif = case_when(
          es_campana_2026 & Módulo %in% c("9.1", "9.2")   ~ "9",
          es_campana_2026 & Módulo %in% c("11.1", "11.2") ~ "11",
          es_campana_2026 & Módulo %in% c("13.1", "13.2") ~ "13",
          es_campana_2026 & Módulo %in% c("14.1", "14.2") ~ "14",
          TRUE ~ Módulo
        )
      ) %>%'''

new_5 = '''    tratt_cat <- tratamientos_base %>%
      filter(as.character(Campaña) == as.character(input$filtro_campana)) %>%
      mutate(
        Módulo_unif = case_when(
          es_campana_2026 & Módulo %in% c("13.1", "13.2") ~ "13",
          es_campana_2026 & Módulo %in% c("14.1", "14.2") ~ "14",
          TRUE ~ Módulo
        )
      ) %>%'''
replacements.append(("5. tratt_cat", old_5, new_5))

# 6. rc_ha_turno in universo_base
old_6 = '''Turno_num_rc = suppressWarnings(as.numeric(Turno)),
          Módulo_rc = case_when(
            es_campana_act_u & as.character(Módulo) %in% c("9.1", "9.2")   ~ "9",
            es_campana_act_u & as.character(Módulo) %in% c("11.1", "11.2") ~ "11",
            es_campana_act_u & as.character(Módulo) %in% c("13.1", "13.2") ~ "13",
            es_campana_act_u & as.character(Módulo) %in% c("14.1", "14.2") ~ "14",
            !es_campana_act_u & as.character(Módulo) == "9" & between(Turno_num_rc, 1, 11)  ~ "9.2",
            !es_campana_act_u & as.character(Módulo) == "9" & Turno_num_rc >= 12            ~ "9.1",
            !es_campana_act_u & as.character(Módulo) == "11" & between(Turno_num_rc, 1, 12) ~ "11.1",
            !es_campana_act_u & as.character(Módulo) == "11" & Turno_num_rc >= 13           ~ "11.2",
            TRUE                                                                            ~ as.character(Módulo)
          ),'''

new_6 = '''Turno_num_rc = suppressWarnings(as.numeric(Turno)),
          Módulo_rc = case_when(
            es_campana_act_u & as.character(Módulo) %in% c("13.1", "13.2") ~ "13",
            es_campana_act_u & as.character(Módulo) %in% c("14.1", "14.2") ~ "14",
            as.character(Módulo) == "9" & between(Turno_num_rc, 1, 11)  ~ "9.2",
            as.character(Módulo) == "9" & Turno_num_rc >= 12            ~ "9.1",
            as.character(Módulo) == "11" & between(Turno_num_rc, 1, 12) ~ "11.1",
            as.character(Módulo) == "11" & Turno_num_rc >= 13           ~ "11.2",
            TRUE                                                         ~ as.character(Módulo)
          ),'''
replacements.append(("6. rc_ha_turno", old_6, new_6))

# 7. sap_ha_valvs_turno in universo_base
old_7 = '''Turno_num_s = suppressWarnings(as.numeric(Turno_Norm)),
          Módulo_s = case_when(
            es_campana_2026 & Módulo_Norm %in% c("9.1", "9.2")   ~ "9",
            es_campana_2026 & Módulo_Norm %in% c("11.1", "11.2") ~ "11",
            es_campana_2026 & Módulo_Norm %in% c("13.1", "13.2") ~ "13",
            es_campana_2026 & Módulo_Norm %in% c("14.1", "14.2") ~ "14",
            !es_campana_2026 & as.character(Módulo_Norm) == "9" & between(Turno_num_s, 1, 11)  ~ "9.2",
            !es_campana_2026 & as.character(Módulo_Norm) == "9" & Turno_num_s >= 12            ~ "9.1",
            !es_campana_2026 & as.character(Módulo_Norm) == "11" & between(Turno_num_s, 1, 12) ~ "11.1",
            !es_campana_2026 & as.character(Módulo_Norm) == "11" & Turno_num_s >= 13           ~ "11.2",
            TRUE                                                                                ~ as.character(Módulo_Norm)
          ),'''

new_7 = '''Turno_num_s = suppressWarnings(as.numeric(Turno_Norm)),
          Módulo_s = case_when(
            es_campana_2026 & Módulo_Norm %in% c("13.1", "13.2") ~ "13",
            es_campana_2026 & Módulo_Norm %in% c("14.1", "14.2") ~ "14",
            as.character(Módulo_Norm) == "9" & between(Turno_num_s, 1, 11)  ~ "9.2",
            as.character(Módulo_Norm) == "9" & Turno_num_s >= 12            ~ "9.1",
            as.character(Módulo_Norm) == "11" & between(Turno_num_s, 1, 12) ~ "11.1",
            as.character(Módulo_Norm) == "11" & Turno_num_s >= 13           ~ "11.2",
            TRUE                                                           ~ as.character(Módulo_Norm)
          ),'''
replacements.append(("7. sap_ha_valvs_turno", old_7, new_7))

# 8. valvulas_calc in btn_ajustar_semana
old_8 = '''valvulas_calc <- df_valvulas %>%
        mutate(
          codigo_norm = norm_codigo_valvula(coalesce(codigo_norm, codigo)),
          Sector      = case_when(as.character(Módulo) %in% c("9", "11", "12") ~ "9", TRUE ~ trimws(as.character(Sector))),
          Turno_num   = suppressWarnings(as.numeric(Turno)),
          Módulo      = case_when(
            es_campana_act & Módulo %in% c("9.1", "9.2")   ~ "9",
            es_campana_act & Módulo %in% c("11.1", "11.2") ~ "11",
            es_campana_act & Módulo %in% c("13.1", "13.2") ~ "13",
            es_campana_act & Módulo %in% c("14.1", "14.2") ~ "14",
            !es_campana_act & Módulo == "9" & between(Turno_num, 1, 11)  ~ "9.2",
            !es_campana_act & Módulo == "9" & Turno_num >= 12            ~ "9.1",
            !es_campana_act & Módulo == "11" & between(Turno_num, 1, 12) ~ "11.1",
            !es_campana_act & Módulo == "11" & Turno_num >= 13           ~ "11.2",
            TRUE                                                         ~ trimws(as.character(Módulo))
          ),'''

new_8 = '''valvulas_calc <- df_valvulas %>%
        mutate(
          codigo_norm = norm_codigo_valvula(coalesce(codigo_norm, codigo)),
          Turno_num   = suppressWarnings(as.numeric(Turno)),
          Sector      = case_when(as.character(Módulo) %in% c("9", "9.1", "9.2", "11", "11.1", "11.2", "12") ~ "9", TRUE ~ trimws(as.character(Sector))),
          Módulo      = case_when(
            es_campana_act & Módulo %in% c("13.1", "13.2") ~ "13",
            es_campana_act & Módulo %in% c("14.1", "14.2") ~ "14",
            as.character(Módulo) == "9" & between(Turno_num, 1, 11)  ~ "9.2",
            as.character(Módulo) == "9" & Turno_num >= 12            ~ "9.1",
            as.character(Módulo) == "11" & between(Turno_num, 1, 12) ~ "11.1",
            as.character(Módulo) == "11" & Turno_num >= 13           ~ "11.2",
            TRUE                                                     ~ trimws(as.character(Módulo))
          ),'''
replacements.append(("8. valvulas_calc", old_8, new_8))

# 10A. df_hist in rendimientos historicos
old_10a = '''    df_hist <- df_hist %>%
      mutate(
        Sector = case_when(as.character(Módulo) %in% c("9", "11", "12") ~ "9", TRUE ~ trimws(as.character(Sector))),
        Módulo = trimws(as.character(Módulo)),
        Módulo = case_when(
          es_campana_2026 & Módulo %in% c("9.1", "9.2")   ~ "9",
          es_campana_2026 & Módulo %in% c("11.1", "11.2") ~ "11",
          es_campana_2026 & Módulo %in% c("13.1", "13.2") ~ "13",
          es_campana_2026 & Módulo %in% c("14.1", "14.2") ~ "14",
          TRUE ~ Módulo
        ),'''

new_10a = '''    df_hist <- df_hist %>%
      mutate(
        Turno_num = suppressWarnings(as.numeric(Turno)),
        Sector    = case_when(as.character(Módulo) %in% c("9", "9.1", "9.2", "11", "11.1", "11.2", "12") ~ "9", TRUE ~ trimws(as.character(Sector))),
        Módulo    = trimws(as.character(Módulo)),
        Módulo    = case_when(
          es_campana_2026 & Módulo %in% c("13.1", "13.2") ~ "13",
          es_campana_2026 & Módulo %in% c("14.1", "14.2") ~ "14",
          as.character(Módulo) == "9" & between(Turno_num, 1, 11)  ~ "9.2",
          as.character(Módulo) == "9" & Turno_num >= 12            ~ "9.1",
          as.character(Módulo) == "11" & between(Turno_num, 1, 12) ~ "11.1",
          as.character(Módulo) == "11" & Turno_num >= 13           ~ "11.2",
          TRUE ~ Módulo
        ),'''
replacements.append(("10A. df_hist", old_10a, new_10a))

# 10B. df_ant in rendimientos semana anterior
old_10b = '''    df_ant <- df_ant %>%
      mutate(
        Sector = case_when(as.character(Módulo) %in% c("9", "11", "12") ~ "9", TRUE ~ trimws(as.character(Sector))),
        Módulo = trimws(as.character(Módulo)),
        Módulo = case_when(
          es_campana_2026 & Módulo %in% c("9.1", "9.2")   ~ "9",
          es_campana_2026 & Módulo %in% c("11.1", "11.2") ~ "11",
          es_campana_2026 & Módulo %in% c("13.1", "13.2") ~ "13",
          es_campana_2026 & Módulo %in% c("14.1", "14.2") ~ "14",
          TRUE ~ Módulo
        ),'''

new_10b = '''    df_ant <- df_ant %>%
      mutate(
        Turno_num = suppressWarnings(as.numeric(Turno)),
        Sector    = case_when(as.character(Módulo) %in% c("9", "9.1", "9.2", "11", "11.1", "11.2", "12") ~ "9", TRUE ~ trimws(as.character(Sector))),
        Módulo    = trimws(as.character(Módulo)),
        Módulo    = case_when(
          es_campana_2026 & Módulo %in% c("13.1", "13.2") ~ "13",
          es_campana_2026 & Módulo %in% c("14.1", "14.2") ~ "14",
          as.character(Módulo) == "9" & between(Turno_num, 1, 11)  ~ "9.2",
          as.character(Módulo) == "9" & Turno_num >= 12            ~ "9.1",
          as.character(Módulo) == "11" & between(Turno_num, 1, 12) ~ "11.1",
          as.character(Módulo) == "11" & Turno_num >= 13           ~ "11.2",
          TRUE ~ Módulo
        ),'''
replacements.append(("10B. df_ant", old_10b, new_10b))

# Execute replacements
for name, o, n in replacements:
    count = text.count(o)
    assert count == 1, f"Failed for {name}: count is {count}"
    text = text.replace(o, n, 1)
    print(f"[OK] Applied: {name}")

with open(src, 'w', encoding='utf-8', newline='\r\n') as f:
    f.write(text)

print("[SUCCESS] app.R successfully updated with consistent subdivisions for 9.1/9.2 and 11.1/11.2!")
