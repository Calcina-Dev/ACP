import shutil

src = 'D:/ACP/Actualizacion Semanal/app.R'
bak = 'D:/ACP/Actualizacion Semanal/app.R.bak_sector_m12'
shutil.copyfile(bak, src)
print("[INFO] Restored clean state from:", bak)

with open(src, 'r', encoding='utf-8', newline='') as f:
    text = f.read()

# 1. Update 1: df_valvulas initialization
old_1 = '  if (exists("df_valvulas") && is.data.frame(df_valvulas)) {\r\n    if (!"Grupo" %in% names(df_valvulas)) {'
new_1 = '''  if (exists("df_valvulas") && is.data.frame(df_valvulas)) {\r
    df_valvulas <- df_valvulas %>%\r
      mutate(\r
        Sector = case_when(\r
          as.character(Módulo) == "12" ~ "9",\r
          TRUE ~ as.character(Sector)\r
        )\r
      )\r
    if (!"Grupo" %in% names(df_valvulas)) {'''
assert old_1 in text, "old_1 not found!"
text = text.replace(old_1, new_1, 1)
print("[OK] Update 1 applied (df_valvulas initialization)")

# 2. Update 2: catastro in obtener_cosecha_conciliada_semana
old_2 = '    catastro <- if (exists("df_valvulas") && is.data.frame(df_valvulas) && nrow(df_valvulas) > 0) {\r\n      df_valvulas %>%\r\n        mutate(codigo_norm = norm_codigo_valvula(codigo)) %>%\r\n        distinct(codigo_norm, .keep_all = TRUE) %>%\r\n        select(codigo_norm, Sector, Módulo, Turno, Válvula, Variedad_cat = Variedad, Area_ha = Área)\r\n    } else {'
new_2 = '''    catastro <- if (exists("df_valvulas") && is.data.frame(df_valvulas) && nrow(df_valvulas) > 0) {\r
      df_valvulas %>%\r
        mutate(\r
          codigo_norm = norm_codigo_valvula(codigo),\r
          Sector      = case_when(as.character(Módulo) == "12" ~ "9", TRUE ~ as.character(Sector))\r
        ) %>%\r
        distinct(codigo_norm, .keep_all = TRUE) %>%\r
        select(codigo_norm, Sector, Módulo, Turno, Válvula, Variedad_cat = Variedad, Area_ha = Área)\r
    } else {'''
assert old_2 in text, "old_2 not found!"
text = text.replace(old_2, new_2, 1)
print("[OK] Update 2 applied (catastro in obtener_cosecha_conciliada_semana)")

# 3. Update 3A: cosecha_preparada Branch 1
old_3a = '      df_concil %>%\r\n        filter(Es_Valido_Operativo) %>%\r\n        mutate(\r\n          Sector   = trimws(as.character(Sector)),'
new_3a = '''      df_concil %>%\r
        filter(Es_Valido_Operativo) %>%\r
        mutate(\r
          Sector   = case_when(as.character(Módulo) == "12" ~ "9", TRUE ~ trimws(as.character(Sector))),'''
assert old_3a in text, "old_3a not found!"
text = text.replace(old_3a, new_3a, 1)
print("[OK] Update 3A applied (cosecha_preparada branch 1)")

# 4. Update 3B: cosecha_preparada Branch 2
old_3b = '        mutate(\r\n          Dia      = factor(dias_semana[wday(Fecha, week_start = 1)], levels = dias_semana),\r\n          Sector   = trimws(as.character(Sector)),'
new_3b = '''        mutate(\r
          Dia      = factor(dias_semana[wday(Fecha, week_start = 1)], levels = dias_semana),\r
          Sector   = case_when(as.character(Módulo) == "12" ~ "9", TRUE ~ trimws(as.character(Sector))),'''
assert old_3b in text, "old_3b not found!"
text = text.replace(old_3b, new_3b, 1)
print("[OK] Update 3B applied (cosecha_preparada branch 2)")

# 5. Update 4: valvulas_calc in btn_ajustar_semana
old_4 = '          codigo_norm = norm_codigo_valvula(coalesce(codigo_norm, codigo)),\r\n          Sector      = trimws(as.character(Sector)),\r\n          Turno_num   = suppressWarnings(as.numeric(Turno)),'
new_4 = '''          codigo_norm = norm_codigo_valvula(coalesce(codigo_norm, codigo)),\r
          Sector      = case_when(as.character(Módulo) == "12" ~ "9", TRUE ~ trimws(as.character(Sector))),\r
          Turno_num   = suppressWarnings(as.numeric(Turno)),'''
assert old_4 in text, "old_4 not found!"
text = text.replace(old_4, new_4, 1)
print("[OK] Update 4 applied (valvulas_calc Sector normalization)")

# 6. Update 5: sectores_u in btn_ajustar_semana
old_5 = '      sectores_u <- if (!is.null(u_base) && nrow(u_base) > 0) {\r\n        u_base %>%\r\n          distinct(\r\n            Módulo_norm = trimws(as.character(Módulo)),\r\n            Turno_norm = trimws(as.character(Turno)),\r\n            Var_norm = limpiar_variedad(Variedad),\r\n            Sector_Visual = trimws(as.character(Sector))\r\n          )\r\n      } else {'
new_5 = '''      sectores_u <- if (!is.null(u_base) && nrow(u_base) > 0) {\r
        u_base %>%\r
          group_by(\r
            Módulo_norm = trimws(as.character(Módulo)),\r
            Turno_norm  = trimws(as.character(Turno)),\r
            Var_norm    = limpiar_variedad(Variedad)\r
          ) %>%\r
          summarise(\r
            Sector_Visual = first(na.omit(trimws(as.character(Sector)))),\r
            .groups = "drop"\r
          )\r
      } else {'''
assert old_5 in text, "old_5 not found!"
text = text.replace(old_5, new_5, 1)
print("[OK] Update 5 applied (sectores_u 1:1 grouping)")

with open(src, 'w', encoding='utf-8', newline='') as f:
    f.write(text)

print("[SUCCESS] app.R successfully updated!")
