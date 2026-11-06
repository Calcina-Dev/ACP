with open('D:/ACP/Actualizacion Semanal/app.R.bak_sector_m12', 'r', encoding='utf-8', newline='') as f:
    text = f.read()

targets = [
    ('T1', '  if (exists("df_valvulas") && is.data.frame(df_valvulas)) {\r\n    if (!"Grupo" %in% names(df_valvulas)) {'),
    ('T2', '    catastro <- if (exists("df_valvulas") && is.data.frame(df_valvulas) && nrow(df_valvulas) > 0) {\r\n      df_valvulas %>%\r\n        mutate(codigo_norm = norm_codigo_valvula(codigo)) %>%\r\n        distinct(codigo_norm, .keep_all = TRUE) %>%\r\n        select(codigo_norm, Sector, Módulo, Turno, Válvula, Variedad_cat = Variedad, Area_ha = Área)\r\n    } else {'),
    ('T3A', '      df_concil %>%\r\n        filter(Es_Valido_Operativo) %>%\r\n        mutate(\r\n          Sector   = trimws(as.character(Sector)),'),
    ('T3B', '        mutate(\r\n          Dia      = factor(dias_semana[wday(Fecha, week_start = 1)], levels = dias_semana),\r\n          Sector   = trimws(as.character(Sector)),'),
    ('T4', '          codigo_norm = norm_codigo_valvula(coalesce(codigo_norm, codigo)),\r\n          Sector      = trimws(as.character(Sector)),\r\n          Turno_num   = suppressWarnings(as.numeric(Turno)),'),
    ('T5', '      sectores_u <- if (!is.null(u_base) && nrow(u_base) > 0) {\r\n        u_base %>%\r\n          distinct(\r\n            Módulo_norm = trimws(as.character(Módulo)),\r\n            Turno_norm = trimws(as.character(Turno)),\r\n            Var_norm = limpiar_variedad(Variedad),\r\n            Sector_Visual = trimws(as.character(Sector))\r\n          )\r\n      } else {')
]

for name, t in targets:
    print(f"{name}: found = {t in text}")
