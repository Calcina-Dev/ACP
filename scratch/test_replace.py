import re

file_path = 'D:/ACP/Actualizacion Semanal/app.R'
with open(file_path, 'r', encoding='utf-8') as f:
    content = f.read()

# Replacement 1: df_valvulas initialization
t1 = '  if (exists("df_valvulas") && is.data.frame(df_valvulas)) {\n    if (!"Grupo" %in% names(df_valvulas)) {'
t1_crlf = '  if (exists("df_valvulas") && is.data.frame(df_valvulas)) {\r\n    if (!"Grupo" %in% names(df_valvulas)) {'
r1_crlf = '''  if (exists("df_valvulas") && is.data.frame(df_valvulas)) {\r
    df_valvulas <- df_valvulas %>%\r
      mutate(\r
        Sector = case_when(\r
          as.character(Módulo) == "12" ~ "9",\r
          TRUE ~ as.character(Sector)\r
        )\r
      )\r
    if (!"Grupo" %in% names(df_valvulas)) {'''

print("Target 1 found (CRLF):", t1_crlf in content)

# Replacement 2: catastro in obtener_cosecha_conciliada_semana
t2_crlf = '''    catastro <- if (exists("df_valvulas") && is.data.frame(df_valvulas) && nrow(df_valvulas) > 0) {\r
      df_valvulas %>%\r
        mutate(codigo_norm = norm_codigo_valvula(codigo)) %>%\r
        distinct(codigo_norm, .keep_all = TRUE) %>%\r
        select(codigo_norm, Sector, Módulo, Turno, Válvula, Variedad_cat = Variedad, Area_ha = Área)\r
    }'''

r2_crlf = '''    catastro <- if (exists("df_valvulas") && is.data.frame(df_valvulas) && nrow(df_valvulas) > 0) {\r
      df_valvulas %>%\r
        mutate(\r
          codigo_norm = norm_codigo_valvula(codigo),\r
          Sector      = case_when(as.character(Módulo) == "12" ~ "9", TRUE ~ as.character(Sector))\r
        ) %>%\r
        distinct(codigo_norm, .keep_all = TRUE) %>%\r
        select(codigo_norm, Sector, Módulo, Turno, Válvula, Variedad_cat = Variedad, Area_ha = Área)\r
    }'''

print("Target 2 found (CRLF):", t2_crlf in content)

# Replacement 3: sectores_u in btn_ajustar_semana
t3_crlf = '''      sectores_u <- if (!is.null(u_base) && nrow(u_base) > 0) {\r
        u_base %>%\r
          distinct(\r
            Módulo_norm = trimws(as.character(Módulo)),\r
            Turno_norm = trimws(as.character(Turno)),\r
            Var_norm = limpiar_variedad(Variedad),\r
            Sector_Visual = trimws(as.character(Sector))\r
          )\r
      } else {'''

r3_crlf = '''      sectores_u <- if (!is.null(u_base) && nrow(u_base) > 0) {\r
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

print("Target 3 found (CRLF):", t3_crlf in content)
