with open('D:/ACP/Actualizacion Semanal/app.R.bak_9_11_floral', 'r', encoding='utf-8') as f:
    text = f.read()

checks = [
    ('cat', 'catastro <- if (exists("df_valvulas") && is.data.frame(df_valvulas)'),
    ('cat_close', '} else {'),
    ('areas', 'areas_cat <- areas_base %>%'),
    ('areas_close', 'summarise(Área_cat = sum(Área, na.rm = TRUE)'),
    ('tratt', 'tratt_cat <- tratamientos_base %>%'),
    ('tratt_close', 'distinct(Módulo_unif, .keep_all = TRUE)'),
    ('rc', 'Turno_num_rc = suppressWarnings(as.numeric(Turno)),'),
    ('rc_close', 'Turno_rc   = as.character(Turno),'),
    ('sap', 'Turno_num_s = suppressWarnings(as.numeric(Turno_Norm)),'),
    ('sap_close', 'Turno_s = as.character(Turno_Norm),'),
    ('ant', 'Turno_num = suppressWarnings(as.numeric(Turno_Norm)),'),
    ('ant_close', 'Turno = trimws(as.character(Turno_Norm)),'),
    ('sap_av', 'Turno_num   = suppressWarnings(as.numeric(Turno_Norm)),'),
    ('sap_av_close', 'Turno       = trimws(as.character(Turno_Norm)),'),
    ('vc', 'valvulas_calc <- df_valvulas %>%'),
    ('vc_turn', 'Turno       = trimws(as.character(Turno)),')
]

for name, s in checks:
    pos = text.find(s)
    print(f'{name}: {pos}')
