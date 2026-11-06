with open('D:/ACP/Actualizacion Semanal/app.R.bak_9_11_floral', 'r', encoding='utf-8') as f:
    text = f.read()

# Let's inspect catastro block:
idx = text.find('catastro <- if (exists("df_valvulas")')
print("--- CATASTRO ---")
print(repr(text[idx:idx+450]))

# Let's inspect areas_cat block:
idx = text.find('areas_cat <- areas_base %>%')
print("--- AREAS_CAT ---")
print(repr(text[idx:idx+350]))

# Let's inspect tratt_cat block:
idx = text.find('tratt_cat <- tratamientos_base %>%')
print("--- TRATT_CAT ---")
print(repr(text[idx:idx+300]))

# Let's inspect rc_ha_turno block:
idx = text.find('rc_ha_turno <-')
if idx == -1:
    idx = text.find('Turno_num_rc')
print("--- RC_HA_TURNO ---")
print(repr(text[idx-50:idx+450]))

# Let's inspect sap_ha_valvs_turno block:
idx = text.find('Turno_num_s = suppressWarnings')
print("--- SAP_HA_VALVS_TURNO ---")
print(repr(text[idx-50:idx+650]))

# Let's inspect df_ant_valvs / btn_ajustar_semana:
idx = text.find('df_ant_valvs <-')
print("--- DF_ANT_VALVS ---")
print(repr(text[idx:idx+650]))

# Let's inspect sap_valv_avance:
idx = text.find('sap_valv_avance <-')
print("--- SAP_VALV_AVANCE ---")
print(repr(text[idx:idx+650]))

# Let's inspect valvulas_calc:
idx = text.find('valvulas_calc <- df_valvulas %>%')
print("--- VALVULAS_CALC ---")
print(repr(text[idx:idx+850]))
