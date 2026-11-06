with open('D:/ACP/Actualizacion Semanal/app.R.bak_9_11_floral', 'r', encoding='utf-8') as f:
    text = f.read()

# 6. rc_ha_turno
idx = text.find('Turno_num_rc = suppressWarnings(as.numeric(Turno)),')
idx_end = text.find('Turno_rc   = as.character(Turno),', idx)
print("6. rc_ha_turno:")
print(repr(text[idx:idx_end]))

# 7. sap_ha_valvs_turno
idx = text.find('Turno_num_s = suppressWarnings(as.numeric(Turno_Norm)),')
idx_end = text.find('Turno_s = as.character(Turno_Norm),', idx)
print("\n7. sap_ha_valvs_turno:")
print(repr(text[idx:idx_end]))

# 8. valvulas_calc
idx = text.find('valvulas_calc <- df_valvulas %>%')
idx_end = text.find('Turno       = trimws(as.character(Turno)),', idx)
print("\n8. valvulas_calc:")
print(repr(text[idx:idx_end]))

# 9A. Rend_Base_Con_Floral
idx = text.find('Rend_Base_Con_Floral = round(Rend_Base_Turno * Factor_Floral, 2),')
idx_start = text.rfind('\n', 0, idx) + 1
idx_end = text.find('\n', idx) + 1
print("\n9A. Rend_Base_Con_Floral:")
print(repr(text[idx_start:idx_end]))

# 9B. Detalle_Floral
idx = text.find('!Esta_Proyectado | (Turno_Completo_Lun_Mar & !Es_Ciclo_Corto)')
idx_start = text.rfind('\n', 0, idx) + 1
idx_end = text.find('\n', idx) + 1
print("\n9B. Detalle_Floral:")
print(repr(text[idx_start:idx_end]))

# 9C. counts
idx = text.find('n_premios  <- sum(rend_turno$Tipo_Efecto_Floral == "Premio"')
idx_start = text.rfind('\n', 0, idx) + 1
idx_end = text.find('\n', text.find('n_castigos', idx)) + 1
print("\n9C. counts:")
print(repr(text[idx_start:idx_end]))

# 10. deshacer_ajuste
idx = text.find('es_campana_2026 & Módulo %in% c("13.1", "13.2") ~ "13"')
idx_start = text.rfind('\n', 0, text.rfind('Módulo = case_when', 0, idx)) + 1
idx_end = text.find('),', idx) + 2
print("\n10. deshacer_ajuste:")
print(repr(text[idx_start:idx_end]))
