lines <- readLines("Actualizacion Semanal/app.R", encoding = "UTF-8")

cat("Line of ui_kpi_factor_floral_semana in UI:\n")
print(grep('uiOutput\\("ui_kpi_factor_floral_semana"\\)', lines))

cat("Line of factor_medio in ui_kpi_factor_floral_semana:\n")
print(grep('factor_medio   <- mean\\(df_fac\\$Ajuste_Floral_Pct', lines))

cat("Line of pct_cast_txt in abrir_modal_diagnostico_turno:\n")
print(grep('pct_cast_txt <- if \\(grepl\\("Castigo floral', lines))

cat("Line of Rend_Base_Con_Floral in ejecutar_ajuste_semana:\n")
print(grep('Rend_Base_Con_Floral = case_when', lines))

cat("Line of turno_avance_ha in ejecutar_ajuste_semana:\n")
print(grep('turno_avance_ha <- valvulas_calc %>%', lines))
