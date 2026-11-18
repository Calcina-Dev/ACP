source("Actualizacion Semanal/app.R", local = TRUE)

dias_previos_u <- c("Lunes", "Martes", "Miércoles")
cols_ha_prev_u <- paste0("Ha_", dias_previos_u)
cols_ha_sap_exist_u <- intersect(cols_ha_prev_u, names(df_hist_oficial_wide))
cols_kg_sap_exist_u <- intersect(paste0("Kg_", dias_previos_u), names(df_hist_oficial_wide))

df_sap_sem_u <- df_hist_oficial_wide %>%
  filter(as.numeric(Semana) == 46, as.character(Módulo_Norm) == "10", as.character(Turno_Norm) == "18") %>%
  mutate(
    Tiene_Cosecha_Prev_Sap = (if (length(cols_ha_sap_exist_u) > 0) rowSums(across(all_of(cols_ha_sap_exist_u)), na.rm = TRUE) else 0) > 0 |
      (if (length(cols_kg_sap_exist_u) > 0) rowSums(across(all_of(cols_kg_sap_exist_u)), na.rm = TRUE) else 0) > 0
  )

print(df_sap_sem_u %>% select(Codigo_Valv, Area_Ha, Tiene_Cosecha_Prev_Sap, any_of(cols_ha_prev_u)))
cat("Sum Area_Ha with Tiene_Cosecha_Prev_Sap:\n")
print(sum(df_sap_sem_u$Area_Ha[df_sap_sem_u$Tiene_Cosecha_Prev_Sap], na.rm = TRUE))

# Also check Rutas de campo (rc_sem_u)
rc_sem_u <- tryCatch(obtener_rutas_campo_en_vivo(46), error = function(e) cargar_rutas_cosecha()) %>%
  filter(as.numeric(Semana) == 46, as.character(Módulo) == "10", as.character(Turno) == "18")

cat("\nRutas de campo for M10-T18 in S46:\n")
print(rc_sem_u %>% select(any_of(c("Codigo_Valvula", "codigo", "Turno", "Variedad", "Ha_Total_Prog", "Kg_Total_Prog", cols_ha_prev_u, "Ha_Jueves"))))
