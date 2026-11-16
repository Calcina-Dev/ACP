load("Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
m2 <- df_cosecha[df_cosecha$Módulo == "2", ]
cat("Turnos M2 en df_cosecha:", sort(unique(as.numeric(m2$Turno))), "\n")
m2_c <- df_conteo_historico[df_conteo_historico$Módulo == "2", ]
cat("Turnos M2 en df_conteo_historico:", sort(unique(as.numeric(m2_c$Turno))), "\n")
