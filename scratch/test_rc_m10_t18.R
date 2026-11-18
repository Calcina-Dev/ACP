source("Actualizacion Semanal/app.R", local = TRUE)

rc_sem_u <- tryCatch(obtener_rutas_campo_en_vivo(46), error = function(e) cargar_rutas_cosecha()) %>%
  filter(as.numeric(Semana) == 46, as.character(Módulo) == "10", as.character(Turno) == "18")

print(rc_sem_u %>% select(Codigo_Valvula, Ha_Total_Prog, Kg_Total_Prog, Ha_Martes, Ha_Miércoles, Ha_Jueves))
