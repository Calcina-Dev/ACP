# inspect_valve_45.R
source("D:/ACP/Actualizacion Semanal/app.R", local = TRUE)

df_c <- obtener_cosecha_conciliada_semana(46, "2026")
v45 <- df_c %>% filter(as.character(Módulo) == "10", as.character(Válvula) == "45")
cat("Válvula 45 Módulo 10:\n")
print(v45 %>% select(Sector, Módulo, Turno, Válvula, Variedad, Estado_Concil, Tiene_Campo, Tiene_SAP, Kg_Total_sap, Kg_Sin_Ruta, Kg_Lunes_sap, Ha_Lunes_rc))
