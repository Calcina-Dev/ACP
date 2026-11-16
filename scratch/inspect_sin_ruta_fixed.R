# inspect_sin_ruta_fixed.R
source("D:/ACP/Actualizacion Semanal/app.R", local = TRUE)

df_c <- obtener_cosecha_conciliada_semana(46, "2026")
sr_rows <- df_c %>% filter(Estado_Concil == "Kilos SAP sin Ruta")
cat(sprintf("Filas Kilos SAP sin Ruta: %d\n", nrow(sr_rows)))
print(sr_rows %>% select(Sector, Módulo, Turno, Válvula, Variedad, Estado_Concil, Kg_Total_sap, Kg_Sin_Ruta))
cat(sprintf("Total Kg SAP sin Ruta: %f\n", sum(coalesce(sr_rows$Kg_Sin_Ruta, sr_rows$Kg_Total_sap), na.rm = TRUE)))
