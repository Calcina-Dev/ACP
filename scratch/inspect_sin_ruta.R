# inspect_sin_ruta.R
source("D:/ACP/Actualizacion Semanal/app.R", local = TRUE)

df_c <- obtener_cosecha_conciliada_semana(46, "2026")
cat(sprintf("Total filas df_c: %d\n", nrow(df_c)))
cat("Columnas de df_c:\n")
print(names(df_c))

sr_rows <- df_c[df_c$Estado_Concil == "Kilos SAP sin Ruta" | df_c$Estado == "Kilos SAP sin Ruta", ]
cat(sprintf("Filas Kilos SAP sin Ruta: %d\n", nrow(sr_rows)))
print(sr_rows[, intersect(c("Sector", "Módulo", "Turno", "Válvula", "Variedad", "Estado_Concil", "Estado", "Kg_Total_sap", "Kg_Sin_Ruta"), names(df_c))])
