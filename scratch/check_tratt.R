load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
if (exists("df_tratamientos")) {
  print(table(as.character(df_tratamientos$Campaña), as.character(df_tratamientos$Módulo)))
}
