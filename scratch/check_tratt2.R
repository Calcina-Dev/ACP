load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
if (exists("df_tratamientos")) {
  print(head(df_tratamientos, 10))
  print(names(df_tratamientos))
}
