load('D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData')
cat('Objects in RData:', ls(), '\n')
if (exists("df_cosecha")) {
  m12 <- df_cosecha[as.character(df_cosecha$Módulo) == '12', ]
  cat('df_cosecha M12 rows:', nrow(m12), '\n')
  print(table(m12$Campaña, m12$Sector))
}
