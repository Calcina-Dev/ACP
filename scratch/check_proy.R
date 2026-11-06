load('D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData')
if (exists("df_proyecciones")) {
  m12_p <- df_proyecciones[as.character(df_proyecciones$Módulo) == '12', ]
  cat('df_proyecciones M12 rows:', nrow(m12_p), '\n')
  if (nrow(m12_p) > 0) {
    print(table(m12_p$Campaña, m12_p$Sector))
  }
}
