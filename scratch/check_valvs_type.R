v <- readRDS('Actualizacion Semanal/ajuste_valvulas_semana.rds')
cat('Class:', class(v), 'nrow:', nrow(v), '\n')
if (nrow(v) > 0) {
  print(sapply(v, class))
  if ('Tipo_Asignacion' %in% names(v)) {
    print(table(v, useNA='always'))
  }
}
