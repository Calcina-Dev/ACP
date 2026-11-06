ed <- readRDS('Actualizacion Semanal/ediciones_manuales_cosecha.rds')
cat('Names:', paste(names(ed), collapse=', '), '
')
if ('Tipo_Edicion' %in% names(ed)) {
  print(table(as.character(ed), useNA='always'))
}
cat('Total rows:', nrow(ed), '
')
print(head(as.data.frame(ed), 3))
