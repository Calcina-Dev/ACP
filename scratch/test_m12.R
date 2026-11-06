tryCatch({
  load('D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData')
  library(dplyr)
  cat('--- df_proyecciones M12 ---\n')
  if (exists('df_proyecciones')) {
    print(head(df_proyecciones %>% filter(as.character(Módulo) == '12') %>% select(any_of(c('Sector', 'Módulo', 'Turno', 'Variedad', 'Semana', 'Proyección', 'Proyeccion')))))
  }
  cat('--- df_cosecha M12 S12 ---\n')
  if (exists('df_cosecha')) {
    print(head(df_cosecha %>% filter(as.character(Módulo) == '12') %>% select(any_of(c('Sector', 'Módulo', 'Turno', 'Variedad', 'Semana', 'Kg Total')))))
  }
  cat('--- df_valvulas M12 ---\n')
  if (exists('df_valvulas')) {
    print(head(df_valvulas %>% filter(as.character(Módulo) == '12') %>% select(any_of(c('Sector', 'Módulo', 'Turno', 'Variedad', 'Codigo_Valvula')))))
  }
}, error = function(e) {
  cat('ERROR:', conditionMessage(e), '\n')
})
