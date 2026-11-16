suppressMessages(library(dplyr))
e <- new.env()
load("Actualizacion Semanal/datos_maestros_cosecha_2026.RData", envir = e)
print(e$df_manifest_proy %>% filter(semana >= 45))
