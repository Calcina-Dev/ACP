suppressPackageStartupMessages(library(dplyr))

if (file.exists("D:/ACP/Actualizacion Semanal/dias_maduracion_modulo_variedad.rds")) {
  df_m <- readRDS("D:/ACP/Actualizacion Semanal/dias_maduracion_modulo_variedad.rds")
  cat("dias_maduracion_modulo_variedad for Modulo 7:\n")
  print(df_m %>% filter(trimws(as.character(Módulo)) == "7"))
}

if (file.exists("D:/ACP/Actualizacion Semanal/dias_maduracion_turno.rds")) {
  df_t <- readRDS("D:/ACP/Actualizacion Semanal/dias_maduracion_turno.rds")
  cat("\ndias_maduracion_turno for Modulo 7:\n")
  print(df_t %>% filter(trimws(as.character(Módulo)) == "7"))
}
