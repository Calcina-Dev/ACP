suppressPackageStartupMessages(library(dplyr))

f_rds <- "D:/ACP/Actualizacion Semanal/dias_maduracion_turno.rds"
if (file.exists(f_rds)) {
  df <- readRDS(f_rds)
  cat("Current dias_maduracion_turno.rds:\n")
  print(df)
  
  # Update Turno 4 VENTURA to 49 days, r = 0.991
  df <- df %>%
    mutate(
      Dias_Maduracion = ifelse(Módulo == "7" & Turno == "4" & Variedad == "VENTURA", 49L, Dias_Maduracion),
      Correlacion = ifelse(Módulo == "7" & Turno == "4" & Variedad == "VENTURA", 0.991, Correlacion),
      Fecha_Calibracion = ifelse(Módulo == "7" & Turno == "4" & Variedad == "VENTURA", as.character(Sys.Date()), Fecha_Calibracion)
    )
  saveRDS(df, f_rds)
  cat("\nUpdated dias_maduracion_turno.rds:\n")
  print(readRDS(f_rds))
}
