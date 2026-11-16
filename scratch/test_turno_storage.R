library(dplyr)

# Test functions
cargar_maduracion_turno_disco <- function() {
  rutas <- c("D:/ACP/Actualizacion Semanal/dias_maduracion_turno.rds", "dias_maduracion_turno.rds")
  for (r in rutas) {
    if (file.exists(r)) {
      df <- tryCatch(readRDS(r), error = function(e) NULL)
      if (!is.null(df) && is.data.frame(df)) return(df)
    }
  }
  tibble(Módulo = character(), Turno = character(), Variedad = character(), Dias_Maduracion = integer(), Correlacion = numeric(), Fecha_Calibracion = character())
}

guardar_maduracion_turno_disco <- function(df_mad_t) {
  if (is.null(df_mad_t) || !is.data.frame(df_mad_t)) return(FALSE)
  tryCatch(saveRDS(df_mad_t, "D:/ACP/Actualizacion Semanal/dias_maduracion_turno.rds"), error = function(e) NULL)
  tryCatch(saveRDS(df_mad_t, "dias_maduracion_turno.rds"), error = function(e) NULL)
  return(TRUE)
}

df_test <- tibble(
  Módulo = "2", Turno = "1", Variedad = "SEKOYA POP", Dias_Maduracion = 77L, Correlacion = 0.791, Fecha_Calibracion = as.character(Sys.Date())
)
guardar_maduracion_turno_disco(df_test)
loaded <- cargar_maduracion_turno_disco()
cat("Loaded rows:", nrow(loaded), "Módulo:", loaded$Módulo[1], "Dias:", loaded$Dias_Maduracion[1], "\n")
