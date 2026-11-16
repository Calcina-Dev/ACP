suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(tibble)
})

load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
ediciones <- readRDS("D:/ACP/Actualizacion Semanal/ediciones_manuales_cosecha.rds")
rutas <- readRDS("D:/ACP/Rutas de Cosecha/rutas_cosecha_mapa.rds")
escenarios_file <- "D:/ACP/Actualizacion Semanal/escenarios_ajuste_semanal.rds"
df_esc <- if (file.exists(escenarios_file)) readRDS(escenarios_file) else tibble()

cat("Escenarios disponibles:", nrow(df_esc), "\n")
if (nrow(df_esc) > 0) {
  print(df_esc %>% select(Id_Escenario, Nombre, Rango_Dias, Total_Kg_Ajustados))
}
