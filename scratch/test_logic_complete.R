suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(tibble)
  library(plotly)
})

load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
ediciones <- readRDS("D:/ACP/Actualizacion Semanal/ediciones_manuales_cosecha.rds")
escenarios_file <- "D:/ACP/Actualizacion Semanal/escenarios_ajuste_semanal.rds"
df_esc <- readRDS(escenarios_file)

esc <- df_esc %>% filter(Nombre == "Escenario Martes a Viernes (09-Nov. 14:37)")
if (nrow(esc) == 0) esc <- df_esc[3, ]

cat("Testing complete comparison logic...\n")
cat("Escenario:", esc$Nombre, "Rango:", esc$Rango_Dias, "\n")
