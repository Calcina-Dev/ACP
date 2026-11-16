suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
})

load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
ediciones <- readRDS("D:/ACP/Actualizacion Semanal/ediciones_manuales_cosecha.rds")
escenarios_file <- "D:/ACP/Actualizacion Semanal/escenarios_ajuste_semanal.rds"
df_esc <- readRDS(escenarios_file)

esc_3 <- df_esc %>% filter(grepl("Martes a Viernes", Nombre))
ed_3 <- esc_3$Ediciones_Data[[1]]

cat("Módulos en ed_3:\n")
print(unique(ed_3$Módulo))
