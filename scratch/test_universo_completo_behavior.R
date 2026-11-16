library(dplyr)
load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
ediciones_manuales <- readRDS("D:/ACP/Actualizacion Semanal/ediciones_manuales_cosecha.rds")
escenarios_data    <- readRDS("D:/ACP/Actualizacion Semanal/escenarios_ajuste_semanal.rds")

# Look at how universo_completo works in app.R
# Let's inspect the lines in app.R around universo_completo
lines <- readLines("D:/ACP/Actualizacion Semanal/app.R", encoding = "UTF-8")
idx_uc <- grep("universo_completo <- reactive", lines)[1]
cat("universo_completo starts at line:", idx_uc, "\n")
cat(lines[idx_uc:(idx_uc + 75)], sep = "\n")
