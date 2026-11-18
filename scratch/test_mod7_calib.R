suppressPackageStartupMessages(library(dplyr))
load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")

cat("=== Checking optimizar_dias_maduracion for all turnos of Modulo 7 VENTURA ===\n")
source("scratch/test_helpers_maduracion.R", local = TRUE)
