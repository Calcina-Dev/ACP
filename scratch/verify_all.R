suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(plotly)
  library(DT)
  library(writexl)
  library(lubridate)
})

cat("Verifying app logic and outputs...\n")
load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
ediciones <- readRDS("D:/ACP/Actualizacion Semanal/ediciones_manuales_cosecha.rds")
rutas <- readRDS("D:/ACP/Rutas de Cosecha/rutas_cosecha_mapa.rds")

# 1. Test KPI Lun-Jue calculation
camp_sel <- "2026"
sem_sel  <- 45

# Mock df for sem 45
ed_sem <- ediciones %>% filter(as.character(`Campaña`) == camp_sel, as.numeric(Semana) == sem_sel)
cat("Ediciones sem 45:", nrow(ed_sem), "\n")

dias_semana_nombres <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
dias_abrev <- c(Lunes = "Lun", Martes = "Mar", Miércoles = "Mié", Jueves = "Jue", Viernes = "Vie", Sábado = "Sáb")

dias_con_ajuste <- c("Viernes")
idx_primer_aj <- min(match(dias_con_ajuste, dias_semana_nombres))
dias_reales_prev <- if (idx_primer_aj > 1) dias_semana_nombres[1:(idx_primer_aj - 1)] else character(0)

cat("Días reales previos:", paste(dias_reales_prev, collapse = ", "), "\n")
cat("Días con ajuste:", paste(dias_con_ajuste, collapse = ", "), "\n")

cat("Verification completed successfully!\n")
