library(dplyr)
library(tidyr)
load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")

sem_sel  <- 45
sem_ant  <- 44
camp_sel <- "2026"
dia_inicio_nom <- "Martes"
dia_fin_nom    <- "Viernes"
dias_nombres   <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
dias_cols_kg   <- c("Kg_Lunes", "Kg_Martes", "Kg_Miércoles", "Kg_Jueves", "Kg_Viernes", "Kg_Sábado")
dia_inicio_idx <- which(dias_nombres == dia_inicio_nom)[1]
dia_fin_idx    <- which(dias_nombres == dia_fin_nom)[1]
dias_ajustables_nom <- dias_nombres[dia_inicio_idx:dia_fin_idx]

# Helper functions from app.R
norm_codigo_valvula <- function(x) trimws(toupper(as.character(x)))
limpiar_variedad    <- function(x) trimws(toupper(as.character(x)))

# Universo base
df_cosecha_sem <- df_cosecha %>% filter(as.character(Campaña) == camp_sel, as.numeric(Semana) == sem_sel)

# Let's inspect valvulas_calc for Modulo 1
source("D:/ACP/Actualizacion Semanal/app.R") # wait, don't source the whole app because shinyApp runs
