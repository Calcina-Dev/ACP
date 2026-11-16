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
es_campana_act <- TRUE
modo_frec <- "auto"
prop_umbral_cfg <- 0.15
umbral_avance_cfg <- 15

norm_codigo_valvula <- function(x) trimws(toupper(as.character(x)))
limpiar_variedad    <- function(x) trimws(toupper(as.character(x)))

# Let's inspect df_frec_valvs_2sem
# In app.R, df_frec_valvs_2sem is computed by calcular_frecuencias_valvula_2semanas
# Let's see if that function exists
source("D:/ACP/scratch/extract_and_run_ajuste.R") # wait
