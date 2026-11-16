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

# Let's extract lines from app.R from 11620 to 12900 into a function and run for M1
# But let's first check what turnos_activos_filtrados() gives when Módulo 1 is filtered!
turnos_activos <- df_proyecciones %>%
  filter(as.character(Campaña) == camp_sel, as.numeric(Semana) == sem_sel, as.character(Módulo) == "1") %>%
  mutate(
    Módulo   = trimws(as.character(Módulo)),
    Turno    = trimws(as.character(Turno)),
    Variedad = limpiar_variedad(Variedad)
  ) %>%
  distinct(Sector, Módulo, Turno, Variedad)

cat("Turnos activos en filtro:\n")
print(turnos_activos)

# Now, let's run the exact simulation code
# Universo base:
source("D:/ACP/Actualizacion Semanal/app.R") # wait! we saw this runs shinyApp at the end.
