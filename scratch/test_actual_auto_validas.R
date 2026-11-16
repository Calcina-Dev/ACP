library(dplyr)
library(tidyr)
load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")

ed_actuales <- readRDS("D:/ACP/Actualizacion Semanal/ediciones_manuales_cosecha.rds")
sem_sel <- 45
camp_sel <- "2026"
dia_inicio_nom <- "Martes"
dia_fin_nom <- "Viernes"
dias_nombres <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")

# Let's inspect what claves_manuales exists
ed_man <- ed_actuales %>% filter(as.character(Campaña) == camp_sel, as.numeric(Semana) == sem_sel)
cat("ed_man rows for week 45:", nrow(ed_man), "\n")
print(ed_man %>% filter(as.character(Módulo) == "1"))
