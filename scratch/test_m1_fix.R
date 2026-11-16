library(dplyr)
library(tidyr)
load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")

sem_sel  <- 45
camp_sel <- "2026"
dia_inicio_nom <- "Martes"
dia_fin_nom    <- "Viernes"
dias_nombres   <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
dia_inicio_idx <- which(dias_nombres == dia_inicio_nom)[1]
dia_fin_idx    <- which(dias_nombres == dia_fin_nom)[1]
dias_ajustables_nom <- dias_nombres[dia_inicio_idx:dia_fin_idx]
dias_previos_bloqueo <- if (dia_inicio_idx > 1) dias_nombres[1:(dia_inicio_idx - 1)] else character(0)

cat("Rango a ajustar:", paste(dias_ajustables_nom, collapse=", "), "\n")
cat("Días previos bloqueados por SAP:", paste(dias_previos_bloqueo, collapse=", "), "\n")
