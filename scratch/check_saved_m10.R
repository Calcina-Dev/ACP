suppressPackageStartupMessages(library(dplyr))
aj_valv <- readRDS("D:/ACP/Actualizacion Semanal/ajuste_valvulas_semana.rds")
aj_s46_m10 <- aj_valv %>% filter(Semana == 46, grepl("10", as.character(Módulo)))
print(as.data.frame(aj_s46_m10[, c("codigo", "Turno", "Variedad", "Dia_Asignado", "Num_Pasada", "Ha_Asignada", "Kg_Asignado", "Dia_Desde", "Dia_Hasta")]))
