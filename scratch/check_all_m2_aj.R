suppressMessages({
  library(dplyr)
})

aj_valvs <- readRDS("Actualizacion Semanal/ajuste_valvulas_semana.rds")
m2 <- aj_valvs %>% filter(Semana == 45, Módulo == "2")
print(as.data.frame(m2[, c("Turno", "codigo", "Dia_Asignado", "Kg_Asignado", "Ha_Asignada", "Dia_Desde", "Dia_Hasta")]))
