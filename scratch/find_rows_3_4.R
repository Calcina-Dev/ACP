suppressPackageStartupMessages({
  library(dplyr)
})

valvs <- readRDS("Actualizacion Semanal/ajuste_valvulas_semana.rds")
ed <- readRDS("Actualizacion Semanal/ediciones_manuales_cosecha.rds")

# Buscar turnos con Proy o Kilos parecidos a Row 3 y 4
cat("Buscar en valvs:\n")
print(valvs %>% filter(abs(Kg_Asignado - 863.94) < 1 | abs(Kg_Asignado - 1884.59) < 1) %>% 
  select(Sector, Módulo, Turno, Variedad, Dia_Asignado, Ha_Asignada, Kg_Asignado, Motivo_Ajuste))

cat("\nBuscar en ed:\n")
print(ed %>% filter(abs(Valor - 863.94) < 1 | abs(Valor - 1884.59) < 1) %>% 
  select(Sector, Módulo, Turno, Variedad, Campo, Ha_Asignada, Valor, Motivo_Ajuste))
