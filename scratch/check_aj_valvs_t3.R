suppressMessages({
  library(dplyr)
})

# Check ajuste_valvulas_semana.rds to see the exact records that produced the image!
aj_valvs <- readRDS("Actualizacion Semanal/ajuste_valvulas_semana.rds")
cat("aj_valvs columns:\n")
print(names(aj_valvs))

t3_aj <- aj_valvs %>% filter(Módulo == "2", Turno == "3")
cat("\nRecords in ajuste_valvulas_semana for M2 T3:\n")
print(t3_aj)
