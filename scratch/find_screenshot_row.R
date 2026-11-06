suppressPackageStartupMessages({
  library(dplyr)
})

valvs <- readRDS("Actualizacion Semanal/ajuste_valvulas_semana.rds")
s <- valvs %>% filter(abs(Kg_Asignado - 3006.97) < 5 | abs(Kg_Asignado - 2997.66) < 5)
print(s)

# Also search by Area == 6.45
print(valvs %>% group_by(Campaña, Semana, Sector, Módulo, Turno, Variedad) %>% 
  summarise(tot_kg = sum(Kg_Asignado), tot_ha = sum(Ha_Asignada), dias = paste(unique(Dia_Asignado), collapse=","), .groups="drop") %>%
  filter(abs(tot_kg - 6004.63) < 10 | abs(tot_ha - 6.45) < 0.1))
