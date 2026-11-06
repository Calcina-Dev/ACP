library(dplyr)
if (file.exists("Actualizacion Semanal/ajuste_valvulas_semana.rds")) {
  valvs <- readRDS("Actualizacion Semanal/ajuste_valvulas_semana.rds")
  s45 <- valvs %>% filter(Semana == 45)
  print(s45 %>% count(Sector, Módulo, Turno, Variedad, Dia_Desde, Dia_Hasta, Motivo_Ajuste))
}
