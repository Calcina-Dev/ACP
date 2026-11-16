load("Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
library(dplyr)

p12 <- df_proyecciones %>% filter(Campaña == 2026, Semana == 46, as.character(Módulo) == "12")
print(p12 %>% select(Módulo, Turno, Variedad, Proyección, Kg_Proy_Turno))
