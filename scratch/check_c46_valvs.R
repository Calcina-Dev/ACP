load("Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
library(dplyr)

# Let's inspect df_cosecha for Week 46 Modulo 12
c46 <- df_cosecha %>% filter(Campaña == 2026, Semana == 46, as.character(Módulo) == "12")
print(c46 %>% select(Fecha, Turno, Válvula, `Kg Total`, Área))
