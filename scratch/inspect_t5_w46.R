load("Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
library(dplyr)

c26_t5_w46 <- df_cosecha %>% filter(Campaña == 2026, Semana == 46, as.character(Módulo) == "12", as.character(Turno) == "5")
print(c26_t5_w46 %>% select(Fecha, Módulo, Turno, Válvula, `Kg Total`, Área))
