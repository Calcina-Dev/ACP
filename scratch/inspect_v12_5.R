load("Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
library(dplyr)

# Let's inspect rc_valv_avance and sap_valv_avance for week 46 mod 12 turno 5
v12_5 <- df_valvulas %>% filter(as.character(Módulo) == "12", as.character(Turno) == "5")
print(v12_5 %>% select(Módulo, Turno, Válvula, Área, Variedad))
