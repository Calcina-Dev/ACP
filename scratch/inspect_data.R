load("Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
library(dplyr)

cat("Objects loaded:\n")
print(ls())

# Check valvulas for Modulo 12
if (exists("df_valvulas")) {
  v12 <- df_valvulas %>% filter(as.character(Módulo) == "12")
  print(v12 %>% select(Módulo, Turno, Válvula, Área, Variedad))
}

# Check proyecciones for Modulo 12, week 45
if (exists("df_proyecciones")) {
  p12 <- df_proyecciones %>% filter(as.character(Módulo) == "12", Semana == 45)
  print(p12 %>% select(Módulo, Turno, Variedad, any_of(c("Ha_Semana", "Ha_1ra", "Ha_2da", "Kg_Semana", "Kilos"))))
}
