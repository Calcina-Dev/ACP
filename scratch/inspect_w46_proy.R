load("Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
library(dplyr)

# Let's inspect df_proyecciones for week 46, modulo 12
p12_w46 <- df_proyecciones %>% filter(Campaña == 2026, Semana == 46, as.character(Módulo) == "12")
print(p12_w46 %>% select(Módulo, Turno, Variedad, any_of(c("Ha_Semana", "Ha_1ra", "Ha_2da", "Kg_Semana", "Kilos", "Rend_Base"))))
