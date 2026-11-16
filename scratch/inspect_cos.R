load("Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
library(dplyr)

cos12 <- df_cosecha %>% filter(as.character(Módulo) == "12", as.character(Turno) %in% c("1", "5"))
print(head(cos12, 20))
print(cos12 %>% group_by(Semana, Turno, Dia_Semana) %>% summarise(Kg = sum(Kilos_Reales, na.rm = TRUE), Ha = sum(Ha_Cosechadas, na.rm = TRUE), .groups = "drop"))
