load("Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
library(dplyr)
library(lubridate)

c26 <- df_cosecha %>% filter(Campaña == 2026, Semana == 45, as.character(Módulo) == "12")
cat("Rows in 2026 w45 mod 12:", nrow(c26), "\n")
print(c26 %>% select(Fecha, Módulo, Turno, Válvula, Variedad, `Kg Total`, Área, any_of(c("Kilos_Reales", "Ha_Cosechadas"))))
