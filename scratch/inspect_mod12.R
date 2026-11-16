# Script to inspect Modulo 12 Turno 1 and 5 in Semana 45
source("Actualizacion Semanal/global.R", local = TRUE)
source("Actualizacion Semanal/app.R", local = FALSE)

# Let's check valvulas for Modulo 12
v_mod12 <- df_valvulas %>% filter(Módulo == "12")
cat("Valvulas Modulo 12 count:", nrow(v_mod12), "\n")
print(v_mod12 %>% select(Módulo, Turno, Válvula, Área, Variedad))
