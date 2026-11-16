# test_mod12_turnos.R
source("D:/ACP/Actualizacion Semanal/app.R", local = TRUE)

sem_sel <- 46
camp_sel <- "2026"

# Check how valvulas_calc is built for Modulo 12
# Let's run desplazar_dia_frecuencia for Lunes and Martes with Frecuencia = 6
cat("desplazar_dia_frecuencia('Lunes', 6):\n")
res_lun <- desplazar_dia_frecuencia("Lunes", 6, c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado"), 2L, "Martes", 6L, "Sábado")
print(res_lun)

cat("desplazar_dia_frecuencia('Martes', 6):\n")
res_mar <- desplazar_dia_frecuencia("Martes", 6, c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado"), 2L, "Martes", 6L, "Sábado")
print(res_mar)
