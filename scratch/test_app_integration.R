# Script to test the actual app logic on week 46
source("Actualizacion Semanal/global.R", local = TRUE)

cat("Global loaded. Now checking desplazar_dia_2da_pasada:\n")
# Source the helper functions from app.R
dias_nombres <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
cat("Lunes -> 2da pasada:", desplazar_dia_2da_pasada("Lunes", 6, dias_nombres, 2, "Martes", 5, "Viernes"), "\n")
cat("Martes -> 2da pasada:", desplazar_dia_2da_pasada("Martes", 6, dias_nombres, 2, "Martes", 5, "Viernes"), "\n")
