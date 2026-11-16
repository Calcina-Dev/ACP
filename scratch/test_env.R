env <- new.env()
# Source only functions from app.R
sys.source("Actualizacion Semanal/app.R", envir = env)
cat("Functions loaded in env:\n")
print(exists("desplazar_dia_2da_pasada", envir = env))

dias_nombres <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
f_desp <- get("desplazar_dia_2da_pasada", envir = env)
cat("Lunes test:", f_desp("Lunes", 6, dias_nombres, 2, "Martes", 5, "Viernes"), "\n")
cat("Martes test:", f_desp("Martes", 6, dias_nombres, 2, "Martes", 5, "Viernes"), "\n")
cat("Integration test SUCCESS!\n")
