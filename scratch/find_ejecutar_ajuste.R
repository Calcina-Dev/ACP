lines <- readLines("Actualizacion Semanal/app.R", encoding = "UTF-8")
print(grep("ejecutar_ajuste_semana <- function", lines))
