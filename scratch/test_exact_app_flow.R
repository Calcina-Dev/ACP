suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
})

load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")

source_lines <- readLines("D:/ACP/Actualizacion Semanal/app.R", warn = FALSE, encoding = "UTF-8")

# Let's check what desplazar_dia_2da_pasada('Lunes', 4, ...) returns
eval(parse(text = source_lines[736:753]))

dias_nombres <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
cat("desplazar_dia_2da_pasada('Lunes', 4, ..., dia_inicio_idx=3, dia_fin_idx=4):\n")
res_frec4 <- desplazar_dia_2da_pasada("Lunes", 4L, dias_nombres, 3L, "Miércoles", 4L, "Jueves")
cat("Result with Frec = 4:", res_frec4, "\n")

cat("desplazar_dia_2da_pasada('Lunes', 3, ..., dia_inicio_idx=3, dia_fin_idx=4):\n")
res_frec3 <- desplazar_dia_2da_pasada("Lunes", 3L, dias_nombres, 3L, "Miércoles", 4L, "Jueves")
cat("Result with Frec = 3:", res_frec3, "\n")
