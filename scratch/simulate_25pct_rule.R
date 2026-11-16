suppressPackageStartupMessages({
  library(dplyr)
})

# Let's inspect the calculation in app.R by sourcing or simulating it for S46
# We can load the RDS data or run the exact rend_turno logic
aj <- readRDS("D:/ACP/Actualizacion Semanal/ajuste_valvulas_semana.rds")
cat("Ajuste valvulas semana rows:", nrow(aj), "\n")
