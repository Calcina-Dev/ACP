suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
})

# Let's extract the exact lines of ejecutar_ajuste_semana from app.R
app_lines <- readLines("D:/ACP/Actualizacion Semanal/app.R", warn = FALSE, encoding = "UTF-8")
start_idx <- which(grepl("ejecutar_ajuste_semana <- function", app_lines))[1]
cat("ejecutar_ajuste_semana starts at line:", start_idx, "\n")

# Let's read from start_idx onwards to find where reg_2da_lun is evaluated
reg_idx <- which(grepl("reg_2da_lun <- valvulas_calc", app_lines))
cat("reg_2da_lun at line:", reg_idx, "\n")
