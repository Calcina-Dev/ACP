suppressMessages({
  library(readxl)
  library(dplyr)
})

base_dir <- "G:/Unidades compartidas/Compartido Proyecciones/3.- Arándano/Campaña 26-27/Proyección Semanal"
f_det_v2  <- file.path(base_dir, "Proyección Semanal Detallada", "26. Proyecciones Semana 46 + 5 semanas ACP (Conteo S44)_V2.xlsx")
f_integ   <- file.path(base_dir, "Proyección Semanal Detallada", "Integradas", "26. Proyecciones Semana 46 + 5 semanas ACP (Conteo S44)_V2_Integrada.xlsx")

# Check sheet Calculo in Integrada
calc_integ <- read_excel(f_integ, sheet = "Calculo", skip = 3)
cat("Sum Proyección (kg) in Integrada Calculo:", sum(suppressWarnings(as.numeric(calc_integ$`Proyección (kg)`)), na.rm = TRUE), "\n")

# Check sheet Calculo in V2 Detallada
calc_v2 <- read_excel(f_det_v2, sheet = "Calculo", skip = 3)
cat("Sum Proyección (kg) in V2 Calculo:", sum(suppressWarnings(as.numeric(calc_v2$`Proyección (kg)`)), na.rm = TRUE), "\n")
