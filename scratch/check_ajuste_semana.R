suppressMessages({
  library(readxl)
  library(dplyr)
})

base_dir <- "G:/Unidades compartidas/Compartido Proyecciones/3.- Arándano/Campaña 26-27/Proyección Semanal"
f_det_v1  <- file.path(base_dir, "Proyección Semanal Detallada", "26. Proyecciones Semana 46 + 5 semanas ACP (Conteo S44).xlsx")
f_det_v2  <- file.path(base_dir, "Proyección Semanal Detallada", "26. Proyecciones Semana 46 + 5 semanas ACP (Conteo S44)_V2.xlsx")

# In V2 sheet "Ajuste Semana"
aj_v2 <- read_excel(f_det_v2, sheet = "Ajuste Semana")
cat("Ajuste Semana V2 columns:\n")
print(names(aj_v2))

proy_col <- aj_v2$Proyección
proy_num <- suppressWarnings(as.numeric(proy_col))
cat("\nSum Proyección in Ajuste Semana V2:", sum(proy_num, na.rm = TRUE), "\n")

# Check if there is Ajuste col
if ("Ajuste" %in% names(aj_v2)) {
  cat("Sum Ajuste in Ajuste Semana V2:", sum(suppressWarnings(as.numeric(aj_v2$Ajuste)), na.rm = TRUE), "\n")
}

# Now check V1
cat("\n--- DETALLADA V1 ---\n")
calc_v1 <- read_excel(f_det_v1, sheet = "Calculo", skip = 3)
cat("Sum Proyección (kg) in Calculo V1:", sum(suppressWarnings(as.numeric(calc_v1$`Proyección (kg)`)), na.rm = TRUE), "\n")

aj_v1 <- read_excel(f_det_v1, sheet = "Ajuste Semana")
cat("Sum Proyección in Ajuste Semana V1:", sum(suppressWarnings(as.numeric(aj_v1$Proyección)), na.rm = TRUE), "\n")
if ("Ajuste" %in% names(aj_v1)) {
  cat("Sum Ajuste in Ajuste Semana V1:", sum(suppressWarnings(as.numeric(aj_v1$Ajuste)), na.rm = TRUE), "\n")
}
