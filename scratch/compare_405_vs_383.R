suppressMessages({
  library(readxl)
  library(dplyr)
})

base_dir <- "G:/Unidades compartidas/Compartido Proyecciones/3.- Arándano/Campaña 26-27/Proyección Semanal"
f_form_v2 <- file.path(base_dir, "Proyección Semanal Formalizada", "26.- Proyección Cosecha Arándano ACP _ 2026 _ Sem 46 al 51_V2.xlsx")
f_det_v2  <- file.path(base_dir, "Proyección Semanal Detallada", "26. Proyecciones Semana 46 + 5 semanas ACP (Conteo S44)_V2.xlsx")

# 1. Look at sheet "SEM 46" in Formalizada V2
sem46_form <- read_excel(f_form_v2, sheet = "SEM 46")
cat("SEM 46 Formalizada V2 rows:", nrow(sem46_form), "\n")
print(head(sem46_form, 20))

# 2. Look at sheet "Calculo" in Detallada V2
calc_det <- read_excel(f_det_v2, sheet = "Calculo", skip = 3)
cat("Calculo Detallada V2 rows:", nrow(calc_det), "\n")
print(names(calc_det)[1:20])

# Load df_proyecciones from RData
e <- new.env()
load("Actualizacion Semanal/datos_maestros_cosecha_2026.RData", envir = e)
p46_rdata <- e$df_proyecciones %>% filter(Semana == 46)

cat("\nRData Proyecciones sum:", sum(p46_rdata$Proyección, na.rm = TRUE), "\n")
cat("By Variety in RData:\n")
print(p46_rdata %>% group_by(Variedad) %>% summarise(Kg = sum(Proyección, na.rm = TRUE)))

cat("\nBy Sector in RData:\n")
print(p46_rdata %>% group_by(Sector) %>% summarise(Kg = sum(Proyección, na.rm = TRUE)))

cat("\nBy Modulo in RData:\n")
print(p46_rdata %>% group_by(Módulo) %>% summarise(Kg = sum(Proyección, na.rm = TRUE)) %>% print(n = 30))
