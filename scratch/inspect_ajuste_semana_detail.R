suppressMessages({
  library(readxl)
  library(dplyr)
})

base_dir <- "G:/Unidades compartidas/Compartido Proyecciones/3.- Arándano/Campaña 26-27/Proyección Semanal"
f_det_v2  <- file.path(base_dir, "Proyección Semanal Detallada", "26. Proyecciones Semana 46 + 5 semanas ACP (Conteo S44)_V2.xlsx")

aj <- read_excel(f_det_v2, sheet = "Ajuste Semana")
cat("Names in Ajuste Semana:\n")
print(names(aj))

cat("\nFirst 10 rows of Ajuste Semana:\n")
print(head(aj %>% select(1:18), 10))

# Let's see the unique values or sums
# Is there a column with the adjusted weekly volume?
# Let's inspect rows for Modulo 2, 3, 4, 5, 12, 14
aj_sub <- aj %>%
  filter(as.character(Módulo) %in% c("2", "3", "4", "5", "12", "14"))
print(head(aj_sub %>% select(Sector, Módulo, Turno, Variedad, Proyección, any_of(c("Ajuste", "Diferencia...19", "Lunes...12", "Martes...13"))), 15))
