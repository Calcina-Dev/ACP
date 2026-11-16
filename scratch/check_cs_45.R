suppressPackageStartupMessages({
  library(dplyr)
})

load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
ediciones <- readRDS("D:/ACP/Actualizacion Semanal/ediciones_manuales_cosecha.rds")

# Check cosecha_semana for sem 45
cs_45 <- cosecha_semana %>% filter(Semana == 45)
cat("cs_45 rows:", nrow(cs_45), "\n")
print(names(cs_45))

# Check real harvest in cs_45 on Friday
if ("Viernes" %in% names(cs_45)) {
  cat("Total Viernes in cs_45:", sum(cs_45$Viernes, na.rm = TRUE), "\n")
}
# What are the column names in cs_45?
