suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

load("Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
ed_all <- readRDS("Actualizacion Semanal/ediciones_manuales_cosecha.rds")

# Check u_base for semana 47
u_base <- universo_base_2026 %>% filter(as.numeric(Semana) == 47)
cat("u_base rows:", nrow(u_base), "\n")
cat("u_base columns:", paste(names(u_base), collapse=", "), "\n")

# Check Lunes column in u_base
cat("Sum of Lunes in u_base:", sum(coalesce(u_base$Lunes, 0)), "\n")
cat("Sum of Martes in u_base:", sum(coalesce(u_base$Martes, 0)), "\n")

# Check sum of all days in u_base
dias_sem <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
for (d in dias_sem) {
  if (d %in% names(u_base)) {
    cat(sprintf("%s: %.1f kg\n", d, sum(coalesce(u_base[[d]], 0))))
  }
}
