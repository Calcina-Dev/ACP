suppressPackageStartupMessages({
  library(dplyr)
})

# Let's verify what columns exist in df from app.R logic
load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
# Let's check df_cosecha for sem 45
cs_45 <- df_cosecha %>% filter(Semana == 45, !coalesce(Es_Testblock, FALSE))
cat("cs_45 rows:", nrow(cs_45), "\n")
dias <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
for (d in dias) {
  if (d %in% names(cs_45)) {
    cat(sprintf("%s: %s kg\n", d, format(round(sum(cs_45[[d]], na.rm = TRUE), 1), big.mark = ",")))
  }
}
cat("Total Lun-Jue:", format(round(sum(cs_45$Lunes, cs_45$Martes, cs_45$Miércoles, cs_45$Jueves, na.rm = TRUE), 1), big.mark = ","), "\n")
cat("Total Viernes:", format(round(sum(cs_45$Viernes, na.rm = TRUE), 1), big.mark = ","), "\n")
cat("Total Sem:", format(round(sum(cs_45$Total_Semana, na.rm = TRUE), 1), big.mark = ","), "\n")
