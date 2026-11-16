suppressPackageStartupMessages({
  library(dplyr)
})
load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")

sap_sem <- df_hist_oficial_wide %>% filter(as.numeric(Semana) == 45)

cat("SAP sem 45 total:\n")
cat("Lunes:", sum(sap_sem$Kg_Lunes, na.rm=TRUE), "\n")
cat("Martes:", sum(sap_sem$Kg_Martes, na.rm=TRUE), "\n")
cat("Miércoles:", sum(sap_sem$Kg_Miércoles, na.rm=TRUE), "\n")
cat("Jueves:", sum(sap_sem$Kg_Jueves, na.rm=TRUE), "\n")
cat("Viernes:", sum(sap_sem$Kg_Viernes, na.rm=TRUE), "\n")
cat("Sábado:", sum(sap_sem$Kg_Sábado, na.rm=TRUE), "\n")
cat("Total Lun-Jue:", sum(sap_sem$Kg_Lunes + sap_sem$Kg_Martes + sap_sem$Kg_Miércoles + sap_sem$Kg_Jueves, na.rm=TRUE), "\n")
cat("Total Week:", sum(sap_sem$Kg_Total_Sem, na.rm=TRUE), "\n")
