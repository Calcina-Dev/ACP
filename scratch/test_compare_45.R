suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})
load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
ed <- readRDS("D:/ACP/Actualizacion Semanal/ediciones_manuales_cosecha.rds")

ed_45 <- ed %>% filter(as.numeric(Semana) == 45)
cat("Semana 45 in ed:", nrow(ed_45), "records\n")
print(head(ed_45))

# Check SAP for sem 45
sap_45 <- df_hist_oficial_wide %>% filter(as.numeric(Semana) == 45)
cat("SAP sem 45:", nrow(sap_45), "records\n")

# Check which days are in ed_45
print(table(ed_45$Campo))
