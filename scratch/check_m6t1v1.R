suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
})

load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")

# Let's inspect df_hist_oficial_wide for sem 45
sap_45 <- df_hist_oficial_wide %>% filter(as.numeric(Semana) == 45)
cat("SAP sem 45 rows:", nrow(sap_45), "\n")

# Let's read rutas_cosecha_mapa.rds for sem 45
rc <- readRDS("D:/ACP/Rutas de Cosecha/rutas_cosecha_mapa.rds")
rc_45 <- rc %>% filter(as.numeric(Semana) == 45)
cat("RC sem 45 rows:", nrow(rc_45), "\n")

# Check M6-T1-V1 in SAP
print(sap_45 %>% filter(grepl("M6-T1-V1$", Codigo_Valv) | grepl("M06-T01-V01", Codigo_Valv) | grepl("M6.*T1.*V1", Codigo_Valv)))

# Check M6-T1-V1 in RC
print(rc_45 %>% filter(grepl("M6.*T1.*V1", Codigo_Valvula) | grepl("M6.*T1.*V1", codigo)))
