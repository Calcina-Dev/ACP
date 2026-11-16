suppressPackageStartupMessages({
  library(dplyr)
})
load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
sap_45 <- df_hist_oficial_wide %>% filter(as.numeric(Semana) == 45, Codigo_Valv == "M6-T1-V1")
rc <- readRDS("D:/ACP/Rutas de Cosecha/rutas_cosecha_mapa.rds")
rc_45 <- rc %>% filter(as.numeric(Semana) == 45, Codigo_Valvula == "M6-T1-V1" | codigo == "M6-T1-V1")

cat("SAP 45 M6-T1-V1:\n")
print(glimpse(sap_45 %>% select(starts_with("Kg_"), starts_with("Ha_"))))

cat("RC 45 M6-T1-V1:\n")
print(glimpse(rc_45 %>% select(starts_with("Ha_"), starts_with("Kg_"))))
