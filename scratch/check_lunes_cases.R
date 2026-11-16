suppressPackageStartupMessages({
  library(dplyr)
})
load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
rc_raw <- readRDS("D:/ACP/Rutas de Cosecha/rutas_cosecha_mapa.rds")

# Check Lunes cases
sap_sem <- df_hist_oficial_wide %>% filter(as.numeric(Semana) == 45)
rc_sem <- rc_raw %>% filter(as.numeric(Semana) == 45)

col_cod_rc <- intersect(c("Codigo_Valvula", "codigo", "Codigo_Valv"), names(rc_sem))[1]

# Let's inspect M10-T17-V51, M10-T4-V11, M6-T1-V1
print(sap_sem %>% filter(Codigo_Valv %in% c("M10-T17-V51", "M10-T4-V11", "M6-T1-V1")) %>%
  select(Codigo_Valv, starts_with("Kg_")))

print(rc_sem %>% filter(.data[[col_cod_rc]] %in% c("M10-T17-V51", "M10-T4-V11", "M6-T1-V1")) %>%
  select(1:8, starts_with("Ha_")))
