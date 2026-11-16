suppressPackageStartupMessages({
  library(dplyr)
})
rc_raw <- readRDS("D:/ACP/Rutas de Cosecha/rutas_cosecha_mapa.rds")
rc_sem <- rc_raw %>% filter(as.numeric(Semana) == 45)
col_cod_rc <- intersect(c("Codigo_Valvula", "codigo", "Codigo_Valv"), names(rc_sem))[1]

print(rc_sem %>% filter(.data[[col_cod_rc]] == "M10-T17-V51") %>% select(Tipo_Registro, starts_with("Ha_"), starts_with("Kg_")))
print(rc_sem %>% filter(.data[[col_cod_rc]] == "M10-T4-V11") %>% select(Tipo_Registro, starts_with("Ha_"), starts_with("Kg_")))
