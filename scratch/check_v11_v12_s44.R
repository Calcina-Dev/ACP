suppressMessages({
  library(dplyr)
})

hist_wide <- readRDS("D:/ACP/Rutas de Cosecha/historico_oficial_wide_2026.rds")
valv_7d <- hist_wide %>% filter(as.numeric(Semana) == 44, Codigo_Valv %in% c("M2-T3-V11", "M2-T3-V12", "M2-T3-V9", "M2-T3-V10"))
print(as.data.frame(valv_7d[, c("Codigo_Valv", "Kg_Miércoles", "Kg_Jueves", "Kg_Viernes", "Ha_Miércoles", "Ha_Jueves", "Ha_Viernes")]))
