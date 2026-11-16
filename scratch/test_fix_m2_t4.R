suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
ed <- readRDS("D:/ACP/Actualizacion Semanal/ediciones_manuales_cosecha.rds")

# Inspect row 7
row7 <- ed %>% filter(Semana == 46, Módulo == "2", Turno == "4")
cat("Current row 7:\n")
print(row7 %>% select(Semana, Módulo, Turno, Variedad, Campo, Valor, Ha_Asignada, Ha_Asignada_1ra, Ha_Asignada_2da, Kg_Asignado_1ra, Kg_Asignado_2da))

# If we correct row 7 to have 1ra pasada:
row7_fixed <- row7 %>%
  mutate(
    Ha_Asignada_1ra = Ha_Asignada,
    Kg_Asignado_1ra = Valor,
    Ha_Asignada_2da = 0,
    Kg_Asignado_2da = 0
  )
cat("\nFixed row 7:\n")
print(row7_fixed %>% select(Semana, Módulo, Turno, Variedad, Campo, Valor, Ha_Asignada, Ha_Asignada_1ra, Ha_Asignada_2da, Kg_Asignado_1ra, Kg_Asignado_2da))

# Kg/Ha Aj. 1ª Pas:
kgha_aj_1ra <- round(row7_fixed$Kg_Asignado_1ra / row7_fixed$Ha_Asignada_1ra, 2)
cat("\nResulting Kg/Ha Aj. 1ª Pas:", kgha_aj_1ra, "\n")
