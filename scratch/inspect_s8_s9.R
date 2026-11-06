load('D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData')
library(dplyr)
cat("--- Rows with Sector 8 and Módulo_Norm 12 in df_hist_oficial_wide ---\n")
s8 <- df_hist_oficial_wide %>% filter(Sector == 8, Módulo_Norm == "12")
print(s8 %>% select(Campaña, Semana, Pasada, Sector, Módulo_Norm, Turno_Norm, Codigo_Valv, Variedad, Kg_Total_Sem))

cat("\n--- Rows with Sector 9 and Módulo_Norm 12 in df_hist_oficial_wide ---\n")
s9 <- df_hist_oficial_wide %>% filter(Sector == 9, Módulo_Norm == "12")
print(head(s9 %>% select(Campaña, Semana, Pasada, Sector, Módulo_Norm, Turno_Norm, Codigo_Valv, Variedad, Kg_Total_Sem)))
