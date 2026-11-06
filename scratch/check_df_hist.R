load('D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData')
library(dplyr)
print(df_hist_oficial_wide %>% filter(grepl("M12", Codigo_Valv)) %>% distinct(Sector, Módulo_Norm) %>% head(10))
