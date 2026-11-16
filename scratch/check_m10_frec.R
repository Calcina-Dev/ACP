suppressPackageStartupMessages(library(dplyr))
df_frec_mv <- readRDS("D:/ACP/Actualizacion Semanal/frecuencias_modulo_variedad_cosecha.rds")
print(df_frec_mv %>% filter(Módulo == "10"))
