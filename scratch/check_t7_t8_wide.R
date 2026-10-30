load("G:/Unidades compartidas/Compartido Proyecciones/3.- Ar\u00e1ndano/datos_maestros_cosecha_2026.RData")
suppressPackageStartupMessages(library(dplyr))

df_ant <- df_hist_oficial_wide %>% 
  filter(Semana == 43, Módulo_Norm == "4", Turno_Norm %in% c("7", "8"))
print(as.data.frame(df_ant %>% select(Módulo_Norm, Turno_Norm, Codigo_Valv, Variedad, Area_Ha, Kg_Total_Sem, Ha_Total_Sem, Kg_Sábado, Ha_Sábado)))
