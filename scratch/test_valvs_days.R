suppressMessages({
  library(dplyr)
})
load("G:/Unidades compartidas/Compartido Proyecciones/3.- Arándano/datos_maestros_cosecha_2026.RData")

sap_m10 <- df_hist_oficial_wide %>% 
  filter(as.numeric(Semana) == 46, as.character(Módulo_Norm) == "10", as.character(Turno_Norm) == "18")
print(sap_m10 %>% select(Codigo_Valv, Area_Ha, Kg_Martes, Kg_Miércoles, Kg_Jueves))
