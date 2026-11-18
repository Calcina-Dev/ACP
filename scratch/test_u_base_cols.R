load("Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
suppressPackageStartupMessages(library(dplyr))

# Let's inspect df_valvulas or how universo_base is constructed
print(df_proyecciones %>% filter(as.numeric(Semana) == 47, as.character(Módulo) == "13", as.character(Turno) == "1") %>% select(Proyección, Kg_Proy_Turno))
