suppressPackageStartupMessages(library(dplyr))
ed_h <- readRDS("D:/ACP/scratch/ed_head.rds")
print(names(ed_h))
print(ed_h %>% filter(grepl("9|11", as.character(Módulo))) %>% select(any_of(c("Campaña", "Semana", "Sector", "Módulo", "Turno", "Variedad", "Tipo_Edicion", "Kg_Total", "Kilos_Ajustados"))))
