ed_h <- readRDS("D:/ACP/scratch/ed_head.rds")
cat("ed_head Módulo values:\n")
print(table(as.character(ed_h$Módulo)))
print(ed_h %>% filter(grepl("9|11", as.character(Módulo))) %>% select(Campaña, Semana, Sector, Módulo, Turno, Variedad, Tipo_Edicion) %>% head(20))
