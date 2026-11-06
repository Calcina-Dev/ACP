v_head <- readRDS("D:/ACP/scratch/valv_head.rds")
cat("valvulas_normalizadas.rds at HEAD:\n")
print(unique(v_head$Módulo))
print(table(as.character(v_head$Módulo), as.character(v_head$Sector)))
