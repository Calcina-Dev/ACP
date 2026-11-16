suppressPackageStartupMessages({
  library(dplyr)
})

ediciones_file <- "D:/ACP/Actualizacion Semanal/ediciones_manuales_cosecha.rds"
ediciones <- readRDS(ediciones_file)
cat("Colnames ediciones:\n")
print(names(ediciones))
print(head(ediciones, 2))
