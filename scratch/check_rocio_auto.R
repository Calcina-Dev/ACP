suppressPackageStartupMessages(library(dplyr))
# Ver algunas filas de ROCIO en auto_ediciones
source("d:/ACP/scratch/test_full_engine_integration.R")
cat("\n=== FILAS DE ROCIO EN AUTO EDICIONES ===\n")
auto_ediciones %>% filter(Variedad == "ROCIO") %>% head(15) %>% as.data.frame() %>% print()
