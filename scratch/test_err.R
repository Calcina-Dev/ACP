library(dplyr)
library(tibble)
res <- tryCatch({
  bind_rows(tibble(), tibble()) %>% group_by(Módulo, Turno, Variedad) %>% summarise(x = sum(Ha_Asignada))
}, error = function(e) e$message)
cat("Error:", res, "\n")
