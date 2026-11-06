library(dplyr)
ed <- readRDS("D:/ACP/Actualizacion Semanal/ediciones_manuales_cosecha.rds")
m12_sec8 <- ed %>% filter(grepl("^12$", trimws(as.character(Módulo))), Sector == "8")
cat("Módulo 12 with Sector == 8 in ediciones_manuales_cosecha.rds:\n")
print(m12_sec8)

cat("\nMódulo 12 Turno 12 in ALL semanas:\n")
m12_t12 <- ed %>% filter(grepl("^12$", trimws(as.character(Módulo))), grepl("^12$", trimws(as.character(Turno))))
print(m12_t12)
