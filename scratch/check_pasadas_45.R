suppressPackageStartupMessages({
  library(dplyr)
})

load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")

# Let's inspect df_cosecha for sem 45 and pasadas
cs_45 <- df_cosecha %>% filter(Campaña == "2026", Semana == 45)
cat("cs_45 rows:", nrow(cs_45), "\n")
print(table(cs_45$Tipo_Cosecha))

# What about rutas_cosecha_mapa?
rutas_file <- "D:/ACP/Rutas de Cosecha/rutas_cosecha_mapa.rds"
if (file.exists(rutas_file)) {
  rutas <- readRDS(rutas_file)
  r_45 <- rutas %>% filter(Campaña == 2026, Semana == 45)
  cat("Rutas 45 rows:", nrow(r_45), "\n")
  print(table(r_45$Pasada))
  # Sum of ha by pasada
  print(r_45 %>% group_by(Pasada) %>% summarise(Ha = sum(Ha_Neta, na.rm = TRUE), .groups = "drop"))
}
