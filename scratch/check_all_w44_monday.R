load("G:/Unidades compartidas/Compartido Proyecciones/3.- Ar\u00e1ndano/datos_maestros_cosecha_2026.RData")
suppressPackageStartupMessages(library(dplyr))

cat("=== TODA LA COSECHA W44 LUNES (2026-10-26) ===\n")
df_cosecha %>%
  filter(Semana == 44, as.character(as.Date(Fecha)) == "2026-10-26") %>%
  select(Sector, Módulo, Turno, Variedad, `Kg Total`) %>%
  print(n=50)
