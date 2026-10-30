load("G:/Unidades compartidas/Compartido Proyecciones/3.- Ar\u00e1ndano/datos_maestros_cosecha_2026.RData")
suppressPackageStartupMessages(library(dplyr))

cat("=== PROYECCION W44 ===\n")
if (exists("df_proyecciones")) {
  d <- df_proyecciones %>%
    filter(Semana == 44, Módulo == "4", Turno %in% c("7", "8"))
  print(as.data.frame(d))
}

cat("\n=== COSECHA W44 ===\n")
if (exists("df_cosecha")) {
  d <- df_cosecha %>%
    filter(Semana == 44, Módulo == "4", Turno %in% c("7", "8"))
  print(as.data.frame(d))
}

cat("\n=== COSECHA W43 ===\n")
if (exists("df_cosecha")) {
  d <- df_cosecha %>%
    filter(Semana == 43, Módulo == "4", Turno %in% c("7", "8")) %>%
    group_by(Fecha, Módulo, Turno, Válvula) %>%
    summarise(Kg = sum(`Kg Total`), .groups = "drop")
  print(as.data.frame(d))
}
