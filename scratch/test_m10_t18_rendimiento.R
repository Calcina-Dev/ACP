suppressMessages({
  library(dplyr)
  library(lubridate)
})

# Cargar datos maestros
load("G:/Unidades compartidas/Compartido Proyecciones/3.- Arándano/datos_maestros_cosecha_2026.RData")

cat("=== Verificando M10 - T18 en Rutas de Campo y SAP (Semana 46) ===\n")

# 1. Rutas de campo
if (exists("df_rutas_campo")) {
  rc_m10 <- df_rutas_campo %>% 
    filter(as.numeric(Semana) == 46, as.character(Módulo) == "10", as.character(Turno) == "18")
  cat("Rutas de campo M10-T18:\n")
  print(rc_m10 %>% select(any_of(c("Semana", "Módulo", "Turno", "Variedad", "Codigo_Valvula", "Ha_Lunes", "Ha_Martes", "Ha_Miércoles", "Ha_Jueves", "Ha_Viernes", "Ha_Sábado", "Kg_Total_Prog"))))
}

# 2. SAP oficial
if (exists("df_hist_oficial_wide")) {
  sap_m10 <- df_hist_oficial_wide %>% 
    filter(as.numeric(Semana) == 46, as.character(Módulo_Norm) == "10", as.character(Turno_Norm) == "18")
  cat("\nSAP oficial wide M10-T18:\n")
  print(sap_m10 %>% select(any_of(c("Semana", "Módulo_Norm", "Turno_Norm", "Variedad", "Codigo_Valv", "Area_Ha", "Kg_Lunes", "Kg_Martes", "Kg_Miércoles", "Kg_Jueves", "Kg_Viernes", "Kg_Sábado"))))
  
  cat("\nTotales de kilos SAP por día M10-T18:\n")
  for (d in c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")) {
    col_k <- paste0("Kg_", d)
    val <- if (col_k %in% names(sap_m10)) sum(coalesce(sap_m10[[col_k]], 0)) else 0
    cat(sprintf("  %s: %.2f kg\n", d, val))
  }
  
  cat("\nVálvulas únicas en SAP:\n")
  valvs_sap <- sap_m10 %>% distinct(Codigo_Valv, Area_Ha)
  print(valvs_sap)
  cat(sprintf("Total Ha SAP válvulas: %.2f ha\n", sum(valvs_sap$Area_Ha, na.rm = TRUE)))
}
