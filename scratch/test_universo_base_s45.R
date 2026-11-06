load('D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData')
library(dplyr)

cat("--- Objects in datos_maestros_cosecha_2026.RData ---\n")
print(ls())

cat("--- df_cosecha Semana 45 M12 ---\n")
if (exists("df_cosecha")) {
  print(df_cosecha %>% filter(Semana == 45, as.character(Módulo) == "12") %>% select(any_of(c("Sector", "Módulo", "Turno", "Variedad", "Semana", "Kg Total"))))
}

cat("\n--- df_hist_oficial_wide Semana 45 ---\n")
if (exists("df_hist_oficial_wide")) {
  print(df_hist_oficial_wide %>% filter(as.numeric(Semana) == 45) %>% head(5))
}

cat("\n--- df_proyecciones Semana 45 M12 ---\n")
if (exists("df_proyecciones")) {
  print(df_proyecciones %>% filter(Semana == 45, as.character(Módulo) == "12") %>% select(any_of(c("Sector", "Módulo", "Turno", "Variedad", "Semana", "Proyección"))))
}

cat("\n--- df_valvulas M12 ---\n")
if (exists("df_valvulas")) {
  print(head(df_valvulas %>% filter(as.character(Módulo) == "12") %>% select(any_of(c("Sector", "Módulo", "Turno", "Variedad", "codigo", "Codigo_Valvula")))))
}

cat("\n--- Rutas de cosecha in Rutas de Cosecha ---\n")
rc_files <- list.files("D:/ACP/Rutas de Cosecha", full.names = TRUE)
print(rc_files)
