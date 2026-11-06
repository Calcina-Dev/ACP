library(dplyr)
e <- new.env()
load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData", envir = e)

cat("--- DF_PROYECCIONES for Modulo 12, Turno 12 ---\n")
if ("df_proyecciones" %in% ls(e)) {
  p12 <- e$df_proyecciones %>% filter(grepl("^12$", trimws(as.character(Módulo))), grepl("^12$", trimws(as.character(Turno))))
  print(p12 %>% select(any_of(c("Campaña", "Semana", "Sector", "Módulo", "Turno", "Variedad", "Proyección"))))
}

cat("\n--- DF_COSECHA for Modulo 12, Turno 12 ---\n")
if ("df_cosecha" %in% ls(e)) {
  c12 <- e$df_cosecha %>% filter(grepl("^12$", trimws(as.character(Módulo))), grepl("^12$", trimws(as.character(Turno))))
  print(c12 %>% select(any_of(c("Campaña", "Semana", "Sector", "Módulo", "Turno", "Variedad", "Kg Total", "Fecha"))) %>% head(10))
}

cat("\n--- DF_HIST_OFICIAL_WIDE for Modulo 12, Turno 12 ---\n")
if ("df_hist_oficial_wide" %in% ls(e)) {
  h12 <- e$df_hist_oficial_wide %>% filter(grepl("^12$", trimws(as.character(Módulo_Norm))), grepl("^12$", trimws(as.character(Turno_Norm))))
  print(h12 %>% select(any_of(c("Semana", "Sector", "Módulo_Norm", "Turno_Norm", "Variedad", "Kg_Total_Sem"))) %>% head(10))
}
