load("Actualizacion Semanal RNA/datos_maestros_cosecha_2026.RData")
library(dplyr)

cat("df_hist_oficial_wide for M13 T2 S45:\n")
df_h <- df_hist_oficial_wide %>% filter(as.numeric(Semana) == 45, grepl("13", Módulo_Norm), Turno_Norm == "2")
print(as.data.frame(df_h %>% select(Semana, Pasada, Codigo_Valv, Variedad, Area_Ha, Kg_Lunes, Kg_Jueves, Ha_Lunes, Ha_Jueves, Kg_Total_Sem)))

cat("\nrc_sem for M13 T2 S45:\n")
ruta_rc <- "d:/ACP/Rutas de Cosecha/rutas_campo_oficial_2026.rds"
if (file.exists(ruta_rc)) {
  rc <- readRDS(ruta_rc)
  rc_sub <- rc %>% filter(Semana == 45, grepl("13", Módulo), Turno == "2")
  print(as.data.frame(rc_sub %>% select(Semana, any_of(c("Num_Pasada", "codigo", "Módulo", "Turno", "Variedad", "Ha_Lunes", "Ha_Jueves", "Kg_Lunes", "Kg_Jueves")))))
}
