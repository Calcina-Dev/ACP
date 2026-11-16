load("Actualizacion Semanal RNA/datos_maestros_cosecha_2026.RData")
library(dplyr)

ruta_rc <- "d:/ACP/Rutas de Cosecha/rutas_campo_oficial_2026.rds"
rc <- if (file.exists(ruta_rc)) readRDS(ruta_rc) else tibble()
rc_s45 <- rc %>% filter(Semana == 45, grepl("13", Módulo), Turno == "2")
cat("rc for M13 T2 S45:\n")
print(as.data.frame(rc_s45 %>% select(Num_Pasada, codigo, Módulo, Turno, Variedad, starts_with("Ha_"))))

cat("\nOfficial wide for M13 T2 S45:\n")
df_h <- df_hist_oficial_wide %>% filter(as.numeric(Semana) == 45, grepl("13", Módulo_Norm), Turno_Norm == "2")
print(as.data.frame(df_h %>% select(Codigo_Valv, starts_with("Kg_"), starts_with("Ha_"))))
