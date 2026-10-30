load("G:/Unidades compartidas/Compartido Proyecciones/3.- Ar\u00e1ndano/datos_maestros_cosecha_2026.RData")
suppressPackageStartupMessages(library(dplyr))

# Ver estructura de df_hist_oficial_wide en sem 43
df_ant <- df_hist_oficial_wide %>% filter(Semana == 43)
cat("Columnas en df_hist_oficial_wide:\n")
print(names(df_ant))

cat("\nEjemplo filas M4:\n")
df_ant %>% filter(Módulo_Norm == "4") %>% head(10) %>% as.data.frame() %>% print()
