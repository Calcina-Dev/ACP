load("Actualizacion Semanal RNA/datos_maestros_cosecha_2026.RData")
library(dplyr)
library(lubridate)

# Check df_cosecha for Modulo 13, Turno 2, Rocio
df_sub <- df_cosecha %>%
  filter(grepl("13", as.character(Módulo)), as.character(Turno) == "2", grepl("ROCIO", Variedad)) %>%
  filter(Campaña == 2026) %>%
  arrange(desc(Semana), Fecha)

cat("df_cosecha for M13 T2 ROCIO (2026):\n")
print(head(df_sub %>% select(Campaña, Semana, Fecha, `Kg Total`, Área, Tipo_Cosecha), 15))

# Check df_hist_oficial_wide
if (exists("df_hist_oficial_wide")) {
  df_hw <- df_hist_oficial_wide %>%
    filter(grepl("13", as.character(Módulo_Norm)), as.character(Turno_Norm) == "2", grepl("ROCIO", Variedad)) %>%
    arrange(desc(Semana))
  cat("\ndf_hist_oficial_wide for M13 T2 ROCIO:\n")
  print(head(df_hw %>% select(Semana, Codigo_Valv, starts_with("Kg_"), starts_with("Ha_")), 5))
}
