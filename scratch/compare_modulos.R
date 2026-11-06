load('D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData')
library(dplyr)
cat("--- df_cosecha: Módulo to Sector mapping ---\n")
cos_map <- df_cosecha %>%
  filter(Campaña == 2026) %>%
  mutate(Módulo_clean = as.character(as.numeric(sub("\\..*", "", as.character(Módulo))))) %>%
  group_by(Módulo_clean, Sector) %>%
  summarise(kg = sum(`Kg Total`, na.rm = TRUE), .groups = "drop") %>%
  arrange(as.numeric(Módulo_clean))
print(cos_map, n = 30)

cat("\n--- df_valvulas: Módulo to Sector mapping ---\n")
valv_map <- df_valvulas %>%
  mutate(Módulo_clean = as.character(as.numeric(sub("\\..*", "", as.character(Módulo))))) %>%
  group_by(Módulo_clean, Sector) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(as.numeric(Módulo_clean))
print(valv_map, n = 30)
