suppressPackageStartupMessages(library(dplyr))
df_conteo <- readRDS("D:/ACP/Actualizacion Semanal/historico_conteo.rds")
c_m7t4_v <- df_conteo %>%
  filter(
    as.character(Campaña) == "2026",
    trimws(as.character(Módulo)) == "7",
    trimws(as.character(Turno)) == "4",
    trimws(toupper(as.character(Variedad))) == "VENTURA"
  ) %>%
  mutate(Semana_Num = as.numeric(Semana)) %>%
  group_by(Semana_Num) %>%
  summarise(
    Organos = sum(Boton + Flor, na.rm = TRUE),
    Muestra = sum(MuestraF, na.rm = TRUE),
    Carga = Organos / Muestra,
    .groups = "drop"
  ) %>%
  arrange(Semana_Num)
print(as.data.frame(c_m7t4_v))
