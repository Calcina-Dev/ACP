suppressPackageStartupMessages(library(dplyr))
load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")

# Let's inspect turnos in Modulo 7 VENTURA
t_cos <- df_cosecha %>%
  filter(as.character(Campaña) == "2026", trimws(as.character(Módulo)) == "7") %>%
  distinct(Turno, Variedad)
print(as.data.frame(t_cos))

t_con <- df_conteo_historico %>%
  filter(as.character(Campaña) == "2026", trimws(as.character(Módulo)) == "7") %>%
  distinct(Turno, Variedad)
print(as.data.frame(t_con))
