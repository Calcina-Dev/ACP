suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
})

load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")

cs_45 <- df_cosecha %>%
  filter(Campaña == "2026", Semana == 45, !coalesce(Es_Testblock, FALSE)) %>%
  mutate(
    Módulo_norm = case_when(
      as.character(Módulo) %in% c("13.1", "13.2") ~ "13",
      as.character(Módulo) %in% c("14.1", "14.2") ~ "14",
      TRUE ~ as.character(Módulo)
    ),
    Fecha_d = as.Date(Fecha),
    Dia_Nom = c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Domingo")[wday(Fecha_d, week_start = 1)]
  )

cs_m13 <- cs_45 %>% filter(Módulo_norm == "13")
cat("M13 rows in cs_45:", nrow(cs_m13), "Total kg:", sum(cs_m13$`Kg Total`), "\n")
print(cs_m13 %>% group_by(Turno, Dia_Nom) %>% summarise(Kg = sum(`Kg Total`), .groups = "drop"))
