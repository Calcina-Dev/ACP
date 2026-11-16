suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(lubridate)
  library(plotly)
})

load("G:/Unidades compartidas/Compartido Proyecciones/3.- Arándano/datos_maestros_cosecha_2026.RData")

orden_semana_campana <- function(sem) {
  sem_num <- suppressWarnings(as.numeric(sem))
  ifelse(!is.na(sem_num) & sem_num < 20, sem_num + 53, sem_num)
}

# Simulate floral count for Modulo 3 Turno 1
df_c <- df_conteo_historico %>%
  filter(as.character(Campaña) == "2026",
         trimws(as.character(Módulo)) == "3",
         trimws(as.character(Turno)) == "1")

df_flor_sum <- df_c %>%
  mutate(Semana_Num = suppressWarnings(as.numeric(Semana))) %>%
  filter(!is.na(Semana_Num)) %>%
  group_by(Semana_Num) %>%
  summarise(
    Organos = sum(Boton + Flor, na.rm = TRUE),
    Muestra = sum(MuestraF, na.rm = TRUE),
    Carga = ifelse(Muestra > 0, round(Organos / Muestra, 1), round(mean(Total_Floral_Planta, na.rm = TRUE), 1)),
    .groups = "drop"
  ) %>%
  arrange(Semana_Num)

cat("df_flor_sum:\n")
print(head(df_flor_sum, 5))

dias_mad <- 67
shift_sem <- dias_mad / 7
df_flor_sum <- df_flor_sum %>%
  mutate(
    Semana_Proy = Semana_Num + shift_sem,
    Orden_Sem = orden_semana_campana(Semana_Proy)
  )

cat("\nWith shift:\n")
print(head(df_flor_sum, 5))
