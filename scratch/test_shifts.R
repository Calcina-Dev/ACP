library(dplyr)
load("Actualizacion Semanal/datos_maestros_cosecha_2026.RData")

m2_c <- df_conteo_historico %>%
  filter(as.character(Campaña) == "2026", Módulo %in% c("2", "02"), Turno %in% c("1", "01")) %>%
  mutate(Semana = as.numeric(Semana)) %>%
  group_by(Semana) %>%
  summarise(
    Flor = sum(Boton + Flor, na.rm = TRUE) / pmax(1, sum(MuestraF, na.rm = TRUE)),
    .groups = "drop"
  )

m2_y <- df_cosecha %>%
  filter(Campaña == 2026, Módulo %in% c("2", "02"), Turno %in% c("1", "01"), !coalesce(Es_Testblock, FALSE)) %>%
  mutate(Semana = as.numeric(Semana)) %>%
  group_by(Semana) %>%
  summarise(
    Rend = sum(`Kg Total`, na.rm = TRUE) / pmax(0.1, first(na.omit(Área))),
    .groups = "drop"
  )

cors <- sapply(6:15, function(sw) {
  df_m <- m2_y %>%
    mutate(Semana_Flor = Semana - sw) %>%
    inner_join(m2_c, by = c("Semana_Flor" = "Semana")) %>%
    filter(!is.na(Rend), !is.na(Flor))
  if (nrow(df_m) < 4) return(NA_real_)
  cor(df_m$Rend, df_m$Flor)
})

res_df <- tibble(Semanas = 6:15, Dias_Aprox = (6:15)*7, Correlacion = cors)
print(res_df %>% arrange(desc(Correlacion)))
