suppressPackageStartupMessages(library(dplyr))
load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")

m <- "7"
v <- "VENTURA"
t <- "4"
d <- 49
camp_chr <- "2026"
camp_num <- 2026

# Filtered with variety:
df_y <- df_cosecha %>%
  filter(
    (as.character(Campaña) == camp_chr | (!is.na(camp_num) & Campaña == camp_num)),
    trimws(as.character(Módulo)) == trimws(as.character(m)),
    trimws(as.character(Turno)) == trimws(as.character(t)),
    trimws(toupper(as.character(Variedad))) == trimws(toupper(as.character(v))),
    !coalesce(Es_Testblock, FALSE),
    `Kg Total` > 0
  ) %>%
  mutate(Semana_Num = suppressWarnings(as.numeric(Semana))) %>%
  filter(!is.na(Semana_Num)) %>%
  group_by(Semana_Num) %>%
  summarise(
    Kg_Total = sum(`Kg Total`, na.rm = TRUE),
    Area     = first(na.omit(Área)),
    Rend     = ifelse(!is.na(Area) && Area > 0, round(Kg_Total / Area, 1), round(Kg_Total, 1)),
    .groups  = "drop"
  ) %>%
  arrange(Semana_Num)

df_c <- df_conteo_historico %>%
  filter(
    (as.character(Campaña) == camp_chr | (!is.na(camp_num) & Campaña == camp_num)),
    trimws(as.character(Módulo)) == trimws(as.character(m)),
    trimws(as.character(Turno)) == trimws(as.character(t)),
    trimws(toupper(as.character(Variedad))) == trimws(toupper(as.character(v)))
  ) %>%
  mutate(Semana_Num = suppressWarnings(as.numeric(Semana))) %>%
  filter(!is.na(Semana_Num)) %>%
  group_by(Semana_Num) %>%
  summarise(
    Organos = sum(Boton + Flor, na.rm = TRUE),
    Muestra = sum(MuestraF, na.rm = TRUE),
    Carga   = ifelse(Muestra > 0, round(Organos / Muestra, 1), round(mean(Total_Floral_Planta, na.rm = TRUE), 1)),
    .groups = "drop"
  ) %>%
  arrange(Semana_Num)

cat("Cosecha Real Ventura M7 T4:\n")
print(as.data.frame(df_y))

shift_sem <- d / 7
df_c_shifted <- df_c %>%
  mutate(Semana_Proy = Semana_Num + shift_sem)
cat("\nConteo Floral Shifted (+7 sem / 49 d):\n")
print(as.data.frame(df_c_shifted %>% filter(Semana_Proy >= 30, Semana_Proy <= 50) %>% select(Semana_Num, Semana_Proy, Carga)))

pico_y <- df_y$Semana_Num[which.max(df_y$Rend)]
pico_c_proy <- df_c_shifted$Semana_Proy[which.max(df_c_shifted$Carga)]
cat(sprintf("\nPico Real: Sem %d | Pico Flor Proy (+49d): Sem %d | Δ = %d sem\n",
            pico_y, as.integer(pico_c_proy), abs(pico_y - as.integer(pico_c_proy))))
