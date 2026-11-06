library(dplyr)
library(tidyr)
e <- new.env()
load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData", envir = e)

# Recreate universo_base logic for Semana 45, Campaña 2026
df_cosecha <- e$df_cosecha %>% filter(as.character(Campaña) == "2026", Semana == 45, !coalesce(Es_Testblock, FALSE))
df_proy <- e$df_proyecciones %>% filter(as.character(Campaña) == "2026", Semana == 45)

dias_semana <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Domingo")

cosecha_preparada <- df_cosecha %>%
  mutate(
    Dia      = factor(dias_semana[lubridate::wday(Fecha, week_start = 1)], levels = dias_semana),
    Sector   = trimws(as.character(Sector)),
    Módulo   = trimws(as.character(Módulo)),
    Turno    = trimws(as.character(Turno)),
    Variedad = toupper(trimws(as.character(Variedad)))
  ) %>%
  group_by(Sector, Módulo, Turno, Variedad, Dia) %>%
  summarise(Kg_Dia = sum(`Kg Total`, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Dia, values_from = Kg_Dia, values_fill = 0)

for (d in dias_semana) {
  if (!d %in% names(cosecha_preparada)) cosecha_preparada[[d]] <- numeric(nrow(cosecha_preparada))
}

cosecha_preparada <- cosecha_preparada %>%
  mutate(
    Sector_join   = as.character(trimws(Sector)),
    Módulo_join   = as.character(trimws(Módulo)),
    Turno_join    = as.character(trimws(Turno)),
    Variedad_join = as.character(Variedad)
  )

df_proy <- df_proy %>%
  mutate(
    Sector_join   = as.character(trimws(Sector)),
    Módulo_join   = as.character(trimws(Módulo)),
    Turno_join    = as.character(trimws(Turno)),
    Variedad_join = as.character(Variedad)
  )

universo <- full_join(
  cosecha_preparada,
  df_proy,
  by = c("Sector_join", "Módulo_join", "Turno_join", "Variedad_join")
) %>%
  mutate(
    Sector   = coalesce(as.character(Sector.x), as.character(Sector.y), Sector_join),
    Módulo   = coalesce(as.character(Módulo.x), as.character(Módulo.y), Módulo_join),
    Turno    = coalesce(as.character(Turno.x), as.character(Turno.y), Turno_join),
    Variedad = as.character(coalesce(as.character(Variedad.x), as.character(Variedad.y), Variedad_join))
  )

cat("universo for Modulo 12, Turno 12:\n")
u12 <- universo %>% filter(grepl("^12$", trimws(as.character(Módulo))), grepl("^12$", trimws(as.character(Turno))))
print(u12 %>% select(Sector, Módulo, Turno, Variedad, Proyección, any_of(c("Lunes", "Martes", "Miércoles", "Jueves"))))
