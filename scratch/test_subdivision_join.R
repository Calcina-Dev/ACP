suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
})

limpiar_variedad <- function(x) {
  if (is.null(x) || length(x) == 0) return(character(0))
  x_str <- toupper(trimws(as.character(x)))
  x_str <- as.character(chartr("ÁÉÍÓÚÜ", "AEIOUU", x_str))
  as.character(ifelse(grepl("EB\\s*9\\s*[-_.]\\s*2", x_str, ignore.case = TRUE), "MAGICA", x_str))
}

load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")

sem_sel <- 45
camp_sel <- "2026"

cat("=== 1. TEST PROYECCION SEMANA 45 ===\n")
proy_sem <- df_proyecciones %>%
  filter(Semana == sem_sel) %>%
  mutate(
    Sector_join = as.character(trimws(Sector)),
    Módulo_join = as.character(trimws(Módulo_join)),
    Turno_join  = as.character(trimws(Turno_join)),
    Variedad_join = as.character(limpiar_variedad(Variedad_join))
  ) %>%
  group_by(Sector_join, Módulo_join, Turno_join, Variedad_join) %>%
  summarise(Variedad = first(Variedad), Proyección = sum(Proyección, na.rm = TRUE), .groups = "drop")

print(proy_sem %>% filter(grepl("9|11|12", Módulo_join)) %>% count(Sector_join, Módulo_join))

cat("\n=== 2. TEST COSECHA PREPARADA SEMANA 45 CON SUBDIVISION ===\n")
df_filtrado <- df_cosecha %>%
  filter(as.character(Campaña) == camp_sel, Semana == sem_sel, !coalesce(Es_Testblock, FALSE))

cosecha_prep <- df_filtrado %>%
  mutate(
    Turno_num = suppressWarnings(as.numeric(Turno)),
    Sector    = case_when(as.character(Módulo) %in% c("9", "9.1", "9.2", "11", "11.1", "11.2", "12") ~ "9", TRUE ~ trimws(as.character(Sector))),
    Módulo    = case_when(
      as.character(Módulo) %in% c("13.1", "13.2") ~ "13",
      as.character(Módulo) %in% c("14.1", "14.2") ~ "14",
      as.character(Módulo) == "9" & between(Turno_num, 1, 11)  ~ "9.2",
      as.character(Módulo) == "9" & Turno_num >= 12            ~ "9.1",
      as.character(Módulo) == "11" & between(Turno_num, 1, 12) ~ "11.1",
      as.character(Módulo) == "11" & Turno_num >= 13           ~ "11.2",
      TRUE                                                     ~ trimws(as.character(Módulo))
    ),
    Turno = trimws(as.character(Turno)),
    Variedad = limpiar_variedad(Variedad),
    Dia = factor(c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Domingo")[wday(Fecha, week_start = 1)],
                 levels = c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Domingo"))
  ) %>%
  group_by(Sector, Módulo, Turno, Variedad, Dia) %>%
  summarise(Kg_Dia = sum(`Kg Total`, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Dia, values_from = Kg_Dia, values_fill = 0)

print(cosecha_prep %>% filter(grepl("9|11|12", Módulo)) %>% count(Sector, Módulo))

cat("\n=== 3. FULL JOIN UNIVERSO ===\n")
cosecha_prep <- cosecha_prep %>%
  mutate(
    Sector_join   = as.character(trimws(Sector)),
    Módulo_join   = as.character(trimws(Módulo)),
    Turno_join    = as.character(trimws(Turno)),
    Variedad_join = as.character(limpiar_variedad(Variedad))
  )

universo <- full_join(cosecha_prep, proy_sem, by = c("Sector_join", "Módulo_join", "Turno_join", "Variedad_join")) %>%
  mutate(
    Sector = coalesce(as.character(Sector), Sector_join),
    Módulo = coalesce(as.character(Módulo), Módulo_join),
    Turno  = coalesce(as.character(Turno), Turno_join),
    Variedad = as.character(limpiar_variedad(coalesce(as.character(Variedad.x), as.character(Variedad.y), Variedad_join)))
  )

cat("Universo Módulos 9, 11, 12 count por Sector y Módulo:\n")
print(universo %>% filter(grepl("9|11|12", Módulo)) %>% count(Sector, Módulo))

cat("\nCheck duplicados de turno en universo (M9, M11, M12):\n")
dups <- universo %>%
  filter(grepl("9|11|12", Módulo)) %>%
  count(Sector, Módulo, Turno, Variedad) %>%
  filter(n > 1)
cat("Duplicados encontrados:", nrow(dups), "\n")
if (nrow(dups) > 0) print(dups)
