load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
library(dplyr)
library(tidyr)
library(lubridate)

limpiar_variedad <- function(var) {
  v <- toupper(trimws(as.character(var)))
  v <- gsub("['\"`]", "", v)
  v <- gsub("\\s+", " ", v)
  v <- chartr("ÁÉÍÓÚ", "AEIOU", v)
  v <- case_when(
    grepl("SEKOYA.*BEAUT", v) ~ "SEKOYA BEAUTY",
    grepl("SEKOYA.*CRUNCH", v) ~ "SEKOYA CRUNCH",
    grepl("SEKOYA.*POP", v)    ~ "SEKOYA POP",
    grepl("MATIAS", v)         ~ "MATIAS",
    grepl("MAGICA", v)         ~ "MAGICA",
    grepl("ROCIO", v)          ~ "ROCIO",
    grepl("MANILA", v)         ~ "MANILA",
    grepl("MALIBU", v)         ~ "MALIBU",
    grepl("MADEIRA", v)        ~ "MADEIRA",
    grepl("ATLAS", v)          ~ "ATLAS BLUE",
    grepl("BEAUTY", v)         ~ "SEKOYA BEAUTY",
    grepl("CRUNCH", v)         ~ "SEKOYA CRUNCH",
    grepl("POP", v)            ~ "SEKOYA POP",
    TRUE                       ~ v
  )
  v
}

sem_sel <- 45
camp_sel <- "2026"

# Proyección
df_proy <- df_proyecciones %>%
  filter(as.character(Campaña) == camp_sel, as.numeric(Semana) == sem_sel) %>%
  mutate(
    Sector_join   = as.character(trimws(Sector)),
    Módulo_join   = as.character(trimws(Módulo)),
    Turno_join    = as.character(trimws(Turno)),
    Variedad_join = as.character(limpiar_variedad(Variedad))
  )

# Cosecha SAP
df_filtrado <- df_cosecha %>%
  filter(as.character(Campaña) == camp_sel, as.numeric(Semana) == sem_sel, !coalesce(Es_Testblock, FALSE))

dias_semana <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Domingo")

cosecha_preparada <- df_filtrado %>%
  mutate(
    Dia      = factor(dias_semana[wday(Fecha, week_start = 1)], levels = dias_semana),
    Sector   = case_when(as.character(Módulo) == "12" ~ "9", TRUE ~ trimws(as.character(Sector))),
    Módulo   = trimws(as.character(Módulo)),
    Turno    = trimws(as.character(Turno)),
    Variedad = limpiar_variedad(Variedad)
  ) %>%
  group_by(Sector, Módulo, Turno, Variedad, Dia) %>%
  summarise(Kg_Dia = sum(`Kg Total`, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Dia, values_from = Kg_Dia, values_fill = 0) %>%
  mutate(
    Sector_join   = as.character(trimws(Sector)),
    Módulo_join   = as.character(trimws(Módulo)),
    Turno_join    = as.character(trimws(Turno)),
    Variedad_join = as.character(limpiar_variedad(Variedad))
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
    Variedad = as.character(limpiar_variedad(coalesce(as.character(Variedad.x), as.character(Variedad.y), Variedad_join)))
  )

cat("--- Filas en Universo Base para Módulo 12 Turno 12 ---\n")
m12_t12_u <- universo %>% filter(as.character(Módulo) == "12", as.character(Turno) == "12")
print(m12_t12_u %>% select(any_of(c("Sector", "Módulo", "Turno", "Variedad", "Proyección", "Lunes", "Martes", "Miércoles", "Jueves"))))

# Ediciones en RDS
ed_todas <- readRDS("D:/ACP/Actualizacion Semanal/ediciones_manuales_cosecha.rds")
ed_s45 <- ed_todas %>% filter(as.character(Campaña) == camp_sel, as.numeric(Semana) == sem_sel)

cat("\n--- Ediciones en RDS para Módulo 12 Turno 12 ---\n")
print(ed_s45 %>% filter(as.character(Módulo) == "12", as.character(Turno) == "12") %>% select(Campaña, Semana, Sector, Módulo, Turno, Variedad, Campo, Valor, Tipo_Edicion))

cat("\nTotal de filas M12 T12 en universo:", nrow(m12_t12_u), "\n")
load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
library(dplyr)

cat("--- Checking distinct Modulo and Turno in Universo Base ---\n")
dups <- universo %>%
  group_by(Módulo, Turno, Variedad) %>%
  filter(n() > 1)

cat("Number of duplicated (Modulo, Turno, Variedad) in universo:", nrow(dups), "\n")
if (nrow(dups) > 0) {
  print(dups %>% select(Sector, Módulo, Turno, Variedad))
} else {
  cat("✔ 100% CLEAN: ZERO DUPLICATES in universo base across all modules!\n")
}
