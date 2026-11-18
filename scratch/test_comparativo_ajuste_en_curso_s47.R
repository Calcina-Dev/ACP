suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

# Cargar app.R en un entorno para evaluar evaluar_escenario o replicar la lógica exacta de app.R
source_lines <- readLines("Actualizacion Semanal/app.R", encoding = "UTF-8", warn = FALSE)

# Extraer evaluar_escenario
idx_start <- grep("evaluar_escenario <- function\\(df_ed, esc_info = tibble\\(\\)\\) \\{", source_lines)
# Find matching end bracket
depth <- 0
idx_end <- NA
for (i in idx_start:length(source_lines)) {
  line <- source_lines[i]
  # count { and }
  opens <- nchar(gsub("[^{]", "", line))
  closes <- nchar(gsub("[^}]", "", line))
  depth <- depth + opens - closes
  if (depth == 0) {
    idx_end <- i
    break
  }
}

cat("evaluar_escenario extracted from line", idx_start, "to", idx_end, "\n")
eval_code <- paste(source_lines[idx_start:idx_end], collapse = "\n")

# Setup environment
dias_semana_nombres <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
limpiar_variedad <- function(x) {
  if (is.null(x) || length(x) == 0) return(character(0))
  v <- toupper(trimws(as.character(x)))
  as.character(ifelse(grepl("EB\\s*9\\s*[-_.]\\s*2", v, ignore.case = TRUE), "MAGICA", v))
}

input <- list(
  filtro_campana = "2026",
  filtro_semana = 47,
  ajustar_desde_dia = "Martes",
  ajustar_hasta_dia = "Viernes"
)

# Mock u_base with real harvest on Lunes and Proyeccion
load("Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
dias_semana <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Domingo")

df_filtrado <- df_cosecha %>%
  filter(
    as.character(Campaña) == "2026",
    Semana == 47,
    !coalesce(Es_Testblock, FALSE)
  )

cosecha_preparada <- df_filtrado %>%
  mutate(
    Dia       = factor(dias_semana[lubridate::wday(Fecha, week_start = 1)], levels = dias_semana),
    Turno_num = suppressWarnings(as.numeric(Turno)),
    Sector    = case_when(as.character(Módulo) %in% c("9", "9.1", "9.2", "11", "11.1", "11.2", "12") ~ "9", TRUE ~ trimws(as.character(Sector))),
    Módulo    = trimws(as.character(Módulo)),
    Módulo    = case_when(
      Módulo %in% c("13.1", "13.2") ~ "13",
      Módulo %in% c("14.1", "14.2") ~ "14",
      as.character(Módulo) == "9" & between(Turno_num, 1, 11)  ~ "9.2",
      as.character(Módulo) == "9" & Turno_num >= 12            ~ "9.1",
      as.character(Módulo) == "11" & between(Turno_num, 1, 12) ~ "11.1",
      as.character(Módulo) == "11" & Turno_num >= 13           ~ "11.2",
      TRUE ~ Módulo
    ),
    Turno    = trimws(as.character(Turno)),
    Variedad = limpiar_variedad(Variedad)
  ) %>%
  group_by(Sector, Módulo, Turno, Variedad, Dia) %>%
  summarise(Kg_Dia = sum(`Kg Total`, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Dia, values_from = Kg_Dia, values_fill = 0) %>%
  mutate(Es_Testblock = FALSE)

for (d in dias_semana) {
  if (!d %in% names(cosecha_preparada)) cosecha_preparada[[d]] <- numeric(nrow(cosecha_preparada))
}

u_base <- cosecha_preparada %>%
  mutate(
    Área = 10,
    Proyección = 1000,
    `Ha Real 1ª Pas` = 0,
    `Ha Real 2ª Pas` = 0
  )

rc_avance_semana <- tibble(
  Módulo = character(), Turno = character(), Variedad = character(),
  Ha_Real_Semana_1ra = numeric(), Ha_Real_Semana_2da = numeric(), Ha_Real_Semana_Tot = numeric()
)

ed_all <- readRDS("Actualizacion Semanal/ediciones_manuales_cosecha.rds")
ed_eval <- ed_all %>% filter(as.character(`Campaña`) == "2026", as.numeric(Semana) == 47, coalesce(Valor, 0) > 0)

# Evaluate the function in this environment
eval(parse(text = eval_code))

res_esc <- evaluar_escenario(ed_eval, tibble())

cat("\n=== RESULTADOS TEST COMPARATIVO S47 (AJUSTE EN CURSO) ===\n")
cat("Suma Kg_Real_Prev (Lunes):", sum(res_esc$Kg_Real_Prev), "kg\n")
cat("Suma Kg_Ajustado_Esc (Mar-Vie):", sum(res_esc$Kg_Ajustado_Esc), "kg\n")
cat("Suma Kg_Ajustado_Semana (Total):", sum(res_esc$Kg_Ajustado_Semana), "kg\n")

# Verify that Kg_Real_Prev > 0 and incorporates Lunes
stopifnot(sum(res_esc$Kg_Real_Prev) > 50000)
stopifnot(sum(res_esc$Kg_Ajustado_Semana) > 100000)

cat("✔ TEST EXITOSO: Cosecha Ajustada incluye correctamente los kilos reales previos de Lunes!\n")
