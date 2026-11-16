library(dplyr)

# Cargar app.R y extraer datos de prueba
rc <- readRDS("D:/ACP/Rutas de Cosecha/rutas_cosecha_mapa.rds")
sem_num <- 46

rc_sem <- rc %>%
  filter(as.numeric(Semana) == as.numeric(sem_num)) %>%
  filter(!("Tipo_Registro" %in% names(.)) | Tipo_Registro != "Rectificacion")

cols_ha_dias <- intersect(c("Ha_Lunes", "Ha_Martes", "Ha_Miércoles", "Ha_Jueves", "Ha_Viernes", "Ha_Sábado"), names(rc_sem))
rc_sem$Ha_Sem_Fila <- if (length(cols_ha_dias) > 0) rowSums(rc_sem[, cols_ha_dias], na.rm = TRUE) else 0

rc_avance_semana <- rc_sem %>%
  mutate(
    Turno_num_rc = suppressWarnings(as.numeric(Turno)),
    Turno_clean  = ifelse(!is.na(Turno_num_rc), as.character(Turno_num_rc), trimws(as.character(Turno))),
    Módulo_clean = trimws(as.character(Módulo)),
    Sector_clean = trimws(as.character(Sector)),
    Var_clean    = toupper(trimws(gsub("[^A-Za-z0-9]", "", Variedad))),
    num_pas_rc   = if ("Num_Pasada" %in% names(.)) suppressWarnings(as.numeric(Num_Pasada)) else 1
  ) %>%
  group_by(Sector = Sector_clean, Módulo = Módulo_clean, Turno = Turno_clean, Variedad = Var_clean) %>%
  summarise(
    Ha_Real_Semana_1ra = sum(ifelse(coalesce(num_pas_rc, 1) == 1, Ha_Sem_Fila, 0), na.rm = TRUE),
    Ha_Real_Semana_2da = sum(ifelse(coalesce(num_pas_rc, 1) == 2, Ha_Sem_Fila, 0), na.rm = TRUE),
    Ha_Real_Semana_Tot = sum(Ha_Sem_Fila, na.rm = TRUE),
    .groups = "drop"
  )

# Simular u_base para M1
m1_base <- tibble(
  Sector_clean = "6",
  Módulo_clean = "1",
  Turno_clean = as.character(1:5),
  Var_clean = "SEKOYAPOP",
  Área = c(9.24, 9.32, 9.32, 9.32, 8.99),
  `Ha Real 1ª Pas` = c(0, 0, 0, 0, 0), # Como viene de universo_base cuando DESDE=Miércoles
  Kg_Real_Semana = c(1167.9, 1297.9, 1210.8, 1310.2, 1144.4)
)

res <- m1_base %>%
  left_join(rc_avance_semana, by = c("Sector_clean" = "Sector", "Módulo_clean" = "Módulo", "Turno_clean" = "Turno", "Var_clean" = "Variedad")) %>%
  mutate(
    Ha_Real_Semana_1ra = coalesce(Ha_Real_Semana_1ra, 0),
    Ha_Real_Prev_U     = coalesce(`Ha Real 1ª Pas`, 0),
    Ha_Campo = case_when(
      Ha_Real_Semana_1ra > 0 ~ round(pmin(Área, Ha_Real_Semana_1ra), 2),
      Ha_Real_Prev_U > 0     ~ round(pmin(Área, Ha_Real_Prev_U), 2),
      Kg_Real_Semana > 0 & Área > 0 ~ Área,
      TRUE                   ~ 0
    ),
    Pct_Termino  = case_when(
      Área > 0 & Ha_Campo >= (Área - 0.05) ~ 100,
      Área > 0 & (Ha_Campo / Área) >= 0.98  ~ 100,
      Área > 0 ~ round(pmin(100, (Ha_Campo / Área) * 100), 1),
      TRUE ~ 0
    ),
    Rend_Real = ifelse(Ha_Campo > 0 & Kg_Real_Semana > 0, round(Kg_Real_Semana / Ha_Campo, 1), NA_real_)
  )

print(res %>% select(Módulo_clean, Turno_clean, Área, Ha_Campo, Pct_Termino, Kg_Real_Semana, Rend_Real))
