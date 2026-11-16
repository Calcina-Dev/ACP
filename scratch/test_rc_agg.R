library(dplyr)
rc <- readRDS("D:/ACP/Rutas de Cosecha/rutas_cosecha_mapa.rds")
sem_num <- 46

rc_sem <- rc %>%
  filter(as.numeric(Semana) == as.numeric(sem_num)) %>%
  filter(!("Tipo_Registro" %in% names(.)) | Tipo_Registro != "Rectificacion")

cols_ha <- intersect(c("Ha_Lunes", "Ha_Martes", "Ha_Miércoles", "Ha_Jueves", "Ha_Viernes", "Ha_Sábado"), names(rc_sem))
rc_sem$Ha_Total_Semana <- rowSums(rc_sem[, cols_ha], na.rm = TRUE)

rc_ha_turno_sem <- rc_sem %>%
  mutate(
    Turno_num_rc = suppressWarnings(as.numeric(Turno)),
    Módulo_clean = trimws(as.character(Módulo)),
    Sector_clean = trimws(as.character(Sector)),
    Turno_clean  = ifelse(!is.na(Turno_num_rc), as.character(Turno_num_rc), trimws(as.character(Turno))),
    Var_clean    = toupper(trimws(gsub("[^A-Za-z0-9]", "", Variedad))),
    num_pas_rc   = if ("Num_Pasada" %in% names(.)) suppressWarnings(as.numeric(Num_Pasada)) else 1
  ) %>%
  group_by(Sector = Sector_clean, Módulo = Módulo_clean, Turno = Turno_clean, Variedad = Var_clean) %>%
  summarise(
    Ha_Rc_Semana_1ra = sum(ifelse(coalesce(num_pas_rc, 1) == 1, Ha_Total_Semana, 0), na.rm = TRUE),
    Ha_Rc_Semana_2da = sum(ifelse(coalesce(num_pas_rc, 1) == 2, Ha_Total_Semana, 0), na.rm = TRUE),
    Ha_Rc_Semana_Tot = sum(Ha_Total_Semana, na.rm = TRUE),
    .groups = "drop"
  )

print(rc_ha_turno_sem %>% filter(Módulo == "1"))
cat("\nMódulo 8:\n")
print(rc_ha_turno_sem %>% filter(Módulo == "8"))
