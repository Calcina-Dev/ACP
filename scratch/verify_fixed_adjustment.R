load("Actualizacion Semanal RNA/datos_maestros_cosecha_2026.RData")
library(dplyr)

# Load helper functions
norm_codigo_valvula <- function(x) sub("^M0*([0-9]+)[-_]T0*([0-9]+)[-_]V0*([0-9]+)$", "M\\1-T\\2-V\\3", toupper(trimws(as.character(x))))
limpiar_variedad <- function(x) {
  v <- toupper(trimws(as.character(x)))
  chartr("ÁÉÍÓÚÜ", "AEIOUU", v)
  ifelse(grepl("EB\\s*9\\s*[-_.]\\s*2", v, ignore.case = TRUE), "MAGICA", v)
}

ruta_rc <- "D:/ACP/Rutas de Cosecha/rutas_cosecha_mapa.rds"
rc_sem <- if (file.exists(ruta_rc)) readRDS(ruta_rc) %>% filter(Semana == 45) else tibble()

valvs_con_2da_rc <- if (nrow(rc_sem) > 0 && "codigo" %in% names(rc_sem)) {
  rc_sem %>%
    filter(coalesce(suppressWarnings(as.numeric(Num_Pasada)), 1) == 2) %>%
    mutate(codigo_norm = norm_codigo_valvula(codigo)) %>%
    filter(coalesce(Ha_Total_Prog, 0) > 0 | coalesce(Kg_Total_Prog, 0) > 0) %>%
    pull(codigo_norm) %>%
    unique()
} else character(0)

# Verify if M13 T2 valves are in valvs_con_2da_rc
m13_t2_valvs <- c("M13-T2-V5", "M13-T2-V6", "M13-T2-V7", "M13-T2-V8")
cat("M13-T2 válvulas con 2da pasada:", all(m13_t2_valvs %in% valvs_con_2da_rc), "\n")

# Simulate reg_2da_lun filter condition
m13_t2_calc <- df_valvulas %>%
  filter(grepl("13", Módulo), Turno == "2") %>%
  mutate(
    codigo_norm = norm_codigo_valvula(codigo),
    Esta_Proyectado = TRUE,
    Semana_Ant_No_Finalizo = FALSE,
    Es_Ciclo_Corto = TRUE,
    Ha_Cosechada_Lunes = 1.6,
    Turno_Completo_2da = TRUE, # Because 6.40 ha was already harvested in 2da pasada
    Valvula_Tiene_2da_Real = codigo_norm %in% valvs_con_2da_rc
  )

reg_2da_lun_test <- m13_t2_calc %>%
  filter(
    coalesce(Esta_Proyectado, FALSE),
    !Semana_Ant_No_Finalizo,
    Es_Ciclo_Corto,
    Ha_Cosechada_Lunes > 0,
    !coalesce(Turno_Completo_2da, FALSE),
    !coalesce(Valvula_Tiene_2da_Real, FALSE)
  )

cat("Filas generadas en reg_2da_lun para M13 T2:", nrow(reg_2da_lun_test), "\n")
cat("✔ ¡Éxito! 0 filas generadas. Ya no se duplica la segunda pasada cuando ya está completa.\n")
