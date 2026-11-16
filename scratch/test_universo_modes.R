library(dplyr)
load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
ediciones_manuales <- readRDS("D:/ACP/Actualizacion Semanal/ediciones_manuales_cosecha.rds")
escenarios_data    <- readRDS("D:/ACP/Actualizacion Semanal/escenarios_ajuste_semanal.rds")

# Simulate universo_base for M13-T1
# Let's see what df_cosecha has for M13-T1
norm_mod <- function(m) {
  case_when(
    as.character(m) %in% c("13.1", "13.2") ~ "13",
    as.character(m) %in% c("14.1", "14.2") ~ "14",
    TRUE ~ trimws(as.character(m))
  )
}
dias_nombres_wday <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Domingo")
dias_validos      <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")

# Ediciones filtradas for active scenario (Martes a Viernes)
esc_row <- escenarios_data %>% filter(Semana == 45) %>% tail(1)
ed_filtradas <- esc_row$Ediciones_Data[[1]]

# Turno 1 of M13
# In universo_base, M13-T1 has:
# Lunes: NA or real SAP
# Jueves: 169.69 kg (from df_cosecha)
u_base_t1 <- tibble(
  Sector = "3", Módulo = "13", Turno = "1", Variedad = "ROCIO", Tipo = "Comercial", Proyección = 893.5,
  Lunes = NA_real_, Lunes_tipo = "none", Lunes_bloqueado = FALSE, Lunes_manual = FALSE,
  Martes = NA_real_, Martes_tipo = "none", Martes_bloqueado = FALSE, Martes_manual = FALSE,
  Miércoles = NA_real_, Miércoles_tipo = "none", Miércoles_bloqueado = FALSE, Miércoles_manual = FALSE,
  Jueves = 169.69, Jueves_tipo = "real", Jueves_bloqueado = TRUE, Jueves_manual = FALSE,
  Viernes = NA_real_, Viernes_tipo = "none", Viernes_bloqueado = FALSE, Viernes_manual = FALSE,
  Sábado = NA_real_, Sábado_tipo = "none", Sábado_bloqueado = FALSE, Sábado_manual = FALSE
)

# Function to run the logic:
test_modo <- function(modo_detalle) {
  universo <- u_base_t1
  if (modo_detalle == "ajustado") {
    d_desde_sel <- "Martes"
    idx_ini <- match(d_desde_sel, dias_validos)
    if (idx_ini <= 6) {
      dias_a_limpiar <- dias_validos[idx_ini:6]
      for (d_clean in dias_a_limpiar) {
        universo[[d_clean]] <- NA_real_
        universo[[paste0(d_clean, "_tipo")]]      <- "none"
        universo[[paste0(d_clean, "_bloqueado")]] <- FALSE
        universo[[paste0(d_clean, "_manual")]]    <- FALSE
      }
    }
  }
  
  # Apply ed_filtradas
  ed_t1 <- ed_filtradas %>% filter(as.character(Módulo) == "13", as.character(Turno) == "1")
  for (i in seq_len(nrow(ed_t1))) {
    tipo_i  <- coalesce(as.character(ed_t1$Tipo_Edicion[i]), "manual")
    campo_i <- ed_t1$Campo[i]
    val_i   <- as.numeric(ed_t1$Valor[i])
    lock_col <- paste0(campo_i, "_bloqueado")
    debe_aplicar <- if (modo_detalle == "ajustado") TRUE else !universo[[lock_col]][1]
    if (debe_aplicar) {
      universo[[campo_i]][1] <- val_i
      universo[[paste0(campo_i, "_tipo")]][1] <- tipo_i
    }
  }
  return(universo)
}

res_ajustado <- test_modo("ajustado")
res_real     <- test_modo("real")

cat("VISTA AJUSTADO para M13-T1:\n")
print(res_ajustado %>% select(Lunes, Martes, Miércoles, Jueves, Viernes, Sábado))

cat("\nVISTA REAL para M13-T1:\n")
print(res_real %>% select(Lunes, Martes, Miércoles, Jueves, Viernes, Sábado))
