library(dplyr)
library(tidyr)
load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")

sem_sel  <- 45
camp_sel <- "2026"
dia_inicio_nom <- "Martes"
dia_fin_nom    <- "Viernes"
dias_nombres   <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
dia_inicio_idx <- which(dias_nombres == dia_inicio_nom)[1]
dia_fin_idx    <- which(dias_nombres == dia_fin_nom)[1]
dias_ajustables_nom <- dias_nombres[dia_inicio_idx:dia_fin_idx]
dias_previos_bloq <- if (dia_inicio_idx > 1) dias_nombres[1:(dia_inicio_idx - 1)] else character(0)

# Simulate u_base for M1
u_base <- df_proyecciones %>%
  filter(as.character(Campaña) == camp_sel, as.numeric(Semana) == sem_sel, as.character(Módulo) == "1") %>%
  mutate(
    Sector = "6", Módulo = "1", Turno = as.character(Turno), Variedad = "SEKOYA POP",
    Lunes = ifelse(Turno == "2", 591.74, NA_real_),
    Lunes_bloqueado = !is.na(Lunes),
    Martes = case_when(Turno == "1" ~ 1431.81, Turno == "2" ~ 744.84, Turno == "5" ~ 856.71, TRUE ~ NA_real_),
    Martes_bloqueado = !is.na(Martes),
    Miércoles = case_when(Turno == "3" ~ 1684.68, Turno == "4" ~ 1554.66, Turno == "5" ~ 698.26, TRUE ~ NA_real_),
    Miércoles_bloqueado = !is.na(Miércoles),
    Jueves = NA_real_, Jueves_bloqueado = FALSE,
    Viernes = NA_real_, Viernes_bloqueado = FALSE,
    Sábado = NA_real_, Sábado_bloqueado = FALSE
  )

# Compute claves_sap_bloqueadas with the fixed logic:
claves_sap_bloqueadas <- character(0)
claves_sap_bloqueadas_mtv <- character(0)
for (dia_col in dias_previos_bloq) {
  lock_col <- paste0(dia_col, "_bloqueado")
  if (lock_col %in% names(u_base)) {
    bloq_idx <- which(coalesce(u_base[[lock_col]], FALSE) == TRUE)
    if (length(bloq_idx) > 0) {
      claves_sap_bloqueadas <- c(
        claves_sap_bloqueadas,
        paste(trimws(as.character(u_base$Sector[bloq_idx])), trimws(as.character(u_base$Módulo[bloq_idx])), trimws(as.character(u_base$Turno[bloq_idx])), trimws(toupper(as.character(u_base$Variedad[bloq_idx]))), dia_col, sep = "___")
      )
      claves_sap_bloqueadas_mtv <- c(
        claves_sap_bloqueadas_mtv,
        paste(trimws(as.character(u_base$Módulo[bloq_idx])), trimws(as.character(u_base$Turno[bloq_idx])), trimws(toupper(as.character(u_base$Variedad[bloq_idx]))), dia_col, sep = "___")
      )
    }
  }
}

cat("Claves SAP bloqueadas (solo previas):\n")
print(claves_sap_bloqueadas_mtv)

# In auto_ediciones:
# All 5 turnos have adjustments on Martes and/or Miércoles:
auto_ediciones <- tibble(
  Sector = "6", Módulo = "1", Turno = as.character(1:5), Variedad = "SEKOYA POP",
  Campo = c("Martes", "Martes", "Miércoles", "Miércoles", "Martes"),
  Valor = c(2310, 2330, 2330, 2330, 2248),
  Ha_Asignada = c(9.24, 9.32, 9.32, 9.32, 8.99),
  Tipo_Edicion = "auto"
)

claves_auto_mtv <- paste(auto_ediciones$Módulo, auto_ediciones$Turno, auto_ediciones$Variedad, auto_ediciones$Campo, sep = "___")
auto_validas <- auto_ediciones[!(claves_auto_mtv %in% claves_sap_bloqueadas_mtv), ]

cat("\nauto_validas count:", nrow(auto_validas), "de 5 turnos!\n")
print(auto_validas)
