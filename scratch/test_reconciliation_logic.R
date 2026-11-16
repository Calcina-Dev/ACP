suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
})

load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
rc_raw <- readRDS("D:/ACP/Rutas de Cosecha/rutas_cosecha_mapa.rds")

norm_codigo_valvula <- function(x) {
  if (is.null(x)) return(character(0))
  x <- toupper(trimws(as.character(x)))
  x <- gsub("\\s+", "", x)
  x <- sub("^VALVULA[-_]?", "", x)
  x <- sub("^V[-_]?", "", x)
  m_sub <- sub(".*M0*([0-9]+(?:\\.[0-9]+)?).*", "\\1", x)
  t_sub <- sub(".*T0*([0-9]+).*", "\\1", x)
  v_sub <- sub(".*V0*([0-9]+)$", "\\1", x)
  valid <- grepl("M0*[0-9]+.*T0*[0-9]+.*V0*[0-9]+", x)
  ifelse(valid, sprintf("M%s-T%s-V%s", m_sub, t_sub, v_sub), x)
}

norm_ascii <- function(x) {
  tolower(chartr("áéíóúÁÉÍÓÚ", "aeiouAEIOU", trimws(as.character(x))))
}

sem_num <- 45
pasada_str <- "Todas"
fecha_hoy <- Sys.Date()
sem_actual_num <- lubridate::isoweek(fecha_hoy)
wday_hoy <- lubridate::wday(fecha_hoy, week_start = 1)
es_semana_actual <- (sem_num == sem_actual_num)
dias_cols <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
dias_ascii <- c("lunes", "martes", "miercoles", "jueves", "viernes", "sabado")

dias_cerrados_nom <- if (es_semana_actual) {
  if (wday_hoy > 1) dias_cols[1:min(wday_hoy - 1, 6)] else character(0)
} else if (sem_num < sem_actual_num) {
  dias_cols
} else {
  character(0)
}

# 1. df_campo
col_cod <- intersect(c("Codigo_Valvula", "codigo", "Codigo_Valv"), names(rc_raw))[1]
rc_sem <- rc_raw %>%
  filter(as.numeric(Semana) == sem_num)

df_campo <- rc_sem %>%
  mutate(codigo_norm = norm_codigo_valvula(.data[[col_cod]])) %>%
  filter(!is.na(codigo_norm), nzchar(codigo_norm)) %>%
  group_by(codigo_norm) %>%
  summarise(
    Ha_Lunes_rc     = sum(coalesce(Ha_Lunes, 0), na.rm = TRUE),
    Ha_Martes_rc    = sum(coalesce(Ha_Martes, 0), na.rm = TRUE),
    Ha_Miercoles_rc = sum(coalesce(Ha_Miércoles, 0), na.rm = TRUE),
    Ha_Jueves_rc    = sum(coalesce(Ha_Jueves, 0), na.rm = TRUE),
    Ha_Viernes_rc   = sum(coalesce(Ha_Viernes, 0), na.rm = TRUE),
    Ha_Sabado_rc    = sum(coalesce(Ha_Sábado, 0), na.rm = TRUE),
    Ha_Total_rc     = Ha_Lunes_rc + Ha_Martes_rc + Ha_Miercoles_rc + Ha_Jueves_rc + Ha_Viernes_rc + Ha_Sabado_rc,
    .groups         = "drop"
  )

# 2. df_sap
sap_sem <- df_hist_oficial_wide %>% filter(as.numeric(Semana) == sem_num)
df_sap <- sap_sem %>%
  mutate(codigo_norm = norm_codigo_valvula(Codigo_Valv)) %>%
  filter(!is.na(codigo_norm), nzchar(codigo_norm)) %>%
  group_by(codigo_norm) %>%
  summarise(
    Sector           = first(Sector),
    Módulo           = first(Módulo_Norm),
    Turno            = first(Turno_Norm),
    Variedad_sap     = first(Variedad),
    Kg_Lunes_sap     = sum(coalesce(Kg_Lunes, 0), na.rm = TRUE),
    Kg_Martes_sap    = sum(coalesce(Kg_Martes, 0), na.rm = TRUE),
    Kg_Miercoles_sap = sum(coalesce(Kg_Miércoles, 0), na.rm = TRUE),
    Kg_Jueves_sap    = sum(coalesce(Kg_Jueves, 0), na.rm = TRUE),
    Kg_Viernes_sap   = sum(coalesce(Kg_Viernes, 0), na.rm = TRUE),
    Kg_Sabado_sap    = sum(coalesce(Kg_Sábado, 0), na.rm = TRUE),
    Kg_Total_sap     = Kg_Lunes_sap + Kg_Martes_sap + Kg_Miercoles_sap + Kg_Jueves_sap + Kg_Viernes_sap + Kg_Sabado_sap,
    .groups          = "drop"
  )

todos_codigos <- unique(c(df_campo$codigo_norm, df_sap$codigo_norm))
df_rec <- tibble(codigo_norm = todos_codigos) %>%
  left_join(df_campo, by = "codigo_norm") %>%
  left_join(df_sap, by = "codigo_norm") %>%
  mutate(
    across(starts_with("Ha_"), ~ coalesce(.x, 0)),
    across(starts_with("Kg_"), ~ coalesce(.x, 0)),
    Tiene_Campo  = Ha_Total_rc > 0,
    Tiene_SAP    = Kg_Total_sap > 0,
    
    Kg_Lunes_rec     = Kg_Lunes_sap,
    Kg_Martes_rec    = Kg_Martes_sap,
    Kg_Miercoles_rec = Kg_Miercoles_sap,
    Kg_Jueves_rec    = Kg_Jueves_sap,
    Kg_Viernes_rec   = Kg_Viernes_sap,
    Kg_Sabado_rec    = Kg_Sabado_sap,
    Kg_Total_rec     = Kg_Total_sap,
    
    Kg_Lunes_sr      = 0,
    Kg_Martes_sr     = 0,
    Kg_Miercoles_sr  = 0,
    Kg_Jueves_sr     = 0,
    Kg_Viernes_sr    = 0,
    Kg_Sabado_sr     = 0,
    Kg_Sin_Ruta      = 0,
    Kg_Desfase       = 0,
    
    Estado_Concil        = "Conciliado OK",
    Tipo_Alerta          = "conciliado",
    Diagnostico          = "Campo y SAP coinciden plenamente.",
    Es_Valido_Operativo  = TRUE
  )

for (i in seq_len(nrow(df_rec))) {
  r <- df_rec[i, ]
  
  # REGLA 1: SAP sin Ruta -> Excluir de cálculos y pintado
  if (!r$Tiene_Campo && r$Tiene_SAP) {
    df_rec$Estado_Concil[i]        <- "Kilos SAP sin Ruta"
    df_rec$Tipo_Alerta[i]          <- "inconsistencia"
    df_rec$Diagnostico[i]          <- sprintf("Pesado en SAP (%s kg), pero no programado en ruta de campo. Excluido de mapa y cálculos.", format(round(r$Kg_Total_sap, 0), big.mark = ","))
    df_rec$Es_Valido_Operativo[i]  <- FALSE
    df_rec$Kg_Sin_Ruta[i]          <- r$Kg_Total_sap
    df_rec$Kg_Lunes_sr[i]          <- r$Kg_Lunes_sap
    df_rec$Kg_Martes_sr[i]         <- r$Kg_Martes_sap
    df_rec$Kg_Miercoles_sr[i]      <- r$Kg_Miercoles_sap
    df_rec$Kg_Jueves_sr[i]         <- r$Kg_Jueves_sap
    df_rec$Kg_Viernes_sr[i]        <- r$Kg_Viernes_sap
    df_rec$Kg_Sabado_sr[i]         <- r$Kg_Sabado_sap
    
    df_rec$Kg_Lunes_rec[i]         <- 0
    df_rec$Kg_Martes_rec[i]        <- 0
    df_rec$Kg_Miercoles_rec[i]     <- 0
    df_rec$Kg_Jueves_rec[i]        <- 0
    df_rec$Kg_Viernes_rec[i]       <- 0
    df_rec$Kg_Sabado_rec[i]        <- 0
    df_rec$Kg_Total_rec[i]         <- 0
    next
  }
  
  # REGLA 3: Ruta sin Kilos en SAP
  if (r$Tiene_Campo && !r$Tiene_SAP) {
    dias_c <- character(0)
    if (r$Ha_Lunes_rc > 0) dias_c <- c(dias_c, "Lunes")
    if (r$Ha_Martes_rc > 0) dias_c <- c(dias_c, "Martes")
    if (r$Ha_Miercoles_rc > 0) dias_c <- c(dias_c, "Miércoles")
    if (r$Ha_Jueves_rc > 0) dias_c <- c(dias_c, "Jueves")
    if (r$Ha_Viernes_rc > 0) dias_c <- c(dias_c, "Viernes")
    if (r$Ha_Sabado_rc > 0) dias_c <- c(dias_c, "Sábado")
    
    tiene_previos <- any(dias_c %in% dias_cerrados_nom)
    if (es_semana_actual && !tiene_previos) {
      df_rec$Estado_Concil[i]        <- "Cosecha Hoy (D+1)"
      df_rec$Tipo_Alerta[i]          <- "en_proceso"
      df_rec$Diagnostico[i]          <- sprintf("Cosechado hoy (%s) en campo. Pendiente de recepción en balanza de packing mañana.", paste(dias_c, collapse = ", "))
      df_rec$Es_Valido_Operativo[i]  <- TRUE
      df_rec$Kg_Total_rec[i]         <- 0
    } else {
      df_rec$Estado_Concil[i]        <- "Ruta sin Kilos SAP"
      df_rec$Tipo_Alerta[i]          <- "inconsistencia"
      df_rec$Diagnostico[i]          <- sprintf("Cosechado en campo [%s], 0 kg en SAP tras cierre. En reporte de incidencias para edición manual en Excel.",
                                                paste(dias_c, collapse = ", "))
      df_rec$Es_Valido_Operativo[i]  <- TRUE
      df_rec$Kg_Lunes_rec[i]         <- 0
      df_rec$Kg_Martes_rec[i]        <- 0
      df_rec$Kg_Miercoles_rec[i]     <- 0
      df_rec$Kg_Jueves_rec[i]        <- 0
      df_rec$Kg_Viernes_rec[i]       <- 0
      df_rec$Kg_Sabado_rec[i]        <- 0
      df_rec$Kg_Total_rec[i]         <- 0
    }
    next
  }
  
  # REGLA 2: Cruce de días cerrados
  if (r$Tiene_Campo && r$Tiene_SAP) {
    kg_dias <- c(Lunes = r$Kg_Lunes_sap, Martes = r$Kg_Martes_sap, Miércoles = r$Kg_Miercoles_sap,
                 Jueves = r$Kg_Jueves_sap, Viernes = r$Kg_Viernes_sap, Sábado = r$Kg_Sabado_sap)
    ha_dias <- c(Lunes = r$Ha_Lunes_rc, Martes = r$Ha_Martes_rc, Miércoles = r$Ha_Miercoles_rc,
                 Jueves = r$Ha_Jueves_rc, Viernes = r$Ha_Viernes_rc, Sábado = r$Ha_Sabado_rc)
    
    dias_c_act <- names(ha_dias)[ha_dias > 0]
    dias_s_act <- names(kg_dias)[kg_dias > 0]
    
    dias_c_cerr <- dias_c_act[dias_c_act %in% dias_cerrados_nom]
    dias_s_cerr <- dias_s_act[dias_s_act %in% dias_cerrados_nom]
    
    # Evaluar cada día donde SAP reportó kilos
    dias_match   <- character(0)
    dias_desfase <- character(0) # D+1 (cosechó día anterior)
    dias_sin_r   <- character(0) # Sin cosecha ese día ni el anterior
    
    kg_sin_r_tot <- 0
    kg_desf_tot  <- 0
    
    for (ds in dias_s_cerr) {
      if (ds %in% dias_c_cerr) {
        dias_match <- c(dias_match, ds)
      } else {
        # Verificar si cosechó el día inmediatamente anterior (desfase D+1)
        idx_s <- match(ds, dias_cols)
        dia_ant <- if (idx_s > 1) dias_cols[idx_s - 1] else NA_character_
        if (!is.na(dia_ant) && (dia_ant %in% dias_c_cerr)) {
          dias_desfase <- c(dias_desfase, ds)
          kg_desf_tot  <- kg_desf_tot + kg_dias[ds]
        } else {
          dias_sin_r   <- c(dias_sin_r, ds)
          kg_sin_r_tot <- kg_sin_r_tot + kg_dias[ds]
        }
      }
    }
    
    # Procesar Sin Ruta
    if (length(dias_sin_r) > 0) {
      df_rec$Kg_Sin_Ruta[i] <- round(kg_sin_r_tot, 2)
      for (dsr in dias_sin_r) {
        col_rec <- paste0("Kg_", tools::toTitleCase(chartr("áéíóúÁÉÍÓÚ", "aeiouAEIOU", tolower(dsr))), "_rec")
        col_sr  <- paste0("Kg_", tools::toTitleCase(chartr("áéíóúÁÉÍÓÚ", "aeiouAEIOU", tolower(dsr))), "_sr")
        df_rec[[col_rec]][i] <- 0
        df_rec[[col_sr]][i]  <- round(kg_dias[dsr], 2)
      }
    }
    
    # Procesar Desfase D+1 (reasignar al día de campo previo)
    if (length(dias_desfase) > 0) {
      df_rec$Kg_Desfase[i] <- round(kg_desf_tot, 2)
      for (dd in dias_desfase) {
        idx_s <- match(dd, dias_cols)
        dia_ant <- dias_cols[idx_s - 1]
        col_dest <- paste0("Kg_", tools::toTitleCase(chartr("áéíóúÁÉÍÓÚ", "aeiouAEIOU", tolower(dia_ant))), "_rec")
        col_orig <- paste0("Kg_", tools::toTitleCase(chartr("áéíóúÁÉÍÓÚ", "aeiouAEIOU", tolower(dd))), "_rec")
        df_rec[[col_orig]][i] <- 0
        df_rec[[col_dest]][i] <- round(df_rec[[col_dest]][i] + kg_dias[dd], 2)
      }
    }
    
    # Actualizar Kg_Total_rec
    df_rec$Kg_Total_rec[i] <- round(sum(c(df_rec$Kg_Lunes_rec[i], df_rec$Kg_Martes_rec[i], df_rec$Kg_Miercoles_rec[i],
                                          df_rec$Kg_Jueves_rec[i], df_rec$Kg_Viernes_rec[i], df_rec$Kg_Sabado_rec[i])), 2)
    
    # Estado final del registro
    if (length(dias_sin_r) > 0) {
      df_rec$Estado_Concil[i]        <- "Kilos SAP sin Ruta"
      df_rec$Tipo_Alerta[i]          <- "inconsistencia"
      df_rec$Diagnostico[i]          <- sprintf("Pesado en SAP sin ruta en campo en [%s] (%s kg). Excluido de mapa y cálculo agronómico.",
                                                paste(dias_sin_r, collapse = ", "), format(round(kg_sin_r_tot, 0), big.mark = ","))
      # Si tiene campo en otros días válidos, mantenerlo operativo para esos días; si no tiene nada válido, FALSE
      df_rec$Es_Valido_Operativo[i]  <- (df_rec$Kg_Total_rec[i] > 0 || any(ha_dias[setdiff(dias_cols, dias_sin_r)] > 0))
    } else if (length(dias_desfase) > 0) {
      df_rec$Estado_Concil[i]        <- "Desfase Reasignado"
      df_rec$Tipo_Alerta[i]          <- "conciliado"
      df_rec$Diagnostico[i]          <- sprintf("Desfase de %s kg en [%s] reasignado por coincidencia con ruta de campo previa. Usado operativamente (Azul).",
                                                format(round(kg_desf_tot, 0), big.mark = ","), paste(dias_desfase, collapse = ", "))
      df_rec$Es_Valido_Operativo[i]  <- TRUE
    } else if (length(dias_match) > 0) {
      df_rec$Estado_Concil[i]        <- "Conciliado OK"
      df_rec$Tipo_Alerta[i]          <- "conciliado"
      df_rec$Diagnostico[i]          <- "Campo y SAP coinciden plenamente en válvula y días."
      df_rec$Es_Valido_Operativo[i]  <- TRUE
    } else {
      # No hubo match ni desfase ni sin ruta directo
      df_rec$Estado_Concil[i]        <- "Desfase de Día"
      df_rec$Tipo_Alerta[i]          <- "inconsistencia"
      df_rec$Diagnostico[i]          <- sprintf("Desfase total: Campo [%s] vs SAP [%s]. Kilos fuera de cálculo.",
                                                paste(dias_c_cerr, collapse = ", "), paste(dias_s_cerr, collapse = ", "))
      df_rec$Es_Valido_Operativo[i]  <- FALSE
      df_rec$Kg_Total_rec[i]         <- 0
    }
  }
}

cat("\n=== SUMMARY OF RECONCILIATION STATES ===\n")
print(table(df_rec$Estado_Concil))

cat("\n=== KILOS SAP SIN RUTA ===\n")
sub_sr <- df_rec %>% filter(Estado_Concil == "Kilos SAP sin Ruta")
cat("Count of valves:", nrow(sub_sr), "\n")
cat("Sum of Kg_Sin_Ruta:", sum(sub_sr$Kg_Sin_Ruta), "\n")
print(sub_sr %>% select(codigo_norm, Kg_Sin_Ruta, Kg_Total_sap, Kg_Total_rec, Ha_Total_rc, Diagnostico))
