library(dplyr)
library(tibble)

cat("=== TESTING FULL SIMULATION OF SHINY SERVER ACTIONS ===\n")

# Mock initial state
area_turno <- 5.11
ha_real_1ra <- 0
ha_real_2da <- 0
campana_sel <- "2026"
semana_sel <- 45
modulo_sel <- "Módulo 13"
turno_sel <- "Turno 1"
var_sel <- "ROCIO"

ed_actuales <- tibble(
  Campaña = character(), Semana = numeric(), Sector = character(), Módulo = character(),
  Turno = character(), Variedad = character(), Campo = character(), Valor = numeric(),
  Ha_Asignada = numeric(), Ha_Asignada_1ra = numeric(), Kg_Asignado_1ra = numeric(),
  Ha_Asignada_2da = numeric(), Kg_Asignado_2da = numeric(), Tipo_Edicion = character(),
  Modo_Ingreso = character(), Motivo_Ajuste = character(), Fecha_Edicion = as.POSIXct(character())
)

limpiar_variedad <- function(x) {
  toupper(trimws(as.character(x)))
}

asegurar_modo_ingreso <- function(df) {
  if (!("Modo_Ingreso" %in% names(df))) {
    df$Modo_Ingreso <- if (nrow(df) > 0) {
      ifelse(grepl("kg/ha en", coalesce(df$Motivo_Ajuste, "")), "kgha", "kilos")
    } else {
      character(0)
    }
  }
  df
}

simular_guardado <- function(ed_actuales, dia_campo, modo, kilos_in, kgha_in = 0, ha_in = 0, pasada_sel = 1L) {
  ed_actuales <- asegurar_modo_ingreso(ed_actuales)
  ha_real_pasada <- if (pasada_sel == 2L) ha_real_2da else ha_real_1ra
  ha_pool_pasada <- max(0, round(area_turno - ha_real_pasada, 2))
  
  val_final <- if (modo == "kgha") round(kgha_in * ha_in, 2) else round(kilos_in, 2)
  
  ed_otros_pasada <- if (nrow(ed_actuales) > 0) {
    ed_actuales %>%
      filter(
        as.character(Campaña) == campana_sel,
        as.numeric(Semana)    == semana_sel,
        as.character(Módulo)   == modulo_sel,
        as.character(Turno)    == turno_sel,
        limpiar_variedad(Variedad) == limpiar_variedad(var_sel),
        as.character(Campo)    != as.character(dia_campo),
        coalesce(Tipo_Edicion, "") == "manual",
        if (pasada_sel == 2L) coalesce(Ha_Asignada_2da, 0) > 0 | coalesce(Kg_Asignado_2da, 0) > 0
        else coalesce(Ha_Asignada_1ra, 0) > 0 | coalesce(Kg_Asignado_1ra, 0) > 0
      )
  } else tibble()
  
  if (nrow(ed_otros_pasada) > 0) {
    if (!("Modo_Ingreso" %in% names(ed_otros_pasada))) {
      ed_otros_pasada$Modo_Ingreso <- ifelse(grepl("kg/ha en", coalesce(ed_otros_pasada$Motivo_Ajuste, "")), "kgha", "kilos")
    }
    ed_fijas <- ed_otros_pasada %>%
      filter(coalesce(Modo_Ingreso, "") == "kgha" | grepl("kg/ha en", coalesce(Motivo_Ajuste, "")))
    ed_prorr <- ed_otros_pasada %>%
      filter(!(coalesce(Modo_Ingreso, "") == "kgha" | grepl("kg/ha en", coalesce(Motivo_Ajuste, ""))))
  } else {
    ed_fijas <- tibble()
    ed_prorr <- tibble()
  }
  ha_fijas <- if (nrow(ed_fijas) > 0) sum(if (pasada_sel == 2L) coalesce(ed_fijas$Ha_Asignada_2da, 0) else coalesce(ed_fijas$Ha_Asignada_1ra, 0), na.rm = TRUE) else 0
  ha_pool_prorr <- max(0, round(ha_pool_pasada - ha_fijas, 2))
  
  ha_final <- 0
  if (modo == "kgha") {
    ha_final <- round(ha_in, 2)
    if (nrow(ed_prorr) > 0) {
      ha_restante <- max(0, round(ha_pool_prorr - ha_final, 2))
      kg_tot_otros <- sum(coalesce(as.numeric(ed_prorr$Valor), 0), na.rm = TRUE)
      ha_acum <- 0
      for (i in seq_len(nrow(ed_prorr))) {
        d_campo <- ed_prorr$Campo[i]
        ha_nueva_i <- if (i == nrow(ed_prorr)) {
          max(0, round(ha_restante - ha_acum, 2))
        } else {
          if (kg_tot_otros > 0) round(ha_restante * (as.numeric(ed_prorr$Valor[i]) / kg_tot_otros), 2) else 0
        }
        ha_acum <- ha_acum + ha_nueva_i
        idx <- which(ed_actuales$Campo == d_campo)
        ed_actuales$Ha_Asignada[idx] <- ha_nueva_i
        if (pasada_sel == 2L) ed_actuales$Ha_Asignada_2da[idx] <- ha_nueva_i else ed_actuales$Ha_Asignada_1ra[idx] <- ha_nueva_i
      }
    }
  } else {
    if (nrow(ed_prorr) > 0) {
      kg_tot_otros <- sum(coalesce(as.numeric(ed_prorr$Valor), 0), na.rm = TRUE)
      kg_total_conjunto <- kg_tot_otros + val_final
      ha_acum <- 0
      for (i in seq_len(nrow(ed_prorr))) {
        d_campo <- ed_prorr$Campo[i]
        ha_nueva_i <- if (kg_total_conjunto > 0) round(ha_pool_prorr * (as.numeric(ed_prorr$Valor[i]) / kg_total_conjunto), 2) else 0
        ha_acum <- ha_acum + ha_nueva_i
        idx <- which(ed_actuales$Campo == d_campo)
        ed_actuales$Ha_Asignada[idx] <- ha_nueva_i
        if (pasada_sel == 2L) ed_actuales$Ha_Asignada_2da[idx] <- ha_nueva_i else ed_actuales$Ha_Asignada_1ra[idx] <- ha_nueva_i
        ed_actuales$Motivo_Ajuste[idx] <- sprintf("Manual prorrateado (%s kg en %.2f ha)", format(round(as.numeric(ed_prorr$Valor[i]), 1)), ha_nueva_i)
      }
      ha_final <- max(0, round(ha_pool_prorr - ha_acum, 2))
    } else {
      ha_final <- round(ha_pool_prorr, 2)
    }
  }
  
  motivo_final <- if (modo == "kgha") {
    sprintf("Manual (%s kg/ha en %.2f ha)", format(round(kgha_in, 1)), ha_final)
  } else if (nrow(ed_prorr) > 0) {
    sprintf("Manual prorrateado (%s kg en %.2f ha)", format(round(val_final, 1)), ha_final)
  } else {
    sprintf("Manual (%s kg en %.2f ha faltantes)", format(round(val_final, 1)), ha_final)
  }
  
  nueva_ed <- tibble(
    Campaña = campana_sel, Semana = semana_sel, Sector = "Sector 3", Módulo = modulo_sel,
    Turno = turno_sel, Variedad = var_sel, Campo = dia_campo, Valor = val_final,
    Ha_Asignada = ha_final,
    Ha_Asignada_1ra = ifelse(pasada_sel == 1L, ha_final, 0),
    Kg_Asignado_1ra = ifelse(pasada_sel == 1L, val_final, 0),
    Ha_Asignada_2da = ifelse(pasada_sel == 2L, ha_final, 0),
    Kg_Asignado_2da = ifelse(pasada_sel == 2L, val_final, 0),
    Tipo_Edicion = "manual", Modo_Ingreso = modo, Motivo_Ajuste = motivo_final,
    Fecha_Edicion = Sys.time()
  )
  
  ed_actuales <- ed_actuales %>% filter(Campo != dia_campo) %>% bind_rows(nueva_ed)
  return(ed_actuales)
}

simular_restablecer <- function(ed_actuales, dia_campo, pasada_sel = 1L) {
  ed_actuales <- asegurar_modo_ingreso(ed_actuales)
  ha_real_pasada <- if (pasada_sel == 2L) ha_real_2da else ha_real_1ra
  ha_pool_pasada <- max(0, round(area_turno - ha_real_pasada, 2))
  
  ed_filtradas <- ed_actuales %>% filter(Campo != dia_campo)
  
  ed_otros_pasada <- ed_filtradas %>%
    filter(coalesce(Tipo_Edicion, "") == "manual",
           if (pasada_sel == 2L) coalesce(Ha_Asignada_2da, 0) > 0 | coalesce(Kg_Asignado_2da, 0) > 0
           else coalesce(Ha_Asignada_1ra, 0) > 0 | coalesce(Kg_Asignado_1ra, 0) > 0)
  
  if (nrow(ed_otros_pasada) > 0) {
    if (!("Modo_Ingreso" %in% names(ed_otros_pasada))) {
      ed_otros_pasada$Modo_Ingreso <- ifelse(grepl("kg/ha en", coalesce(ed_otros_pasada$Motivo_Ajuste, "")), "kgha", "kilos")
    }
    ed_fijas <- ed_otros_pasada %>%
      filter(coalesce(Modo_Ingreso, "") == "kgha" | grepl("kg/ha en", coalesce(Motivo_Ajuste, "")))
    ed_prorr <- ed_otros_pasada %>%
      filter(!(coalesce(Modo_Ingreso, "") == "kgha" | grepl("kg/ha en", coalesce(Motivo_Ajuste, ""))))
  } else {
    ed_fijas <- tibble()
    ed_prorr <- tibble()
  }
  ha_fijas <- if (nrow(ed_fijas) > 0) sum(if (pasada_sel == 2L) coalesce(ed_fijas$Ha_Asignada_2da, 0) else coalesce(ed_fijas$Ha_Asignada_1ra, 0), na.rm = TRUE) else 0
  ha_pool_prorr <- max(0, round(ha_pool_pasada - ha_fijas, 2))
  
  if (nrow(ed_prorr) > 0) {
    kg_tot_rest <- sum(coalesce(as.numeric(ed_prorr$Valor), 0), na.rm = TRUE)
    ha_acum <- 0
    for (i in seq_len(nrow(ed_prorr))) {
      d_campo <- ed_prorr$Campo[i]
      ha_nueva_i <- if (i == nrow(ed_prorr)) {
        max(0, round(ha_pool_prorr - ha_acum, 2))
      } else {
        if (kg_tot_rest > 0) round(ha_pool_prorr * (as.numeric(ed_prorr$Valor[i]) / kg_tot_rest), 2) else 0
      }
      ha_acum <- ha_acum + ha_nueva_i
      idx <- which(ed_filtradas$Campo == d_campo)
      ed_filtradas$Ha_Asignada[idx] <- ha_nueva_i
      if (pasada_sel == 2L) ed_filtradas$Ha_Asignada_2da[idx] <- ha_nueva_i else ed_filtradas$Ha_Asignada_1ra[idx] <- ha_nueva_i
      ed_filtradas$Motivo_Ajuste[idx] <- if (nrow(ed_prorr) > 1) {
        sprintf("Manual prorrateado (%s kg en %.2f ha)", format(round(as.numeric(ed_prorr$Valor[i]), 1)), ha_nueva_i)
      } else {
        sprintf("Manual (%s kg en %.2f ha faltantes)", format(round(as.numeric(ed_prorr$Valor[i]), 1)), ha_nueva_i)
      }
    }
  }
  return(ed_filtradas)
}

# Run Step 1: Miércoles 300 kg
cat("Paso 1: Guardando Miércoles con 300 kg directos...\n")
ed_actuales <- simular_guardado(ed_actuales, "Miércoles", "kilos", 300)
stopifnot(nrow(ed_actuales) == 1)
stopifnot(ed_actuales$Ha_Asignada_1ra[1] == 5.11)
cat(sprintf("-> Miércoles: %.2f ha (OK)\n", ed_actuales$Ha_Asignada_1ra[1]))

# Run Step 2: Jueves 200 kg
cat("\nPaso 2: Guardando Jueves con 200 kg directos...\n")
ed_actuales <- simular_guardado(ed_actuales, "Jueves", "kilos", 200)
stopifnot(nrow(ed_actuales) == 2)
m_row <- ed_actuales %>% filter(Campo == "Miércoles")
j_row <- ed_actuales %>% filter(Campo == "Jueves")
cat(sprintf("-> Miércoles: %.2f ha | Jueves: %.2f ha | Suma: %.2f ha\n", 
            m_row$Ha_Asignada_1ra, j_row$Ha_Asignada_1ra, m_row$Ha_Asignada_1ra + j_row$Ha_Asignada_1ra))
stopifnot(m_row$Ha_Asignada_1ra == 3.07)
stopifnot(j_row$Ha_Asignada_1ra == 2.04)
stopifnot(abs(m_row$Ha_Asignada_1ra + j_row$Ha_Asignada_1ra - 5.11) < 0.001)

# Run Step 3: Viernes 100 kg
cat("\nPaso 3: Guardando Viernes con 100 kg directos...\n")
ed_actuales <- simular_guardado(ed_actuales, "Viernes", "kilos", 100)
stopifnot(nrow(ed_actuales) == 3)
m_row <- ed_actuales %>% filter(Campo == "Miércoles")
j_row <- ed_actuales %>% filter(Campo == "Jueves")
v_row <- ed_actuales %>% filter(Campo == "Viernes")
cat(sprintf("-> Miércoles: %.2f ha | Jueves: %.2f ha | Viernes: %.2f ha | Suma: %.2f ha\n", 
            m_row$Ha_Asignada_1ra, j_row$Ha_Asignada_1ra, v_row$Ha_Asignada_1ra, 
            m_row$Ha_Asignada_1ra + j_row$Ha_Asignada_1ra + v_row$Ha_Asignada_1ra))
stopifnot(m_row$Ha_Asignada_1ra == 2.56)
stopifnot(j_row$Ha_Asignada_1ra == 1.70)
stopifnot(v_row$Ha_Asignada_1ra == 0.85)
stopifnot(abs(m_row$Ha_Asignada_1ra + j_row$Ha_Asignada_1ra + v_row$Ha_Asignada_1ra - 5.11) < 0.001)

# Run Step 4: Restablecer Jueves
cat("\nPaso 4: Restableciendo Jueves (eliminando Jueves y rebalanceando Miércoles y Viernes)...\n")
ed_actuales <- simular_restablecer(ed_actuales, "Jueves")
stopifnot(nrow(ed_actuales) == 2)
m_row <- ed_actuales %>% filter(Campo == "Miércoles")
v_row <- ed_actuales %>% filter(Campo == "Viernes")
cat(sprintf("-> Miércoles: %.2f ha | Viernes: %.2f ha | Suma: %.2f ha\n", 
            m_row$Ha_Asignada_1ra, v_row$Ha_Asignada_1ra, m_row$Ha_Asignada_1ra + v_row$Ha_Asignada_1ra))
stopifnot(m_row$Ha_Asignada_1ra == 3.83)
stopifnot(v_row$Ha_Asignada_1ra == 1.28)
stopifnot(abs(m_row$Ha_Asignada_1ra + v_row$Ha_Asignada_1ra - 5.11) < 0.001)

# Run Step 5: Restablecer Viernes
cat("\nPaso 5: Restableciendo Viernes (dejando solo Miércoles con todos los 5.11 ha)...\n")
ed_actuales <- simular_restablecer(ed_actuales, "Viernes")
stopifnot(nrow(ed_actuales) == 1)
m_row <- ed_actuales %>% filter(Campo == "Miércoles")
cat(sprintf("-> Miércoles: %.2f ha (Esperado: 5.11 ha)\n", m_row$Ha_Asignada_1ra))
stopifnot(m_row$Ha_Asignada_1ra == 5.11)

cat("\n=== ALL SHINY SIMULATION TESTS PASSED WITH 100% SUCCESS! ===\n")
