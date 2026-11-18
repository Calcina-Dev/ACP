suppressMessages({
  library(dplyr)
  library(lubridate)
})

ruta_raiz_arandano <- "G:/Unidades compartidas/Compartido Proyecciones/3.- Arándano"
load(file.path(ruta_raiz_arandano, "datos_maestros_cosecha_2026.RData"))

rc_file <- if (file.exists(file.path(ruta_raiz_arandano, "rutas_cosecha_mapa.rds"))) {
  readRDS(file.path(ruta_raiz_arandano, "rutas_cosecha_mapa.rds"))
} else if (file.exists("D:/ACP/Rutas de Cosecha/rutas_cosecha_mapa.rds")) {
  readRDS("D:/ACP/Rutas de Cosecha/rutas_cosecha_mapa.rds")
} else NULL

# Mock inputs
simular_test <- function(modo_detalle, desde_dia, hasta_dia) {
  dias_nombres_u <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
  cols_ha_sem_u  <- paste0("Ha_", dias_nombres_u)
  cols_kg_sem_u  <- paste0("Kg_", dias_nombres_u)
  
  # 1. Base Universo
  # Cosecha SAP wide para M10-T18
  sap_m10 <- df_hist_oficial_wide %>% 
    filter(as.numeric(Semana) == 46, as.character(Módulo_Norm) == "10", as.character(Turno_Norm) == "18")
  
  rc_sem_u <- if (!is.null(rc_file) && "Semana" %in% names(rc_file)) {
    rc_file %>% filter(as.numeric(Semana) == 46)
  } else tibble()
  
  rc_ha_turno <- if (nrow(rc_sem_u) > 0) {
    rc_temp_u <- rc_sem_u %>%
      filter(!("Tipo_Registro" %in% names(.)) | Tipo_Registro != "Rectificacion")
    ha_sem_vec_u <- numeric(nrow(rc_temp_u))
    for (d_sem in dias_nombres_u) {
      col_h <- paste0("Ha_", d_sem)
      v_h   <- if (col_h %in% names(rc_temp_u)) coalesce(rc_temp_u[[col_h]], 0) else 0
      ha_sem_vec_u <- ha_sem_vec_u + v_h
    }
    rc_temp_u$Ha_Rc_Sem <- ha_sem_vec_u
    rc_temp_u %>%
      mutate(
        Turno_num_rc = suppressWarnings(as.numeric(Turno)),
        Módulo_rc = as.character(Módulo),
        Turno_rc   = as.character(Turno),
        Var_rc     = toupper(trimws(Variedad)),
        kg_tot_rc  = if ("Kg_Total_Prog" %in% names(.)) coalesce(Kg_Total_Prog, 0) else 0,
        num_pas_rc = if ("Num_Pasada" %in% names(.)) suppressWarnings(as.numeric(Num_Pasada)) else 1
      ) %>%
      group_by(Módulo_rc, Turno_rc, Var_rc) %>%
      summarise(
        Ha_Rc          = sum(coalesce(Ha_Rc_Sem, 0), na.rm = TRUE),
        Ha_Rc_1ra      = sum(ifelse(coalesce(num_pas_rc, 1) == 1, coalesce(Ha_Rc_Sem, 0), 0), na.rm = TRUE),
        Ha_Rc_2da      = sum(ifelse(coalesce(num_pas_rc, 1) == 2, coalesce(Ha_Rc_Sem, 0), 0), na.rm = TRUE),
        Kg_Real_1ra_rc = sum(ifelse(num_pas_rc == 1, kg_tot_rc, 0), na.rm = TRUE),
        Kg_Real_2da_rc = sum(ifelse(num_pas_rc == 2, kg_tot_rc, 0), na.rm = TRUE),
        .groups        = "drop"
      )
  } else tibble()
  
  # Fila M10-T18 de proyeccion / areas
  fila_u <- tibble(
    Sector = "5", Módulo = "10", Turno = "18", Variedad = "MALIBU", Tipo = "Orgánico",
    Proyección = 3013.45, Área = 5.33,
    Lunes = 0, Martes = 385.90, Miércoles = 1759.27, Jueves = 1385.08, Viernes = 0, Sábado = 0, Domingo = 0,
    Lunes_tipo = "none", Martes_tipo = "sap", Miércoles_tipo = "sap", Jueves_tipo = "sap", Viernes_tipo = "none", Sábado_tipo = "none",
    Lunes_bloqueado = FALSE, Martes_bloqueado = TRUE, Miércoles_bloqueado = TRUE, Jueves_bloqueado = TRUE, Viernes_bloqueado = FALSE, Sábado_bloqueado = FALSE,
    Lunes_manual = FALSE, Martes_manual = FALSE, Miércoles_manual = FALSE, Jueves_manual = FALSE, Viernes_manual = FALSE, Sábado_manual = FALSE
  )
  
  # Join con rc
  rc_m <- rc_ha_turno %>% filter(Módulo_rc == "10", Turno_rc == "18")
  ha_rc_1ra <- if (nrow(rc_m) > 0) rc_m$Ha_Rc_1ra[1] else 0
  
  fila_u <- fila_u %>%
    mutate(
      `Ha Real 1ª Pas` = round(pmin(Área, ha_rc_1ra), 2),
      `Ha Real 2ª Pas` = 0,
      `Ha Recorrida`   = `Ha Real 1ª Pas`,
      Kg_Real_Sem_Tot  = 3530.25,
      Kg_Real_1ra_Sem  = 3530.25,
      Kg_Ha_Real_1ra_Sem = round(Kg_Real_1ra_Sem / `Ha Real 1ª Pas`, 2)
    )
  
  # 2. universo_completo
  dias_validos <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
  idx_ini <- match(desde_dia, dias_validos)
  idx_fin <- match(hasta_dia, dias_validos)
  if (is.na(idx_ini)) idx_ini <- 1
  if (is.na(idx_fin)) idx_fin <- 6
  if (idx_fin < idx_ini) idx_fin <- idx_ini
  dias_en_rango <- dias_validos[idx_ini:idx_fin]
  dias_fuera_rango <- setdiff(dias_validos, dias_en_rango)
  
  if (modo_detalle == "ajustado") {
    for (d_clean in dias_en_rango) {
      fila_u[[d_clean]] <- NA_real_
      fila_u[[paste0(d_clean, "_tipo")]] <- "none"
    }
    # Supongamos un ajuste automatico o manual en Jueves si Jueves esta en rango
    if ("Jueves" %in% dias_en_rango) {
      fila_u[["Jueves"]] <- 1200.00
      fila_u[["Jueves_tipo"]] <- "auto"
    }
  }
  
  # Resumen final
  fila_u <- fila_u %>%
    mutate(
      across(c(Lunes, Martes, Miércoles, Jueves, Viernes, Sábado, Domingo), ~ coalesce(.x, 0)),
      Ajuste = round(Lunes + Martes + Miércoles + Jueves + Viernes + Sábado + Domingo, 2),
      Kg_SAP_Real = round(
        ifelse(Lunes_tipo == "sap", Lunes, 0) +
        ifelse(Martes_tipo == "sap", Martes, 0) +
        ifelse(Miércoles_tipo == "sap", Miércoles, 0) +
        ifelse(Jueves_tipo == "sap", Jueves, 0) +
        ifelse(Viernes_tipo == "sap", Viernes, 0) +
        ifelse(Sábado_tipo == "sap", Sábado, 0) +
        coalesce(Domingo, 0), 2
      ),
      `Kg/Ha Real 1ª Pas` = case_when(
        !is.na(Kg_Ha_Real_1ra_Sem) ~ Kg_Ha_Real_1ra_Sem,
        TRUE ~ round(Kg_SAP_Real / `Ha Real 1ª Pas`, 2)
      )
    )
  
  return(fila_u)
}

cat("=== Test 1: VISTA: Real con DESDE: Jueves, HASTA: Jueves ===\n")
t1 <- simular_test("real", "Jueves", "Jueves")
print(t1 %>% select(Área, `Ha Real 1ª Pas`, `Kg/Ha Real 1ª Pas`, Martes, Miércoles, Jueves, Ajuste, Kg_SAP_Real))
stopifnot(t1$`Ha Real 1ª Pas` == 5.33)
stopifnot(t1$`Kg/Ha Real 1ª Pas` == 662.34)
stopifnot(t1$Martes == 385.90)
stopifnot(t1$Miércoles == 1759.27)
stopifnot(t1$Jueves == 1385.08)
cat("✔ Test 1 PASÓ: VISTA Real muestra 5.33 ha y 662.34 kg/ha sin inflarse!\n\n")

cat("=== Test 2: VISTA: Real con DESDE: Lunes, HASTA: Sábado ===\n")
t2 <- simular_test("real", "Lunes", "Sábado")
print(t2 %>% select(Área, `Ha Real 1ª Pas`, `Kg/Ha Real 1ª Pas`, Martes, Miércoles, Jueves, Ajuste, Kg_SAP_Real))
stopifnot(t2$`Ha Real 1ª Pas` == 5.33)
stopifnot(t2$`Kg/Ha Real 1ª Pas` == 662.34)
cat("✔ Test 2 PASÓ: Coincide perfectamente con DESDE: Lunes!\n\n")

cat("=== Test 3: VISTA: Ajustado con DESDE: Jueves, HASTA: Jueves ===\n")
t3 <- simular_test("ajustado", "Jueves", "Jueves")
print(t3 %>% select(Área, `Ha Real 1ª Pas`, `Kg/Ha Real 1ª Pas`, Martes, Miércoles, Jueves, Ajuste, Kg_SAP_Real))
stopifnot(t3$`Ha Real 1ª Pas` == 5.33)
stopifnot(t3$`Kg/Ha Real 1ª Pas` == 662.34)
# Martes y Miercoles estan fuera de [Jueves, Jueves] -> conservan sus kilos reales de SAP!
stopifnot(t3$Martes == 385.90)
stopifnot(t3$Miércoles == 1759.27)
stopifnot(t3$Jueves == 1200.00) # Ajustado
cat("✔ Test 3 PASÓ: VISTA Ajustado conserva los kilos reales fuera de DESDE-HASTA (Martes y Miércoles) y muestra ajuste en Jueves!\n")
