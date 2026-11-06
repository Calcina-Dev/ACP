library(dplyr)

cat("=== SCRIPT DE UNIFICACIÓN PARA MÓDULOS 9 Y 11 Y SECTOR 9 ===\n")

# 1. valvulas_normalizadas.rds
ruta_valvs <- "D:/ACP/Rutas de Cosecha/valvulas_normalizadas.rds"
if (file.exists(ruta_valvs)) {
  v <- readRDS(ruta_valvs)
  
  # Corregir Sector de Módulo 11 (los de Sector 11 pasan a 9)
  n_sec11 <- sum(as.character(v$Módulo) == "11" & as.character(v$Sector) == "11")
  v$Sector[as.character(v$Módulo) == "11"] <- "9"
  v$Sector[as.character(v$Módulo) == "9"]  <- "9"
  v$Sector[as.character(v$Módulo) == "12"] <- "9"
  
  # Asegurar que Módulo no tenga .1 o .2
  v$Módulo[as.character(v$Módulo) %in% c("9.1", "9.2")]   <- "9"
  v$Módulo[as.character(v$Módulo) %in% c("11.1", "11.2")] <- "11"
  
  saveRDS(v, ruta_valvs)
  cat("✔ valvulas_normalizadas.rds actualizado. Válvulas M11 corregidas de Sector 11 a 9:", n_sec11, "\n")
}

# 2. historico_oficial_wide_2026.rds
ruta_how <- "D:/ACP/Rutas de Cosecha/historico_oficial_wide_2026.rds"
if (file.exists(ruta_how)) {
  how <- readRDS(ruta_how)
  col_m <- if ("Módulo_Norm" %in% names(how)) "Módulo_Norm" else "Módulo"
  
  # Unificar Módulo
  how[[col_m]][as.character(how[[col_m]]) %in% c("9.1", "9.2")]   <- "9"
  how[[col_m]][as.character(how[[col_m]]) %in% c("11.1", "11.2")] <- "11"
  
  # Corregir Sector
  mask_9_11_12 <- as.character(how[[col_m]]) %in% c("9", "11", "12")
  how$Sector[mask_9_11_12] <- if (is.numeric(how$Sector)) 9 else "9"
  
  saveRDS(how, ruta_how)
  cat("✔ historico_oficial_wide_2026.rds actualizado y unificado.\n")
}

# 3. RData en todas sus ubicaciones
rdata_paths <- c(
  "D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData",
  "D:/ACP/Actualizacion data maestra/datos_maestros_cosecha_2026.RData",
  "D:/ACP/Rutas de Cosecha/datos_maestros_cosecha_2026.RData"
)

for (rp in rdata_paths) {
  if (file.exists(rp)) {
    env <- new.env()
    load(rp, envir = env)
    
    # Corregir df_valvulas
    if (exists("df_valvulas", envir = env) && is.data.frame(env$df_valvulas)) {
      env$df_valvulas$Sector[as.character(env$df_valvulas$Módulo) %in% c("9", "11", "12")] <- "9"
      env$df_valvulas$Módulo[as.character(env$df_valvulas$Módulo) %in% c("9.1", "9.2")]   <- "9"
      env$df_valvulas$Módulo[as.character(env$df_valvulas$Módulo) %in% c("11.1", "11.2")] <- "11"
    }
    
    # Corregir df_hist_oficial_wide
    if (exists("df_hist_oficial_wide", envir = env) && is.data.frame(env$df_hist_oficial_wide)) {
      col_m <- if ("Módulo_Norm" %in% names(env$df_hist_oficial_wide)) "Módulo_Norm" else "Módulo"
      env$df_hist_oficial_wide[[col_m]][as.character(env$df_hist_oficial_wide[[col_m]]) %in% c("9.1", "9.2")]   <- "9"
      env$df_hist_oficial_wide[[col_m]][as.character(env$df_hist_oficial_wide[[col_m]]) %in% c("11.1", "11.2")] <- "11"
      mask_m <- as.character(env$df_hist_oficial_wide[[col_m]]) %in% c("9", "11", "12")
      env$df_hist_oficial_wide$Sector[mask_m] <- if (is.numeric(env$df_hist_oficial_wide$Sector)) 9 else "9"
    }
    
    # Corregir df_cosecha para campaña 2026
    if (exists("df_cosecha", envir = env) && is.data.frame(env$df_cosecha)) {
      m2026 <- env$df_cosecha$Campaña == 2026
      env$df_cosecha$Módulo[m2026 & env$df_cosecha$Módulo %in% c("9.1", "9.2")]   <- "9"
      env$df_cosecha$Módulo[m2026 & env$df_cosecha$Módulo %in% c("11.1", "11.2")] <- "11"
      env$df_cosecha$Sector[m2026 & env$df_cosecha$Módulo %in% c("9", "11", "12")] <- if (is.numeric(env$df_cosecha$Sector)) 9 else "9"
    }
    
    objs <- ls(envir = env)
    save(list = objs, envir = env, file = rp)
    cat("✔ RData actualizado:", rp, "\n")
  }
}

# 4. ajuste_valvulas_semana.rds
av_paths <- c(
  "D:/ACP/Actualizacion Semanal/ajuste_valvulas_semana.rds",
  "D:/ACP/ajuste_valvulas_semana.rds"
)
for (ap in av_paths) {
  if (file.exists(ap)) {
    av <- readRDS(ap)
    if (nrow(av) > 0 && "Módulo" %in% names(av)) {
      av$Módulo[as.character(av$Módulo) %in% c("9.1", "9.2")]   <- "9"
      av$Módulo[as.character(av$Módulo) %in% c("11.1", "11.2")] <- "11"
      av$Sector[as.character(av$Módulo) %in% c("9", "11", "12")] <- "9"
      saveRDS(av, ap)
      cat("✔ ajuste_valvulas_semana.rds actualizado:", ap, "\n")
    }
  }
}

# 5. ediciones_manuales_cosecha.rds
ed_paths <- c(
  "D:/ACP/Actualizacion Semanal/ediciones_manuales_cosecha.rds",
  "D:/ACP/Actualizacion data maestra/ediciones_manuales_cosecha.rds",
  "D:/ACP/ediciones_manuales_cosecha.rds"
)
for (ep in ed_paths) {
  if (file.exists(ep)) {
    ed <- readRDS(ep)
    if (nrow(ed) > 0 && "Módulo" %in% names(ed)) {
      ed$Módulo[as.character(ed$Módulo) %in% c("9.1", "9.2")]   <- "9"
      ed$Módulo[as.character(ed$Módulo) %in% c("11.1", "11.2")] <- "11"
      ed$Sector[as.character(ed$Módulo) %in% c("9", "11", "12")] <- "9"
      
      has_te <- "Tipo_Edicion" %in% names(ed)
      has_fe <- "Fecha_Edicion" %in% names(ed)
      if (has_te && has_fe) {
        ed <- ed %>% arrange(desc(Tipo_Edicion == "manual"), desc(Fecha_Edicion))
      } else if (has_fe) {
        ed <- ed %>% arrange(desc(Fecha_Edicion))
      }
      
      cols_dist <- intersect(c("Campaña", "Semana", "Sector", "Módulo", "Turno", "Variedad", "Campo"), names(ed))
      ed <- ed %>% distinct(across(all_of(cols_dist)), .keep_all = TRUE)
      
      saveRDS(ed, ep)
      cat("✔ ediciones_manuales_cosecha.rds actualizado:", ep, "| Filas:", nrow(ed), "\n")
    }
  }
}

cat("=== DATASETS MAESTROS Y RDS COMPLETADOS CON ÉXITO ===\n")
