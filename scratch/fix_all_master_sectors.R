suppressPackageStartupMessages(library(dplyr))

cat("=== 1. RUTAS DE COSECHA MAPA ===\n")
rc_path <- "D:/ACP/Rutas de Cosecha/rutas_cosecha_mapa.rds"
if (file.exists(rc_path)) {
  rc <- readRDS(rc_path)
  rc <- rc %>%
    mutate(
      Sector = case_when(
        as.character(Módulo) %in% c("9", "11", "12") ~ "9",
        TRUE ~ as.character(Sector)
      ),
      Módulo = case_when(
        as.character(Módulo) %in% c("9.1", "9.2")   ~ "9",
        as.character(Módulo) %in% c("11.1", "11.2") ~ "11",
        as.character(Módulo) %in% c("13.1", "13.2") ~ "13",
        as.character(Módulo) %in% c("14.1", "14.2") ~ "14",
        TRUE ~ as.character(Módulo)
      )
    )
  saveRDS(rc, rc_path)
  cat("[OK] rutas_cosecha_mapa.rds updated. Sector table:\n")
  print(table(as.character(rc$Módulo), as.character(rc$Sector)))
}

cat("\n=== 2. HISTORICO CONTEO ===\n")
hc_paths <- c(
  "D:/ACP/Actualizacion Semanal/historico_conteo.rds",
  "D:/ACP/Rutas de Cosecha/historico_conteo.rds"
)
for (hp in hc_paths) {
  if (file.exists(hp)) {
    hc <- readRDS(hp)
    hc <- hc %>%
      mutate(
        Sector = case_when(
          as.character(Módulo) %in% c("9", "11", "12") ~ "9",
          TRUE ~ as.character(Sector)
        ),
        Módulo = case_when(
          as.character(Módulo) %in% c("9.1", "9.2")   ~ "9",
          as.character(Módulo) %in% c("11.1", "11.2") ~ "11",
          as.character(Módulo) %in% c("13.1", "13.2") ~ "13",
          as.character(Módulo) %in% c("14.1", "14.2") ~ "14",
          TRUE ~ as.character(Módulo)
        )
      )
    saveRDS(hc, hp)
    cat("[OK] historico_conteo.rds updated at:", hp, "\n")
  }
}

cat("\n=== 3. EDICIONES MANUALES COSECHA ===\n")
ed_paths <- c(
  "D:/ACP/Actualizacion Semanal/ediciones_manuales_cosecha.rds",
  "D:/ACP/Actualizacion data maestra/ediciones_manuales_cosecha.rds",
  "D:/ACP/ediciones_manuales_cosecha.rds"
)
for (ep in ed_paths) {
  if (file.exists(ep)) {
    ed <- readRDS(ep)
    if (nrow(ed) > 0) {
      ed <- ed %>%
        mutate(
          Sector = case_when(
            as.character(Módulo) %in% c("9", "11", "12") ~ "9",
            TRUE ~ as.character(Sector)
          ),
          Módulo = case_when(
            as.character(Módulo) %in% c("9.1", "9.2")   ~ "9",
            as.character(Módulo) %in% c("11.1", "11.2") ~ "11",
            as.character(Módulo) %in% c("13.1", "13.2") ~ "13",
            as.character(Módulo) %in% c("14.1", "14.2") ~ "14",
            TRUE ~ as.character(Módulo)
          )
        )
      # Deduplicate keeping manual over auto
      if ("Tipo_Edicion" %in% names(ed) && "Fecha_Edicion" %in% names(ed)) {
        ed <- ed %>% arrange(desc(Tipo_Edicion == "manual"), desc(Fecha_Edicion))
      }
      cols_dist <- intersect(c("Campaña", "Semana", "Sector", "Módulo", "Turno", "Variedad", "Campo"), names(ed))
      if (length(cols_dist) > 0) {
        ed <- ed %>% distinct(across(all_of(cols_dist)), .keep_all = TRUE)
      }
      saveRDS(ed, ep)
      cat("[OK] ediciones_manuales_cosecha.rds updated at:", ep, "| Filas:", nrow(ed), "\n")
    }
  }
}

cat("\n=== 4. AJUSTE VALVULAS SEMANA ===\n")
aj_paths <- c(
  "D:/ACP/Actualizacion Semanal/ajuste_valvulas_semana.rds",
  "D:/ACP/ajuste_valvulas_semana.rds"
)
for (ap in aj_paths) {
  if (file.exists(ap)) {
    aj <- readRDS(ap)
    if (nrow(aj) > 0) {
      aj <- aj %>%
        mutate(
          Sector = case_when(
            as.character(Módulo) %in% c("9", "11", "12") ~ "9",
            TRUE ~ as.character(Sector)
          ),
          Módulo = case_when(
            as.character(Módulo) %in% c("9.1", "9.2")   ~ "9",
            as.character(Módulo) %in% c("11.1", "11.2") ~ "11",
            as.character(Módulo) %in% c("13.1", "13.2") ~ "13",
            as.character(Módulo) %in% c("14.1", "14.2") ~ "14",
            TRUE ~ as.character(Módulo)
          )
        )
      cols_dist <- intersect(c("Campaña", "Semana", "Sector", "Módulo", "Turno", "Variedad", "Codigo_Valvula", "Fecha"), names(aj))
      if (length(cols_dist) > 0) {
        aj <- aj %>% distinct(across(all_of(cols_dist)), .keep_all = TRUE)
      }
      saveRDS(aj, ap)
      cat("[OK] ajuste_valvulas_semana.rds updated at:", ap, "| Filas:", nrow(aj), "\n")
    }
  }
}

cat("\n=== 5. DATOS MAESTROS RDATA ===\n")
dm_rdata_paths <- c(
  "D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData",
  "D:/ACP/Actualizacion data maestra/datos_maestros_cosecha_2026.RData",
  "D:/ACP/Rutas de Cosecha/datos_maestros_cosecha_2026.RData"
)
for (drp in dm_rdata_paths) {
  if (file.exists(drp)) {
    env <- new.env()
    load(drp, envir = env)
    
    if (exists("df_valvulas", envir = env)) {
      env$df_valvulas <- env$df_valvulas %>%
        mutate(
          Sector = case_when(
            as.character(Módulo) %in% c("9", "11", "12") ~ "9",
            TRUE ~ as.character(Sector)
          ),
          Módulo = case_when(
            as.character(Módulo) %in% c("9.1", "9.2")   ~ "9",
            as.character(Módulo) %in% c("11.1", "11.2") ~ "11",
            as.character(Módulo) %in% c("13.1", "13.2") ~ "13",
            as.character(Módulo) %in% c("14.1", "14.2") ~ "14",
            TRUE ~ as.character(Módulo)
          )
        )
    }
    
    if (exists("df_hist_oficial_wide", envir = env)) {
      mod_col <- if ("Módulo_Norm" %in% names(env$df_hist_oficial_wide)) "Módulo_Norm" else "Módulo"
      env$df_hist_oficial_wide[[mod_col]] <- case_when(
        as.character(env$df_hist_oficial_wide[[mod_col]]) %in% c("9.1", "9.2")   ~ "9",
        as.character(env$df_hist_oficial_wide[[mod_col]]) %in% c("11.1", "11.2") ~ "11",
        as.character(env$df_hist_oficial_wide[[mod_col]]) %in% c("13.1", "13.2") ~ "13",
        as.character(env$df_hist_oficial_wide[[mod_col]]) %in% c("14.1", "14.2") ~ "14",
        TRUE ~ as.character(env$df_hist_oficial_wide[[mod_col]])
      )
      env$df_hist_oficial_wide$Sector <- case_when(
        as.character(env$df_hist_oficial_wide[[mod_col]]) %in% c("9", "11", "12") ~ "9",
        TRUE ~ as.character(env$df_hist_oficial_wide$Sector)
      )
    }
    
    if (exists("df_cosecha", envir = env)) {
      env$df_cosecha <- env$df_cosecha %>%
        mutate(
          Sector = case_when(
            as.character(Campaña) == "2026" & as.character(Módulo) %in% c("9", "11", "12") ~ "9",
            TRUE ~ as.character(Sector)
          ),
          Módulo = case_when(
            as.character(Campaña) == "2026" & as.character(Módulo) %in% c("9.1", "9.2")   ~ "9",
            as.character(Campaña) == "2026" & as.character(Módulo) %in% c("11.1", "11.2") ~ "11",
            as.character(Campaña) == "2026" & as.character(Módulo) %in% c("13.1", "13.2") ~ "13",
            as.character(Campaña) == "2026" & as.character(Módulo) %in% c("14.1", "14.2") ~ "14",
            TRUE ~ as.character(Módulo)
          )
        )
    }
    
    save(list = ls(envir = env), envir = env, file = drp)
    cat("[OK] datos_maestros_cosecha_2026.RData updated at:", drp, "\n")
  }
}

cat("\n=== ALL DATASETS UPDATED AND HOMOLOGATED TO SECTOR 9 SUCCESSFULLY ===\n")
