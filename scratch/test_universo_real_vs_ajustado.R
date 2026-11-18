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

dias_nombres_u <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
cols_ha_sem_u  <- paste0("Ha_", dias_nombres_u)
cols_kg_sem_u  <- paste0("Kg_", dias_nombres_u)

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

# SAP
cols_ha_sap_exist_u <- intersect(cols_ha_sem_u, names(df_hist_oficial_wide))
cols_kg_sap_exist_u <- intersect(cols_kg_sem_u, names(df_hist_oficial_wide))

df_sap_sem_u <- df_hist_oficial_wide %>%
  filter(as.numeric(Semana) == 46) %>%
  mutate(
    Turno_num_s = suppressWarnings(as.numeric(Turno_Norm)),
    Módulo_s = as.character(Módulo_Norm),
    Turno_s = as.character(Turno_Norm),
    Var_s   = toupper(trimws(Variedad)),
    Tiene_Cosecha_Sem_Sap = (if (length(cols_ha_sap_exist_u) > 0) rowSums(across(all_of(cols_ha_sap_exist_u)), na.rm = TRUE) else 0) > 0 |
      (if (length(cols_kg_sap_exist_u) > 0) rowSums(across(all_of(cols_kg_sap_exist_u)), na.rm = TRUE) else 0) > 0
  )

sap_ha_valvs_turno <- df_sap_sem_u %>%
  group_by(Módulo_s, Turno_s, Var_s) %>%
  summarise(
    Ha_Valvs_SAP = sum(Area_Ha[!duplicated(Codigo_Valv) & Tiene_Cosecha_Sem_Sap], na.rm = TRUE),
    .groups = "drop"
  )

cat("Rutas de Campo para M10-T18:\n")
print(rc_ha_turno %>% filter(Módulo_rc == "10", Turno_rc == "18"))

cat("SAP válvulas para M10-T18:\n")
print(sap_ha_valvs_turno %>% filter(Módulo_s == "10", Turno_s == "18"))

# M10-T18 Turn area = 5.33 ha
area_t18 <- 5.33
ha_rc_1ra <- (rc_ha_turno %>% filter(Módulo_rc == "10", Turno_rc == "18"))$Ha_Rc_1ra
ha_real_1ra <- round(pmin(area_t18, ha_rc_1ra), 2)
kg_real <- 3530.25
rend_real <- round(kg_real / ha_real_1ra, 2)

cat(sprintf("\nResultado Final para M10-T18:\n  Área Catálogo: %.2f ha\n  Ha Real 1ª Pas: %.2f ha\n  Kg Real SAP: %.2f kg\n  Kg/Ha Real 1ª Pas: %.2f kg/ha\n",
            area_t18, ha_real_1ra, kg_real, rend_real))
