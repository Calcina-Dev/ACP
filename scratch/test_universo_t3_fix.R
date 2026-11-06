suppressMessages({
  library(dplyr)
})

rc_map <- readRDS("D:/ACP/Rutas de Cosecha/rutas_cosecha_mapa.rds")
hist_wide <- readRDS("D:/ACP/Rutas de Cosecha/historico_oficial_wide_2026.rds")
valvs <- readRDS("D:/ACP/Rutas de Cosecha/valvulas_normalizadas.rds")

sem_sel <- 45
dia_inicio_nom <- "Viernes"
dias_nombres <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
dia_inicio_idx <- match(dia_inicio_nom, dias_nombres)
dias_previos_u <- if (dia_inicio_idx > 1) dias_nombres[1:(dia_inicio_idx - 1)] else character(0)

# rc_sem_u
rc_sem_u <- rc_map %>% filter(as.numeric(Semana) == sem_sel)

# Ha_Rc_Prev with fix
ha_rc_prev_vec <- numeric(nrow(rc_sem_u))
for (d in dias_previos_u) {
  col_h <- paste0("Ha_", d)
  col_k <- paste0("Kg_", d)
  val_h <- if (col_h %in% names(rc_sem_u)) coalesce(rc_sem_u[[col_h]], 0) else 0
  val_k <- if (col_k %in% names(rc_sem_u)) coalesce(rc_sem_u[[col_k]], 0) else 0
  es_ejec <- (d %in% c("Lunes", "Martes")) | (val_k > 0)
  ha_rc_prev_vec <- ha_rc_prev_vec + ifelse(es_ejec, val_h, 0)
}
rc_sem_u$Ha_Rc_Prev <- ha_rc_prev_vec

rc_ha_turno <- rc_sem_u %>%
  filter(Módulo == "2", Turno == "3") %>%
  summarise(Ha_Rc = sum(Ha_Rc_Prev, na.rm = TRUE))
cat("Ha_Rc for M2 T3 with fix:\n")
print(rc_ha_turno)

# Ha_Valvs_SAP for M2 T3
cols_ha_sap <- paste0("Ha_", dias_previos_u)
cols_kg_sap <- paste0("Kg_", dias_previos_u)
cols_ha_sap_ex <- intersect(cols_ha_sap, names(hist_wide))
cols_kg_sap_ex <- intersect(cols_kg_sap, names(hist_wide))

df_sap_sem_u <- hist_wide %>%
  filter(as.numeric(Semana) == sem_sel, Módulo_Norm == "2", Turno_Norm == "3") %>%
  mutate(
    Tiene_Cosecha_Prev_Sap = (if (length(cols_ha_sap_ex) > 0) rowSums(across(all_of(cols_ha_sap_ex)), na.rm = TRUE) else 0) > 0 |
      (if (length(cols_kg_sap_ex) > 0) rowSums(across(all_of(cols_kg_sap_ex)), na.rm = TRUE) else 0) > 0
  )

ha_valvs_sap <- sum(df_sap_sem_u$Area_Ha[!duplicated(df_sap_sem_u$Codigo_Valv) & df_sap_sem_u$Tiene_Cosecha_Prev_Sap], na.rm = TRUE)
cat("Ha_Valvs_SAP for M2 T3:\n")
print(ha_valvs_sap)

# Area total
area_total <- 9.32
ha_recorrida <- ha_valvs_sap
ha_aj_1ra <- area_total - ha_recorrida
cat("Result for Turno 3:\n")
cat("Area:", area_total, "Ha Recorrida:", ha_recorrida, "Ha Aj 1ra:", ha_aj_1ra, "\n")
