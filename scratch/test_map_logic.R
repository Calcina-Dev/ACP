suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

cat("=== TEST DE SOLUCIÓN: SINCRONIZACIÓN CORRECTA VALVS + EDICIONES ===\n")

valvs_sem <- readRDS('Actualizacion Semanal/ajuste_valvulas_semana.rds') %>%
  filter(as.numeric(Semana) == 46, coalesce(Num_Pasada, 1L) == 1L)

ed_sem <- readRDS('Actualizacion Semanal/ediciones_manuales_cosecha.rds') %>%
  filter(as.numeric(Semana) == 46)

man_filas <- ed_sem %>% filter(Tipo_Edicion == "manual", Campo %in% c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado"), coalesce(Valor, 0) > 0)

# FUNCIÓN PROPUESTA:
ajustar_valvulas_con_manuales <- function(df_aj_valvs, df_man, df_valv_catalogo) {
  if (is.null(df_aj_valvs)) df_aj_valvs <- tibble()
  if (is.null(df_man) || nrow(df_man) == 0) return(df_aj_valvs)
  
  df_res <- df_aj_valvs
  
  # Agrupar ediciones manuales por Turno (Sector, Módulo, Turno)
  turnos_man <- df_man %>%
    distinct(Sector, Módulo, Turno, Variedad)
  
  for (t_idx in seq_len(nrow(turnos_man))) {
    tm <- turnos_man[t_idx, ]
    mod_c <- trimws(as.character(tm$Módulo))
    tur_c <- trimws(as.character(tm$Turno))
    var_c <- trimws(as.character(tm$Variedad))
    
    # Filas manuales de este turno
    ed_t <- df_man %>%
      filter(
        trimws(as.character(Módulo)) == mod_c,
        trimws(as.character(Turno))  == tur_c
      ) %>%
      arrange(Campo)
    
    dias_man <- unique(as.character(ed_t$Campo))
    
    # Buscar si ya existen válvulas en df_res
    idx_v <- which(
      trimws(as.character(df_res$Módulo)) == mod_c &
      trimws(as.character(df_res$Turno))  == tur_c
    )
    
    if (length(idx_v) > 0) {
      valvs_t <- df_res[idx_v, ]
      
      # Si los días de las válvulas no coinciden con los días manuales, redistribuir válvulas entre días
      dias_v <- unique(as.character(valvs_t$Dia_Asignado))
      if (!all(dias_v %in% dias_man) || !all(dias_man %in% dias_v)) {
        # Redistribuir válvulas discretamente entre los días manuales según Ha_Asignada de cada día
        # Ordenar válvulas
        ha_por_dia <- setNames(coalesce(as.numeric(ed_t$Ha_Asignada), 0), as.character(ed_t$Campo))
        if (sum(ha_por_dia) <= 0) {
          ha_por_dia <- setNames(coalesce(as.numeric(ed_t$Valor), 0), as.character(ed_t$Campo))
        }
        
        # Asignación greedy de válvulas a días
        dias_ord <- names(ha_por_dia)
        ha_disp <- ha_por_dia
        dias_asig_v <- character(nrow(valvs_t))
        
        for (vi in seq_len(nrow(valvs_t))) {
          # Asignar al día con más cupo pendiente
          best_dia <- dias_ord[which.max(ha_disp)]
          dias_asig_v[vi] <- best_dia
          ha_disp[best_dia] <- ha_disp[best_dia] - coalesce(as.numeric(valvs_t$Ha_Asignada[vi]), 0)
        }
        valvs_t$Dia_Asignado <- dias_asig_v
      }
      
      # Actualizar Kg_Asignado de las válvulas según los kilos del día manual
      for (d in dias_man) {
        ed_d <- ed_t %>% filter(as.character(Campo) == d)
        val_d <- if (nrow(ed_d) > 0) as.numeric(ed_d$Valor[1]) else 0
        
        idx_vd <- which(as.character(valvs_t$Dia_Asignado) == d)
        if (length(idx_vd) > 0) {
          tot_ha_d <- sum(coalesce(as.numeric(valvs_t$Ha_Asignada[idx_vd]), 0))
          if (tot_ha_d > 0) {
            valvs_t$Kg_Asignado[idx_vd] <- round(val_d * (coalesce(as.numeric(valvs_t$Ha_Asignada[idx_vd]), 0) / tot_ha_d), 2)
          } else {
            valvs_t$Kg_Asignado[idx_vd] <- round(val_d / length(idx_vd), 2)
          }
        }
      }
      
      df_res[idx_v, ] <- valvs_t
      
    } else {
      # El turno NO estaba en df_res: sintetizar desde catálogo de válvulas
      vt <- df_valv_catalogo %>%
        filter(
          trimws(as.character(Módulo)) == mod_c,
          trimws(as.character(Turno))  == tur_c
        )
      if (nrow(vt) == 0) next
      
      ha_por_dia <- setNames(coalesce(as.numeric(ed_t$Ha_Asignada), 0), as.character(ed_t$Campo))
      if (sum(ha_por_dia) <= 0) ha_por_dia <- setNames(coalesce(as.numeric(ed_t$Valor), 0), as.character(ed_t$Campo))
      
      dias_ord <- names(ha_por_dia)
      ha_disp <- ha_por_dia
      dias_asig_v <- character(nrow(vt))
      
      for (vi in seq_len(nrow(vt))) {
        best_dia <- dias_ord[which.max(ha_disp)]
        dias_asig_v[vi] <- best_dia
        ha_disp[best_dia] <- ha_disp[best_dia] - coalesce(as.numeric(vt$Área[vi]), 0)
      }
      
      vt_syn <- vt %>%
        transmute(
          Campaña         = "2026",
          Semana          = 46,
          id              = id,
          codigo          = codigo,
          Sector          = tm$Sector,
          Módulo          = mod_c,
          Turno           = tur_c,
          Variedad        = var_c,
          Dia_Asignado    = dias_asig_v,
          Kg_Asignado     = 0,
          Ha_Asignada     = Área,
          Num_Pasada      = 1L,
          Tipo_Asignacion = "manual",
          Dia_Desde       = "Miércoles",
          Dia_Hasta       = "Sábado"
        )
      
      for (d in dias_man) {
        ed_d <- ed_t %>% filter(as.character(Campo) == d)
        val_d <- if (nrow(ed_d) > 0) as.numeric(ed_d$Valor[1]) else 0
        idx_vd <- which(as.character(vt_syn$Dia_Asignado) == d)
        if (length(idx_vd) > 0) {
          tot_ha_d <- sum(coalesce(as.numeric(vt_syn$Ha_Asignada[idx_vd]), 0))
          if (tot_ha_d > 0) {
            vt_syn$Kg_Asignado[idx_vd] <- round(val_d * (coalesce(as.numeric(vt_syn$Ha_Asignada[idx_vd]), 0) / tot_ha_d), 2)
          } else {
            vt_syn$Kg_Asignado[idx_vd] <- round(val_d / length(idx_vd), 2)
          }
        }
      }
      
      df_res <- bind_rows(df_res, vt_syn)
    }
  }
  
  df_res
}

# Ejecutar la solución
df_fixed <- ajustar_valvulas_con_manuales(valvs_sem, man_filas, valvs_sem %>% rename(Área = Ha_Asignada))

cat("\n--- RESULTADO DE LA SOLUCIÓN: Turno 9 y 10 ---\n")
print(df_fixed %>% filter(Turno %in% c("9", "10"), Módulo == "5") %>% select(codigo, Turno, Dia_Asignado, Kg_Asignado, Ha_Asignada))

cat("\nTotal filas:", nrow(df_fixed %>% filter(Turno %in% c("9", "10"), Módulo == "5")), "\n")
stopifnot(nrow(df_fixed %>% filter(Turno %in% c("9", "10"), Módulo == "5")) == 8)

# Simular df_aj_wide con la solución:
dias_todos <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
df_aj_wide <- df_fixed %>%
  filter(Turno %in% c("9", "10"), Módulo == "5") %>%
  mutate(
    Dia_Asignado_norm = case_when(
      Dia_Asignado %in% c("Miércoles", "Miercoles") ~ "Miercoles",
      TRUE ~ Dia_Asignado
    )
  ) %>%
  group_by(id, codigo, Turno) %>%
  summarise(
    Kg_Miercoles_aj = sum(ifelse(Dia_Asignado_norm == "Miercoles", coalesce(Kg_Asignado, 0), 0)),
    Ha_Miercoles_aj = sum(ifelse(Dia_Asignado_norm == "Miercoles", coalesce(Ha_Asignada, 0), 0)),
    Kg_Jueves_aj    = sum(ifelse(Dia_Asignado_norm == "Jueves", coalesce(Kg_Asignado, 0), 0)),
    Ha_Jueves_aj    = sum(ifelse(Dia_Asignado_norm == "Jueves", coalesce(Ha_Asignada, 0), 0)),
    .groups         = "drop"
  )

cat("\n--- RESULTADO WIDE: Porcentajes y Kilos por válvula ---\n")
print(df_aj_wide)
