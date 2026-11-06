suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

# Simular filas de turno
rend_turno_test <- tibble(
  Módulo = c("5", "5", "5"),
  Turno = c("11", "12", "99"),
  Variedad = c("MAGICA", "MAGICA", "MAGICA"),
  Semana_Ant_No_Finalizo = c(TRUE, TRUE, TRUE),
  Tiene_Cosecha_Act = c(TRUE, TRUE, FALSE),
  Prop_Avance_Turno = c(0.751, 0.753, 0.0),
  Kg_Real_Sem = c(3183.44, 5078.96, 0),
  Rend_Sem_Actual = c(657.74, 1053.73, 0),
  Rend_Ant_Turno = c(539.96, 1192.78, 800.0),
  Rend_Proy = c(668.28, 668.33, 700.0),
  Factor_Floral = c(1.40, 1.40, 1.15),
  Tipo_Efecto_Floral = c("Premio", "Premio", "Premio"),
  Esta_Proyectado = c(TRUE, TRUE, TRUE),
  Turno_Completo_Lun_Mar = c(FALSE, FALSE, FALSE),
  Es_Ciclo_Corto = c(FALSE, FALSE, FALSE)
)

# Aplicar nueva lógica de Rend_Base_Turno, Rend_Base_Con_Floral y Motivo
rend_turno_test <- rend_turno_test %>%
  mutate(
    Rend_Base_Turno = case_when(
      # RAMA 2A: Turno completo en Lun/Mar y es Rocío
      Turno_Completo_Lun_Mar & Es_Ciclo_Corto ~ Rend_Sem_Actual,
      
      # RAMA 1A: Cierre S-1 con avance > 30% en semana actual
      Semana_Ant_No_Finalizo & Tiene_Cosecha_Act & Prop_Avance_Turno > 0.30 ~ Rend_Sem_Actual,
      
      # RAMA 1B: Cierre S-1 sin avance o con avance <= 30%
      Semana_Ant_No_Finalizo ~ ifelse(Rend_Ant_Turno > 0, Rend_Ant_Turno, Rend_Proy),
      
      # RAMA 3A: Sin avance en semana actual
      !Tiene_Cosecha_Act ~ ifelse(Rend_Ant_Turno > 0, Rend_Ant_Turno, Rend_Proy),
      
      # RAMA 3B-1: Con avance > 30%
      Tiene_Cosecha_Act & Prop_Avance_Turno > 0.30 ~ Rend_Sem_Actual,
      
      # RAMA 3B-2: Con avance <= 30%
      Tiene_Cosecha_Act & Prop_Avance_Turno <= 0.30 ~ ifelse(Rend_Ant_Turno > 0, Rend_Ant_Turno, Rend_Proy),
      
      TRUE ~ ifelse(Rend_Ant_Turno > 0, Rend_Ant_Turno, Rend_Proy)
    ),
    
    # Factor floral: NO se aplica si avance > 30%
    Rend_Base_Con_Floral = case_when(
      Tiene_Cosecha_Act & Prop_Avance_Turno > 0.30 ~ round(Rend_Base_Turno, 2),
      TRUE                                         ~ round(Rend_Base_Turno * Factor_Floral, 2)
    ),
    
    Rend_Turno_Aplicar = ifelse(!Esta_Proyectado | (Turno_Completo_Lun_Mar & !Es_Ciclo_Corto), 0, Rend_Base_Con_Floral),
    
    Pct_Ajuste_Flor = round((Factor_Floral - 1) * 100, 1),
    Detalle_Floral = case_when(
      !Esta_Proyectado | (Turno_Completo_Lun_Mar & !Es_Ciclo_Corto) | (Turno_Completo_Lun_Mar & Es_Ciclo_Corto) ~ "",
      Tiene_Cosecha_Act & Prop_Avance_Turno > 0.30 ~ "",
      Tipo_Efecto_Floral == "Premio"  ~ sprintf("Premio floral %+0.1f%%", Pct_Ajuste_Flor),
      Tipo_Efecto_Floral == "Castigo" ~ sprintf("Castigo floral %+0.1f%%", Pct_Ajuste_Flor),
      TRUE                            ~ "Floral neutro (0%)"
    ),
    
    Motivo_Base = case_when(
      !Esta_Proyectado ~ "No proyectado",
      Turno_Completo_Lun_Mar & !Es_Ciclo_Corto ~ "Cerró 100% en Lun/Mar (0 kg pend.)",
      Turno_Completo_Lun_Mar & Es_Ciclo_Corto ~ sprintf("2da pasada Rocío (Rend: %s kg/ha)", format(round(Rend_Sem_Actual))),
      Semana_Ant_No_Finalizo & Tiene_Cosecha_Act & Prop_Avance_Turno > 0.30 ~ sprintf("Avance >30%% (Rend act sem: %s kg/ha)", format(round(Rend_Sem_Actual), big.mark = ",")),
      Semana_Ant_No_Finalizo ~ sprintf("Cierre S-1 incompleta (Rend S-1: %s kg/ha)", format(round(Rend_Ant_Turno), big.mark = ",")),
      !Tiene_Cosecha_Act & Rend_Ant_Turno > 0 ~ sprintf("Sin avance previo (Rend S-1: %s kg/ha)", format(round(Rend_Ant_Turno), big.mark = ",")),
      !Tiene_Cosecha_Act                      ~ sprintf("Sin avance previo (Rend proy: %s kg/ha)", format(round(Rend_Proy), big.mark = ",")),
      Tiene_Cosecha_Act & Prop_Avance_Turno > 0.30 ~ sprintf("Avance >30%% (Rend act sem: %s kg/ha)", format(round(Rend_Sem_Actual), big.mark = ",")),
      TRUE ~ "Otro"
    ),
    
    Motivo_Ajuste = case_when(
      nzchar(Detalle_Floral) ~ paste0(Motivo_Base, " • ", Detalle_Floral),
      TRUE ~ Motivo_Base
    )
  )

print(rend_turno_test %>% select(Turno, Rend_Sem_Actual, Rend_Ant_Turno, Factor_Floral, Rend_Base_Con_Floral, Motivo_Ajuste))
