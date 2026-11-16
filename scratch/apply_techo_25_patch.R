app_path <- "D:/ACP/Actualizacion Semanal/app.R"
content <- paste(readLines(app_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

replace_target <- function(content, name, target, replacement, expected_count = 1) {
  matches <- gregexpr(target, content, fixed = TRUE)[[1]]
  actual_count <- if (length(matches) == 1 && matches[1] == -1) 0 else length(matches)
  if (actual_count != expected_count) {
    stop(sprintf("Target count mismatch for %s: expected %d, got %d", name, expected_count, actual_count))
  }
  content_new <- gsub(target, replacement, content, fixed = TRUE)
  cat(sprintf("[OK] Replaced %s (%d occurrences)\n", name, actual_count))
  return(content_new)
}

# 1. Target 1: Rend_Base_Con_Floral & Motivo calculation in rend_turno
target_1 <- '          # Rendimiento ajustado con Premio o Castigo por tendencia floral de maduración
          # REGLA: Cuando el avance es > 30% del turno o proviene de cosecha fresca representativa, NO se aplica factor floral
          # porque el rendimiento observado ya viene afectado por las condiciones fenológicas vigentes.
          Rend_Base_Con_Floral = case_when(
            Semana_Ant_No_Finalizo & Tiene_Cosecha_Fresca & Prop_Fresca_Turno > prop_umbral_cfg ~ round(Rend_Base_Turno, 2),
            Tiene_Cosecha_Act & Prop_Avance_Turno > prop_umbral_cfg                              ~ round(Rend_Base_Turno, 2),
            TRUE                                                                                ~ round(Rend_Base_Turno * Factor_Floral, 2)
          ),
          
          # Si el turno NO está proyectado en la semana o si se cosechó 100% entre Lunes/Martes y NO es Rocío -> No ajustar (0)
          Rend_Turno_Aplicar = ifelse(!Esta_Proyectado | (Turno_Completo_Lun_Mar & !Es_Ciclo_Corto), 0, Rend_Base_Con_Floral),
          
          Motivo_Base = case_when(
            !Esta_Proyectado ~ "No proyectado",
            Turno_Completo_Lun_Mar & !Es_Ciclo_Corto ~ "Cerró 100% en Lun/Mar (0 kg pend.)",
            Turno_Completo_Lun_Mar & Es_Ciclo_Corto ~ sprintf("2da pasada Rocío (Rend Lunes: %s kg/ha • 60%% sin floración)", trimws(format(round(coalesce(Rend_Lunes_Turno, Rend_Sem_Actual)), big.mark = ","))),
            Semana_Ant_No_Finalizo & Tiene_Cosecha_Fresca & Prop_Fresca_Turno > prop_umbral_cfg ~ sprintf("Cierre S-1 + Muestra fresca %s%% (Rend fresco: %s kg/ha)", round(Prop_Fresca_Turno * 100, 1), trimws(format(round(Rend_Fresco_Turno), big.mark = ","))),
            Semana_Ant_No_Finalizo & Rend_Ant_Turno > 0 ~ sprintf("Cierre S-1 incompleta (Rend S-1: %s kg/ha)", trimws(format(round(Rend_Ant_Turno), big.mark = ","))),
            Semana_Ant_No_Finalizo                      ~ sprintf("Cierre S-1 incompleta (Rend proy: %s kg/ha)", trimws(format(round(Rend_Proy), big.mark = ","))),
            Tiene_Cosecha_Act & Prop_Avance_Turno > prop_umbral_cfg ~ sprintf("Avance >%d%% (Rend act sem: %s kg/ha)", round(umbral_avance_cfg), trimws(format(round(Rend_Sem_Actual), big.mark = ","))),
            !Tiene_Cosecha_Act & Rend_Ant_Turno > 0 ~ sprintf("Sin avance previo (Rend S-1: %s kg/ha)", trimws(format(round(Rend_Ant_Turno), big.mark = ","))),
            !Tiene_Cosecha_Act                      ~ sprintf("Sin avance previo (Rend proy: %s kg/ha)", trimws(format(round(Rend_Proy), big.mark = ","))),
            Tiene_Cosecha_Act & Prop_Avance_Turno <= prop_umbral_cfg & Rend_Ant_Turno > 0 ~ sprintf("Avance <=%d%% (Rend S-1: %s kg/ha)", round(umbral_avance_cfg), trimws(format(round(Rend_Ant_Turno), big.mark = ","))),
            Tiene_Cosecha_Act & Prop_Avance_Turno <= prop_umbral_cfg                      ~ sprintf("Avance <=%d%% (Rend proy: %s kg/ha)", round(umbral_avance_cfg), trimws(format(round(Rend_Proy), big.mark = ","))),
            TRUE ~ ifelse(Rend_Ant_Turno > 0, sprintf("Rend S-1 (%s kg/ha)", trimws(format(round(Rend_Ant_Turno), big.mark = ","))), sprintf("Rend proy (%s kg/ha)", trimws(format(round(Rend_Proy), big.mark = ","))))
          ),
          
          Pct_Ajuste_Flor = round((Factor_Floral - 1) * 100, 1),
          Detalle_Floral = case_when(
            !Esta_Proyectado | (Turno_Completo_Lun_Mar & !Es_Ciclo_Corto) | (Turno_Completo_Lun_Mar & Es_Ciclo_Corto) ~ "",
            Semana_Ant_No_Finalizo & Tiene_Cosecha_Fresca & Prop_Fresca_Turno > prop_umbral_cfg ~ "",
            Tiene_Cosecha_Act & Prop_Avance_Turno > prop_umbral_cfg ~ "",
            Tipo_Efecto_Floral == "Premio"  ~ sprintf("Premio floral %+0.1f%%", Pct_Ajuste_Flor),
            Tipo_Efecto_Floral == "Castigo" ~ sprintf("Castigo floral %+0.1f%%", Pct_Ajuste_Flor),
            TRUE                            ~ "Floral neutro (0%)"
          ),'

replacement_1 <- '          # Regla de Control de Techo (+25% sobre la Proyección Oficial):
          # Si el rendimiento de la semana pasada (S-1) aplicándole el factor floral supera a la
          # proyección oficial por más de 25%, se utiliza la proyección; sino, se sigue con el rendimiento calculado.
          Usa_Base_S1     = (Rend_Base_Turno == Rend_Ant_Turno) & (Rend_Ant_Turno > 0),
          Rend_S1_Floral  = round(Rend_Ant_Turno * Factor_Floral, 2),
          Excede_25_Proy  = Usa_Base_S1 & Esta_Proyectado & (Rend_Proy > 0) & (Rend_S1_Floral > 1.25 * Rend_Proy),
          Pct_Exceso_Proy = round(((Rend_S1_Floral / pmax(1, Rend_Proy)) - 1) * 100, 1),
          
          # Rendimiento ajustado con Premio o Castigo por tendencia floral de maduración
          # REGLA: Cuando el avance es > 30% del turno o proviene de cosecha fresca representativa, NO se aplica factor floral
          # porque el rendimiento observado ya viene afectado por las condiciones fenológicas vigentes.
          Rend_Base_Con_Floral = case_when(
            Semana_Ant_No_Finalizo & Tiene_Cosecha_Fresca & Prop_Fresca_Turno > prop_umbral_cfg ~ round(Rend_Base_Turno, 2),
            Tiene_Cosecha_Act & Prop_Avance_Turno > prop_umbral_cfg                              ~ round(Rend_Base_Turno, 2),
            Excede_25_Proy                                                                      ~ round(Rend_Proy, 2),
            TRUE                                                                                ~ round(Rend_Base_Turno * Factor_Floral, 2)
          ),
          
          # Si el turno NO está proyectado en la semana o si se cosechó 100% entre Lunes/Martes y NO es Rocío -> No ajustar (0)
          Rend_Turno_Aplicar = ifelse(!Esta_Proyectado | (Turno_Completo_Lun_Mar & !Es_Ciclo_Corto), 0, Rend_Base_Con_Floral),
          
          Motivo_Base = case_when(
            !Esta_Proyectado ~ "No proyectado",
            Turno_Completo_Lun_Mar & !Es_Ciclo_Corto ~ "Cerró 100% en Lun/Mar (0 kg pend.)",
            Turno_Completo_Lun_Mar & Es_Ciclo_Corto ~ sprintf("2da pasada Rocío (Rend Lunes: %s kg/ha • 60%% sin floración)", trimws(format(round(coalesce(Rend_Lunes_Turno, Rend_Sem_Actual)), big.mark = ","))),
            Semana_Ant_No_Finalizo & Tiene_Cosecha_Fresca & Prop_Fresca_Turno > prop_umbral_cfg ~ sprintf("Cierre S-1 + Muestra fresca %s%% (Rend fresco: %s kg/ha)", round(Prop_Fresca_Turno * 100, 1), trimws(format(round(Rend_Fresco_Turno), big.mark = ","))),
            Semana_Ant_No_Finalizo & Excede_25_Proy     ~ sprintf("Cierre S-1 incompleta (Rend proy: %s kg/ha)", trimws(format(round(Rend_Proy), big.mark = ","))),
            Semana_Ant_No_Finalizo & Rend_Ant_Turno > 0 ~ sprintf("Cierre S-1 incompleta (Rend S-1: %s kg/ha)", trimws(format(round(Rend_Ant_Turno), big.mark = ","))),
            Semana_Ant_No_Finalizo                      ~ sprintf("Cierre S-1 incompleta (Rend proy: %s kg/ha)", trimws(format(round(Rend_Proy), big.mark = ","))),
            Tiene_Cosecha_Act & Prop_Avance_Turno > prop_umbral_cfg ~ sprintf("Avance >%d%% (Rend act sem: %s kg/ha)", round(umbral_avance_cfg), trimws(format(round(Rend_Sem_Actual), big.mark = ","))),
            !Tiene_Cosecha_Act & Excede_25_Proy     ~ sprintf("Sin avance previo (Rend proy: %s kg/ha)", trimws(format(round(Rend_Proy), big.mark = ","))),
            !Tiene_Cosecha_Act & Rend_Ant_Turno > 0 ~ sprintf("Sin avance previo (Rend S-1: %s kg/ha)", trimws(format(round(Rend_Ant_Turno), big.mark = ","))),
            !Tiene_Cosecha_Act                      ~ sprintf("Sin avance previo (Rend proy: %s kg/ha)", trimws(format(round(Rend_Proy), big.mark = ","))),
            Tiene_Cosecha_Act & Prop_Avance_Turno <= prop_umbral_cfg & Excede_25_Proy     ~ sprintf("Avance <=%d%% (Rend proy: %s kg/ha)", round(umbral_avance_cfg), trimws(format(round(Rend_Proy), big.mark = ","))),
            Tiene_Cosecha_Act & Prop_Avance_Turno <= prop_umbral_cfg & Rend_Ant_Turno > 0 ~ sprintf("Avance <=%d%% (Rend S-1: %s kg/ha)", round(umbral_avance_cfg), trimws(format(round(Rend_Ant_Turno), big.mark = ","))),
            Tiene_Cosecha_Act & Prop_Avance_Turno <= prop_umbral_cfg                      ~ sprintf("Avance <=%d%% (Rend proy: %s kg/ha)", round(umbral_avance_cfg), trimws(format(round(Rend_Proy), big.mark = ","))),
            TRUE ~ ifelse(Rend_Ant_Turno > 0, sprintf("Rend S-1 (%s kg/ha)", trimws(format(round(Rend_Ant_Turno), big.mark = ","))), sprintf("Rend proy (%s kg/ha)", trimws(format(round(Rend_Proy), big.mark = ","))))
          ),
          
          Pct_Ajuste_Flor = round((Factor_Floral - 1) * 100, 1),
          Detalle_Floral = case_when(
            !Esta_Proyectado | (Turno_Completo_Lun_Mar & !Es_Ciclo_Corto) | (Turno_Completo_Lun_Mar & Es_Ciclo_Corto) ~ "",
            Semana_Ant_No_Finalizo & Tiene_Cosecha_Fresca & Prop_Fresca_Turno > prop_umbral_cfg ~ "",
            Tiene_Cosecha_Act & Prop_Avance_Turno > prop_umbral_cfg ~ "",
            Excede_25_Proy                  ~ sprintf("S-1 + floral supera >25%% proy (+%.1f%%) -> Usa proy", Pct_Exceso_Proy),
            Tipo_Efecto_Floral == "Premio"  ~ sprintf("Premio floral %+0.1f%%", Pct_Ajuste_Flor),
            Tipo_Efecto_Floral == "Castigo" ~ sprintf("Castigo floral %+0.1f%%", Pct_Ajuste_Flor),
            TRUE                            ~ "Floral neutro (0%)"
          ),'

content <- replace_target(content, "Target 1 (Rend_Base_Con_Floral & Motivo)", target_1, replacement_1, 1)

# 2. Target 2: Reasoning modal flags & hero tag
target_2 <- '    tiene_premio    <- grepl("Premio floral", motivo_str)
    tiene_castigo   <- grepl("Castigo floral", motivo_str)
    es_cierre_s1          <- grepl("Cierre S-1", motivo_str)
    tiene_muestra_fresca  <- grepl("Muestra fresca", motivo_str)
    es_manual             <- grepl("Edición manual", motivo_str) || (nrow(ed_turno) > 0 & !grepl("Cierre S-1|Avance >|Avance <=|Sin avance|2da pasada|Cerró 100%", motivo_str))
    
    hero_tag <- if (tiene_premio) {
      list(cls = "ek-tag-green", txt = "Premio Floral")
    } else if (tiene_castigo) {'

replacement_2 <- '    tiene_exceso_25 <- grepl("supera >25% proy", motivo_str)
    tiene_premio    <- grepl("Premio floral", motivo_str)
    tiene_castigo   <- grepl("Castigo floral", motivo_str)
    es_cierre_s1          <- grepl("Cierre S-1", motivo_str)
    tiene_muestra_fresca  <- grepl("Muestra fresca", motivo_str)
    es_manual             <- grepl("Edición manual", motivo_str) || (nrow(ed_turno) > 0 & !grepl("Cierre S-1|Avance >|Avance <=|Sin avance|2da pasada|Cerró 100%", motivo_str))
    
    hero_tag <- if (tiene_exceso_25) {
      list(cls = "ek-tag-blue", txt = "Tope Proyección (+25%)")
    } else if (tiene_premio) {
      list(cls = "ek-tag-green", txt = "Premio Floral")
    } else if (tiene_castigo) {'

content <- replace_target(content, "Target 2 (Modal hero tag)", target_2, replacement_2, 1)

# 3. Target 3: Modal step 2
target_3 <- '    if (es_cierre_s1 && tiene_muestra_fresca) {
      pasos <- c(pasos, list(list(
        num = "2",
        titulo = "Factor Floral y Fenología",
        desc = sprintf("Al utilizarse directamente el rendimiento observado de las válvulas frescas del nuevo ciclo (%s kg/ha), este ya refleja las condiciones fenológicas vigentes, por lo que no se aplica factor floral multiplicador.", rend_fresco_txt)
      )))
    } else if (tiene_premio) {'

replacement_3 <- '    if (es_cierre_s1 && tiene_muestra_fresca) {
      pasos <- c(pasos, list(list(
        num = "2",
        titulo = "Factor Floral y Fenología",
        desc = sprintf("Al utilizarse directamente el rendimiento observado de las válvulas frescas del nuevo ciclo (%s kg/ha), este ya refleja las condiciones fenológicas vigentes, por lo que no se aplica factor floral multiplicador.", rend_fresco_txt)
      )))
    } else if (tiene_exceso_25) {
      pct_exc_txt <- if (grepl("supera >25% proy \\\\+([0-9.]+)%", motivo_str)) {
        sub(".*supera >25% proy \\\\+([0-9.]+)%.*", "+\\\\1%", motivo_str)
      } else "+25.0%"
      pasos <- c(pasos, list(list(
        num = "2",
        titulo = "Control de Techo: S-1 + Floral supera Proyección por > 25%",
        desc = sprintf("El rendimiento histórico de S-1 combinado con el factor floral superó a la proyección oficial por más del 25%% (%s). Para evitar sobreestimaciones y asegurar apego presupuestal, el sistema adopta directamente el rendimiento de la proyección oficial.", pct_exc_txt)
      )))
    } else if (tiene_premio) {'

content <- replace_target(content, "Target 3 (Modal Step 2)", target_3, replacement_3, 1)

# Write to app.R
writeLines(strsplit(content, "\n", fixed = TRUE)[[1]], app_path, sep = "\r\n", useBytes = TRUE)
cat("\nFile written successfully to:", app_path, "\n")

# Verify syntax
p <- parse(app_path)
cat(sprintf("SUCCESS: app.R parsed with 0 syntax errors! (%d top-level expressions)\n", length(p)))
