app_rna_path <- "D:/ACP/Actualizacion Semanal RNA/app.R"
content_rna <- paste(readLines(app_rna_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

target_rna <- '          # Rendimiento ajustado con Premio o Castigo por tendencia floral de maduración
          # REGLA: Cuando el avance es > 30% del turno, NO se aplica factor floral
          # porque el rendimiento de la semana actual ya viene afectado por ese factor.
          Rend_Base_Con_Floral = case_when(
            Tiene_Cosecha_Act & Prop_Avance_Turno > 0.30 ~ round(Rend_Base_Turno, 2),
            TRUE                                         ~ round(Rend_Base_Turno * Factor_Floral, 2)
          ),
          
          # Si el turno NO está proyectado en la semana o si se cosechó 100% entre Lunes/Martes y NO es Rocío -> No ajustar (0)
          Rend_Turno_Aplicar = ifelse(!Esta_Proyectado | (Turno_Completo_Lun_Mar & !Es_Ciclo_Corto), 0, Rend_Base_Con_Floral),'

replacement_rna <- '          # Regla de Control de Techo (+25% sobre la Proyección Oficial):
          # Si el rendimiento de la semana pasada (S-1) aplicándole el factor floral supera a la
          # proyección oficial por más de 25%, se utiliza la proyección; sino, se sigue con el rendimiento calculado.
          Usa_Base_S1     = (Rend_Base_Turno == Rend_Ant_Turno) & (Rend_Ant_Turno > 0),
          Rend_S1_Floral  = round(Rend_Ant_Turno * Factor_Floral, 2),
          Excede_25_Proy  = Usa_Base_S1 & Esta_Proyectado & (Rend_Proy > 0) & (Rend_S1_Floral > 1.25 * Rend_Proy),
          Pct_Exceso_Proy = round(((Rend_S1_Floral / pmax(1, Rend_Proy)) - 1) * 100, 1),
          
          # Rendimiento ajustado con Premio o Castigo por tendencia floral de maduración
          # REGLA: Cuando el avance es > 30% del turno, NO se aplica factor floral
          # porque el rendimiento de la semana actual ya viene afectado por ese factor.
          Rend_Base_Con_Floral = case_when(
            Tiene_Cosecha_Act & Prop_Avance_Turno > 0.30 ~ round(Rend_Base_Turno, 2),
            Excede_25_Proy                               ~ round(Rend_Proy, 2),
            TRUE                                         ~ round(Rend_Base_Turno * Factor_Floral, 2)
          ),
          
          # Si el turno NO está proyectado en la semana o si se cosechó 100% entre Lunes/Martes y NO es Rocío -> No ajustar (0)
          Rend_Turno_Aplicar = ifelse(!Esta_Proyectado | (Turno_Completo_Lun_Mar & !Es_Ciclo_Corto), 0, Rend_Base_Con_Floral),'

matches <- gregexpr(target_rna, content_rna, fixed = TRUE)[[1]]
cat("Matches in RNA app:", length(matches), "\n")
if (length(matches) == 1 && matches[1] != -1) {
  content_rna <- gsub(target_rna, replacement_rna, content_rna, fixed = TRUE)
  writeLines(strsplit(content_rna, "\n", fixed = TRUE)[[1]], app_rna_path, sep = "\r\n", useBytes = TRUE)
  p <- parse(app_rna_path)
  cat("RNA app parsed successfully! Expressions:", length(p), "\n")
}
