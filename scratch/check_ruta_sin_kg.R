source("d:/ACP/scratch/test_flowchart_complete.R")

rend_turno <- rend_turno %>%
  mutate(
    Rama = case_when(
      Semana_Ant_No_Finalizo ~ "1. Rama Izq: S-1 No Finalizó (Remanente)",
      Turno_Completo_Lun_Mar & Es_Ciclo_Corto ~ "2A. Rama Der: 100% Lun/Mar -> Rocío (2da Pasada)",
      Turno_Completo_Lun_Mar & !Es_Ciclo_Corto ~ "2B. Rama Der: 100% Lun/Mar -> No Rocío (No Ajustar)",
      !Tiene_Cosecha_Act ~ "3A. Rama Der: Sin Kilos (Proy vs S-1)",
      Tiene_Cosecha_Act & Prop_Avance_Turno > 0.30 ~ "3B-1. Rama Der: Con Kilos >30% (Actual vs S-1)",
      Tiene_Cosecha_Act & Prop_Avance_Turno <= 0.30 ~ "3B-2. Rama Der: Con Kilos <=30% (Proy vs S-1)"
    )
  )

cat("\n--- Turnos con Ruta (Ha_Prog_Turno > 0) pero SIN Kilos en SAP (Kg_Real_Sem == 0) ---\n")
sin_kg <- rend_turno %>%
  filter(Ha_Prog_Turno > 0, Kg_Real_Sem == 0) %>%
  select(Módulo, Turno, Variedad, Area_Total_Turno, Ha_Prog_Turno, Ha_Lunes_Turno, Ha_Martes_Turno, Kg_Real_Sem, Kg_Prog_Turno, Rend_Ant_Turno, Rend_Proy, Rend_Turno_Aplicar, Rama)
print(as.data.frame(sin_kg))

cat("\n--- Turnos con Kilos en SAP (Kg_Real_Sem > 0) pero SIN Ruta (Ha_Prog_Turno == 0) ---\n")
sin_ruta <- rend_turno %>%
  filter(Kg_Real_Sem > 0, Ha_Prog_Turno == 0) %>%
  select(Módulo, Turno, Variedad, Area_Total_Turno, Ha_Prog_Turno, Kg_Real_Sem, Kg_Lunes_Real, es_remanente_minimo, Ha_Recorrida_Turno, Rend_Ant_Turno, Rend_Proy, Rend_Turno_Aplicar, Rama)
print(as.data.frame(sin_ruta))
