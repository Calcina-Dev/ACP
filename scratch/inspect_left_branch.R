source("d:/ACP/scratch/test_flowchart_complete.R")

cat("\n=== DETALLE DE LOS 8 TURNOS EN RAMA IZQUIERDA (S-1 NO FINALIZÓ) ===\n")
rend_turno %>%
  filter(Semana_Ant_No_Finalizo) %>%
  select(Módulo, Turno, Variedad, Area_Total_Turno, Ha_Ant_Turno, Ultimo_Dia_Cosecha_Ant, Ha_Recorrida_Turno, Kg_Real_Sem, Rend_Ant_Turno, Rend_Turno_Aplicar) %>%
  as.data.frame() %>%
  print()
