library(dplyr)
library(tibble)

cat("=== TESTING NUEVA LÓGICA DE RENDIMIENTO DE SEMANA ANTERIOR ===\n")

simular_arbol_rendimiento <- function(Tiene_Cosecha_Act, Prop_Avance_Turno, Rend_Ant_Turno, Rend_Proy, Factor_Floral) {
  Semana_Ant_No_Finalizo <- FALSE
  Turno_Completo_Lun_Mar <- FALSE
  Es_Ciclo_Corto <- FALSE
  Rend_Sem_Actual <- 5000
  Esta_Proyectado <- TRUE
  
  Rend_Base_Turno <- case_when(
    Semana_Ant_No_Finalizo ~ ifelse(Rend_Ant_Turno > 0, Rend_Ant_Turno, Rend_Proy),
    Turno_Completo_Lun_Mar & Es_Ciclo_Corto ~ Rend_Sem_Actual,
    !Tiene_Cosecha_Act ~ ifelse(Rend_Ant_Turno > 0, Rend_Ant_Turno, Rend_Proy),
    Tiene_Cosecha_Act & Prop_Avance_Turno > 0.30 ~ Rend_Sem_Actual,
    Tiene_Cosecha_Act & Prop_Avance_Turno <= 0.30 ~ ifelse(Rend_Ant_Turno > 0, Rend_Ant_Turno, Rend_Proy),
    TRUE ~ ifelse(Rend_Ant_Turno > 0, Rend_Ant_Turno, Rend_Proy)
  )
  
  Rend_Base_Con_Floral <- round(Rend_Base_Turno * Factor_Floral, 2)
  Rend_Turno_Aplicar <- ifelse(!Esta_Proyectado | (Turno_Completo_Lun_Mar & !Es_Ciclo_Corto), 0, Rend_Base_Con_Floral)
  
  Motivo_Base <- case_when(
    !Esta_Proyectado ~ "No proyectado",
    Turno_Completo_Lun_Mar & !Es_Ciclo_Corto ~ "Cerró 100% en Lun/Mar (0 kg pend.)",
    Semana_Ant_No_Finalizo ~ sprintf("Cierre S-1 incompleta (Rend S-1: %s kg/ha)", format(round(Rend_Ant_Turno), big.mark = ",")),
    Turno_Completo_Lun_Mar & Es_Ciclo_Corto ~ "2da pasada Rocío",
    !Tiene_Cosecha_Act & Rend_Ant_Turno > 0 ~ sprintf("Sin avance previo (Rend S-1: %s kg/ha)", format(round(Rend_Ant_Turno), big.mark = ",")),
    !Tiene_Cosecha_Act                      ~ sprintf("Sin avance previo (Rend proy: %s kg/ha)", format(round(Rend_Proy), big.mark = ",")),
    Tiene_Cosecha_Act & Prop_Avance_Turno > 0.30 ~ sprintf("Avance >30%% (Rend act sem: %s kg/ha)", format(round(Rend_Sem_Actual), big.mark = ",")),
    Tiene_Cosecha_Act & Prop_Avance_Turno <= 0.30 & Rend_Ant_Turno > 0 ~ sprintf("Avance <=30%% (Rend S-1: %s kg/ha)", format(round(Rend_Ant_Turno), big.mark = ",")),
    Tiene_Cosecha_Act & Prop_Avance_Turno <= 0.30                      ~ sprintf("Avance <=30%% (Rend proy: %s kg/ha)", format(round(Rend_Proy), big.mark = ",")),
    TRUE ~ ifelse(Rend_Ant_Turno > 0, sprintf("Rend S-1 (%s kg/ha)", format(round(Rend_Ant_Turno), big.mark = ",")), sprintf("Rend proy (%s kg/ha)", format(round(Rend_Proy), big.mark = ",")))
  )
  
  list(
    Rend_Base = Rend_Base_Turno,
    Rend_Con_Floral = Rend_Base_Con_Floral,
    Motivo = Motivo_Base
  )
}

# Prueba Caso 1 con Rend S-1 existente
res1 <- simular_arbol_rendimiento(Tiene_Cosecha_Act = FALSE, Prop_Avance_Turno = 0, Rend_Ant_Turno = 3500, Rend_Proy = 2000, Factor_Floral = 1.10)
cat("Prueba Caso 1 (Sin avance, con Rend S-1):\n")
cat("  Rend Base:", res1$Rend_Base, "(Esperado: 3500)\n")
cat("  Rend Con Floral:", res1$Rend_Con_Floral, "(Esperado: 3850)\n")
cat("  Motivo:", res1$Motivo, "\n")
stopifnot(res1$Rend_Base == 3500)
stopifnot(res1$Rend_Con_Floral == 3850)
stopifnot(grepl("Rend S-1: 3,500", res1$Motivo))

# Prueba Caso 1 sin Rend S-1 (Fallback a Proy)
res1_fb <- simular_arbol_rendimiento(Tiene_Cosecha_Act = FALSE, Prop_Avance_Turno = 0, Rend_Ant_Turno = 0, Rend_Proy = 2000, Factor_Floral = 1.10)
cat("\nPrueba Caso 1 Fallback (Sin avance, sin Rend S-1):\n")
cat("  Rend Base:", res1_fb$Rend_Base, "(Esperado: 2000)\n")
cat("  Rend Con Floral:", res1_fb$Rend_Con_Floral, "(Esperado: 2200)\n")
cat("  Motivo:", res1_fb$Motivo, "\n")
stopifnot(res1_fb$Rend_Base == 2000)
stopifnot(res1_fb$Rend_Con_Floral == 2200)
stopifnot(grepl("Rend proy: 2,000", res1_fb$Motivo))

# Prueba Caso 2 con Rend S-1 existente (Avance <= 30%)
res2 <- simular_arbol_rendimiento(Tiene_Cosecha_Act = TRUE, Prop_Avance_Turno = 0.20, Rend_Ant_Turno = 4000, Rend_Proy = 2500, Factor_Floral = 0.95)
cat("\nPrueba Caso 2 (Avance <= 30%, con Rend S-1):\n")
cat("  Rend Base:", res2$Rend_Base, "(Esperado: 4000)\n")
cat("  Rend Con Floral:", res2$Rend_Con_Floral, "(Esperado: 3800)\n")
cat("  Motivo:", res2$Motivo, "\n")
stopifnot(res2$Rend_Base == 4000)
stopifnot(res2$Rend_Con_Floral == 3800)
stopifnot(grepl("Avance <=30% \\(Rend S-1: 4,000", res2$Motivo))

# Prueba Caso > 30% (se mantiene Rend Sem Actual = 5000)
res_gt30 <- simular_arbol_rendimiento(Tiene_Cosecha_Act = TRUE, Prop_Avance_Turno = 0.50, Rend_Ant_Turno = 4000, Rend_Proy = 2500, Factor_Floral = 1.0)
cat("\nPrueba Avance > 30% (Rend Sem Actual):\n")
cat("  Rend Base:", res_gt30$Rend_Base, "(Esperado: 5000)\n")
stopifnot(res_gt30$Rend_Base == 5000)

cat("\n=== ALL UNIT TESTS PASSED WITH 100% SUCCESS! ===\n")
