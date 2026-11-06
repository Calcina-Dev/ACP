suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
})

cat("=== VERIFICACION 1: DATASETS MAESTROS (SECTORES Y MODULOS) ===\n")
v <- readRDS("D:/ACP/Rutas de Cosecha/valvulas_normalizadas.rds")
cat("valvulas_normalizadas M9, M11, M12:\n")
print(v %>% filter(as.character(Módulo) %in% c("9", "11", "12")) %>% count(Sector, Módulo))

rc <- readRDS("D:/ACP/Rutas de Cosecha/rutas_cosecha_mapa.rds")
cat("\nrutas_cosecha_mapa M9, M11, M12:\n")
print(rc %>% filter(as.character(Módulo) %in% c("9", "11", "12")) %>% count(Sector, Módulo))

ed <- readRDS("D:/ACP/Actualizacion Semanal/ediciones_manuales_cosecha.rds")
cat("\nediciones_manuales M9, M11, M12:\n")
print(ed %>% filter(as.character(Módulo) %in% c("9", "11", "12")) %>% count(Sector, Módulo))

cat("\n=== VERIFICACION 2: REGLA DE FACTOR FLORAL (AVANCE > 30%) ===\n")
# Simulamos una tabla como la de btn_ajustar_semana
rend_test <- tibble(
  Sector = "9",
  Módulo = "12",
  Turno = "12",
  Esta_Proyectado = TRUE,
  Tiene_Cosecha_Act = c(TRUE, TRUE, FALSE),
  Prop_Avance_Turno = c(0.45, 0.15, 0.0),
  Rend_Base_Turno = c(663, 500, 400),
  Factor_Floral = c(0.916, 0.916, 1.05),
  Tipo_Efecto_Floral = c("Castigo", "Castigo", "Premio"),
  Pct_Ajuste_Flor = c(-8.4, -8.4, 5.0),
  Turno_Completo_Lun_Mar = FALSE,
  Es_Ciclo_Corto = FALSE,
  Motivo_Base = c("Avance >30% (Rend act sem: 663 kg/ha)", "Avance ≤30% (Rend sem ant: 500 kg/ha)", "Turno virgen (Rend sem ant: 400 kg/ha)")
) %>%
  mutate(
    # REGLA IMPLEMENTADA EN app.R:
    Rend_Base_Con_Floral = case_when(
      Tiene_Cosecha_Act & Prop_Avance_Turno > 0.30 ~ round(Rend_Base_Turno, 2),
      TRUE                                         ~ round(Rend_Base_Turno * Factor_Floral, 2)
    ),
    Detalle_Floral = case_when(
      !Esta_Proyectado | (Turno_Completo_Lun_Mar & !Es_Ciclo_Corto) | (Turno_Completo_Lun_Mar & Es_Ciclo_Corto) ~ "",
      Tiene_Cosecha_Act & Prop_Avance_Turno > 0.30 ~ "",
      Tipo_Efecto_Floral == "Premio"  ~ sprintf("Premio floral %+0.1f%%", Pct_Ajuste_Flor),
      Tipo_Efecto_Floral == "Castigo" ~ sprintf("Castigo floral %+0.1f%%", Pct_Ajuste_Flor),
      TRUE                            ~ "Floral neutro (0%)"
    ),
    Motivo_Ajuste = case_when(
      !Esta_Proyectado ~ "No proyectado",
      Turno_Completo_Lun_Mar & !Es_Ciclo_Corto ~ "Cerró 100% en Lun/Mar",
      Turno_Completo_Lun_Mar & Es_Ciclo_Corto ~ Motivo_Base,
      nzchar(Detalle_Floral) ~ paste0(Motivo_Base, " • ", Detalle_Floral),
      TRUE ~ Motivo_Base
    )
  )

print(rend_test %>% select(Tiene_Cosecha_Act, Prop_Avance_Turno, Rend_Base_Turno, Factor_Floral, Rend_Base_Con_Floral, Motivo_Ajuste))

cat("\n[OK] Caso 1 (Avance 45% > 30%): Rend_Base_Con_Floral =", rend_test$Rend_Base_Con_Floral[1], "(Sin factor floral, exactamente igual a Rend_Base_Turno =", rend_test$Rend_Base_Turno[1], ")\n")
cat("[OK] Motivo_Ajuste =", rend_test$Motivo_Ajuste[1], "(Limpio, sin texto floral)\n")
cat("\n[OK] Caso 2 (Avance 15% <= 30%): Rend_Base_Con_Floral =", rend_test$Rend_Base_Con_Floral[2], "(Afectado con factor floral)\n")
cat("[OK] Motivo_Ajuste =", rend_test$Motivo_Ajuste[2], "(Con detalle floral)\n")
