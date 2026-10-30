load("G:/Unidades compartidas/Compartido Proyecciones/3.- Ar\u00e1ndano/datos_maestros_cosecha_2026.RData")
suppressPackageStartupMessages(library(dplyr))

# Rendimiento Historico Semana 43
# T7: V25(1.65), V26(1.60), V27(1.13 de 1.62). Total ha cosechada S43 = 4.38 ha. Total Kg = 4649.485 kg.
# Rend S43 T7 = 4649.485 / 4.38 = 1061.53 kg/ha.

# T8: V29(0.93), V30(1.50), V31(1.54). Total ha cosechada S43 = 3.97 ha. Total Kg = 3732.216 kg.
# Rend S43 T8 = 3732.216 / 3.97 = 940.10 kg/ha.

# Semana 44:
# Lunes Real cosechado (Remanente S43 cerrado el Lunes):
# T7: V28 (1.60 ha) + V27 remanente (0.34 ha) = 1.94 ha -> 2059.16 kg (1061.42 kg/ha)
# T8: V32 (1.55 ha) = 1.55 ha -> 1673.23 kg (1079.50 kg/ha)

cat("=== TURNO 7 (MAGICA, Area Total = 6.47 ha) ===\n")
cat("Lunes Real (Remanente): 2,059.16 kg (1.94 ha: V28 1.60 ha + V27 0.34 ha)\n")
cat("Valvulas intactas cosechadas Sabado S43: V25 (1.65 ha) y V26 (1.60 ha) = 3.25 ha\n")
cat(" -> Programadas para SABADO (ciclo 7d completo):\n")
cat("    Area = 3.25 ha * Rend_S43 (1061.53 kg/ha) =", 3.25 * 1061.53, "kg\n")
cat("Valvulas cerradas Lunes que tienen 2da pasada Viernes (4 dias de crecimiento / 7d = 57.14%):\n")
cat("    Area = 1.94 ha * (Rend_S43 * 4/7) =", 1.94 * (1061.53 * 4/7), "kg\n")
cat("Total Semana T7 estimado =", 2059.16 + (3.25 * 1061.53) + (1.94 * (1061.53 * 4/7)), "kg\n")
cat("Proyeccion Oficial T7 = 7,582.94 kg\n\n")

cat("=== TURNO 8 (MAGICA, Area Total = 5.52 ha) ===\n")
cat("Lunes Real (Remanente): 1,673.23 kg (1.55 ha: V32)\n")
cat("Valvulas intactas cosechadas Sabado S43: V29(0.93) + V30(1.50) + V31(1.54) = 3.97 ha\n")
cat(" -> Programadas para SABADO (ciclo 7d completo):\n")
cat("    Area = 3.97 ha * Rend_S43 (940.10 kg/ha) =", 3.97 * 940.10, "kg\n")
cat("Valvula cerrada Lunes que tiene 2da pasada Viernes (4 dias de crecimiento / 7d = 57.14%):\n")
cat("    Area = 1.55 ha * (Rend_S43 * 4/7) =", 1.55 * (940.10 * 4/7), "kg\n")
cat("Total Semana T8 estimado =", 1673.23 + (3.97 * 940.10) + (1.55 * (940.10 * 4/7)), "kg\n")
cat("Proyeccion Oficial T8 = 5,898.87 kg\n")
