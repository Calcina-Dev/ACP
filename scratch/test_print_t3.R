rc_map <- readRDS("D:/ACP/Rutas de Cosecha/rutas_cosecha_mapa.rds")
hist_wide <- readRDS("D:/ACP/Rutas de Cosecha/historico_oficial_wide_2026.rds")

cat("=== RC_MAP ===\n")
t3 <- rc_map[rc_map$Semana==45 & rc_map$Módulo=="2" & rc_map$Turno=="3", ]
print(as.data.frame(t3[, c("codigo", "Ha_Miércoles", "Kg_Miércoles", "Ha_Jueves", "Kg_Jueves", "Ha_Viernes", "Kg_Viernes")]))

cat("=== HIST_WIDE ===\n")
h3 <- hist_wide[hist_wide$Semana==45 & hist_wide$Codigo_Valv %in% t3$codigo, ]
print(as.data.frame(h3[, c("Codigo_Valv", "Ha_Miércoles", "Kg_Miércoles", "Ha_Jueves", "Kg_Jueves", "Ha_Viernes", "Kg_Viernes")]))
