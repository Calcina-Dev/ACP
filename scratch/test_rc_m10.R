suppressMessages({
  library(dplyr)
  library(lubridate)
})

# Cargar app helper environment
ruta_raiz_arandano <- "G:/Unidades compartidas/Compartido Proyecciones/3.- Arándano"
load(file.path(ruta_raiz_arandano, "datos_maestros_cosecha_2026.RData"))

rc_file <- if (file.exists(file.path(ruta_raiz_arandano, "rutas_cosecha_mapa.rds"))) {
  readRDS(file.path(ruta_raiz_arandano, "rutas_cosecha_mapa.rds"))
} else if (file.exists("D:/ACP/Rutas de Cosecha/rutas_cosecha_mapa.rds")) {
  readRDS("D:/ACP/Rutas de Cosecha/rutas_cosecha_mapa.rds")
} else NULL

cat("Rutas cargadas:", if (!is.null(rc_file)) nrow(rc_file) else 0, "filas\n")

if (!is.null(rc_file)) {
  rc_m10 <- rc_file %>% 
    filter(as.numeric(Semana) == 46, as.character(Módulo) == "10", as.character(Turno) == "18")
  cat("Rutas de Campo S46 M10-T18:\n")
  print(rc_m10 %>% select(any_of(c("Codigo_Valvula", "Ha_Lunes", "Ha_Martes", "Ha_Miércoles", "Ha_Jueves", "Ha_Viernes", "Ha_Sábado", "Kg_Total_Prog"))))
}
