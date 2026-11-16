load("Actualizacion Semanal RNA/datos_maestros_cosecha_2026.RData")
library(dplyr)

ruta_rc <- "d:/ACP/Rutas de Cosecha/rutas_campo_oficial_2026.rds"
if (file.exists(ruta_rc)) {
  rc <- readRDS(ruta_rc)
  cat("rc class:", class(rc), "names:\n")
  print(head(names(rc), 20))
  if (is.list(rc) && !is.data.frame(rc)) {
    cat("rc is a list of weeks!\n")
    print(names(rc))
    if ("45" %in% names(rc)) {
      rc45 <- rc[["45"]]
      print(names(rc45))
      sub45 <- rc45 %>% filter(grepl("13", Módulo), Turno == "2")
      print(sub45)
    }
  }
}
