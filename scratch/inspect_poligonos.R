v <- readRDS("D:/ACP/Rutas de Cosecha/catalogo_poligonos_valvulas.rds")
cat("Names of catalogo_poligonos_valvulas.rds:\n")
print(names(v))
cat("\nClass:\n")
print(class(v))
if (is.data.frame(v)) {
  library(dplyr)
  if ("Módulo" %in% names(v) && "Sector" %in% names(v)) {
    print(v %>% filter(as.character(Módulo) == "12") %>% head(5))
  }
}
