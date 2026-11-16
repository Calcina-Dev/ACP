suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

limpiar_variedad <- function(x) {
  if (is.null(x) || length(x) == 0) return(character(0))
  x_str <- toupper(trimws(as.character(x)))
  v <- as.character(chartr("ÁÉÍÓÚÜ", "AEIOUU", x_str))
  as.character(ifelse(grepl("EB\\s*9\\s*[-_.]\\s*2", v, ignore.case = TRUE), "MAGICA", v))
}

ed <- readRDS("D:/ACP/Actualizacion Semanal/ediciones_manuales_cosecha.rds")

# Correct the bad records
ed_fixed <- ed %>%
  mutate(
    is_not_rocio = (limpiar_variedad(Variedad) != "ROCIO"),
    Ha_Asignada_1ra = ifelse(is_not_rocio, coalesce(Ha_Asignada, 0), Ha_Asignada_1ra),
    Kg_Asignado_1ra = ifelse(is_not_rocio, coalesce(Valor, 0), Kg_Asignado_1ra),
    Ha_Asignada_2da = ifelse(is_not_rocio, 0, Ha_Asignada_2da),
    Kg_Asignado_2da = ifelse(is_not_rocio, 0, Kg_Asignado_2da)
  ) %>%
  select(-is_not_rocio)

cat("Fixed rows in ed_fixed for M2 T4:\n")
print(ed_fixed %>% filter(Semana == 46, Módulo == "2", Turno == "4"))
