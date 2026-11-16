suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

# Load the data files
rutas_mapa <- readRDS("D:/ACP/Rutas de Cosecha/rutas_cosecha_mapa.rds")
valvs <- readRDS("D:/ACP/Rutas de Cosecha/valvulas_normalizadas.rds")
hist_wide <- readRDS("D:/ACP/Rutas de Cosecha/historico_oficial_wide_2026.rds")
cosecha_file <- "D:/ACP/Data/cosecha_actual.rds"
if (!file.exists(cosecha_file)) {
  cosecha_file <- list.files("D:/ACP", pattern = "cosecha.*\\.rds$", full.names = TRUE, recursive = TRUE)[1]
}
cat("Cosecha file:", cosecha_file, "\n")
df_cosecha <- readRDS(cosecha_file)

cat("Cosecha columns:", paste(head(names(df_cosecha), 10), collapse=", "), "\n")

# Check for Semana 44, 45, 46
# In screenshot: What week?
# Let's check max week in df_cosecha
cat("Max semana in df_cosecha:", max(df_cosecha$Semana, na.rm = TRUE), "\n")
sem <- max(df_cosecha$Semana, na.rm = TRUE)

cat("\n--- Check Cosecha M10 S5 Sem", sem, "---\n")
cos_m10 <- df_cosecha %>%
  filter(Semana == sem, grepl("10", as.character(Módulo)))
print(head(cos_m10 %>% select(Semana, Módulo, Turno, Variedad, Fecha, any_of(c("Kg Total", "Kg_Total", "Kilos", "Superficie", "Area")))))

cat("\n--- Check Rutas Mapa Sem", sem, "---\n")
if (!is.null(rutas_mapa)) {
  rc_m10 <- rutas_mapa %>%
    filter(Semana == sem, grepl("10", as.character(Módulo)))
  print(head(rc_m10))
}

cat("\n--- Check Hist Wide Sem", sem, "---\n")
if (!is.null(hist_wide)) {
  hw_m10 <- hist_wide %>%
    filter(Semana == sem, grepl("10", as.character(Modulo)))
  print(head(hw_m10))
}
