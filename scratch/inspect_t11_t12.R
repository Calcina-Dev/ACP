suppressPackageStartupMessages({
  library(dplyr)
})

# Cargar archivos relevantes
rdata_files <- list.files("Rutas de Cosecha", pattern = "\\.rdata$", full.names = TRUE, ignore.case = TRUE)
cat("RData files:", paste(rdata_files, collapse=", "), "\n")

# Cargar rdata
load(rdata_files[1])

cat("\n--- TURNO 11 (Sector 8, Módulo 5, Turno 11, MAGICA) ---\n")
if (exists("df_valvulas")) {
  v11 <- df_valvulas %>% filter(as.character(Módulo) == "5", as.character(Turno) == "11")
  cat("Valvulas del Turno 11:\n")
  print(v11 %>% select(any_of(c("Sector", "Módulo", "Turno", "codigo", "Área", "Area", "Variedad"))))
}

if (exists("df_cosecha")) {
  cat("\nCosecha S44 Turno 11:\n")
  print(df_cosecha %>% filter(Semana == 44, as.character(Módulo) == "5", as.character(Turno) == "11") %>% 
    select(any_of(c("Semana", "Dia", "Módulo", "Turno", "codigo", "Kg_Total", "Ha_Total"))))
  
  cat("\nCosecha S45 Turno 11:\n")
  print(df_cosecha %>% filter(Semana == 45, as.character(Módulo) == "5", as.character(Turno) == "11") %>% 
    select(any_of(c("Semana", "Dia", "Módulo", "Turno", "codigo", "Kg_Total", "Ha_Total"))))
}

cat("\n--- TURNO 12 (Sector 8, Módulo 5, Turno 12, MAGICA) ---\n")
if (exists("df_cosecha")) {
  cat("\nCosecha S44 Turno 12:\n")
  print(df_cosecha %>% filter(Semana == 44, as.character(Módulo) == "5", as.character(Turno) == "12") %>% 
    select(any_of(c("Semana", "Dia", "Módulo", "Turno", "codigo", "Kg_Total", "Ha_Total"))))
  
  cat("\nCosecha S45 Turno 12:\n")
  print(df_cosecha %>% filter(Semana == 45, as.character(Módulo) == "5", as.character(Turno) == "12") %>% 
    select(any_of(c("Semana", "Dia", "Módulo", "Turno", "codigo", "Kg_Total", "Ha_Total"))))
}
