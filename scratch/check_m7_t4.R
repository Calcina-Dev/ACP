options(warn = 1)
suppressPackageStartupMessages(library(dplyr))
tryCatch({
  load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
  cat("RData loaded. Objects:\n")
  print(ls())
  
  if (exists("df_cosecha")) {
    cat("df_cosecha exists, nrow:", nrow(df_cosecha), "\n")
    df_m7t4 <- df_cosecha %>%
      filter(trimws(as.character(Módulo)) == "7", trimws(as.character(Turno)) == "4")
    cat("df_m7t4 rows:", nrow(df_m7t4), "\n")
    
    # summarize by semana
    s_m7t4 <- df_m7t4 %>%
      group_by(Campaña, Semana, Variedad) %>%
      summarise(
        Kg = sum(`Kg Total`, na.rm = TRUE),
        Area = first(na.omit(Área)),
        Rend = Kg / Area,
        .groups = "drop"
      ) %>%
      arrange(Campaña, suppressWarnings(as.numeric(Semana)))
    print(as.data.frame(s_m7t4))
  }
  
  # Also check historico_conteo
  if (file.exists("D:/ACP/Actualizacion Semanal/historico_conteo.rds")) {
    df_conteo <- readRDS("D:/ACP/Actualizacion Semanal/historico_conteo.rds")
    c_m7t4 <- df_conteo %>%
      filter(trimws(as.character(Módulo)) == "7", trimws(as.character(Turno)) == "4")
    cat("df_conteo m7t4 rows:", nrow(c_m7t4), "\n")
    print(head(as.data.frame(c_m7t4), 10))
  }
  
}, error = function(e) {
  cat("ERROR:", conditionMessage(e), "\n")
  traceback()
})
