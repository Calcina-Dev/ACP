library(dplyr)
e <- new.env()
load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData", envir = e)

cat("=== Cosecha 2026 for Modulo 12 ===\n")
c26 <- e$df_cosecha %>% filter(as.character(Campaña) == "2026", grepl("^12$", trimws(as.character(Módulo))))
print(c26 %>% distinct(Campaña, Sector, Módulo))

cat("\n=== Cosecha ALL years for Modulo 12 ===\n")
print(e$df_cosecha %>% filter(grepl("^12$", trimws(as.character(Módulo)))) %>% distinct(Campaña, Sector, Módulo))

# Check Rutas de cosecha
rc_files <- list.files("D:/ACP/Rutas de Cosecha", pattern = "\\.xlsx$|\\.rds$", full.names = TRUE)
cat("\n=== Rutas de Cosecha files ===\n")
for (rcf in rc_files[1:min(5, length(rc_files))]) {
  cat(rcf, "\n")
  if (grepl("\\.rds$", rcf)) {
    rc_d <- readRDS(rcf)
    if ("codigo" %in% names(rc_d)) {
      m12 <- rc_d %>% filter(grepl("M12", codigo))
      cat("  M12 in rds: ", nrow(m12), "rows. Sectors:", paste(unique(m12$Sector), collapse=", "), "\n")
    }
  }
}
