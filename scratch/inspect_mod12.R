library(dplyr)

# Load RData
rdata_files <- list.files("D:/ACP", pattern = "\\.RData$", recursive = TRUE, full.names = TRUE)
cat("Found RData files:\n")
for (rf in rdata_files) cat(" ", rf, "\n")

e <- new.env()
if (file.exists("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")) {
  load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData", envir = e)
} else if (file.exists("D:/ACP/datos_maestros_cosecha_2026.RData")) {
  load("D:/ACP/datos_maestros_cosecha_2026.RData", envir = e)
}

cat("Objects in RData:", ls(e), "\n")

if ("df_valvulas" %in% ls(e)) {
  v12 <- e$df_valvulas %>% filter(grepl("12", as.character(Módulo)))
  cat("\ndf_valvulas for Módulo 12:\n")
  print(v12 %>% select(any_of(c("Sector", "Módulo", "Turno", "Variedad", "Área", "codigo"))) %>% head(10))
  cat("Distinct Sector, Módulo in df_valvulas:\n")
  print(e$df_valvulas %>% filter(grepl("^12$", trimws(as.character(Módulo)))) %>% distinct(Sector, Módulo, Turno, Variedad))
}

if ("areas_base" %in% ls(e)) {
  cat("\nareas_base for Módulo 12:\n")
  print(e$areas_base %>% filter(grepl("^12$", trimws(as.character(Módulo)))) %>% distinct(Sector, Módulo, Turno, Variedad, Campaña))
}

# Check ediciones_manuales
rds_ed <- list.files("D:/ACP", pattern = "ediciones_manuales\\.rds$", recursive = TRUE, full.names = TRUE)
for (r in rds_ed) {
  cat("\nChecking", r, ":\n")
  df_ed <- readRDS(r)
  m12_ed <- df_ed %>% filter(grepl("^12$", trimws(as.character(Módulo))))
  if (nrow(m12_ed) > 0) {
    print(m12_ed %>% select(any_of(c("Campaña", "Semana", "Sector", "Módulo", "Turno", "Variedad", "Campo", "Valor", "Tipo_Edicion"))))
  }
}

# Check rv_ajuste_valvulas
rds_aj <- list.files("D:/ACP", pattern = "ajuste_valvulas.*\\.rds$", recursive = TRUE, full.names = TRUE)
for (r in rds_aj) {
  cat("\nChecking", r, ":\n")
  df_aj <- readRDS(r)
  m12_aj <- df_aj %>% filter(grepl("^12$", trimws(as.character(Módulo))))
  if (nrow(m12_aj) > 0) {
    print(m12_aj %>% select(any_of(c("Campaña", "Semana", "Sector", "Módulo", "Turno", "Variedad", "Dia_Asignado", "Kg_Asignado", "Ha_Asignada", "Tipo_Asignacion"))))
  }
}
