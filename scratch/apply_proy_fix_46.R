suppressMessages({
  library(dplyr)
})

rdata_local <- "Actualizacion Semanal/datos_maestros_cosecha_2026.RData"
rdata_drive <- "G:/Unidades compartidas/Compartido Proyecciones/3.- Arándano/datos_maestros_cosecha_2026.RData"

# Load current RData
e <- new.env()
load(rdata_local, envir = e)

cat("Loaded objects:", ls(e), "\n")
cat("Original S46 sum:", sum((e$df_proyecciones %>% filter(Semana == 46))$Proyección, na.rm = TRUE), "\n")

# Target official module-variety totals for Semana 46 (from Formalizada V2 / Resumen Proyección)
ajustes_mod_var <- tibble(
  Módulo   = c("2", "3", "4", "5", "12", "14", "12"),
  Variedad = c("SEKOYA POP", "SEKOYA POP", "MAGICA", "MAGICA", "ROCIO", "ROCIO", "MADEIRA"),
  Kg_Delta = c(2000, 4000, 4000, 2300, 2000, 3000, 5000)
)

df_p_fixed <- e$df_proyecciones %>%
  left_join(ajustes_mod_var, by = c("Módulo", "Variedad")) %>%
  group_by(Campaña, Semana, Módulo, Variedad) %>%
  mutate(
    Base_Mod_Var = sum(Proyección, na.rm = TRUE),
    factor_aj = if (first(Semana) == 46 && !is.na(first(Kg_Delta)) && first(Base_Mod_Var) > 0) {
      (1 + first(Kg_Delta) / first(Base_Mod_Var))
    } else {
      1
    },
    Proyección = round(Proyección * factor_aj, 2),
    Kg_Proy_Turno = Proyección
  ) %>%
  ungroup() %>%
  select(-Kg_Delta, -Base_Mod_Var, -factor_aj)

s46_fixed_sum <- sum((df_p_fixed %>% filter(Semana == 46))$Proyección, na.rm = TRUE)
cat("Updated S46 sum:", s46_fixed_sum, "\n")
stopifnot(abs(s46_fixed_sum - 405865.2) < 0.1)

# Assign to environment
e$df_proyecciones <- df_p_fixed

# Update manifest if present
if ("df_manifest_proy" %in% ls(e)) {
  e$df_manifest_proy <- e$df_manifest_proy %>%
    mutate(
      Total_Kg_Proy = ifelse(semana == 46, round(s46_fixed_sum), Total_Kg_Proy)
    )
}

# Update metadata if present
if ("metadata_actualizacion" %in% ls(e)) {
  e$metadata_actualizacion$Total_Kilos_Proyeccion <- sum(e$df_proyecciones$Proyección, na.rm = TRUE)
  if (!is.null(e$metadata_actualizacion$Matriz_Control_Proyecciones)) {
    e$metadata_actualizacion$Matriz_Control_Proyecciones <- e$df_manifest_proy
  }
}

# Save locally
save(
  list = ls(e),
  envir = e,
  file = rdata_local
)
cat("Saved to local:", rdata_local, "\n")

# Save to drive if exists
if (file.exists(rdata_drive)) {
  save(
    list = ls(e),
    envir = e,
    file = rdata_drive
  )
  cat("Saved to Google Drive:", rdata_drive, "\n")
}

# Verification
e_check <- new.env()
load(rdata_local, envir = e_check)
cat("Verification S46 sum after reload:", sum((e_check$df_proyecciones %>% filter(Semana == 46))$Proyección, na.rm = TRUE), "\n")
