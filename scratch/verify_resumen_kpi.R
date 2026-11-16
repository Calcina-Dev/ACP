suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

# Simulate ui_kpi_real_detalle
df_concil <- res
kg_sin_ruta_excl <- if ("Kg_Sin_Ruta" %in% names(df_concil)) {
  sum(coalesce(df_concil$Kg_Sin_Ruta, 0), na.rm = TRUE)
} else {
  0
}
cat("ui_kpi_real_detalle badge Sin Ruta:", round(kg_sin_ruta_excl, 0), "kg\n")

# Simulate datos_resumen_base
sr_mask <- coalesce(df_concil$Kg_Sin_Ruta, 0) > 0 |
           (!coalesce(df_concil$Tiene_Campo, FALSE) & coalesce(df_concil$Tiene_SAP, FALSE)) |
           coalesce(df_concil$Estado_Concil, "") == "Kilos SAP sin Ruta"

df_sr_sub <- df_concil[sr_mask, ]
cat("df_sr_sub rows:", nrow(df_sr_sub), "\n")

df_sr_resumen <- df_sr_sub %>%
  mutate(
    Lunes      = coalesce(as.numeric(Kg_Lunes_sr), 0),
    Martes     = coalesce(as.numeric(Kg_Martes_sr), 0),
    Miércoles  = coalesce(as.numeric(Kg_Miercoles_sr), 0),
    Jueves     = coalesce(as.numeric(Kg_Jueves_sr), 0),
    Viernes    = coalesce(as.numeric(Kg_Viernes_sr), 0),
    Sábado     = coalesce(as.numeric(Kg_Sabado_sr), 0),
    Ajuste     = Lunes + Martes + Miércoles + Jueves + Viernes + Sábado
  )

cat("Resumen Sin Ruta total kg:", sum(df_sr_resumen$Ajuste), "\n")
cat("Days breakdown in Resumen:\n")
cat("Lunes:", sum(df_sr_resumen$Lunes), "\n")
cat("Martes:", sum(df_sr_resumen$Martes), "\n")
cat("Miércoles:", sum(df_sr_resumen$Miércoles), "\n")
cat("Jueves:", sum(df_sr_resumen$Jueves), "\n")
cat("Viernes:", sum(df_sr_resumen$Viernes), "\n")
cat("Sábado:", sum(df_sr_resumen$Sábado), "\n")

# Check M6-T1 in df_sr_resumen
m6_sr <- df_sr_resumen %>% filter(codigo_norm == "M6-T1-V1")
cat("\nM6-T1-V1 in Resumen:\n")
print(m6_sr %>% select(codigo_norm, Lunes, Martes, Miércoles, Jueves, Viernes, Sábado, Ajuste))
