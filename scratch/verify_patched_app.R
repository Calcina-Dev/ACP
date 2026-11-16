setwd("D:/ACP/Actualizacion Semanal")
load("datos_maestros_cosecha_2026.RData")
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
})

source_lines <- readLines("app.R", encoding = "UTF-8", warn = FALSE)
f_start <- grep("^\\s*obtener_cosecha_conciliada_semana <- function", source_lines)[1]
f_end <- which(grepl("^\\s*obtener_datos_mapa_semana <- function", source_lines))[1] - 1

f_text <- paste(source_lines[f_start:f_end], collapse = "\n")

ruta_raiz_arandano <- "D:/ACP/Rutas de Cosecha"
cargar_rutas_cosecha <- function() {
  readRDS("D:/ACP/Rutas de Cosecha/rutas_cosecha_mapa.rds")
}
obtener_rutas_campo_en_vivo <- function(sem) NULL

eval(parse(text = paste(source_lines[29:42], collapse = "\n"))) # normalizar_texto, limpiar_variedad
eval(parse(text = paste(source_lines[100:102], collapse = "\n"))) # norm_codigo_valvula

eval(parse(text = f_text))

cat("Testing patched obtener_cosecha_conciliada_semana(45)...\n")
res <- obtener_cosecha_conciliada_semana(45, "2026")
cat("Rows returned:", nrow(res), "\n")
cat("\nReconciliation Breakdown:\n")
print(table(res$Estado_Concil))

cat("\nKPI Sin Ruta:\n")
sub_sr <- res %>% filter(Estado_Concil == "Kilos SAP sin Ruta")
cat("Valves count:", nrow(sub_sr), "\n")
cat("Kg_Sin_Ruta sum:", sum(sub_sr$Kg_Sin_Ruta), "\n")

cat("\nM6-T1-V1 detail:\n")
v1 <- res %>% filter(codigo_norm == "M6-T1-V1")
print(v1 %>% select(codigo_norm, Estado_Concil, Kg_Miercoles_rec, Kg_Viernes_rec, Kg_Total_rec, Kg_Sin_Ruta, Ha_Viernes_rc, Ha_Total_rc, Es_Valido_Operativo, Diagnostico))
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
