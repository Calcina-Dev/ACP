suppressMessages({
  library(dplyr)
  library(lubridate)
})

# Load harvest data
df_cosecha <- readRDS("Actualizacion data maestra/datos_historicos_arandano.rds")

# Orden semana
orden_semana_campana <- function(s) {
  s <- as.numeric(s)
  ifelse(s >= 20, s - 19, s + 33)
}

norm_codigo_valvula <- function(x) {
  toupper(gsub("[^A-Za-z0-9]", "", as.character(x)))
}

# Run calcular_frecuencias_valvula_2semanas
sem_actual <- 45
campana_sel <- 2026

sem_act <- as.numeric(sem_actual)
ord_sel <- orden_semana_campana(sem_act)
camp_chr <- as.character(campana_sel)
camp_num <- suppressWarnings(as.numeric(camp_chr))

df_c_camp <- df_cosecha %>%
  filter(
    as.character(Campaña) == camp_chr | (!is.na(camp_num) & Campaña == camp_num),
    coalesce(Tipo_Cosecha, "Comercial") == "Comercial",
    `Kg Total` > 0
  )

# Check what S-1 and S-2 are
df_c_camp <- df_c_camp %>%
  mutate(
    Sem_num = suppressWarnings(as.numeric(Semana)),
    Ord_sem = orden_semana_campana(Sem_num)
  )

cat("Distinct weeks in df_c_camp:\n")
print(sort(unique(df_c_camp$Sem_num)))

# Let's inspect Turno 3 in df_c_camp
t3_c <- df_c_camp %>% filter(Módulo %in% c("2", "02"), Turno %in% c("3", "03"))
cat("Turno 3 harvests:\n")
print(t3_c %>% select(Semana, Fecha, Válvula, `Kg Total`))
