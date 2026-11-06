suppressMessages({
  library(dplyr)
})

# Let's check what Dia_7d, Dia_Habitual, Dia_Auto_Valv are for Turno 3 valves in the app
hist_wide <- readRDS("D:/ACP/Rutas de Cosecha/historico_oficial_wide_2026.rds")
valvs <- readRDS("D:/ACP/Rutas de Cosecha/valvulas_normalizadas.rds")
t3_valvs <- valvs %>% filter(Módulo == "2", Turno == "3")

print(t3_valvs %>% select(Módulo, Turno, codigo, Área))

# Check df_frec_valvs_2sem or how Dia_7d was computed
# Let's inspect app.R lines 10140-10190
sem_sel <- 45
sem_ant <- 44
cols_dias <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
cols_kg <- paste0("Kg_", cols_dias)

df_sem_ant <- hist_wide %>% filter(as.numeric(Semana) == sem_ant)
df_sem_ant$Kg_Total_Ant <- rowSums(df_sem_ant[, intersect(cols_kg, names(df_sem_ant))], na.rm = TRUE)

df_7d_map <- df_sem_ant %>%
  filter(Codigo_Valv %in% t3_valvs$codigo) %>%
  select(Codigo_Valv, any_of(cols_kg))
print(df_7d_map)
