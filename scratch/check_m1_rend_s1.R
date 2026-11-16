library(dplyr)
load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")

cs_w44_m1 <- df_cosecha %>%
  filter(as.character(Campaña) == "2026", as.numeric(Semana) == 44, as.character(Módulo) == "1")

valvs_m1 <- df_valvulas %>%
  filter(as.character(Módulo) == "1") %>%
  group_by(Turno = as.character(Turno)) %>%
  summarise(Area_tot = sum(as.numeric(Área)), .groups="drop")

rend_s1 <- cs_w44_m1 %>%
  group_by(Turno = as.character(Turno)) %>%
  summarise(Kg_Ant = sum(`Kg Total`), .groups="drop") %>%
  left_join(valvs_m1, by = "Turno") %>%
  mutate(Rend_Ant = Kg_Ant / Area_tot)

print(rend_s1)
