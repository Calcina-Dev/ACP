library(dplyr)
rc <- readRDS("D:/ACP/Rutas de Cosecha/rutas_cosecha_mapa.rds")
m1 <- rc %>% filter(as.numeric(Semana) == 46, as.character(Módulo) %in% c("1", "01"))

cols_ha <- c("Ha_Lunes", "Ha_Martes", "Ha_Miércoles", "Ha_Jueves", "Ha_Viernes", "Ha_Sábado")
m1$Ha_Semana <- rowSums(m1[, cols_ha], na.rm = TRUE)

res <- m1 %>%
  group_by(Sector, Módulo, Turno, Variedad, Num_Pasada) %>%
  summarise(
    Ha_Total_Semana = sum(Ha_Semana, na.rm = TRUE),
    .groups = "drop"
  )
print(res)
