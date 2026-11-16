suppressMessages({
  library(readxl)
  library(dplyr)
})

base_dir <- "G:/Unidades compartidas/Compartido Proyecciones/3.- Arándano/Campaña 26-27/Proyección Semanal"
f_det_v2  <- file.path(base_dir, "Proyección Semanal Detallada", "26. Proyecciones Semana 46 + 5 semanas ACP (Conteo S44)_V2.xlsx")

calc_all <- read_excel(f_det_v2, sheet = "Calculo", skip = 3)
names(calc_all)[12] <- "Proy_Kg_Col12"
names(calc_all)[98] <- "Kg_Sem_2"

# Filter rows where M == 3, 4, 5, 12, 14, 2
sub_calc <- calc_all %>%
  filter(M %in% c("2", "3", "4", "5", "12", "14")) %>%
  group_by(M, Variedad) %>%
  summarise(
    Sum_Col12 = sum(suppressWarnings(as.numeric(Proy_Kg_Col12)), na.rm = TRUE),
    Sum_Kg_Sem2 = sum(suppressWarnings(as.numeric(Kg_Sem_2)), na.rm = TRUE),
    .groups = "drop"
  )

print(sub_calc)
