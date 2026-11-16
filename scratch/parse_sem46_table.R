suppressMessages({
  library(readxl)
  library(dplyr)
})

base_dir <- "G:/Unidades compartidas/Compartido Proyecciones/3.- Arándano/Campaña 26-27/Proyección Semanal"
f_form_v2 <- file.path(base_dir, "Proyección Semanal Formalizada", "26.- Proyección Cosecha Arándano ACP _ 2026 _ Sem 46 al 51_V2.xlsx")

sem46 <- read_excel(f_form_v2, sheet = "SEM 46", skip = 5)
names(sem46)[1:15] <- c("Ubicacion", "Modulo", "Tipo", "Variedad", "Vivero", "Dias", "Lunes", "Martes", "Miercoles", "Jueves", "Viernes", "Sabado", "Domingo", "Codigo", "Total")

sem46_filas <- sem46 %>%
  filter(!is.na(Modulo), !grepl("SUB TOTAL|TOTAL", Modulo, ignore.case = TRUE), !grepl("FRECUENCIAS", Modulo, ignore.case = TRUE)) %>%
  select(Modulo, Tipo, Variedad, Total) %>%
  mutate(Total_num = suppressWarnings(as.numeric(Total))) %>%
  filter(!is.na(Total_num))

cat("Rows in SEM 46 Formalizada:\n")
print(sem46_filas, n = 40)
cat("Sum Total_num:", sum(sem46_filas$Total_num), "\n")
