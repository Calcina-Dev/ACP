suppressMessages({
  library(readxl)
  library(dplyr)
})

base_dir <- "G:/Unidades compartidas/Compartido Proyecciones/3.- Arándano/Campaña 26-27/Proyección Semanal"
f_form_v2 <- file.path(base_dir, "Proyección Semanal Formalizada", "26.- Proyección Cosecha Arándano ACP _ 2026 _ Sem 46 al 51_V2.xlsx")
f_det_v2  <- file.path(base_dir, "Proyección Semanal Detallada", "26. Proyecciones Semana 46 + 5 semanas ACP (Conteo S44)_V2.xlsx")

# 1. Sheet "SEM 46" in Formalizada V2
sem46 <- read_excel(f_form_v2, sheet = "SEM 46", skip = 5)
names(sem46)[1:15] <- c("Ubicacion", "Modulo", "Tipo", "Variedad", "Vivero", "Dias", "Lunes", "Martes", "Miercoles", "Jueves", "Viernes", "Sabado", "Domingo", "Codigo", "Total")
sem46_filas <- sem46 %>%
  filter(!is.na(Modulo), !grepl("SUB TOTAL|TOTAL", Modulo, ignore.case = TRUE), !grepl("FRECUENCIAS|UBICACIÓN|MÓDULO", Modulo, ignore.case = TRUE)) %>%
  select(Modulo, Tipo, Variedad, Total) %>%
  mutate(
    Modulo_clean = sub("\\.0$", "", sprintf("%.1f", round(suppressWarnings(as.numeric(Modulo)), 1))),
    Modulo_clean = ifelse(is.na(Modulo_clean), trimws(Modulo), Modulo_clean),
    Var_clean = toupper(trimws(Variedad)),
    Total_num = suppressWarnings(as.numeric(Total))
  ) %>%
  filter(!is.na(Total_num))

# 2. RData df_proyecciones
e <- new.env()
load("Actualizacion Semanal/datos_maestros_cosecha_2026.RData", envir = e)
p46_rdata <- e$df_proyecciones %>%
  filter(Semana == 46) %>%
  mutate(
    Modulo_clean = sub("\\.0$", "", sprintf("%.1f", round(suppressWarnings(as.numeric(Modulo_Raw)), 1))),
    Modulo_clean = ifelse(is.na(Modulo_clean), trimws(Modulo_Raw), Modulo_clean),
    Var_clean = toupper(trimws(Variedad))
  )

p46_rdata_mod_var <- p46_rdata %>%
  group_by(Módulo, Var_clean) %>%
  summarise(Kg_RData = sum(Proyección, na.rm = TRUE), .groups = "drop")

sem46_mod_var <- sem46_filas %>%
  mutate(Módulo = as.character(suppressWarnings(as.numeric(sub("\\..*", "", Modulo_clean))))) %>%
  group_by(Módulo, Var_clean) %>%
  summarise(Kg_Formalizada = sum(Total_num, na.rm = TRUE), .groups = "drop")

comp <- full_join(sem46_mod_var, p46_rdata_mod_var, by = c("Módulo", "Var_clean")) %>%
  mutate(
    Kg_Formalizada = coalesce(Kg_Formalizada, 0),
    Kg_RData = coalesce(Kg_RData, 0),
    Dif = Kg_Formalizada - Kg_RData
  ) %>%
  arrange(desc(abs(Dif)))

cat("=== COMPARISON BY MODULO & VAR (Formalizada 405,865 vs RData 383,565) ===\n")
print(comp %>% filter(abs(Dif) > 0.1), n = 50)
cat("\nTotal Formalizada:", sum(comp$Kg_Formalizada), "\n")
cat("Total RData:", sum(comp$Kg_RData), "\n")
cat("Net Difference:", sum(comp$Dif), "\n")
