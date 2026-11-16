suppressMessages({
  library(readxl)
  library(dplyr)
})

base_dir <- "G:/Unidades compartidas/Compartido Proyecciones/3.- Arándano/Campaña 26-27/Proyección Semanal"
f_det_v2  <- file.path(base_dir, "Proyección Semanal Detallada", "26. Proyecciones Semana 46 + 5 semanas ACP (Conteo S44)_V2.xlsx")

res <- read_excel(f_det_v2, sheet = "Resumen Proyección")

# Let's inspect column names and find Sem 2 column
col_sem2_idx <- which(names(res) == "Sem 2" | res[2, ] == "Sem 2" | res[1, ] == "Sem 2")
cat("Col Sem 2 index:", col_sem2_idx, "Col name:", names(res)[col_sem2_idx], "\n")

# Let's print rows 3 to 75 with Módulo, Variedad, and this Sem 2 column
df_res_sem2 <- tibble(
  Fila     = 1:nrow(res),
  Modulo   = as.character(res[[2]]),
  Variedad = as.character(res[[3]]),
  Licencia = as.character(res[[4]]),
  Tipo     = as.character(res[[5]]),
  Kg_Sem2  = suppressWarnings(as.numeric(res[[col_sem2_idx]]))
) %>%
  filter(!is.na(Kg_Sem2) & Kg_Sem2 > 0)

print(df_res_sem2, n = 50)
cat("\nTotal Kg_Sem2 in Resumen Proyección:", sum(df_res_sem2$Kg_Sem2), "\n")
