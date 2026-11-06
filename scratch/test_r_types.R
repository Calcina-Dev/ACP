suppressPackageStartupMessages(library(dplyr))
df0 <- tibble(Es_Ruta_Sin_Kg = logical(), Num_Pasada = integer())
m1 <- df0 %>% mutate(Tipo = ifelse(Es_Ruta_Sin_Kg, 'a', 'b'))
cat('m1$Tipo class:', class(m1$Tipo), '\n')
m2 <- df0 %>% mutate(Tipo = as.character(ifelse(Es_Ruta_Sin_Kg, 'a', 'b')))
cat('m2$Tipo class:', class(m2$Tipo), '\n')
m3 <- df0 %>% mutate(Tipo = as.character(case_when(Es_Ruta_Sin_Kg ~ 'a', TRUE ~ 'b')))
cat('m3$Tipo class:', class(m3$Tipo), '\n')

# Test bind_rows with existing character column
existing <- tibble(Tipo = "regular")
cat("bind_rows(existing, m1):\n")
tryCatch(print(bind_rows(existing, m1)), error = function(e) cat("Error in m1:", e$message, "\n"))

cat("bind_rows(existing, m2):\n")
tryCatch(print(bind_rows(existing, m2)), error = function(e) cat("Error in m2:", e$message, "\n"))
