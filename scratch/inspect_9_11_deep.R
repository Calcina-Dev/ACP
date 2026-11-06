load('D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData')
library(dplyr)

cat("--- df_cosecha Módulo 9 all campaigns ---\n")
print(df_cosecha %>% filter(grepl("9", as.character(Módulo))) %>%
        distinct(Campaña, Sector, Módulo) %>% arrange(Campaña, Módulo))

cat("\n--- df_cosecha Turnos >= 12 for Módulo 9 in 2026? ---\n")
print(df_cosecha %>% filter(Campaña == 2026, grepl("^9", as.character(Módulo)), as.numeric(Turno) >= 12) %>% head(5))

cat("\n--- df_valvulas for Módulo 9 Turnos >= 12 ---\n")
print(df_valvulas %>% filter(as.character(Módulo) == "9", as.numeric(Turno) >= 12) %>%
        distinct(Sector, Módulo, Turno, Variedad) %>% head(10))

cat("\n--- df_valvulas for Módulo 11 ---\n")
print(df_valvulas %>% filter(as.character(Módulo) == "11") %>%
        group_by(Sector, Turno_Rango = ifelse(as.numeric(Turno) <= 12, "1-12", "13-24")) %>%
        summarise(n = n(), Variedades = paste(unique(Variedad), collapse=", "), .groups = "drop"))
