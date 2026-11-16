library(dplyr)
library(tidyr)
load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")

sem_sel  <- 45
sem_ant  <- 44
camp_sel <- "2026"
dia_inicio_nom <- "Martes"
dia_fin_nom    <- "Viernes"
dias_nombres   <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
dia_inicio_idx <- which(dias_nombres == dia_inicio_nom)[1]
dia_fin_idx    <- which(dias_nombres == dia_fin_nom)[1]
dias_ajustables_nom <- dias_nombres[dia_inicio_idx:dia_fin_idx]

# Check M1 proyecciones
proy_m1 <- df_proyecciones %>%
  filter(as.character(Campaña) == camp_sel, as.numeric(Semana) == sem_sel, as.character(Módulo) == "1") %>%
  mutate(
    Módulo   = trimws(as.character(Módulo)),
    Turno    = trimws(as.character(Turno)),
    Variedad = trimws(toupper(as.character(Variedad)))
  )
print(proy_m1 %>% select(Sector, Módulo, Turno, Variedad, Proyección))

# Check df_valvulas for M1
valvs_m1 <- df_valvulas %>%
  filter(as.character(Módulo) == "1") %>%
  mutate(
    Turno = trimws(as.character(Turno)),
    Variedad = trimws(toupper(as.character(Variedad))),
    Área = as.numeric(Área)
  )

# Check df_cosecha for M1
cs_m1 <- df_cosecha %>%
  filter(as.character(Campaña) == camp_sel, as.numeric(Semana) == sem_sel, as.character(Módulo) == "1")
print(cs_m1 %>% select(Fecha, Turno, `Kg Total`))
