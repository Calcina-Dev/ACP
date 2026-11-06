suppressPackageStartupMessages(library(dplyr))

ed_paths <- c(
  "D:/ACP/Actualizacion Semanal/ediciones_manuales_cosecha.rds",
  "D:/ACP/Actualizacion data maestra/ediciones_manuales_cosecha.rds",
  "D:/ACP/ediciones_manuales_cosecha.rds"
)

for (ep in ed_paths) {
  if (file.exists(ep)) {
    ed <- readRDS(ep)
    if (nrow(ed) > 0) {
      ed <- ed %>%
        mutate(
          Turno_num = suppressWarnings(as.numeric(Turno)),
          Sector = case_when(
            as.character(Módulo) %in% c("9", "9.1", "9.2", "11", "11.1", "11.2", "12") ~ "9",
            TRUE ~ as.character(Sector)
          ),
          Módulo = case_when(
            as.character(Módulo) == "9" & between(Turno_num, 1, 11)  ~ "9.2",
            as.character(Módulo) == "9" & Turno_num >= 12            ~ "9.1",
            as.character(Módulo) == "11" & between(Turno_num, 1, 12) ~ "11.1",
            as.character(Módulo) == "11" & Turno_num >= 13           ~ "11.2",
            TRUE ~ as.character(Módulo)
          )
        ) %>%
        select(-any_of("Turno_num"))
      
      saveRDS(ed, ep)
      cat("[OK] Subdividido ed en:", ep, "\n")
      print(table(as.character(ed$Sector), as.character(ed$Módulo)))
    }
  }
}

aj_paths <- c(
  "D:/ACP/Actualizacion Semanal/ajuste_valvulas_semana.rds",
  "D:/ACP/ajuste_valvulas_semana.rds"
)

for (ap in aj_paths) {
  if (file.exists(ap)) {
    aj <- readRDS(ap)
    if (nrow(aj) > 0) {
      aj <- aj %>%
        mutate(
          Turno_num = suppressWarnings(as.numeric(Turno)),
          Sector = case_when(
            as.character(Módulo) %in% c("9", "9.1", "9.2", "11", "11.1", "11.2", "12") ~ "9",
            TRUE ~ as.character(Sector)
          ),
          Módulo = case_when(
            as.character(Módulo) == "9" & between(Turno_num, 1, 11)  ~ "9.2",
            as.character(Módulo) == "9" & Turno_num >= 12            ~ "9.1",
            as.character(Módulo) == "11" & between(Turno_num, 1, 12) ~ "11.1",
            as.character(Módulo) == "11" & Turno_num >= 13           ~ "11.2",
            TRUE ~ as.character(Módulo)
          )
        ) %>%
        select(-any_of("Turno_num"))
      
      saveRDS(aj, ap)
      cat("[OK] Subdividido aj en:", ap, "\n")
      print(table(as.character(aj$Sector), as.character(aj$Módulo)))
    }
  }
}
