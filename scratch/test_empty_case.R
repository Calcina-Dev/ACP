library(dplyr)
library(tibble)

cat("=== TEST: Empty 2nd pass cases in ejecutar_ajuste_semana ===\n")

reg_2da_remanente <- tibble()
reg_2da_lun <- tibble()

esquema_test <- tibble(
  Módulo      = character(),
  Turno       = character(),
  Variedad    = character(),
  Ha_Asignada = numeric()
)

ha_2da_previa_mar <- bind_rows(
  esquema_test,
  if (exists("reg_2da_remanente") && nrow(reg_2da_remanente) > 0) reg_2da_remanente %>% select(Módulo, Turno, Variedad, Ha_Asignada) else tibble(),
  if (exists("reg_2da_lun") && nrow(reg_2da_lun) > 0) reg_2da_lun %>% select(Módulo, Turno, Variedad, Ha_Asignada) else tibble()
) %>%
  group_by(Módulo, Turno, Variedad) %>%
  summarise(ha_ya_2da_asig = sum(Ha_Asignada, na.rm = TRUE), .groups = "drop")

cat("Success! ha_2da_previa_mar has columns:", paste(names(ha_2da_previa_mar), collapse = ", "), "\n")
stopifnot(all(c("Módulo", "Turno", "Variedad", "ha_ya_2da_asig") %in% names(ha_2da_previa_mar)))
stopifnot(nrow(ha_2da_previa_mar) == 0)

cat(">>> TEST PASSED! <<<\n")
