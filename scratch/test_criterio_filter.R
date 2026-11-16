library(dplyr)

test_df <- tibble(
  Turno = c("1", "2", "6"),
  Motivo_Ajuste = c("Avance >15% (Rend act sem: 303 kg/ha)", "Sin ajuste programado", "Sin ajuste programado"),
  Kg_Ajustado_Esc = c(2598.4, 0.0, 0.0),
  Ha_Ajustada_Esc = c(8.5, 0.0, 0.0)
)

f_crit <- "con_ajuste"
res_con <- test_df %>% filter(
  coalesce(Motivo_Ajuste, "") != "Sin ajuste programado" &
  (coalesce(Kg_Ajustado_Esc, 0) > 0 | coalesce(Ha_Ajustada_Esc, 0) > 0)
)
cat("Con ajuste count:", nrow(res_con), "Turnos:", paste(res_con$Turno, collapse = ", "), "\n")

f_crit <- "sin_ajuste"
res_sin <- test_df %>% filter(
  coalesce(Motivo_Ajuste, "") == "Sin ajuste programado" |
  (coalesce(Kg_Ajustado_Esc, 0) == 0 & coalesce(Ha_Ajustada_Esc, 0) == 0)
)
cat("Sin ajuste count:", nrow(res_sin), "Turnos:", paste(res_sin$Turno, collapse = ", "), "\n")
