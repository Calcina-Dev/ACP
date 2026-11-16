load("Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
library(dplyr)
library(lubridate)
library(nnet)

df_cosecha[["Kg"]] <- df_cosecha[["Kg Total"]]

df_turn_week <- df_cosecha %>%
  filter(Campaña >= 2021, !coalesce(Es_Testblock, FALSE)) %>%
  mutate(
    wd = wday(Fecha, week_start = 1),
    Semana = as.numeric(Semana),
    Campaña = as.numeric(Campaña),
    Módulo = as.character(Módulo),
    Turno = as.character(Turno),
    Variedad = as.character(Variedad)
  ) %>%
  group_by(Campaña, Semana, Módulo, Turno, Variedad) %>%
  summarise(
    Area_Ha = mean(coalesce(Área, 0), na.rm = TRUE),
    Kg_Lun_Mar = sum(Kg[wd %in% c(1, 2)], na.rm = TRUE),
    Kg_Total_Sem = sum(Kg, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(Area_Ha > 0, Kg_Total_Sem > 0) %>%
  mutate(
    Rend_Total = Kg_Total_Sem / Area_Ha,
    Rend_Lun_Mar = Kg_Lun_Mar / Area_Ha,
    Tiene_Lun_Mar = as.numeric(Kg_Lun_Mar > 0)
  ) %>%
  arrange(Módulo, Turno, Variedad, Campaña, Semana) %>%
  group_by(Módulo, Turno, Variedad, Campaña) %>%
  mutate(
    Rend_S_1 = ifelse(Semana == lag(Semana) + 1, lag(Rend_Total), NA_real_)
  ) %>%
  ungroup() %>%
  filter(!is.na(Rend_S_1))

scale_max_rend <- max(df_turn_week$Rend_Total, na.rm = TRUE)

df_turn_week <- df_turn_week %>%
  mutate(
    y_norm = Rend_Total / scale_max_rend,
    x_s1_norm = Rend_S_1 / scale_max_rend,
    x_lm_norm = Rend_Lun_Mar / scale_max_rend,
    x_sem_norm = Semana / 53
  )

train_df <- df_turn_week %>% filter(Campaña <= 2025)
test_df  <- df_turn_week %>% filter(Campaña == 2026)

set.seed(42)
m_rna <- nnet(
  y_norm ~ x_s1_norm + x_lm_norm + Tiene_Lun_Mar + x_sem_norm,
  data = train_df,
  size = 8,
  decay = 0.001,
  maxit = 300,
  linout = TRUE,
  trace = FALSE
)

pred_norm <- predict(m_rna, newdata = test_df)
pred_test <- pred_norm * scale_max_rend

mae_test <- mean(abs(pred_test - test_df$Rend_Total))
rmse_test <- sqrt(mean((pred_test - test_df$Rend_Total)^2))
cor_test <- cor(pred_test, test_df$Rend_Total)
r2_test  <- 1 - sum((test_df$Rend_Total - pred_test)^2) / sum((test_df$Rend_Total - mean(test_df$Rend_Total))^2)

cat(sprintf("=== RESULTADO PRUEBA PILOTO RNA (Entrenamiento 2021-2025 -> Test 2026) ===\n"))
cat(sprintf("Muestras Entrenamiento: %d | Muestras Test: %d\n", nrow(train_df), nrow(test_df)))
cat(sprintf("Correlacion pred vs real (2026): %.3f\n", cor_test))
cat(sprintf("R^2 en Test 2026: %.3f\n", r2_test))
cat(sprintf("MAE: %.2f kg/ha | RMSE: %.2f kg/ha\n", mae_test, rmse_test))
cat(sprintf("Rendimiento promedio real 2026: %.2f kg/ha\n", mean(test_df$Rend_Total)))
cat(sprintf("Error porcentual medio aproximado (MAE/Media): %.1f%%\n", (mae_test / mean(test_df$Rend_Total)) * 100))
