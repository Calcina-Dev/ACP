library(dplyr)
df_c_hist <- if (file.exists("historico_conteo.rds")) readRDS("historico_conteo.rds") else tibble()
cat("nrow df_c_hist:", nrow(df_c_hist), "\n")
cat("colnames df_c_hist:\n")
print(colnames(df_c_hist))
cat("unique Campaña in df_c_hist:\n")
print(unique(df_c_hist$Campaña))
if ("Campaña_Original" %in% colnames(df_c_hist)) {
  cat("unique Campaña_Original in df_c_hist:\n")
  print(unique(df_c_hist$Campaña_Original))
}
