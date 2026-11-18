load("Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
cat("Objects in datos_maestros_cosecha_2026.RData:\n")
ls_objs <- ls()
print(ls_objs)
if ("df_hist_oficial_wide" %in% ls_objs) {
  cat("\ndf_hist_oficial_wide names:\n")
  print(names(df_hist_oficial_wide))
}
