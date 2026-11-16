lines <- readLines("D:/ACP/Actualizacion Semanal/app.R", warn = FALSE, encoding = "UTF-8")
idx_haed <- which(grepl("^\\s*ha_ajustadas_ed <- ed_filtradas %>%", lines))[1]
cat("idx_haed:", idx_haed, "\n")
sub_lines <- lines[idx_haed:(idx_haed + 35)]
for (j in seq_along(sub_lines)) {
  if (grepl("Ha_1ra", sub_lines[j])) cat("Found Ha_1ra at relative offset:", j, "text:", sub_lines[j], "\n")
  if (grepl("Tiene_Detalle_Pas", sub_lines[j])) cat("Found Tiene_Detalle_Pas at relative offset:", j, "text:", sub_lines[j], "\n")
}
