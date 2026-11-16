lines <- readLines("D:/ACP/Actualizacion Semanal/app.R", warn = FALSE, encoding = "UTF-8")
idx_ged <- which(grepl("^guardar_ediciones_disco <- function", lines))[1]
cat("idx_ged:", idx_ged, "\n")
sub_lines <- lines[idx_ged:(idx_ged + 30)]
for (j in seq_along(sub_lines)) {
  if (grepl("Ha_Asignada_1ra", sub_lines[j])) cat("Found Ha_Asignada_1ra at relative offset:", j, "text:", sub_lines[j], "\n")
  if (grepl("Kg_Asignado_2da", sub_lines[j])) cat("Found Kg_Asignado_2da at relative offset:", j, "text:", sub_lines[j], "\n")
}
