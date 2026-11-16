lines <- readLines("Actualizacion Semanal/app.R", encoding = "UTF-8")
idx_shapes <- grep('shapes_list <- list\\(\\)', lines)
cat("Found shapes_list at line:", idx_shapes, "\n")
idx_end <- grep('titulo_y_primario <- if', lines)
cat("Found titulo_y_primario at line:", idx_end, "\n")

# lines from idx_shapes to idx_end - 1 will be replaced
replacement <- c(
  "    shapes_list <- list()",
  "    annotations_list <- list()",
  "    ord_sel_num <- orden_semana_campana(sem_sel)",
  "    ",
  "    if (ord_sel_num >= ord_min && ord_sel_num <= ord_seq[length(ord_seq)]) {",
  "      shapes_list <- list(",
  "        list(",
  "          type = \"line\",",
  "          layer = \"below\",",
  "          x0 = ord_sel_num,",
  "          x1 = ord_sel_num,",
  "          y0 = 0,",
  "          y1 = 1,",
  "          yref = \"paper\",",
  "          line = list(",
  "            color = \"rgba(142, 142, 147, 0.55)\",",
  "            width = 1.5,",
  "            dash = \"dash\"",
  "          )",
  "        )",
  "      )",
  "      ",
  "      annotations_list <- list(",
  "        list(",
  "          x = ord_sel_num,",
  "          y = 1.03,",
  "          yref = \"paper\",",
  "          text = sprintf(\"<b>S%d (Actual)</b>\", sem_sel),",
  "          showarrow = FALSE,",
  "          xanchor = \"center\",",
  "          font = list(",
  "            size = 11,",
  "            color = \"#636366\",",
  "            family = \"-apple-system, BlinkMacSystemFont, 'Inter', sans-serif\"",
  "          ),",
  "          bgcolor = \"rgba(142, 142, 147, 0.12)\",",
  "          bordercolor = \"rgba(142, 142, 147, 0.35)\",",
  "          borderwidth = 1,",
  "          borderpad = 3",
  "        )",
  "      )",
  "    }",
  "    "
)

new_lines <- c(lines[1:(idx_shapes - 1)], replacement, lines[idx_end:length(lines)])
writeLines(new_lines, "Actualizacion Semanal/app.R", useBytes = TRUE)
cat("Successfully updated reference line to sem_sel!\n")
