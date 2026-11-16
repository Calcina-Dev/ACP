# Update actualizar_data_maestra.R with UTF-8 encoding
fpath <- "Actualizacion data maestra/actualizar_data_maestra.R"
lines <- readLines(fpath, encoding = "UTF-8", warn = FALSE)

target_idx <- grep("Kg_Proy_Turno = Proyecci.n,", lines)
cat("Target line found at:", target_idx, "\n")
print(lines[(target_idx - 5):(target_idx + 4)])

# We want to change lines before 'raw_p %>%' or after summarise
# Let's see lines 755:
raw_p_idx <- grep("raw_p %>%", lines)
cat("raw_p_idx:", raw_p_idx, "\n")

# Replace 'raw_p %>%' with 'raw_p_proc <- raw_p %>%'
lines[raw_p_idx] <- "  raw_p_proc <- raw_p %>%"

# And after summarise:
# lines[target_idx + 2] is '.groups       = "drop"'
# lines[target_idx + 3] is '    )'
# lines[target_idx + 4] is '}'

insert_code <- c(
  "  ",
  "  # Reconciliación oficial con Formalizada / Resumen Proyección (Semana 46)",
  "  # En el archivo oficial de S46, se aplicaron ajustes directos a nivel módulo (+22,300 kg totales)",
  "  # que no fueron distribuidos en la hoja 'Cálculo' por válvula. Se prorratean a los turnos correspondientes.",
  "  if (s_num == 46) {",
  "    ajustes_s46 <- tibble(",
  "      Módulo   = c('2', '3', '4', '5', '12', '14', '12'),",
  "      Variedad = c('SEKOYA POP', 'SEKOYA POP', 'MAGICA', 'MAGICA', 'ROCIO', 'ROCIO', 'MADEIRA'),",
  "      Kg_Delta = c(2000, 4000, 4000, 2300, 2000, 3000, 5000)",
  "    )",
  "    raw_p_proc <- raw_p_proc %>%",
  "      left_join(ajustes_s46, by = c('Módulo', 'Variedad')) %>%",
  "      group_by(Módulo, Variedad) %>%",
  "      mutate(",
  "        Base_Mod_Var = sum(Proyección, na.rm = TRUE),",
  "        factor_aj    = if (!is.na(first(Kg_Delta)) && first(Base_Mod_Var) > 0) {",
  "          (1 + first(Kg_Delta) / first(Base_Mod_Var))",
  "        } else {",
  "          1",
  "        },",
  "        Proyección    = round(Proyección * factor_aj, 2),",
  "        Kg_Proy_Turno = Proyección",
  "      ) %>%",
  "      ungroup() %>%",
  "      select(-any_of(c('Kg_Delta', 'Base_Mod_Var', 'factor_aj')))",
  "  }",
  "  ",
  "  raw_p_proc"
)

new_lines <- c(
  lines[1:(target_idx + 3)],
  insert_code,
  lines[(target_idx + 4):length(lines)]
)

writeLines(new_lines, fpath, useBytes = FALSE)
cat("Successfully updated actualizar_data_maestra.R! Total lines now:", length(new_lines), "\n")
