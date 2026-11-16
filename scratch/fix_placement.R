# Revert improper insertion and put it exactly in leer_proyeccion_excel
fpath <- "Actualizacion data maestra/actualizar_data_maestra.R"
lines <- readLines(fpath, encoding = "UTF-8", warn = FALSE)

# 1. Remove the wrongly inserted block around line 216
wrong_start <- grep("# Reconciliaci.n oficial con Formalizada", lines)
wrong_end <- wrong_start + 26 # up to "raw_p_proc"
cat("Removing wrongly inserted lines:", wrong_start, "to", wrong_end, "\n")
lines <- lines[-(wrong_start:wrong_end)]

# 2. In leer_proyeccion_excel:
# find raw_p %>% inside leer_proyeccion_excel
calc_func_start <- grep("leer_proyeccion_excel <- function", lines)
cat("leer_proyeccion_excel starts at line:", calc_func_start, "\n")

# inside that function, find raw_p %>%
rel_idx <- grep("raw_p %>%", lines[calc_func_start:(calc_func_start + 60)])[1]
actual_raw_p_line <- calc_func_start + rel_idx - 1
cat("raw_p line:", actual_raw_p_line, ":", lines[actual_raw_p_line], "\n")

# change to raw_p_proc <- raw_p %>%
lines[actual_raw_p_line] <- "  raw_p_proc <- raw_p %>%"

# find summarise .groups = "drop" inside leer_proyeccion_excel
drop_idx <- grep('\\.groups\\s*=\\s*"drop"', lines[actual_raw_p_line:(actual_raw_p_line + 40)])[1]
actual_drop_line <- actual_raw_p_line + drop_idx - 1
closing_paren_line <- actual_drop_line + 1
cat("closing paren line:", closing_paren_line, ":", lines[closing_paren_line], "\n")

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
  lines[1:closing_paren_line],
  insert_code,
  lines[(closing_paren_line + 1):length(lines)]
)

writeLines(new_lines, fpath, useBytes = FALSE)
cat("Successfully fixed and placed in leer_proyeccion_excel!\n")
