suppressPackageStartupMessages(library(dplyr))

limpiar_variedad <- function(x) {
  if (is.null(x) || length(x) == 0) return(character(0))
  v <- toupper(trimws(as.character(x)))
  as.character(ifelse(grepl("EB\\s*9\\s*[-_.]\\s*2", v, ignore.case = TRUE), "MAGICA", v))
}

df_mad_t <- readRDS("D:/ACP/Actualizacion Semanal/dias_maduracion_turno.rds")
df_mad_m <- readRDS("D:/ACP/Actualizacion Semanal/dias_maduracion_modulo_variedad.rds")

mod_f <- "7"
tur_f <- "4"
var_f <- "VENTURA"

# OLD logic:
sub_t_old <- df_mad_t %>% filter(trimws(as.character(Módulo)) == trimws(as.character(mod_f)),
                                 trimws(as.character(Turno)) == trimws(as.character(tur_f)))
d_res_old <- as.numeric(sub_t_old$Dias_Maduracion[1])
cat("OLD logic for M7 T4:\n")
cat("sub_t rows:", nrow(sub_t_old), "\n")
print(sub_t_old)
cat("dias_desfase:", d_res_old, "-> shift_sem:", d_res_old / 7, "\n\n")

# NEW logic:
sub_t_new <- df_mad_t %>% filter(trimws(as.character(Módulo)) == trimws(as.character(mod_f)),
                                 trimws(as.character(Turno)) == trimws(as.character(tur_f)))
if (!is.null(var_f) && length(var_f) > 0) {
  sub_t_v <- sub_t_new %>% filter(limpiar_variedad(Variedad) %in% limpiar_variedad(var_f))
  if (nrow(sub_t_v) > 0) sub_t_new <- sub_t_v
}
d_res_new <- as.numeric(sub_t_new$Dias_Maduracion[1])
cat("NEW logic for M7 T4 (with VENTURA):\n")
cat("sub_t rows:", nrow(sub_t_new), "\n")
print(sub_t_new)
cat("dias_desfase:", d_res_new, "-> shift_sem:", d_res_new / 7, "\n")
