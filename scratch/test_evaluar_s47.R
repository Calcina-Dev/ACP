suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

# Mock Shiny input
input <- list(
  filtro_campana = "2026",
  filtro_semana = 47,
  ajustar_desde_dia = "Martes",
  ajustar_hasta_dia = "Viernes"
)

dias_semana_nombres <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
limpiar_variedad <- function(x) {
  if (is.null(x) || length(x) == 0) return(character(0))
  v <- toupper(trimws(as.character(x)))
  as.character(ifelse(grepl("EB\\s*9\\s*[-_.]\\s*2", v, ignore.case = TRUE), "MAGICA", v))
}

# Load actual editions
ed_all <- readRDS("Actualizacion Semanal/ediciones_manuales_cosecha.rds")
df_ed <- ed_all %>% filter(as.character(`Campaña`) == "2026", as.numeric(Semana) == 47, coalesce(Valor, 0) > 0)

# Load real u_base simulation from df_cosecha
load("Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
dias_semana <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Domingo")

df_filtrado <- df_cosecha %>%
  filter(
    as.character(Campaña) == "2026",
    Semana == 47,
    !coalesce(Es_Testblock, FALSE)
  )

cosecha_preparada <- df_filtrado %>%
  mutate(
    Dia       = factor(dias_semana[lubridate::wday(Fecha, week_start = 1)], levels = dias_semana),
    Turno_num = suppressWarnings(as.numeric(Turno)),
    Sector    = case_when(as.character(Módulo) %in% c("9", "9.1", "9.2", "11", "11.1", "11.2", "12") ~ "9", TRUE ~ trimws(as.character(Sector))),
    Módulo    = trimws(as.character(Módulo)),
    Módulo    = case_when(
      Módulo %in% c("13.1", "13.2") ~ "13",
      Módulo %in% c("14.1", "14.2") ~ "14",
      as.character(Módulo) == "9" & between(Turno_num, 1, 11)  ~ "9.2",
      as.character(Módulo) == "9" & Turno_num >= 12            ~ "9.1",
      as.character(Módulo) == "11" & between(Turno_num, 1, 12) ~ "11.1",
      as.character(Módulo) == "11" & Turno_num >= 13           ~ "11.2",
      TRUE ~ Módulo
    ),
    Turno    = trimws(as.character(Turno)),
    Variedad = limpiar_variedad(Variedad)
  ) %>%
  group_by(Sector, Módulo, Turno, Variedad, Dia) %>%
  summarise(Kg_Dia = sum(`Kg Total`, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Dia, values_from = Kg_Dia, values_fill = 0) %>%
  mutate(Es_Testblock = FALSE)

for (d in dias_semana) {
  if (!d %in% names(cosecha_preparada)) cosecha_preparada[[d]] <- numeric(nrow(cosecha_preparada))
}

u_base <- cosecha_preparada %>%
  mutate(
    Área = 10,
    Proyección = 1000,
    `Ha Real 1ª Pas` = 0,
    `Ha Real 2ª Pas` = 0
  )

rc_avance_semana <- tibble(
  Módulo = character(), Turno = character(), Variedad = character(),
  Ha_Real_Semana_1ra = numeric(), Ha_Real_Semana_2da = numeric(), Ha_Real_Semana_Tot = numeric()
)

# New evaluar_escenario implementation
evaluar_escenario <- function(df_ed, esc_info = tibble()) {
  dias_ajuste  <- character(0)
  dias_previos <- character(0)
  
  if (!is.null(esc_info) && nrow(esc_info) > 0 && !is.na(esc_info$Dia_Desde[1]) && nzchar(as.character(esc_info$Dia_Desde[1]))) {
    d_desde <- as.character(esc_info$Dia_Desde[1])
    d_hasta <- if ("Dia_Hasta" %in% names(esc_info) && !is.na(esc_info$Dia_Hasta[1]) && nzchar(as.character(esc_info$Dia_Hasta[1]))) {
      as.character(esc_info$Dia_Hasta[1])
    } else {
      "Sábado"
    }
    idx_d <- match(d_desde, dias_semana_nombres)
    idx_h <- match(d_hasta, dias_semana_nombres)
    if (is.na(idx_d)) idx_d <- 1
    if (is.na(idx_h)) idx_h <- 6
    if (idx_h < idx_d) idx_h <- idx_d
    dias_ajuste <- dias_semana_nombres[idx_d:idx_h]
    if (idx_d > 1) dias_previos <- dias_semana_nombres[1:(idx_d - 1)]
  } else {
    d_desde <- tryCatch({
      if (!is.null(input$ajustar_desde_dia) && as.character(input$ajustar_desde_dia) %in% dias_semana_nombres) {
        as.character(input$ajustar_desde_dia)
      } else {
        NULL
      }
    }, error = function(e) NULL)
    
    d_hasta <- tryCatch({
      if (!is.null(input$ajustar_hasta_dia) && as.character(input$ajustar_hasta_dia) %in% dias_semana_nombres) {
        as.character(input$ajustar_hasta_dia)
      } else {
        NULL
      }
    }, error = function(e) NULL)
    
    if (!is.null(d_desde) && !is.null(d_hasta)) {
      idx_d <- match(d_desde, dias_semana_nombres)
      idx_h <- match(d_hasta, dias_semana_nombres)
      if (is.na(idx_d)) idx_d <- 1
      if (is.na(idx_h)) idx_h <- 6
      if (idx_h < idx_d) idx_h <- idx_d
      dias_ajuste <- dias_semana_nombres[idx_d:idx_h]
      if (idx_d > 1) dias_previos <- dias_semana_nombres[1:(idx_d - 1)]
    } else if (!is.null(df_ed) && nrow(df_ed) > 0) {
      df_ed_calc <- if ("Tipo_Edicion" %in% names(df_ed)) df_ed %>% filter(coalesce(Tipo_Edicion, "") != "ruta_sin_kg") else df_ed
      dias_ajuste <- intersect(dias_semana_nombres, unique(as.character(df_ed_calc$Campo)))
      if (length(dias_ajuste) == 0) dias_ajuste <- dias_semana_nombres
      min_idx <- min(match(dias_ajuste, dias_semana_nombres))
      if (min_idx > 1) dias_previos <- dias_semana_nombres[1:(min_idx - 1)]
    } else {
      dias_ajuste <- dias_semana_nombres
    }
  }
  
  cat("dias_ajuste:", paste(dias_ajuste, collapse=", "), "\n")
  cat("dias_previos:", paste(dias_previos, collapse=", "), "\n")
  
  ed_filtrada <- if (!is.null(df_ed) && nrow(df_ed) > 0) {
    df_base_ed <- if ("Tipo_Edicion" %in% names(df_ed)) {
      df_ed %>% filter(coalesce(Tipo_Edicion, "") != "ruta_sin_kg")
    } else {
      df_ed
    }
    df_base_ed %>%
      filter(Campo %in% dias_ajuste, coalesce(Valor, 0) > 0) %>%
      mutate(
        Turno_num    = suppressWarnings(as.numeric(Turno)),
        Turno_clean  = ifelse(!is.na(Turno_num), as.character(Turno_num), trimws(as.character(Turno))),
        Módulo_clean = trimws(as.character(Módulo)),
        Sector_clean = trimws(as.character(Sector)),
        Var_clean    = toupper(trimws(limpiar_variedad(Variedad))),
        es_rocio     = (Var_clean == "ROCIO"),
        es_p2        = (es_rocio & (
          coalesce(Kg_Asignado_2da, 0) > 0 |
          coalesce(Ha_Asignada_2da, 0) > 0 |
          grepl("2da pasada|2ª pasada", coalesce(Motivo_Ajuste, ""), ignore.case = TRUE)
        )),
        kg_p1        = ifelse(!es_p2, coalesce(Kg_Asignado_1ra, Valor), 0),
        ha_p1        = ifelse(!es_p2, coalesce(Ha_Asignada_1ra, Ha_Asignada), 0),
        kg_p2        = ifelse(es_p2, coalesce(Kg_Asignado_2da, Valor), 0),
        ha_p2        = ifelse(es_p2, coalesce(Ha_Asignada_2da, Ha_Asignada), 0)
      )
  } else {
    tibble()
  }
  
  ed_p1 <- if (nrow(ed_filtrada) > 0) {
    ed_filtrada %>%
      filter(kg_p1 > 0 | ha_p1 > 0) %>%
      group_by(Sector = Sector_clean, Módulo = Módulo_clean, Turno = Turno_clean, Variedad = Var_clean) %>%
      summarise(
        Ha_Ajustada_Esc = round(sum(coalesce(ha_p1, 0), na.rm = TRUE), 2),
        Kg_Ajustado_Esc = round(sum(coalesce(kg_p1, 0), na.rm = TRUE), 1),
        Motivo_Ajuste   = "Ajuste programado",
        .groups = "drop"
      )
  } else {
    tibble(Sector = character(), Módulo = character(), Turno = character(), Variedad = character(), Ha_Ajustada_Esc = numeric(), Kg_Ajustado_Esc = numeric(), Motivo_Ajuste = character())
  }
  
  ed_p2 <- if (nrow(ed_filtrada) > 0) {
    ed_filtrada %>%
      filter(kg_p2 > 0 | ha_p2 > 0) %>%
      group_by(Sector = Sector_clean, Módulo = Módulo_clean, Turno = Turno_clean, Variedad = Var_clean) %>%
      summarise(
        Ha_Ajustada_Esc = round(sum(coalesce(ha_p2, 0), na.rm = TRUE), 2),
        Kg_Ajustado_Esc = round(sum(coalesce(kg_p2, 0), na.rm = TRUE), 1),
        Motivo_Ajuste   = "2ª Pasada Rocío",
        .groups = "drop"
      )
  } else {
    tibble(Sector = character(), Módulo = character(), Turno = character(), Variedad = character(), Ha_Ajustada_Esc = numeric(), Kg_Ajustado_Esc = numeric(), Motivo_Ajuste = character())
  }
  
  cols_prev_u <- intersect(dias_previos, names(u_base))
  cols_sem_u  <- intersect(dias_semana_nombres, names(u_base))
  
  res_1ra <- u_base %>%
    mutate(
      Turno_num    = suppressWarnings(as.numeric(Turno)),
      Turno_clean  = ifelse(!is.na(Turno_num), as.character(Turno_num), trimws(as.character(Turno))),
      Módulo_clean = trimws(as.character(Módulo)),
      Sector_clean = trimws(as.character(Sector)),
      Var_clean    = toupper(trimws(limpiar_variedad(Variedad))),
      Pasada       = "1ª Pasada",
      Kg_Real_Prev_Tot = if (length(cols_prev_u) > 0) round(rowSums(across(all_of(cols_prev_u)), na.rm = TRUE), 1) else 0,
      Kg_Real_Sem_Tot  = round(rowSums(across(all_of(cols_sem_u)), na.rm = TRUE), 1),
      Kg_Real_2da_Sem  = 0,
      Kg_Real_1ra_Sem  = Kg_Real_Sem_Tot,
      Kg_Real_Semana   = Kg_Real_1ra_Sem,
      Kg_Real_Prev     = round(pmin(Kg_Real_1ra_Sem, Kg_Real_Prev_Tot), 1),
      Kg_Proy_Oficial  = round(coalesce(Proyección, 0), 1)
    ) %>%
    left_join(ed_p1, by = c("Sector_clean" = "Sector", "Módulo_clean" = "Módulo", "Turno_clean" = "Turno", "Var_clean" = "Variedad")) %>%
    mutate(
      Ha_Ajustada_Esc    = coalesce(Ha_Ajustada_Esc, 0),
      Kg_Ajustado_Esc    = coalesce(Kg_Ajustado_Esc, 0),
      Kg_Ajustado_Semana = round(Kg_Real_Prev + Kg_Ajustado_Esc, 1)
    )
  
  res_2da <- u_base %>%
    mutate(
      Turno_num    = suppressWarnings(as.numeric(Turno)),
      Turno_clean  = ifelse(!is.na(Turno_num), as.character(Turno_num), trimws(as.character(Turno))),
      Módulo_clean = trimws(as.character(Módulo)),
      Sector_clean = trimws(as.character(Sector)),
      Var_clean    = toupper(trimws(limpiar_variedad(Variedad))),
      Pasada       = "2ª Pasada",
      Kg_Real_Prev_Tot = if (length(cols_prev_u) > 0) round(rowSums(across(all_of(cols_prev_u)), na.rm = TRUE), 1) else 0,
      Kg_Real_2da_Sem  = 0,
      Kg_Real_1ra_Sem  = 0,
      Kg_Real_Prev     = 0,
      Kg_Real_Semana   = 0,
      Kg_Proy_Oficial  = 0
    ) %>%
    left_join(ed_p2, by = c("Sector_clean" = "Sector", "Módulo_clean" = "Módulo", "Turno_clean" = "Turno", "Var_clean" = "Variedad")) %>%
    mutate(
      Ha_Ajustada_Esc    = coalesce(Ha_Ajustada_Esc, 0),
      Kg_Ajustado_Esc    = coalesce(Kg_Ajustado_Esc, 0),
      Kg_Ajustado_Semana = round(Kg_Real_Prev + Kg_Ajustado_Esc, 1)
    ) %>%
    filter(Kg_Ajustado_Esc > 0)
  
  bind_rows(res_1ra, res_2da)
}

res <- evaluar_escenario(df_ed, tibble())

cat("\n--- RESULTADOS DE EVALUAR ESCENARIO ---\n")
cat("Total Kg_Real_Prev:", sum(res$Kg_Real_Prev), "\n")
cat("Total Kg_Ajustado_Esc:", sum(res$Kg_Ajustado_Esc), "\n")
cat("Total Kg_Ajustado_Semana:", sum(res$Kg_Ajustado_Semana), "\n")
cat("Total Kg_Real_Semana:", sum(res$Kg_Real_Semana), "\n")
