import re

with open('D:/ACP/Actualizacion Semanal/app.R', 'r', encoding='utf-8', newline='') as f:
    text = f.read()

# Target 1: df_valvulas initialization
t1 = '''  if (exists("df_valvulas") && is.data.frame(df_valvulas)) {\r
    df_valvulas <- df_valvulas %>%\r
      mutate(\r
        Sector = case_when(\r
          as.character(Módulo) == "12" ~ "9",\r
          TRUE ~ as.character(Sector)\r
        )\r
      )\r
    if (!"Grupo" %in% names(df_valvulas)) {'''
print("T1 in text:", t1 in text)

# Target 2: cosecha_preparada branch 1
t2 = '''      df_concil %>%\r
        filter(Es_Valido_Operativo) %>%\r
        mutate(\r
          Sector   = case_when(as.character(Módulo) == "12" ~ "9", TRUE ~ trimws(as.character(Sector))),\r
          Módulo   = trimws(as.character(Módulo)),\r
          Módulo   = case_when(\r
            es_campana_2026 & Módulo %in% c("13.1", "13.2") ~ "13",\r
            es_campana_2026 & Módulo %in% c("14.1", "14.2") ~ "14",\r
            TRUE ~ Módulo\r
          ),'''
print("T2 in text:", t2 in text)

# Target 3: cosecha_preparada branch 2
t3 = '''        mutate(\r
          Dia      = factor(dias_semana[wday(Fecha, week_start = 1)], levels = dias_semana),\r
          Sector   = case_when(as.character(Módulo) == "12" ~ "9", TRUE ~ trimws(as.character(Sector))),\r
          Módulo   = trimws(as.character(Módulo)),\r
          Módulo   = case_when(\r
            es_campana_2026 & Módulo %in% c("13.1", "13.2") ~ "13",\r
            es_campana_2026 & Módulo %in% c("14.1", "14.2") ~ "14",\r
            TRUE ~ Módulo\r
          ),'''
print("T3 in text:", t3 in text)

# Target 4: Rend_Base_Con_Floral & Detalle_Floral
t4 = '''          # Rendimiento ajustado con Premio o Castigo por tendencia floral de maduración\r
          Rend_Base_Con_Floral = round(Rend_Base_Turno * Factor_Floral, 2),'''
print("T4 in text:", t4 in text)

t5 = '''          Pct_Ajuste_Flor = round((Factor_Floral - 1) * 100, 1),\r
          Detalle_Floral = case_when(\r
            !Esta_Proyectado | (Turno_Completo_Lun_Mar & !Es_Ciclo_Corto) | (Turno_Completo_Lun_Mar & Es_Ciclo_Corto) ~ "",\r
            Tipo_Efecto_Floral == "Premio"  ~ sprintf("Premio floral %+0.1f%%", Pct_Ajuste_Flor),\r
            Tipo_Efecto_Floral == "Castigo" ~ sprintf("Castigo floral %+0.1f%%", Pct_Ajuste_Flor),\r
            TRUE                            ~ "Floral neutro (0%)"\r
          ),'''
print("T5 in text:", t5 in text)
