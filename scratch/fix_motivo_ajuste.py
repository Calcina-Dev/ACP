import sys

file_path = "Actualizacion Semanal/app.R"
with open(file_path, "r", encoding="utf-8") as f:
    content = f.read()

target = """          Motivo_Ajuste  = case_when(
            Semana_Ant_No_Finalizo & Valvula_Cerro_En_Lun_Mar ~ {
              pct_fl <- round((Factor_Floral - 1) * 100, 1)
              txt_fl <- case_when(
                Tipo_Efecto_Floral == "Premio"  ~ sprintf("Premio floral %+0.1f%%", pct_fl),
                Tipo_Efecto_Floral == "Castigo" ~ sprintf("Castigo floral %+0.1f%%", pct_fl),
                TRUE                            ~ "Floral neutro (0%)"
              )
              mot_b <- if (Rend_Ant_Turno > 0) sprintf("Cierre S-1 incompleta (Rend S-1: %s kg/ha)", trimws(format(round(Rend_Ant_Turno), big.mark = ","))) else sprintf("Cierre S-1 incompleta (Rend proy: %s kg/ha)", trimws(format(round(Rend_Proy), big.mark = ",")))
              paste0(mot_b, " • ", txt_fl)
            },
            TRUE ~ Motivo_Ajuste
          )"""

replacement = """          Motivo_Ajuste  = case_when(
            Semana_Ant_No_Finalizo & Valvula_Cerro_En_Lun_Mar ~ paste0(
              ifelse(
                coalesce(Rend_Ant_Turno, 0) > 0,
                sprintf("Cierre S-1 incompleta (Rend S-1: %s kg/ha)", trimws(format(round(coalesce(Rend_Ant_Turno, 0)), big.mark = ","))),
                sprintf("Cierre S-1 incompleta (Rend proy: %s kg/ha)", trimws(format(round(coalesce(Rend_Proy, 0)), big.mark = ",")))
              ),
              " • ",
              case_when(
                Tipo_Efecto_Floral == "Premio"  ~ sprintf("Premio floral %+0.1f%%", round((Factor_Floral - 1) * 100, 1)),
                Tipo_Efecto_Floral == "Castigo" ~ sprintf("Castigo floral %+0.1f%%", round((Factor_Floral - 1) * 100, 1)),
                TRUE                            ~ "Floral neutro (0%)"
              )
            ),
            TRUE ~ Motivo_Ajuste
          )"""

content_norm = content.replace("\r\n", "\n")
target_norm = target.replace("\r\n", "\n")
replacement_norm = replacement.replace("\r\n", "\n")

if target_norm not in content_norm:
    print("Error: Target not found!")
    sys.exit(1)

content_norm = content_norm.replace(target_norm, replacement_norm)
content_final = content_norm.replace("\n", "\r\n")

with open(file_path, "w", encoding="utf-8") as f:
    f.write(content_final)

print("SUCCESS: Motivo_Ajuste vectorized successfully!")
