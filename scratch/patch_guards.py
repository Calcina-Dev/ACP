import os

file_path = r"D:\ACP\Actualizacion Semanal\app.R"
with open(file_path, "r", encoding="utf-8") as f:
    code = f.read()

# Pattern to replace:
old_pat = """    ed_fijas <- ed_otros_pasada %>%
      filter(coalesce(Modo_Ingreso, "") == "kgha" | grepl("kg/ha en", coalesce(Motivo_Ajuste, "")))
    ha_fijas <- sum(if (pasada_activa == 2L) coalesce(ed_fijas$Ha_Asignada_2da, 0) else coalesce(ed_fijas$Ha_Asignada_1ra, 0), na.rm = TRUE)
    ha_pool_prorr <- max(0, round(ha_pool_pasada - ha_fijas, 2))
    
    ed_prorr <- ed_otros_pasada %>%
      filter(!(coalesce(Modo_Ingreso, "") == "kgha" | grepl("kg/ha en", coalesce(Motivo_Ajuste, ""))))"""

new_pat = """    if (nrow(ed_otros_pasada) > 0) {
      if (!("Modo_Ingreso" %in% names(ed_otros_pasada))) {
        ed_otros_pasada$Modo_Ingreso <- ifelse(grepl("kg/ha en", coalesce(ed_otros_pasada$Motivo_Ajuste, "")), "kgha", "kilos")
      }
      ed_fijas <- ed_otros_pasada %>%
        filter(coalesce(Modo_Ingreso, "") == "kgha" | grepl("kg/ha en", coalesce(Motivo_Ajuste, "")))
      ed_prorr <- ed_otros_pasada %>%
        filter(!(coalesce(Modo_Ingreso, "") == "kgha" | grepl("kg/ha en", coalesce(Motivo_Ajuste, ""))))
    } else {
      ed_fijas <- tibble()
      ed_prorr <- tibble()
    }
    ha_fijas <- if (nrow(ed_fijas) > 0) sum(if (pasada_activa == 2L) coalesce(ed_fijas$Ha_Asignada_2da, 0) else coalesce(ed_fijas$Ha_Asignada_1ra, 0), na.rm = TRUE) else 0
    ha_pool_prorr <- max(0, round(ha_pool_pasada - ha_fijas, 2))"""

assert code.count(old_pat) == 3, f"Expected 3 occurrences of old_pat, found {code.count(old_pat)}"
code = code.replace(old_pat, new_pat)

with open(file_path, "w", encoding="utf-8") as f:
    f.write(code)

print("SUCCESS: 3 occurrences updated with safety guards!")
