import os

file_path = r"D:\ACP\Actualizacion Semanal\app.R"
with open(file_path, "r", encoding="utf-8") as f:
    code = f.read()

target = """          # ====================================================================
          # NODOS 4, 5, 6 y 7: Árbol de Decisión de Rendimiento por Rama
          # ====================================================================
          Rend_Base_Turno = case_when(
            # RAMA 1 (Izquierda): S-1 NO finalizó -> Usar Rend_S-1 puro (o Proy si S-1=0)
            Semana_Ant_No_Finalizo ~ ifelse(Rend_Ant_Turno > 0, Rend_Ant_Turno, Rend_Proy),
            
            # RAMA 2A (Derecha -> 100% Lunes/Martes -> Es Rocío): 2da pasada con Rend Sem Actual
            Turno_Completo_Lun_Mar & Es_Ciclo_Corto ~ Rend_Sem_Actual,
            
            # RAMA 3A (Derecha -> No Tiene Kilos / Avance en Sem Actual):
            # Base Proyección oficial limpia (el premio o castigo lo define exclusivamente el conteo floral)
            !Tiene_Cosecha_Act ~ Rend_Proy,
            
            # RAMA 3B-1 (Derecha -> Tiene Avance -> Cosechó > 30% del turno):
            # Usar directo Rend_Sem_Actual (rendimiento real observado en campo)
            Tiene_Cosecha_Act & Prop_Avance_Turno > 0.30 ~ Rend_Sem_Actual,
            
            # RAMA 3B-2 (Derecha -> Tiene Avance -> Cosechó <= 30% del turno):
            # Base Proyección oficial limpia (el premio o castigo lo define exclusivamente el conteo floral)
            Tiene_Cosecha_Act & Prop_Avance_Turno <= 0.30 ~ Rend_Proy,
            
            TRUE ~ Rend_Proy
          ),"""

replacement = """          # ====================================================================
          # NODOS 4, 5, 6 y 7: Árbol de Decisión de Rendimiento por Rama
          # ====================================================================
          Rend_Base_Turno = case_when(
            # RAMA 1 (Izquierda): S-1 NO finalizó -> Usar Rend_S-1 puro (o Proy si S-1=0)
            Semana_Ant_No_Finalizo ~ ifelse(Rend_Ant_Turno > 0, Rend_Ant_Turno, Rend_Proy),
            
            # RAMA 2A (Derecha -> 100% Lunes/Martes -> Es Rocío): 2da pasada con Rend Sem Actual
            Turno_Completo_Lun_Mar & Es_Ciclo_Corto ~ Rend_Sem_Actual,
            
            # RAMA 3A (Derecha -> No Tiene Kilos / Avance en Sem Actual):
            # Usar rendimiento de la semana anterior S-1 (o Proy si S-1=0) ajustado con factor floral
            !Tiene_Cosecha_Act ~ ifelse(Rend_Ant_Turno > 0, Rend_Ant_Turno, Rend_Proy),
            
            # RAMA 3B-1 (Derecha -> Tiene Avance -> Cosechó > 30% del turno):
            # Usar directo Rend_Sem_Actual (rendimiento real observado en campo)
            Tiene_Cosecha_Act & Prop_Avance_Turno > 0.30 ~ Rend_Sem_Actual,
            
            # RAMA 3B-2 (Derecha -> Tiene Avance -> Cosechó <= 30% del turno):
            # Muestra pequeña (<= 30%): Usar rendimiento de la semana anterior S-1 (o Proy si S-1=0) con factor floral
            Tiene_Cosecha_Act & Prop_Avance_Turno <= 0.30 ~ ifelse(Rend_Ant_Turno > 0, Rend_Ant_Turno, Rend_Proy),
            
            TRUE ~ ifelse(Rend_Ant_Turno > 0, Rend_Ant_Turno, Rend_Proy)
          ),"""

motivo_target = """            !Tiene_Cosecha_Act ~ sprintf("Sin avance previo (Base: %s kg/ha)", format(round(Rend_Base_Turno), big.mark = ",")),
            Tiene_Cosecha_Act & Prop_Avance_Turno > 0.30 ~ sprintf("Avance >30%% (Rend act sem: %s kg/ha)", format(round(Rend_Sem_Actual), big.mark = ",")),
            Tiene_Cosecha_Act & Prop_Avance_Turno <= 0.30 ~ sprintf("Avance <=30%% (Base: %s kg/ha)", format(round(Rend_Base_Turno), big.mark = ",")),
            TRUE ~ sprintf("Rend proy (%s kg/ha)", format(round(Rend_Base_Turno), big.mark = ","))"""

motivo_replacement = """            !Tiene_Cosecha_Act & Rend_Ant_Turno > 0 ~ sprintf("Sin avance previo (Rend S-1: %s kg/ha)", format(round(Rend_Ant_Turno), big.mark = ",")),
            !Tiene_Cosecha_Act                      ~ sprintf("Sin avance previo (Rend proy: %s kg/ha)", format(round(Rend_Proy), big.mark = ",")),
            Tiene_Cosecha_Act & Prop_Avance_Turno > 0.30 ~ sprintf("Avance >30%% (Rend act sem: %s kg/ha)", format(round(Rend_Sem_Actual), big.mark = ",")),
            Tiene_Cosecha_Act & Prop_Avance_Turno <= 0.30 & Rend_Ant_Turno > 0 ~ sprintf("Avance <=30%% (Rend S-1: %s kg/ha)", format(round(Rend_Ant_Turno), big.mark = ",")),
            Tiene_Cosecha_Act & Prop_Avance_Turno <= 0.30                      ~ sprintf("Avance <=30%% (Rend proy: %s kg/ha)", format(round(Rend_Proy), big.mark = ",")),
            TRUE ~ ifelse(Rend_Ant_Turno > 0, sprintf("Rend S-1 (%s kg/ha)", format(round(Rend_Ant_Turno), big.mark = ",")), sprintf("Rend proy (%s kg/ha)", format(round(Rend_Proy), big.mark = ",")))"""

assert target in code, "target not found"
code = code.replace(target, replacement, 1)

assert motivo_target in code, "motivo_target not found"
code = code.replace(motivo_target, motivo_replacement, 1)

with open(file_path, "w", encoding="utf-8") as f:
    f.write(code)

print("SUCCESS: app.R updated successfully with previous week yield logic!")
