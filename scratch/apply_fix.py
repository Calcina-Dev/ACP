with open('Actualizacion Semanal/app.R', 'r', encoding='utf-8') as f:
    content = f.read()

# 1. universo_base
target1 = '''        es_ejec <- (d_prev %in% c("Lunes", "Martes")) | (v_k > 0)'''
replacement1 = '''        es_ejec <- (d_prev %in% c("Lunes", "Martes"))'''
assert target1 in content, 'target1 not found'
content = content.replace(target1, replacement1, 1)

# 2. ejecutar_ajuste_semana rc loop
target2 = '''          es_ejecutado <- (d %in% c("Lunes", "Martes")) | (val_k > 0)
          ha_prev_vec <- ha_prev_vec + ifelse(es_ejecutado, val_h, 0)
          kg_prev_vec <- kg_prev_vec + val_k'''
replacement2 = '''          es_ejecutado <- (d %in% c("Lunes", "Martes"))
          ha_prev_vec <- ha_prev_vec + ifelse(es_ejecutado, val_h, 0)
          kg_prev_vec <- kg_prev_vec + ifelse(es_ejecutado, val_k, 0)'''
assert target2 in content, 'target2 not found'
content = content.replace(target2, replacement2, 1)

# 3. Ha_Prog and Kg_Prog
target3 = '''          Ha_Prog      = case_when(
            coalesce(Ha_Prog_Sap, 0) > 0                        ~ pmin(Área, Ha_Prog_Sap),
            coalesce(Kg_Prog_Sap, 0) > 0                        ~ Área,
            (dia_inicio_idx > 1 & coalesce(Ha_Lunes_Rc, 0) > 0) ~ pmin(Área, Ha_Lunes_Rc),
            (dia_inicio_idx > 2 & coalesce(Ha_Martes_Rc, 0) > 0) ~ pmin(Área, Ha_Martes_Rc),
            coalesce(Ha_Prog, 0) > 0 & coalesce(Kg_Prog, 0) > 0 ~ pmin(Área, Ha_Prog),
            TRUE                                                ~ 0
          ),
          Kg_Prog      = case_when(
            coalesce(Kg_Prog_Sap, 0) > 0 ~ Kg_Prog_Sap,
            coalesce(Kg_Prog, 0) > 0     ~ Kg_Prog,
            TRUE                         ~ 0
          ),'''

replacement3 = '''          Ha_Prog      = case_when(
            coalesce(Ha_Prog_Sap, 0) > 0                        ~ pmin(Área, Ha_Prog_Sap),
            coalesce(Kg_Prog_Sap, 0) > 0                        ~ Área,
            (dia_inicio_idx > 1 & coalesce(Ha_Lunes_Rc, 0) > 0) ~ pmin(Área, Ha_Lunes_Rc),
            (dia_inicio_idx > 2 & coalesce(Ha_Martes_Rc, 0) > 0) ~ pmin(Área, Ha_Martes_Rc),
            TRUE                                                ~ 0
          ),
          Kg_Prog      = case_when(
            coalesce(Kg_Prog_Sap, 0) > 0                        ~ Kg_Prog_Sap,
            (dia_inicio_idx > 1 & coalesce(Kg_Lunes_Rc, 0) > 0) ~ Kg_Lunes_Rc,
            (dia_inicio_idx > 2 & coalesce(Kg_Martes_Rc, 0) > 0) ~ Kg_Martes_Rc,
            TRUE                                                ~ 0
          ),'''

assert target3 in content, 'target3 not found'
content = content.replace(target3, replacement3, 1)

# Normalize line endings to CRLF
content_crlf = content.replace('\r\n', '\n').replace('\n', '\r\n')
with open('Actualizacion Semanal/app.R', 'w', encoding='utf-8', newline='') as f:
    f.write(content_crlf)

print('APPLIED SUCCESSFULLY')
