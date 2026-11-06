with open('D:/ACP/Actualizacion data maestra/actualizar_data_maestra.R', 'rb') as f:
    content = f.read()

target = b'    codigo_norm = norm_codigo_valvula(codigo),\r\n    M\xc3\xb3dulo      = as.character'
replacement = b'    codigo_norm = norm_codigo_valvula(codigo),\r\n    Sector      = case_when(as.character(M\xc3\xb3dulo) == "12" ~ "9", TRUE ~ as.character(Sector)),\r\n    M\xc3\xb3dulo      = as.character'

if target in content:
    content = content.replace(target, replacement, 1)
    with open('D:/ACP/Actualizacion data maestra/actualizar_data_maestra.R', 'wb') as f:
        f.write(content)
    print("SUCCESS: actualizar_data_maestra.R updated!")
else:
    print("Target not found in actualizar_data_maestra.R")
