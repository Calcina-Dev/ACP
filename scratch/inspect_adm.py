with open('D:/ACP/Actualizacion data maestra/actualizar_data_maestra.R', 'r', encoding='utf-8', newline='') as f:
    lines = f.readlines()

for i in range(234, 253):
    print(repr(lines[i]))
