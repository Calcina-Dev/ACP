with open('D:/ACP/Actualizacion Semanal/app.R', 'r', encoding='utf-8') as f:
    lines = f.readlines()

for i in range(1250, 1260):
    print(f"{i+1}: {repr(lines[i])}")
