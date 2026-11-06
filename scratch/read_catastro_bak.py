with open('D:/ACP/Actualizacion Semanal/app.R.bak_sector_m12', 'r', encoding='utf-8') as f:
    lines = f.readlines()

for i in range(490, 540):
    print(f'{i+1}: {lines[i]}', end='')
