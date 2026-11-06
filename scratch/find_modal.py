with open('Actualizacion Semanal/app.R', 'r', encoding='utf-8', errors='replace') as f:
    lines = f.readlines()
for i, l in enumerate(lines):
    if 'abrir_modal_editar_celda' in l:
        print(f'{i+1}: {l.strip()}')
