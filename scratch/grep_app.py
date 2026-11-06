import sys

pattern = sys.argv[1] if len(sys.argv) > 1 else 'abrir_modal'
with open('Actualizacion Semanal/app.R', 'r', encoding='utf-8', errors='replace') as f:
    for i, l in enumerate(f):
        if pattern.lower() in l.lower():
            print(f'{i+1}: {l.strip()[:100]}')
