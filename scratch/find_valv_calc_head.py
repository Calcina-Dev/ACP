import subprocess

res = subprocess.check_output(['git', 'show', 'HEAD:Actualizacion Semanal/app.R'], encoding='utf-8')
lines = res.splitlines()

for i, line in enumerate(lines):
    if 'valvulas_calc <-' in line:
        print(f"valvulas_calc at line {i+1}")
        for j in range(i, min(i+35, len(lines))):
            print(f"  {lines[j]}")
