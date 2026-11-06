import subprocess

res = subprocess.check_output(['git', 'show', 'HEAD:Actualizacion Semanal/app.R'], encoding='utf-8')
lines = res.splitlines()

for i, line in enumerate(lines):
    if 'rend_turno <-' in line:
        print(f"rend_turno at line {i+1}")
        for j in range(i, min(i+45, len(lines))):
            print(f"  {lines[j]}")
