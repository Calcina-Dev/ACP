import subprocess

res = subprocess.check_output(['git', 'show', 'HEAD:Actualizacion Semanal/app.R'], encoding='utf-8')
lines = res.splitlines()

for i, line in enumerate(lines):
    if 'kg_real_turno <-' in line:
        print(f"kg_real_turno at line {i+1}")
        for j in range(max(0, i-20), min(i+25, len(lines))):
            print(f"  {lines[j]}")
