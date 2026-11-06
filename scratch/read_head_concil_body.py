import subprocess

res = subprocess.check_output(['git', 'show', 'HEAD:Actualizacion Semanal/app.R'], encoding='utf-8')
lines = res.splitlines()

for i in range(1432, 1490):
    print(f"{i+1}: {lines[i]}")
