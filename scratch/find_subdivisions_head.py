import subprocess

res = subprocess.check_output(['git', 'show', 'HEAD:Actualizacion Semanal/app.R'], encoding='utf-8')

for line_no, line in enumerate(res.splitlines(), 1):
    if 'between(Turno' in line or 'between(Turno_num' in line or 'between(Turno_Num' in line:
        print(f"Line {line_no}: {line.strip()}")
