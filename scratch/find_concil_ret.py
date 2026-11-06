import subprocess

res = subprocess.check_output(['git', 'show', 'HEAD:Actualizacion Semanal/app.R'], encoding='utf-8')
lines = res.splitlines()

for i, line in enumerate(lines):
    if 'return(res' in line or 'return(df_concil' in line or 'obtener_cosecha_conciliada_semana' in line:
        print(f"Line {i+1}: {line.strip()}")
