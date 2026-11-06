import subprocess

res = subprocess.check_output(['git', 'show', 'HEAD:Rutas de Cosecha/valvulas_normalizadas.rds'])
with open('D:/ACP/scratch/valv_head.rds', 'wb') as f:
    f.write(res)
print("Extracted successfully!")
