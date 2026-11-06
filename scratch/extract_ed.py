import subprocess

res = subprocess.check_output(['git', 'show', 'HEAD:Actualizacion Semanal/ediciones_manuales_cosecha.rds'])
with open('D:/ACP/scratch/ed_head.rds', 'wb') as f:
    f.write(res)
print("Extracted ed_head.rds successfully!")
