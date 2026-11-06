with open('D:/ACP/Actualizacion Semanal/app.R.bak_sector_m12', 'rb') as f:
    for i in range(5):
        print(f"{i+1}: {repr(f.readline())}")
