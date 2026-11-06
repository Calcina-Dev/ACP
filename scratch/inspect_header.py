with open('D:/ACP/Actualizacion Semanal/app.R', 'rb') as f:
    for i in range(15):
        print(f"{i+1}: {repr(f.readline())}")
