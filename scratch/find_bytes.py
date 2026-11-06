with open('Actualizacion Semanal/app.R', 'rb') as f:
    raw = f.read()

idx = raw.find(b"var colDiasMap =")
print(raw[idx-200:idx+350])
