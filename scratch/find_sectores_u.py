with open('D:/ACP/Actualizacion Semanal/app.R', 'r', encoding='latin-1', newline='') as f:
    text = f.read()

idx = text.find('sectores_u <-')
print(repr(text[idx:idx+300]))
