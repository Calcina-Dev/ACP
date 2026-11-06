with open('D:/ACP/Actualizacion Semanal/app.R', 'r', encoding='latin-1', newline='') as f:
    text = f.read()

idx = text.find('filter(Es_Valido_Operativo)')
print(repr(text[idx:idx+350]))

idx2 = text.find('group_by(Sector, Módulo, Turno, Variedad, Dia)')
print(repr(text[idx2-250:idx2]))
