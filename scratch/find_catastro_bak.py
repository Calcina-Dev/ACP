with open('D:/ACP/Actualizacion Semanal/app.R.bak_sector_m12', 'r', encoding='utf-8') as f:
    text = f.read()

idx = text.find('catastro <- if (exists("df_valvulas")')
print(repr(text[idx:idx+400]))
