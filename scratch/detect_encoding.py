with open('D:/ACP/Actualizacion Semanal/app.R', 'rb') as f:
    raw = f.read(100000)

for enc in ['utf-8', 'latin-1', 'cp1252', 'iso-8859-1']:
    try:
        raw.decode(enc)
        print(f"Decoded with {enc} successfully")
    except UnicodeDecodeError as e:
        print(f"Failed {enc}: {e}")
