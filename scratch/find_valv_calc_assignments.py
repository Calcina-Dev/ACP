with open('Actualizacion Semanal/app.R', 'r', encoding='utf-8') as f:
    text = f.read()

import re
matches = [m.start() for m in re.finditer(r'valvulas_calc\s*<-', text)]
for idx in matches:
    print("--- valvulas_calc assignment at", idx, "---")
    print(text[idx:idx+350])
