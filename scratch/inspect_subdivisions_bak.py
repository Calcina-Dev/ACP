with open('D:/ACP/Actualizacion Semanal/app.R.bak_sector_m12', 'r', encoding='utf-8') as f:
    text = f.read()

import re

# Find all occurrences of 9.1 or 9.2 or 11.1 or 11.2 in app.R.bak_sector_m12
matches = re.findall(r'.{0,60}(?:9\.1|9\.2|11\.1|11\.2).{0,60}', text)
print(f"Total matches: {len(matches)}")
for m in matches[:15]:
    print(repr(m.strip()))
