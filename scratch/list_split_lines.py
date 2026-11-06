import re

with open('D:/ACP/Actualizacion Semanal/app.R', 'r', encoding='utf-8', newline='') as f:
    lines = f.readlines()

occurrences = []
for idx, line in enumerate(lines):
    if re.search(r'9\.1|9\.2|11\.1|11\.2', line):
        occurrences.append(idx + 1)

print(f"Total lines with 9.1/9.2/11.1/11.2: {len(occurrences)}")
for ln in occurrences:
    print(f"Line {ln}: {lines[ln-1].strip()}")
