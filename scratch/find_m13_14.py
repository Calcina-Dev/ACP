with open('D:/ACP/Actualizacion Semanal/app.R.bak_9_11_floral', 'r', encoding='utf-8') as f:
    text = f.read()

target = 'Módulo = case_when(\n          es_campana_2026 & Módulo %in% c("13.1", "13.2") ~ "13"'

pos = 0
while True:
    idx = text.find(target, pos)
    if idx == -1:
        break
    line_no = text[:idx].count('\n') + 1
    print(f'Match at line {line_no}')
    print(repr(text[max(0, idx-100):idx+120]))
    print('-'*50)
    pos = idx + 1
