with open('D:/ACP/Actualizacion Semanal/app.R', 'r', encoding='utf-8', newline='') as f:
    lines = f.readlines()

line_nums = [435, 568, 962, 1268, 2661, 5546, 9422, 9558, 9612, 11378, 11424, 12173]
for ln in line_nums:
    start = max(0, ln - 4)
    end = min(len(lines), ln + 5)
    print(f"=== LINE {ln} ===")
    for i in range(start, end):
        print(f"{i+1}: {lines[i].strip()}")
