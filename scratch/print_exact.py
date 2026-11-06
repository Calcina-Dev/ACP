with open('D:/ACP/Actualizacion Semanal/app.R', 'r', encoding='utf-8', errors='replace') as f:
    lines = f.readlines()

print("Line 1251..1255:")
for l in lines[1251:1255]:
    print(repr(l))

print("\nLine 1728..1735:")
for l in lines[1728:1735]:
    print(repr(l))

print("\nLine 9982..9992:")
for l in lines[9982:9992]:
    print(repr(l))
