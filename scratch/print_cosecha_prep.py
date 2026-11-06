with open('D:/ACP/Actualizacion Semanal/app.R', 'r', encoding='latin-1', newline='') as f:
    lines = f.readlines()

print("Line 5364..5378:")
for l in lines[5364:5378]:
    print(repr(l))

print("\nLine 5391..5405:")
for l in lines[5391:5405]:
    print(repr(l))
