with open("Actualizacion Semanal/app.R", "r", encoding="utf-8") as f:
    lines = f.readlines()

for i, line in enumerate(lines):
    if "Valvula_Cerro_En_Lun_Mar ~ {" in line:
        print(f"Found at line {i+1}:")
        for j in range(i-2, i+15):
            print(f"{j+1}: {repr(lines[j])}")
        break
