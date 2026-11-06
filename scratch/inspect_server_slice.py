with open('Actualizacion Semanal/app.R', 'r', encoding='utf-8') as f:
    lines = f.readlines()

start_idx = None
end_idx = None

for i, line in enumerate(lines):
    if '# BOTÓN AJUSTAR SEMANA (COMBINATORIA RENDIMIENTO' in line:
        start_idx = i - 1  # the comment separator line
    if 'sprintf("↺ Se removieron %d registros automáticos' in line:
        end_idx = i + 4  # until the closing '  })'

print('start_idx:', start_idx, 'lines[start_idx]:', repr(lines[start_idx]))
print('end_idx:', end_idx, 'lines[end_idx]:', repr(lines[end_idx]))
print('Next lines:', repr(lines[end_idx+1]), repr(lines[end_idx+2]))
