import re

with open('Actualizacion Semanal/app.R', 'r', encoding='utf-8') as f:
    content = f.read()

# 1. Update callback in DT
target1 = """        // 1. Mostrar cursor pointer sobre celdas interactivas de días y proyección
        table.on('mouseenter', 'tbody td', function() {
          var cell = table.cell(this);
          if (!cell.index()) return;
          var colIdx = cell.index().column;
          var colDias = [5, 16, 17, 18, 19, 20, 21];
          if (colDias.indexOf(colIdx) !== -1) {
            $(this).css('cursor', 'pointer');
          }
        });

        // 2. Clic en celda para abrir modal interactivo de detalle / cálculo / edición
        table.on('click', 'tbody td', function(e) {
          var cell = table.cell(this);
          if (!cell.index()) return;
          var colIdx = cell.index().column;
          var rowIdx = cell.index().row;
          var rowData = table.row(rowIdx).data();
          if (!rowData) return;
          
          var colDiasMap = { 5: 'Proyección', 16: 'Lunes', 17: 'Martes', 18: 'Miércoles', 19: 'Jueves', 20: 'Viernes', 21: 'Sábado' };
          if (colDiasMap.hasOwnProperty(colIdx)) {
            var lockMap = { 5: 32, 16: 33, 17: 34, 18: 35, 19: 36, 20: 37, 21: 38 };
            if (isLocked === true || isLocked === 1 || isLocked === 'TRUE') {
              Shiny.setInputValue('alerta_celda_bloqueada', { time: new Date().getTime() }, { priority: 'event' });
              return;
            }"""

# Check if target1 matches exactly or if isLocked had lockMap
print("Target 1 in content:", target1 in content)
if not (target1 in content):
    # let's find the exact text in content around callback = JS
    idx = content.find("var colDiasMap =")
    if idx != -1:
        print("Surrounding text in content:\n", content[idx-300:idx+400])
