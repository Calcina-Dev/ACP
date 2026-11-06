with open('Actualizacion Semanal/app.R', 'r', encoding='utf-8') as f:
    text = f.read()

# Normalize CRLF for matching
t1_part = "var colDias = [5, 16, 17, 18, 19, 20, 21];"
t2_part = "modal_celda_activa <- reactiveVal(NULL)"
t3_part = 'col_name <- as.character(info$colName)\n    lock_col <- if (col_name == "Proyección")'
t4_part = 'showNotification(sprintf("Se restableció la celda [%s] del Turno %s a su estado automático.", ctx$campo, ctx$fila$Turno), type = "message", duration = 3)\n  })'

print("t1 found:", t1_part in text)
print("t2 found:", t2_part in text)
print("t3 found:", t3_part in text.replace("\r\n", "\n"))
print("t4 found:", t4_part in text.replace("\r\n", "\n"))
