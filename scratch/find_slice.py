with open('Actualizacion Semanal/app.R', 'r', encoding='utf-8') as f:
    text = f.read()

start_marker = "output$ui_modal_kgha_cuerpo <- renderUI({"
end_marker = "observeEvent(input$modal_kgha_btn_restablecer, {"

pos_start = text.find(start_marker)
pos_end = text.find(end_marker)

print(f"Start pos: {pos_start}, End pos: {pos_end}")
print("Slice to be replaced:")
print(text[pos_start:pos_start+200])
print("...")
print(text[pos_end-200:pos_end])
