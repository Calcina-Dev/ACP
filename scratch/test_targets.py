with open('Actualizacion Semanal/app.R', 'r', encoding='utf-8') as f:
    text = f.read()

target_ui = """actionButton("btn_ajustar_semana", "Ajustar Semana", class = "btn-ajustar-semana", icon = icon("calculator")),
                              actionButton("btn_deshacer_ajuste", "Deshacer Ajuste", class = "btn-deshacer-ajuste", icon = icon("rotate-left")),"""
print('Target UI count:', text.count(target_ui))

target_css = """.btn-deshacer-ajuste:active {
        transform: translateY(0) scale(0.98);
      }"""
print('Target CSS count:', text.count(target_css))

target_dr = """  # Datos filtrados según las selecciones interactivas y agrupados según 'Agrupar por'
  datos_reactivos <- reactive({"""
print('Target DR count:', text.count(target_dr))

start_btn_ajustar = """  # ============================================================================== =
  # BOTÓN AJUSTAR SEMANA (COMBINATORIA RENDIMIENTO PONDERADO + FRECUENCIAS)
  # ============================================================================== =
  observeEvent(input$btn_ajustar_semana, {"""
print('Start btn_ajustar count:', text.count(start_btn_ajustar))

end_btn_deshacer = """    showNotification(
      sprintf("↺ Se removieron %d registros automáticos. Las ediciones manuales se conservaron intactas.", n_removidos),
      type = "message",
      duration = 4
    )
  })"""
print('End btn_deshacer count:', text.count(end_btn_deshacer))
