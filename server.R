server <- function(input, output, session) {

  observeEvent(input$btn_comenzar, {
    nav_select("nav_principal", selected = txt$nav$perfil)
  })

  observeEvent(input$nav_a_inicio, {
    nav_select("nav_principal", selected = "inicio")
  })

  # Empresa seleccionada y su serie reportada, con los años faltantes como NA
  # (un hueco debe VERSE en los gráficos, no desaparecer del eje).
  empresa_id <- reactive({
    req(input$sel_empresa)
    input$sel_empresa
  })

  datos <- reactive({
    series_tg |>
      filter(empresa == empresa_id()) |>
      con_huecos()
  })

  # Secciones del perfil (módulos curados; docs/perfil_esquema.md §6)
  mod_hero_server("hero", empresa_id, datos)
  mod_sec01_server("sec01", datos)
  mod_sec02_server("sec02", empresa_id)
  mod_sec03_server("sec03", empresa_id)
  mod_sec04_server("sec04", empresa_id)

  # Asistente documental: sin contexto cuantitativo en el piloto.
  mod_chat_documental_server("chat_documental")

  # Pestaña Datos: descarga abierta de las series (solo lee cache/ y pivotea)
  mod_datos_server("datos")

}
