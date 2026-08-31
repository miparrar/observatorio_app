server <- function(input, output, session) {

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
  mod_sec01_server("sec01", empresa_id)
  mod_sec02_server("sec02", datos)
  mod_sec03_server("sec03", empresa_id)
  mod_sec04_server("sec04", empresa_id)

}
