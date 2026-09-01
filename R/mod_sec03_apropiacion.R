# mod_sec03_apropiacion.R - Sección 03: Apropiación de plusvalía.
# Del excedente total (PBT + gasto financiero), cuánto se apropia cada quien:
# acreedores · Estado · dueños · lo que queda en la empresa. Solo se muestra
# para empresas con el reparto COMPLETO (receta src/recetas/distribucion.yaml);
# para el resto, el estado vacío declara qué falta (la cascada en el diccionario).

mod_sec03_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "perfil-seccion perfil-seccion-apropiacion",
    section_header("03", "Apropiación de plusvalía"),
    uiOutput(ns("contenido"))
  )
}

mod_sec03_server <- function(id, empresa_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    d <- reactive({
      series_dist |> filter(empresa == empresa_id()) |> con_huecos()
    })

    output$contenido <- renderUI({
      if (nrow(series_dist |> filter(empresa == empresa_id())) == 0) {
        if (empresa_id() %in% c("freeport", "southerncopper")) {
          return(pendiente_card(
            "En construcción - identidad del excedente pendiente en US-GAAP",
            "La cascada SEC está capturada, pero el tag GAAP de la ganancia ",
            "antes de impuestos EXCLUYE las ganancias por método de ",
            "participación (grandes en estas mineras), así que el reparto no ",
            "cuadra con la identidad excedente = acreedores + Estado + neta. ",
            "La validación bloqueó su publicación; se resuelve capturando esa ",
            "línea (ver pendientes)."
          ))
        }
        return(pendiente_card(
          "En construcción para esta empresa",
          "El reparto requiere la cascada de resultados completa (ganancia ",
          "antes de impuestos, impuesto total, ganancia neta y dividendos). ",
          "Para esta empresa la cascada aún no está mapeada en el diccionario ",
          "de captura; se suma con filas de datos, no con código."
        ))
      }
      tagList(
        layout_columns(
          col_widths = c(7, 5),
          card(
            card_header(panel_header("Distribución del excedente", "% del total")),
            highchartOutput(ns("chart_reparto"), height = "380px"),
            nota_pie(ns("nota_reparto"))
          ),
          card(
            card_header(panel_header("Distribución del excedente", "MMUS$")),
            highchartOutput(ns("chart_montos"), height = "380px")
          )
        )
      )
    })

    # Años de ganancia genuina: el reparto porcentual solo tiene sentido con
    # excedente positivo y ganancia neta positiva (en pérdida no hay plusvalía
    # que repartir; la retenida es negativa y el Estado subsidia).
    output$nota_reparto <- renderText({
      dd  <- d() |> filter(!is.na(excedente))
      roy <- dd$anio[!is.na(dd$royalty)]
      perdida <- dd$anio[!is.na(dd$neta) & dd$neta <= 0]
      partes <- c(
        "Excedente = ganancia antes de impuestos + gasto financiero.",
        "Retenida = ganancia neta - dividendos (residual).",
        if (length(roy) > 0) glue("El royalty minero viene separado en {rangos(roy)} (dentro de la barra del Estado)."),
        if (length(perdida) > 0) glue("Años de pérdida omitidos del reparto porcentual ({rangos(perdida)}); ver montos (el Estado subsidia vía crédito fiscal)."),
        if (empresa_id() == "codelco") "OJO Codelco: la Ley Reservada del Cobre (10% de ventas a FFAA) no está capturada como línea propia."
      )
      paste(partes, collapse = " ")
    })

    output$chart_reparto <- renderHighchart({
      dd <- d()
      # En años de pérdida (neta <= 0) el porcentual no aplica -> NA (hueco visible)
      solo_ganancia <- function(x) ifelse(!is.na(dd$neta) & dd$neta > 0, a_mmus(x), NA_real_)
      grafico_base(dd$anio, titulo_y = "% del excedente", formato_y = "{value}%") |>
        hc_plotOptions(column = list(stacking = "percent")) |>
        hc_add_series(name = "Acreedores (intereses)", type = "column",
                      data = solo_ganancia(dd$acreedores), color = paleta$datos[[5]]) |>
        hc_add_series(name = "Estado (impuestos)", type = "column",
                      data = solo_ganancia(dd$estado), color = paleta$datos[[3]]) |>
        hc_add_series(name = "Dueños (dividendos)", type = "column",
                      data = solo_ganancia(dd$duenos), color = paleta$datos[[1]]) |>
        hc_add_series(name = "Queda en la empresa", type = "column",
                      data = solo_ganancia(dd$retenida), color = paleta$datos[[2]]) |>
        hc_tooltip(shared = TRUE) |>
        hc_legend(enabled = TRUE)
    })

    output$chart_montos <- renderHighchart({
      dd <- d()
      grafico_base(dd$anio, titulo_y = "MMUS$") |>
        hc_add_series(name = "Excedente total", type = "line",
                      data = a_mmus(dd$excedente), color = paleta$texto) |>
        hc_add_series(name = "Dueños (dividendos)", type = "line",
                      data = a_mmus(dd$duenos), color = paleta$datos[[1]]) |>
        hc_add_series(name = "Estado (impuestos)", type = "line",
                      data = a_mmus(dd$estado), color = paleta$datos[[3]]) |>
        hc_tooltip(formatter = tt_mmus) |>
        hc_legend(enabled = TRUE)
    })
  })
}
