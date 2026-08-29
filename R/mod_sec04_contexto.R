# mod_sec04_contexto.R - Sección 04: Contexto productivo (COCHILCO).
# Producción de cobre mina: la empresa del perfil vs el total país, en absoluto
# y en participación relativa (%). El precio del cobre vive en la sección 01
# (junto a la tasa). Solo lee cache/cochilco/ (make cochilco).

mod_sec04_ui <- function(id) {
  ns <- NS(id)
  tagList(
    section_header("04", "Contexto: producción de cobre (Chile)"),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Producción de cobre mina - absoluta (miles de TM de fino)"),
        highchartOutput(ns("chart_abs"), height = "340px"),
        nota_pie(ns("nota_abs"))
      ),
      card(
        card_header("Participación en la producción nacional (%)"),
        highchartOutput(ns("chart_rel"), height = "340px"),
        nota_pie(texto = "Producción de la empresa sobre el total país. Fuente: COCHILCO.")
      )
    )
  )
}

mod_sec04_server <- function(id, empresa_id) {
  moduleServer(id, function(input, output, session) {

    total <- produccion_cu |> filter(item == "Total Chile")

    empresa_prod <- reactive({
      it <- item_produccion[[empresa_id()]]
      if (is.na(it)) return(produccion_cu[0, ])
      produccion_cu |> filter(item == it)
    })

    # Empresa y total sobre el eje de años del total (ambos de COCHILCO)
    combinado <- reactive({
      ep <- empresa_prod()
      total |>
        select(anio, total = valor) |>
        left_join(ep |> select(anio, empresa = valor), by = "anio") |>
        arrange(anio)
    })

    output$nota_abs <- renderText({
      it <- item_produccion[[empresa_id()]]
      if (is.na(it)) return("Esta empresa no tiene serie de producción individual en COCHILCO.")
      glue("Serie COCHILCO: '{it}' vs total país. Producción de mina, no ventas.")
    })

    output$chart_abs <- renderHighchart({
      d <- combinado()
      req(nrow(d) > 0)
      hc <- grafico_base(d$anio, titulo_y = "miles TM") |>
        hc_add_series(name = "Total Chile", type = "line",
                      data = round(d$total, 1), color = paleta$datos[[2]])
      if (any(!is.na(d$empresa))) {
        hc <- hc |>
          hc_add_series(name = nombre_empresa[[empresa_id()]], type = "column",
                        data = round(d$empresa, 1), color = paleta$primario)
      }
      hc |> hc_tooltip(formatter = tt_num) |> hc_legend(enabled = TRUE)
    })

    output$chart_rel <- renderHighchart({
      d <- combinado() |> mutate(part = 100 * empresa / total)
      req(any(!is.na(d$part)))
      grafico_base(d$anio, titulo_y = "% del total país", formato_y = "{value}%") |>
        hc_add_series(name = glue("{nombre_empresa[[empresa_id()]]} / total"),
                      type = "area", data = round(d$part, 1),
                      color = paleta$primario, fillOpacity = 0.15) |>
        hc_tooltip(formatter = tt_pct) |>
        hc_legend(enabled = FALSE)
    })
  })
}
