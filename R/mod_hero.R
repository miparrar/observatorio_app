# mod_hero.R - hero del perfil: presenta la empresa antes que sus números.
# Tarjeta de identidad oscura (nombre, razón social, descripción editorial) +
# ficha técnica (src/empresas.yaml) + strip de los 3 KPIs centrales del
# observatorio: tasa de ganancia, masa de ganancia y KTA.

mod_hero_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("identidad")),
    layout_columns(
      col_widths = c(4, 4, 4),
      class = "kpi-strip",
      card(card_body(
        div(class = "kpi-etiqueta", "Tasa de ganancia"),
        div(class = "kpi-valor", textOutput(ns("kpi_tg"), inline = TRUE)),
        div(class = "kpi-etiqueta mt-1",
            uiOutput(ns("kpi_tg_delta"), inline = TRUE),
            " · prom. hist. ", textOutput(ns("kpi_tg_prom"), inline = TRUE))
      )),
      card(card_body(
        div(class = "kpi-etiqueta", "Masa de ganancia"),
        div(class = "kpi-valor", textOutput(ns("kpi_masa"), inline = TRUE)),
        div(class = "kpi-etiqueta mt-1", "ganancia bruta del ejercicio")
      )),
      card(card_body(
        div(class = "kpi-etiqueta", "Capital total adelantado"),
        div(class = "kpi-valor", textOutput(ns("kpi_kta"), inline = TRUE)),
        div(class = "kpi-etiqueta mt-1", "KTA - denominador de la tasa")
      ))
    )
  )
}

mod_hero_server <- function(id, empresa_id, datos) {
  moduleServer(id, function(input, output, session) {

    ficha <- reactive(empresas_ficha[[empresa_id()]])
    tg_ok <- reactive(nrow(datos()) > 0)
    ultimo <- reactive({
      d <- datos() |> filter(!is.na(tasa_ganancia))
      d[nrow(d), ]
    })

    output$identidad <- renderUI({
      f <- ficha()
      operadores <- paste(
        vapply(f$operadores, function(o) glue("{o$nombre} ({o$pct}%)"), character(1)),
        collapse = " · "
      )
      div(
        class = "hero-card",
        layout_columns(
          col_widths = c(7, 5),
          div(
            div(class = "hero-nombre", f$nombre_corto),
            div(class = "hero-razon", f$razon_social),
            div(class = "hero-editorial", f$descripcion_editorial)
          ),
          layout_columns(
            col_widths = c(6, 6),
            ficha_campo("Tipo",         f$tipo),
            ficha_campo("Controlador",  glue("{f$controlador} ({f$controlador_pct}%)")),
            ficha_campo("Constitución", f$constitucion),
            ficha_campo("Ubicación",    f$ubicacion),
            ficha_campo("Subsector",    f$subsector),
            ficha_campo("Propiedad",    operadores)
          )
        )
      )
    })

    output$kpi_tg   <- renderText(if (tg_ok()) fmt_pct(ultimo()$tasa_ganancia) else "n/c")
    output$kpi_masa <- renderText(if (tg_ok()) fmt_mus(ultimo()$ganancia_bruta) else "n/c")
    output$kpi_kta  <- renderText(if (tg_ok()) fmt_mus(ultimo()$kta) else "n/c")
    output$kpi_tg_prom <- renderText(
      if (tg_ok()) fmt_pct(mean(datos()$tasa_ganancia, na.rm = TRUE)) else "-")

    output$kpi_tg_delta <- renderUI({
      if (!tg_ok()) return(span(class = "kpi-delta", "US-GAAP sin trabajo"))
      d     <- datos()
      n     <- nrow(d)
      delta <- (d$tasa_ganancia[n] - d$tasa_ganancia[n - 1]) * 100
      if (is.na(delta)) return(span(class = "kpi-delta", "s/d"))
      glifo <- if (delta >= 0) "▲" else "▼"
      span(class = "kpi-delta", paste(glifo, fmt_pp(delta)))
    })
  })
}
