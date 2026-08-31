# mod_sec04_deuda.R - Sección 04: Deuda.
# Deuda desde cache/<e>/endeudamiento.parquet, en los tres cortes de la
# receta: stock (financiera vs intragrupo), posición por cobrar/por pagar/neto,
# y capacidad de pago (años de masa de ganancia para pagar la deuda neta).
# La tabla de propiedad se movió al hero del perfil.

mod_sec04_ui <- function(id) {
  ns <- NS(id)
  tagList(
    section_header("04", "Deuda"),
    section_intro("Las estructuras de deuda que sostienen —y condicionan— el negocio."),
    card(
      card_header("Stock de deuda: financiera vs intragrupo (MMUS$)"),
      highchartOutput(ns("chart_deuda"), height = "320px"),
      nota_pie(ns("nota_deuda"))
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Posición comercial: por cobrar, por pagar y neto (MMUS$)"),
        highchartOutput(ns("chart_posicion"), height = "340px"),
        nota_pie(ns("nota_posicion"))
      ),
      card(
        card_header("Capacidad de pago"),
        highchartOutput(ns("chart_capacidad"), height = "340px"),
        nota_pie(texto = paste(
          "Años de pago = deuda neta (deuda total - efectivo) / masa de ganancia del año.",
          "Bajo cero = el efectivo supera toda la deuda (caja neta)."))
      )
    )
  )
}

mod_sec04_server <- function(id, empresa_id) {
  moduleServer(id, function(input, output, session) {

    d <- reactive({
      series_endeu |> filter(empresa == empresa_id()) |> con_huecos()
    })

    output$nota_deuda <- renderText({
      dd  <- d()
      rel <- dd$anio[!is.na(dd$deuda_relacionadas) & dd$deuda_relacionadas > 0]
      partes <- c(
        "La deuda intragrupo (con la matriz o el grupo) se separa: además de financiamiento es un canal de apropiación vía intereses.",
        if (empresa_id() == "escondida") "Desde 2020 la línea reportada agrupa préstamos y leasing.",
        if (empresa_id() == "teck" && length(rel) > 0) "QB2 se financió casi entero con deuda de la matriz."
      )
      paste(partes, collapse = " ")
    })

    output$chart_deuda <- renderHighchart({
      dd <- d()
      grafico_base(dd$anio, titulo_y = "MMUS$") |>
        hc_plotOptions(column = list(stacking = "normal")) |>
        hc_add_series(name = "Deuda financiera", type = "column",
                      data = a_mmus(dd$deuda_financiera), color = paleta$datos[[2]]) |>
        hc_add_series(name = "Deuda con relacionadas", type = "column",
                      data = a_mmus(dd$deuda_relacionadas), color = paleta$datos[[5]]) |>
        hc_add_series(name = "Efectivo", type = "line",
                      data = a_mmus(dd$efectivo), color = paleta$datos[[4]]) |>
        hc_tooltip(formatter = tt_mmus) |>
        hc_legend(enabled = TRUE)
    })

    output$nota_posicion <- renderText({
      if (empresa_id() %in% c("el_abra", "collahuasi")) {
        paste("OJO: esta empresa vende vía su grupo - parte de lo comercial vive",
              "en cuentas con relacionadas y el neto comercial puede subestimar.",
              "Ver política de relacionadas (pendiente declarado).")
      } else {
        "Neto = por cobrar - por pagar. Positivo: la empresa financia a sus clientes; negativo: se financia con proveedores."
      }
    })

    output$chart_posicion <- renderHighchart({
      dd <- d()
      grafico_base(dd$anio, titulo_y = "MMUS$") |>
        hc_add_series(name = "Por cobrar", type = "line",
                      data = a_mmus(dd$por_cobrar), color = paleta$datos[[4]]) |>
        hc_add_series(name = "Por pagar", type = "line",
                      data = a_mmus(dd$por_pagar), color = paleta$datos[[5]]) |>
        hc_add_series(name = "Neto comercial", type = "column",
                      data = a_mmus(dd$comercial_neto), color = paleta$datos[[2]]) |>
        hc_tooltip(formatter = tt_mmus) |>
        hc_legend(enabled = TRUE)
    })

    output$chart_capacidad <- renderHighchart({
      dd <- d()
      grafico_base(dd$anio, titulo_y = "Años de masa de ganancia") |>
        hc_add_series(name = "Años para pagar la deuda neta", type = "column",
                      data = round(dd$anios_pago, 2), color = paleta$datos[[1]]) |>
        hc_tooltip(formatter = tt_num) |>
        hc_legend(enabled = FALSE)
    })
  })
}
