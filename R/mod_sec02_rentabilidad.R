# mod_sec02_rentabilidad.R - Sección 02: Rentabilidad.
# Cadena causal completa: la tasa -> ganancia vs capital (base 100) ->
# qué empuja el capital (componentes del KTA) -> composición del costo ->
# ganancia vs costo. Cada gráfico lleva su explicación; la trazabilidad
# (origen del numerador y la depreciación) va como nota al pie, no escondida.

mod_sec02_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "perfil-seccion perfil-seccion-rentabilidad",
    section_header("02", "Rentabilidad"),
    uiOutput(ns("estado")),
    conditionalPanel(
      condition = "output.tiene_tg", ns = ns,

      sub_header("Evolución de la rentabilidad"),

      card(
        card_header(panel_header("Tasa de ganancia y precio del cobre", "por año")),
        highchartOutput(ns("chart_tasa"), height = "380px"),
        nota_pie(ns("nota_serie"))
      ),

      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header(panel_header("Ganancia y capital adelantado", "base 100")),
          highchartOutput(ns("chart_componentes"), height = "400px")
        ),
        card(
          card_header(panel_header("Componentes del capital", "base 100")),
          highchartOutput(ns("chart_kta_comp"), height = "400px")
        )
      ),

      sub_header("Estructura de costos"),

      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header(panel_header("Composición del costo", "MMUS$")),
          highchartOutput(ns("chart_costo"), height = "360px"),
          nota_pie(ns("nota_costo"))
        ),
        card(
          card_header(panel_header("Ganancia bruta y costo de ventas", "MMUS$")),
          highchartOutput(ns("chart_ganancia"), height = "360px")
        )
      )
    )
  )
}

mod_sec02_server <- function(id, datos) {
  moduleServer(id, function(input, output, session) {

    # Empresas sin TG (SEC/US-GAAP): la sección lo DECLARA en vez de esconderse.
    output$tiene_tg <- reactive(nrow(datos()) > 0)
    outputOptions(output, "tiene_tg", suspendWhenHidden = FALSE)

    output$estado <- renderUI({
      if (nrow(datos()) > 0) return(NULL)
      pendiente_card(
        "Tasa de ganancia no computable con esta fuente",
        "Esta empresa reporta a la SEC en US-GAAP, que no exige desglosar el ",
        "gasto en trabajo (el capital variable del KTA). Sin esa pieza, la ",
        "metodología del observatorio no se puede aplicar honestamente. Las ",
        "secciones de apropiación y deuda sí operan con lo que la fuente trae; ",
        "la rentabilidad se sumará si la filer publica el dato (p.ej. vía 20-F IFRS)."
      )
    })

    # ── Notas al pie: procedencia y comparabilidad (trazabilidad) ────────────
    output$nota_serie <- renderText({
      req(nrow(datos()) > 0)
      dd     <- datos() |> filter(!is.na(tasa_ganancia))
      anios  <- datos() |> filter(!is.na(numerador_origen))
      der    <- anios$anio[anios$numerador_origen != "reportado"]
      dep    <- anios$anio[anios$dep_origen != "DepreciationExpense"]
      partes <- c(
        glue("Fuente: EEFF CMF ({min(dd$anio)}-{max(dd$anio)})."),
        if (length(der) == nrow(anios)) "Numerador derivado (ingresos - costo) en toda la serie."
        else if (length(der) > 0) glue("Numerador derivado (ingresos - costo) en {rangos(der)}."),
        if (length(dep) == nrow(anios)) "Depreciación del ejercicio (nota PP&E) en toda la serie."
        else if (length(dep) > 0) glue("Depreciación del ejercicio (nota PP&E) en {rangos(dep)}."),
        "Un corte en la línea es un año sin dato en la fuente."
      )
      paste(partes, collapse = " ")
    })

    output$nota_costo <- renderText({
      req(nrow(datos()) > 0)
      dd  <- datos() |> filter(!is.na(numerador_origen))
      if (length(unique(dd$numerador_origen)) > 1) {
        paste("OJO: el alcance del costo cambia entre regímenes contables",
              "(costo de ventas estricto vs costos totales condensados);",
              "los niveles no son comparables a lo largo de toda la serie.")
      } else {
        "Insumos = residual del costo tras trabajo y depreciación."
      }
    })

    # ── Serie histórica de la tasa + precio del cobre ────────────────────────
    # Doble eje: la tasa (%) y el precio BML (cUS$/lb) comparten los años de la
    # TG para mostrar el co-movimiento (la rentabilidad sigue al ciclo del precio,
    # que es EXTERNO a la empresa). Precio recortado a los años de la serie.
    output$chart_tasa <- renderHighchart({
      req(nrow(datos()) > 0)
      dd <- datos()
      max_tasa <- ceiling(max(dd$tasa_ganancia * 100, na.rm = TRUE) / 50) * 50
      pr <- precio_cobre |> filter(anio %in% dd$anio)
      precio_por_anio <- setNames(round(pr$valor, 1), pr$anio)
      grafico_base(dd$anio) |>
        hc_yAxis_multiples(
          list(title = list(text = "Tasa de ganancia (%)", style = est_eje),
               labels = list(format = "{value}%", style = est_eje), max = max_tasa),
          list(title = list(text = "Precio cobre (cUS$/lb)", style = est_eje),
               labels = list(style = est_eje), opposite = TRUE)
        ) |>
        hc_add_series(name = "Tasa de ganancia", type = "line", yAxis = 0,
                      data = round(dd$tasa_ganancia * 100, 1), color = paleta$primario,
                      marker = list(enabled = TRUE, radius = 3)) |>
        hc_add_series(name = "Precio del cobre (BML)", type = "line", yAxis = 1,
                      data = unname(precio_por_anio[as.character(dd$anio)]),
                      color = paleta$datos[[3]], dashStyle = "ShortDash",
                      marker = list(enabled = FALSE)) |>
        hc_tooltip(shared = TRUE) |>
        hc_legend(enabled = TRUE)
    })

    # ── Ganancia vs capital - índice base 100 ────────────────────────────────
    output$chart_componentes <- renderHighchart({
      req(nrow(datos()) > 0)
      dd <- datos()
      grafico_base(dd$anio, titulo_y = glue("Índice (base 100 = {dd$anio[1]})")) |>
        hc_subtitle(text = "La tasa sube cuando la ganancia crece más rápido que el capital",
                    style = est_eje) |>
        hc_add_series(name = "Ganancia bruta (numerador)", type = "line",
                      data = base100(dd$ganancia_bruta), color = paleta$datos[[1]]) |>
        hc_add_series(name = "KTA - capital (denominador)", type = "line",
                      data = base100(dd$kta), color = paleta$datos[[2]]) |>
        hc_tooltip(formatter = tt_num) |>
        hc_legend(enabled = TRUE)
    })

    # ── Componentes del KTA en niveles ───────────────────────────────────────
    output$chart_kta_comp <- renderHighchart({
      req(nrow(datos()) > 0)
      dd <- datos()
      grafico_base(dd$anio, titulo_y = glue("Índice (base 100 = {dd$anio[1]})")) |>
        hc_add_series(name = "Capital fijo (PP&E productivo)", type = "line",
                      data = base100(dd$cap_fijo_adelantado), color = paleta$datos[[2]]) |>
        hc_add_series(name = "Circulante constante (inventarios)", type = "line",
                      data = base100(dd$circ_constante_adelantado), color = paleta$datos[[3]]) |>
        hc_add_series(name = "Capital variable (remuneraciones)", type = "line",
                      data = base100(dd$cap_variable_adelantado), color = paleta$datos[[1]]) |>
        hc_add_series(name = "Efectivo", type = "line",
                      data = base100(dd$efectivo), color = paleta$datos[[4]]) |>
        hc_add_series(name = "Cuentas comerciales netas", type = "line",
                      data = base100(dd$circ_netas_adelantado), color = paleta$datos[[5]]) |>
        hc_tooltip(formatter = tt_num) |>
        hc_legend(enabled = TRUE)
    })

    # ── Composición del costo (columnas del pipeline, apiladas) ──────────────
    output$chart_costo <- renderHighchart({
      req(nrow(datos()) > 0)
      dd <- datos()
      grafico_base(dd$anio, titulo_y = "MMUS$") |>
        hc_plotOptions(column = list(stacking = "normal")) |>
        hc_add_series(name = "Remuneraciones", type = "column",
                      data = a_mmus(dd$cap_variable_consumido), color = paleta$datos[[1]]) |>
        hc_add_series(name = "Depreciación", type = "column",
                      data = a_mmus(dd$cap_fijo_consumido), color = paleta$datos[[2]]) |>
        hc_add_series(name = "Insumos", type = "column",
                      data = a_mmus(dd$cap_circulante_consumido), color = paleta$datos[[3]]) |>
        hc_tooltip(formatter = tt_mmus) |>
        hc_legend(enabled = TRUE)
    })

    # ── Ganancia bruta vs costo - la brecha es el margen ─────────────────────
    output$chart_ganancia <- renderHighchart({
      req(nrow(datos()) > 0)
      dd <- datos()
      grafico_base(dd$anio, titulo_y = "MMUS$") |>
        hc_add_series(name = "Ganancia bruta", type = "line",
                      data = a_mmus(dd$ganancia_bruta), color = paleta$datos[[1]]) |>
        hc_add_series(name = "Costo de ventas", type = "line",
                      data = a_mmus(dd$costo_venta), color = paleta$datos[[3]]) |>
        hc_tooltip(formatter = tt_mmus) |>
        hc_legend(enabled = TRUE)
    })
  })
}
