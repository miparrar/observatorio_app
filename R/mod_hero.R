# mod_hero.R - hero del perfil: presenta la empresa antes que sus números.
# Portada editorial (nombre, razón social, descripción y ficha institucional) +
# zócalo con los 3 KPI centrales del observatorio: tasa de ganancia, masa de
# ganancia y KTA.

mod_hero_ui <- function(id) {
  ns <- NS(id)
  div(class = "perfil-portada",
    uiOutput(ns("identidad")),
    div(
      class = "hero-kpis",
      div(class = "hero-kpi",
        div(class = "kpi-etiqueta", "Tasa de ganancia"),
        div(class = "kpi-valor", textOutput(ns("kpi_tg"), inline = TRUE)),
        div(class = "hero-kpi-contexto",
            uiOutput(ns("kpi_tg_delta"), inline = TRUE),
            " · prom. hist. ", textOutput(ns("kpi_tg_prom"), inline = TRUE))
      ),
      div(class = "hero-kpi",
        div(class = "kpi-etiqueta", "Masa de ganancia"),
        div(class = "kpi-valor", textOutput(ns("kpi_masa"), inline = TRUE)),
        div(class = "hero-kpi-contexto",
            "Ganancia bruta · ", textOutput(ns("kpi_anio"), inline = TRUE))
      ),
      div(class = "hero-kpi",
        div(class = "kpi-etiqueta", "Capital total adelantado"),
        div(class = "kpi-valor", textOutput(ns("kpi_kta"), inline = TRUE)),
        div(class = "hero-kpi-contexto", "KTA · denominador de la tasa")
      )
    )
  )
}

mod_hero_server <- function(id, empresa_id, datos) {
  moduleServer(id, function(input, output, session) {

    ficha <- reactive(empresas_ficha[[empresa_id()]])
    datos_validos <- reactive(datos() |> filter(!is.na(tasa_ganancia)))
    tg_ok <- reactive(nrow(datos_validos()) > 0)
    ultimo <- reactive({
      d <- datos_validos()
      d[nrow(d), ]
    })

    output$identidad <- renderUI({
      f <- ficha()
      productos <- if (!is.null(f$productos)) {
        paste(f$productos, collapse = " · ")
      } else if (!is.null(f$subsector)) {
        f$subsector
      } else {
        NULL
      }
      controlador <- if (is.null(f$controlador)) {
        NULL
      } else if (is.null(f$controlador_pct)) {
        f$controlador
      } else {
        glue("{f$controlador} ({f$controlador_pct}%)")
      }

      # Si la única fila repite a la propia empresa (Codelco -> Codelco), no
      # agrega información: el controlador ya comunica la propiedad real.
      operadores <- if (is.null(f$operadores)) list() else f$operadores
      operador_redundante <- length(operadores) == 1 &&
        !is.null(operadores[[1]]$nombre) &&
        identical(
          tolower(trimws(as.character(operadores[[1]]$nombre))),
          tolower(trimws(as.character(f$nombre_corto)))
        )
      mostrar_propiedad <- length(operadores) > 0 && !operador_redundante

      div(
        class = "perfil-ficha",
        div(
          class = "perfil-identidad",
          div(class = "hero-copy",
            tags$h1(class = "hero-nombre", f$nombre_corto),
            if (!is.null(f$razon_social)) div(class = "hero-razon", f$razon_social),
            if (!is.null(f$descripcion_editorial)) p(class = "hero-editorial", f$descripcion_editorial)
          ),
          tags$aside(class = "hero-dossier",
            div(class = "hero-meta-grid",
              ficha_campo("Controlador", controlador),
              ficha_campo("Constitución", f$constitucion),
              ficha_campo("Ubicación",    f$ubicacion),
              ficha_campo("Productos", productos)
            ),
            if (mostrar_propiedad) div(
              class = "hero-propiedad",
              div(class = "hero-propiedad-titulo", "Estructura de propiedad"),
              lapply(operadores, function(o) {
                detalle <- c(
                  if (!is.null(o$pct)) glue("{o$pct}%"),
                  if (!is.null(o$pais)) o$pais
                )
                div(class = "hero-propietario",
                  span(class = "hero-propietario-nombre", o$nombre),
                  span(class = "hero-propietario-detalle", paste(detalle, collapse = " · "))
                )
              })
            )
          )
        )
      )
    })

    output$kpi_tg   <- renderText(if (tg_ok()) fmt_pct(ultimo()$tasa_ganancia) else "n/c")
    output$kpi_masa <- renderText(if (tg_ok()) fmt_mus(ultimo()$ganancia_bruta) else "n/c")
    output$kpi_kta  <- renderText(if (tg_ok()) fmt_mus(ultimo()$kta) else "n/c")
    output$kpi_anio <- renderText(if (tg_ok()) ultimo()$anio else "sin datos")
    output$kpi_tg_prom <- renderText(
      if (tg_ok()) fmt_pct(mean(datos_validos()$tasa_ganancia)) else "-")

    output$kpi_tg_delta <- renderUI({
      if (!tg_ok()) return(span(class = "kpi-delta", "US-GAAP sin trabajo"))
      d     <- datos_validos()
      n     <- nrow(d)
      if (n < 2) return(span(class = "kpi-delta", "s/d"))
      delta <- (d$tasa_ganancia[n] - d$tasa_ganancia[n - 1]) * 100
      if (is.na(delta)) return(span(class = "kpi-delta", "s/d"))
      glifo <- if (delta >= 0) "▲" else "▼"
      span(class = "kpi-delta", paste(glifo, fmt_pp(delta)))
    })
  })
}
