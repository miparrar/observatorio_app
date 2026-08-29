# mod_datos.R - pestaña Datos: descarga abierta de las series del observatorio.
# Filosofía WID: los datos que alimentan cada gráfico se descargan tal cual
# (sin redondeo, punto decimal, UTF-8), en CSV o XLSX y en dos orientaciones:
#   vertical   = formato largo/tidy (empresa, anio, indicador, valor)
#   horizontal = una fila por empresa x indicador, años en columnas
# El módulo solo LEE de cache/ y pivotea para presentación - no calcula.

DATASETS_DATOS <- c(
  "Tasa de ganancia (y componentes)" = "tasa_ganancia",
  "Endeudamiento"                    = "endeudamiento",
  "Distribución de la plusvalía"     = "distribucion",
  "Hechos canónicos (base completa)" = "hechos_canonicos"
)

mod_datos_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(3, 3, 2, 2, 2),
      selectInput(ns("dataset"), "Serie", choices = DATASETS_DATOS),
      selectInput(ns("empresas"), "Empresas", choices = NULL, multiple = TRUE),
      radioButtons(ns("orientacion"), "Orientación",
                   choices = c("Vertical (tidy)" = "vertical",
                               "Horizontal (años en columnas)" = "horizontal")),
      radioButtons(ns("formato"), "Formato",
                   choices = c("CSV" = "csv", "XLSX" = "xlsx")),
      div(
        class = "d-flex align-items-end h-100 pb-3",
        downloadButton(ns("descargar"), "Descargar", class = "btn-primary")
      )
    ),
    card(
      card_header(textOutput(ns("titulo_preview"))),
      card_body(DTOutput(ns("preview"))),
      nota_pie(ns("nota_datos"))
    )
  )
}

mod_datos_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    con <- abrir_datos_financieros(database_financiera_path, read_only = TRUE)
    session$onSessionEnded(function() {
      cerrar_datos_financieros(con)
    })

    # Empresas publicadas con el dataset elegido disponible en DuckDB.
    empresas_disponibles <- reactive({
      req(input$dataset)
      table <- tablas_financieras[[input$dataset]]
      req(table)
      con_datos <- empresas_en_tabla_financiera(
        con,
        duckdb_financiero_schema,
        table
      )
      empresas_perfil[empresas_perfil %in% con_datos]
    })

    observeEvent(empresas_disponibles(), {
      disp <- empresas_disponibles()
      updateSelectInput(session, "empresas",
                        choices = setNames(disp, nombre_empresa[disp]),
                        selected = disp)
    })

    # Datos en formato largo (la base de ambas orientaciones). El canónico ya
    # es largo; los indicadores se alargan con sus columnas numéricas.
    datos_largos <- reactive({
      req(input$dataset, input$empresas)
      table <- tablas_financieras[[input$dataset]]
      req(table)
      d <- leer_tabla_financiera(
        con,
        duckdb_financiero_schema,
        table,
        input$empresas
      )
      if (input$dataset == "hechos_canonicos") {
        d |>
          transmute(empresa = empresa_nombre, rut = empresa_rut, anio = anio_dato,
                    indicador = concepto_canonico, valor,
                    unidad, label_origen, seccion_origen, valor_origen, factor_escala)
      } else {
        d |>
          select(-any_of(c("fuente", "variante"))) |>
          pivot_longer(cols = where(is.numeric) & !matches("^anio$"),
                       names_to = "indicador", values_to = "valor") |>
          mutate(unidad = if_else(
            indicador %in% c("tasa_ganancia", "rotacion", "anios_pago", "cobertura_intereses") |
              grepl("_pct$", indicador), "ratio", "MUS$")) |>
          relocate(empresa, anio, indicador, valor, unidad)
      }
    })

    exportable <- reactive({
      d <- datos_largos()
      if (input$orientacion == "vertical") return(d)
      # Horizontal: años en columnas; el resto de columnas de contexto se
      # descartan (el detalle vive en la orientación vertical).
      d |>
        select(any_of(c("empresa", "rut", "indicador", "unidad")), anio, valor) |>
        arrange(anio) |>
        pivot_wider(names_from = anio, values_from = valor)
    })

    output$titulo_preview <- renderText({
      glue("{names(DATASETS_DATOS)[DATASETS_DATOS == input$dataset]} - vista previa")
    })

    output$nota_datos <- renderText({
      paste("Sin redondeo, decimal con punto, montos en MUS$ (miles de US$),",
            "ratios como proporción (0.1 = 10%). La orientación vertical trae",
            "las columnas de trazabilidad; la horizontal solo los valores.")
    })

    output$preview <- renderDT({
      datatable(head(exportable(), 200), rownames = FALSE,
                options = list(pageLength = 10, scrollX = TRUE, dom = "tip"))
    })

    output$descargar <- downloadHandler(
      filename = function() {
        glue("observatorio_mineria_{input$dataset}_{input$orientacion}.{input$formato}")
      },
      content = function(file) {
        d <- exportable()
        if (input$formato == "csv") {
          readr::write_csv(d, file, na = "")
        } else {
          writexl::write_xlsx(d, file)
        }
      }
    )
  })
}
