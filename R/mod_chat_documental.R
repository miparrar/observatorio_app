# mod_chat_documental.R - interfaz minima del asistente documental.

mod_chat_documental_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "mb-3",
      h2(txt$chat_documental$titulo),
      p(class = "text-muted", txt$chat_documental$descripcion)
    ),
    uiOutput(ns("contenido"))
  )
}

mod_chat_documental_server <- function(
  id,
  contexto = reactive(NULL)
) {
  moduleServer(id, function(input, output, session) {
    config_path <- file.path(dir_config, "chat_documental.yaml")
    database_path <- file.path(dir_data, "cache", "observatorio_minero.duckdb")

    error_config <- tryCatch(
      {
        config <- leer_config_chat_documental(config_path)
        NULL
      },
      error = function(error) conditionMessage(error)
    )

    estado <- if (!is.null(error_config)) {
      list(ok = FALSE, message = error_config)
    } else if (!fs::file_exists(database_path)) {
      list(ok = FALSE, message = txt$chat_documental$corpus_faltante)
    } else if (!corpus_documental_disponible(
      database_path,
      config$database$schema
    )) {
      list(ok = FALSE, message = txt$chat_documental$corpus_faltante)
    } else if (!nzchar(Sys.getenv("OPENAI_API_KEY"))) {
      list(ok = FALSE, message = txt$chat_documental$credencial_faltante)
    } else {
      list(ok = TRUE, message = NULL)
    }

    if (!estado$ok) {
      output$contenido <- renderUI({
        bslib::card(
          bslib::card_header(txt$chat_documental$no_disponible),
          bslib::card_body(p(estado$message))
        )
      })
      return(invisible(NULL))
    }

    con <- abrir_corpus_documental(database_path, read_only = TRUE)
    session$onSessionEnded(function() {
      cerrar_corpus_documental(con)
    })

    contexto_inicial <- if (shiny::is.reactive(contexto)) contexto() else contexto
    chat <- crear_chat_documental(con, config, contexto_inicial)

    output$contenido <- renderUI({
      shinychat::chat_mod_ui(
        ns("chat"),
        messages = list(txt$chat_documental$saludo),
        footer = txt$chat_documental$pie
      )
    })

    shinychat::chat_mod_server("chat", client = chat)
  })
}
