# mod_chat_documental.R - interfaz del asistente documental.

mod_chat_documental_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "perfil-seccion perfil-seccion-chat",
    section_header("05", txt$chat_documental$titulo),
    div(
      class = "chat-intro",
      span(class = "chat-kicker", "Evidencia documental"),
      p(txt$chat_documental$descripcion)
    ),
    div(class = "chat-shell", uiOutput(ns("contenido")))
  )
}

mod_chat_documental_server <- function(
  id,
  contexto = reactive(NULL)
) {
  moduleServer(id, function(input, output, session) {
    config_path <- file.path(dir_config, "chat_documental.yaml")
    database_target <- Sys.getenv(
      "OBS_CHAT_DB_TARGET",
      unset = database_financiera_target
    )
    usa_conexion_compartida <- identical(
      database_target,
      database_financiera_target
    )

    error_config <- tryCatch(
      {
        config <- leer_config_chat_documental(config_path)
        NULL
      },
      error = function(error) conditionMessage(error)
    )

    con <- NULL
    error_conexion <- NULL
    if (is.null(error_config)) {
      if (usa_conexion_compartida) {
        con <- db_connection
      } else {
        error_conexion <- tryCatch(
          {
            con <- abrir_corpus_documental(database_target, read_only = TRUE)
            NULL
          },
          error = function(error) conditionMessage(error)
        )
      }
    }

    estado <- if (!is.null(error_config)) {
      list(ok = FALSE, message = error_config)
    } else if (!is.null(error_conexion)) {
      list(ok = FALSE, message = error_conexion)
    } else if (!corpus_documental_disponible(
      con,
      config$database$schema
    )) {
      list(ok = FALSE, message = txt$chat_documental$corpus_faltante)
    } else if (!nzchar(Sys.getenv("OPENAI_API_KEY"))) {
      list(ok = FALSE, message = txt$chat_documental$credencial_faltante)
    } else {
      list(ok = TRUE, message = NULL)
    }

    if (!estado$ok) {
      if (!usa_conexion_compartida && !is.null(con)) {
        cerrar_corpus_documental(con)
      }
      output$contenido <- renderUI({
        bslib::card(
          bslib::card_header(txt$chat_documental$no_disponible),
          bslib::card_body(p(estado$message))
        )
      })
      return(invisible(NULL))
    }

    if (!usa_conexion_compartida) {
      session$onSessionEnded(function() {
        cerrar_corpus_documental(con)
      })
    }

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
