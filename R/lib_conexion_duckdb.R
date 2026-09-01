# lib_conexion_duckdb.R - conexion comun a DuckDB local o MotherDuck.
# La aplicacion solo abre fuentes de datos en modo lectura.

es_target_motherduck <- function(database_target) {
  length(database_target) == 1L &&
    !is.na(database_target) &&
    grepl("^md:", database_target)
}

token_motherduck <- function() {
  token <- Sys.getenv("MOTHERDUCK_TOKEN", unset = Sys.getenv("motherduck_token"))
  if (!nzchar(trimws(token))) {
    stop(
      "No se encontro MOTHERDUCK_TOKEN para conectar a MotherDuck.",
      call. = FALSE
    )
  }

  Sys.setenv(motherduck_token = token)
  invisible(token)
}

nombre_base_motherduck <- function(database_target) {
  database_name <- sub("^md:", "", database_target)
  if (!grepl("^[A-Za-z0-9_]+$", database_name)) {
    stop(
      "El nombre de la base MotherDuck debe contener solo letras, numeros y _. ",
      "Valor recibido: ", database_target,
      call. = FALSE
    )
  }
  database_name
}

resolver_target_observatorio <- function(
  local_path,
  motherduck_database,
  override = Sys.getenv("OBS_DB_TARGET")
) {
  override <- trimws(override)
  if (nzchar(override)) return(override)
  if (fs::file_exists(local_path)) return(local_path)

  if (
    length(motherduck_database) != 1L ||
      is.na(motherduck_database) ||
      !grepl("^[A-Za-z0-9_]+$", motherduck_database)
  ) {
    stop(
      "No existe la DuckDB local y falta database.motherduck_database valido ",
      "en src/duckdb.yaml.",
      call. = FALSE
    )
  }
  paste0("md:", motherduck_database)
}

directorio_extensiones_duckdb <- function(extension_directory = NULL) {
  if (
    is.null(extension_directory) ||
      length(extension_directory) == 0L ||
      is.na(extension_directory[[1]]) ||
      !nzchar(trimws(extension_directory[[1]]))
  ) {
    extension_directory <- tools::R_user_dir(
      "observatorio-minero-app",
      "cache"
    )
  }
  extension_directory <- path.expand(extension_directory[[1]])
  fs::dir_create(extension_directory, recurse = TRUE)
  extension_directory
}

configurar_conexion_observatorio <- function(
  con,
  threads = 2L,
  memory_limit = "1GB"
) {
  threads <- as.integer(threads)
  if (length(threads) != 1L || is.na(threads) || threads < 1L) {
    stop("database.threads debe ser un entero positivo.", call. = FALSE)
  }
  if (
    length(memory_limit) != 1L ||
      is.na(memory_limit) ||
      !nzchar(trimws(memory_limit))
  ) {
    stop("database.memory_limit no puede estar vacio.", call. = FALSE)
  }

  DBI::dbExecute(con, sprintf("SET threads = %d", threads))
  DBI::dbExecute(
    con,
    paste(
      "SET memory_limit =",
      DBI::dbQuoteString(con, memory_limit)
    )
  )
  DBI::dbExecute(con, "SET allow_community_extensions = false")
  DBI::dbExecute(con, "SET autoinstall_known_extensions = false")
  DBI::dbExecute(con, "SET autoload_known_extensions = false")
  invisible(con)
}

abrir_conexion_observatorio <- function(
  database_target,
  read_only = TRUE,
  extension_directory = NULL,
  threads = 2L,
  memory_limit = "1GB"
) {
  if (
    length(database_target) != 1L ||
      is.na(database_target) ||
      !nzchar(trimws(database_target))
  ) {
    stop("El destino DuckDB no puede estar vacio.", call. = FALSE)
  }

  extension_directory <- directorio_extensiones_duckdb(extension_directory)
  options(duckdb.extension_directory = extension_directory)

  if (!es_target_motherduck(database_target)) {
    if (!fs::file_exists(database_target)) {
      stop(
        "DuckDB del Observatorio no encontrada: ", database_target,
        ". Ejecute primero el pipeline de datos.",
        call. = FALSE
      )
    }
    con <- DBI::dbConnect(
      duckdb::duckdb(),
      dbdir = database_target,
      read_only = read_only
    )
  } else {
    token_motherduck()
    database_name <- nombre_base_motherduck(database_target)
    con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")

    tryCatch({
      DBI::dbExecute(con, "INSTALL motherduck")
      DBI::dbExecute(con, "LOAD motherduck")
      database_sql <- as.character(DBI::dbQuoteString(
        con,
        paste0("md:", database_name)
      ))
      DBI::dbExecute(
        con,
        paste0("ATTACH ", database_sql, " (READ_ONLY)")
      )
      DBI::dbExecute(
        con,
        paste0(
          "USE ",
          as.character(DBI::dbQuoteIdentifier(con, database_name))
        )
      )
    }, error = function(error) {
      if (DBI::dbIsValid(con)) DBI::dbDisconnect(con, shutdown = TRUE)
      stop(
        "No se pudo conectar a MotherDuck (", database_target, "): ",
        conditionMessage(error),
        call. = FALSE
      )
    })
  }

  tryCatch(
    configurar_conexion_observatorio(
      con,
      threads = threads,
      memory_limit = memory_limit
    ),
    error = function(error) {
      if (DBI::dbIsValid(con)) DBI::dbDisconnect(con, shutdown = TRUE)
      stop(conditionMessage(error), call. = FALSE)
    }
  )

  attr(con, "observatorio_target") <- database_target
  con
}

cerrar_conexion_observatorio <- function(con) {
  if (!is.null(con) && DBI::dbIsValid(con)) {
    DBI::dbDisconnect(con, shutdown = TRUE)
  }
  invisible(NULL)
}
