# lib_datos_financieros.R - lectura de productos cuantitativos desde DuckDB.
# La app abre conexiones de solo lectura y nunca calcula ni repara datos.

validar_identificador_duckdb <- function(value, campo = "identificador") {
  if (length(value) != 1 || !grepl("^[a-z_][a-z0-9_]*$", value)) {
    stop(campo, " DuckDB invalido: ", value, call. = FALSE)
  }
  invisible(value)
}

abrir_datos_financieros <- function(database_path, read_only = TRUE) {
  if (!fs::file_exists(database_path)) {
    stop(
      "DuckDB financiero no encontrado: ", database_path,
      ". Ejecute make duckdb EMPRESA=<empresa>.",
      call. = FALSE
    )
  }
  DBI::dbConnect(
    duckdb::duckdb(),
    dbdir = database_path,
    read_only = read_only
  )
}

cerrar_datos_financieros <- function(con) {
  if (!is.null(con) && DBI::dbIsValid(con)) {
    DBI::dbDisconnect(con, shutdown = TRUE)
  }
  invisible(NULL)
}

tabla_duckdb_existe <- function(con, schema, table) {
  validar_identificador_duckdb(schema, "Schema")
  validar_identificador_duckdb(table, "Tabla")
  DBI::dbGetQuery(
    con,
    paste(
      "SELECT COUNT(*) > 0 AS existe FROM information_schema.tables",
      "WHERE table_schema = ? AND table_name = ?"
    ),
    params = list(schema, table)
  )$existe[[1]]
}

leer_tabla_financiera <- function(con, schema, table, empresas = NULL) {
  if (!tabla_duckdb_existe(con, schema, table)) return(tibble::tibble())
  if (!is.null(empresas) && length(empresas) == 0) return(tibble::tibble())

  table_sql <- as.character(DBI::dbQuoteIdentifier(
    con,
    DBI::Id(schema = schema, table = table)
  ))
  sql <- paste("SELECT * FROM", table_sql)
  params <- list()
  if (!is.null(empresas)) {
    empresas <- unique(tolower(trimws(as.character(empresas))))
    placeholders <- paste(rep("?", length(empresas)), collapse = ", ")
    sql <- paste0(sql, " WHERE empresa_slug IN (", placeholders, ")")
    params <- as.list(empresas)
  }

  DBI::dbGetQuery(con, sql, params = params) |>
    tibble::as_tibble() |>
    dplyr::select(-dplyr::any_of("empresa_slug"))
}

empresas_en_tabla_financiera <- function(con, schema, table) {
  if (!tabla_duckdb_existe(con, schema, table)) return(character())
  table_sql <- as.character(DBI::dbQuoteIdentifier(
    con,
    DBI::Id(schema = schema, table = table)
  ))
  DBI::dbGetQuery(
    con,
    paste("SELECT DISTINCT empresa_slug FROM", table_sql, "ORDER BY empresa_slug")
  )$empresa_slug
}
