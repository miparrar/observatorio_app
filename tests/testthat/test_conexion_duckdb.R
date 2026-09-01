testthat::local_edition(3)

app_root <- rprojroot::find_root(rprojroot::has_file("global.R"))
source(
  file.path(app_root, "R", "lib_conexion_duckdb.R"),
  local = FALSE
)

testthat::test_that("el destino usa local si existe y MotherDuck si falta", {
  local_path <- tempfile(fileext = ".duckdb")
  file.create(local_path)
  withr::defer(unlink(local_path))

  testthat::expect_equal(
    resolver_target_observatorio(
      local_path,
      "observatorio_minero",
      override = ""
    ),
    local_path
  )

  unlink(local_path)
  testthat::expect_equal(
    resolver_target_observatorio(
      local_path,
      "observatorio_minero",
      override = ""
    ),
    "md:observatorio_minero"
  )

  testthat::expect_equal(
    resolver_target_observatorio(
      local_path,
      "observatorio_minero",
      override = "md:otra_base"
    ),
    "md:otra_base"
  )
})

testthat::test_that("la conexion local es valida y de solo lectura", {
  database_path <- tempfile(fileext = ".duckdb")
  extension_path <- tempfile()
  withr::defer(unlink(database_path))
  withr::defer(unlink(extension_path, recursive = TRUE))

  writer <- DBI::dbConnect(duckdb::duckdb(), dbdir = database_path)
  DBI::dbExecute(writer, "CREATE TABLE prueba AS SELECT 1 AS valor")
  DBI::dbDisconnect(writer, shutdown = TRUE)

  con <- abrir_conexion_observatorio(
    database_path,
    read_only = TRUE,
    extension_directory = extension_path,
    threads = 1L,
    memory_limit = "256MB"
  )
  withr::defer(cerrar_conexion_observatorio(con))

  testthat::expect_true(DBI::dbIsValid(con))
  testthat::expect_equal(
    DBI::dbGetQuery(con, "SELECT valor FROM prueba")$valor,
    1
  )
  testthat::expect_error(
    DBI::dbExecute(con, "CREATE TABLE no_permitida (valor INTEGER)"),
    regexp = "read-only|read only|Cannot execute",
    ignore.case = TRUE
  )
})

testthat::test_that("un nombre remoto invalido se rechaza", {
  testthat::expect_error(
    nombre_base_motherduck("md:nombre-con-guion"),
    "solo letras"
  )
  testthat::expect_error(
    resolver_target_observatorio(
      tempfile(),
      "nombre-con-guion",
      override = ""
    ),
    "motherduck_database"
  )
})
