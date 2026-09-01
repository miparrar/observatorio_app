testthat::local_edition(3)

app_root <- rprojroot::find_root(rprojroot::has_file("global.R"))
source(file.path(app_root, "R", "lib_datos_financieros.R"), local = FALSE)

crear_fixture_financiero <- function() {
  path <- tempfile(fileext = ".duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = path)
  DBI::dbExecute(con, "CREATE SCHEMA financiero")
  DBI::dbExecute(
    con,
    paste(
      "CREATE TABLE financiero.tasa_ganancia (",
      "empresa_slug VARCHAR, empresa VARCHAR, anio INTEGER,",
      "tasa_ganancia DOUBLE)"
    )
  )
  DBI::dbExecute(
    con,
    paste(
      "INSERT INTO financiero.tasa_ganancia VALUES",
      "('codelco', 'codelco', 2023, 0.12),",
      "('sqm', 'sqm', 2023, 0.18)"
    )
  )
  list(path = path, con = con)
}

testthat::test_that("la lectura financiera filtra empresas y oculta la particion", {
  fixture <- crear_fixture_financiero()
  withr::defer(DBI::dbDisconnect(fixture$con, shutdown = TRUE))

  data <- leer_tabla_financiera(
    fixture$con,
    "financiero",
    "tasa_ganancia",
    "codelco"
  )

  testthat::expect_equal(nrow(data), 1)
  testthat::expect_equal(data$empresa, "codelco")
  testthat::expect_false("empresa_slug" %in% names(data))
  testthat::expect_equal(
    empresas_en_tabla_financiera(
      fixture$con,
      "financiero",
      "tasa_ganancia"
    ),
    c("codelco", "sqm")
  )
})

testthat::test_that("una tabla opcional ausente produce una tabla vacia", {
  fixture <- crear_fixture_financiero()
  withr::defer(DBI::dbDisconnect(fixture$con, shutdown = TRUE))

  data <- leer_tabla_financiera(
    fixture$con,
    "financiero",
    "distribucion"
  )
  testthat::expect_s3_class(data, "tbl_df")
  testthat::expect_equal(nrow(data), 0)
})
