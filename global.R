suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(highcharter)
  library(DT)
  library(dplyr)
  library(tidyr)
  library(arrow)
  library(readr)
  library(writexl)
  library(yaml)
  library(fs)
  library(glue)
})

# Raiz de datos del pipeline (observatorio_minero). En LOCAL apunta al repo
# hermano. En DEPLOY (shinyapps/Connect) el bundle incluye cache/ y src/
# planos en el working dir (ver scripts/prepara_deploy.R).
dir_app    <- "."                                                # i18n, R
dir_data   <- Sys.getenv("OBS_DATA_DIR", file.path("..", "observatorio_minero"))  # cache/
dir_config <- Sys.getenv("OBS_CONFIG_DIR", file.path(dir_data, "src"))         # YAMLs

# --- Config ---
estetica   <- yaml::read_yaml(file.path(dir_config, "estetica.yaml"))
paleta     <- estetica$paletas[[estetica$paleta_activa]]
tipografia <- estetica$tipografia

# --- i18n ---
txt <- yaml::read_yaml(file.path(dir_app, "i18n", "es.yaml"))

# --- Registro de empresas (ficha curada por id) ---
empresas_cfg <- yaml::read_yaml(file.path(dir_config, "empresas.yaml"))$empresas
nombre_empresa <- setNames(
  vapply(empresas_cfg, function(e) e$nombre_corto, character(1)),
  vapply(empresas_cfg, function(e) e$id, character(1))
)
empresas_ficha <- setNames(
  empresas_cfg,
  vapply(empresas_cfg, function(e) e$id, character(1))
)

# --- Módulos del perfil (secciones curadas; ver docs/perfil_esquema.md §6) ---
for (f in list.files(file.path(dir_app, "R"), pattern = "\\.R$", full.names = TRUE)) {
  source(f, local = FALSE)
}

# --- Datos financieros validados desde DuckDB ---
# Las empresas publicadas las decide src/perfil.yaml. La app no reconstruye
# productos: make duckdb EMPRESA=<empresa> debe cargarlos previamente.
perfil_cfg <- yaml::read_yaml(file.path(dir_config, "perfil.yaml"))
duckdb_cfg <- yaml::read_yaml(file.path(dir_config, "duckdb.yaml"))
database_financiera_path <- file.path(dir_data, duckdb_cfg$database$path)
duckdb_financiero_schema <- duckdb_cfg$database$schema
tablas_financieras <- setNames(
  vapply(duckdb_cfg$datasets, function(x) x$table, character(1)),
  vapply(duckdb_cfg$datasets, function(x) x$name, character(1))
)

datos_financieros <- local({
  con <- abrir_datos_financieros(database_financiera_path, read_only = TRUE)
  on.exit(cerrar_datos_financieros(con), add = TRUE)
  tablas_perfil <- tablas_financieras[c(
    "tasa_ganancia", "endeudamiento", "distribucion"
  )]
  lapply(tablas_perfil, function(table) {
    leer_tabla_financiera(
      con,
      duckdb_financiero_schema,
      table,
      perfil_cfg$empresas
    )
  })
})

serie_indicador <- function(nombre) {
  data <- datos_financieros[[nombre]]
  if (nrow(data) == 0) {
    return(tibble::tibble(empresa = character(), anio = integer()))
  }
  dplyr::arrange(data, anio)
}

series_tg <- serie_indicador("tasa_ganancia")
series_endeu <- serie_indicador("endeudamiento")
series_dist <- serie_indicador("distribucion")
if (nrow(series_tg) == 0) {
  stop(
    "DuckDB sin tasas para las empresas publicadas. Ejecute make duckdb ",
    "EMPRESA=<empresa> antes de iniciar la app.",
    call. = FALSE
  )
}

# Series de contexto COCHILCO (make cochilco). Tibble vacio si faltan.
serie_cochilco <- function(nombre) {
  p <- file.path(dir_data, "cache", "cochilco", paste0(nombre, ".parquet"))
  if (!fs::file_exists(p)) {
    return(tibble::tibble(
      anio = integer(),
      valor = double(),
      serie = character(),
      item = character()
    ))
  }
  arrow::read_parquet(p) |> dplyr::arrange(anio)
}
# Precio del cobre BML nominal (una vez): contexto de la rentabilidad (sec 01)
# y de la produccion (sec 04). Se recorta a los anios de cada empresa aguas abajo.
precio_cobre <- serie_cochilco("precio_cobre_anual") |> dplyr::filter(serie == "bml_c_us_lb")
produccion_cu <- serie_cochilco("produccion_cobre_empresa_anual")

# item de produccion COCHILCO por empresa del perfil (empresas.yaml)
item_produccion <- setNames(
  vapply(empresas_cfg, function(e) if (is.null(e$item_produccion)) NA_character_ else e$item_produccion, character(1)),
  vapply(empresas_cfg, function(e) e$id, character(1))
)
# Selector de empresa: todas las publicadas en perfil.yaml (tengan o no TG)
empresas_perfil <- perfil_cfg$empresas
choices_empresa <- setNames(empresas_perfil, nombre_empresa[empresas_perfil])

# NOTA: la serie del investigador (cache/rentabilidad_codelco.parquet) NO se
# publica en el dashboard: es el oraculo de VALIDACION del pipeline (make
# verifica), no un producto. El dashboard muestra solo resultados del pipeline.

# --- Helpers de formato ---
fmt_pct <- function(x, decimales = 1) {
  paste0(formatC(x * 100, digits = decimales, format = "f",
                 decimal.mark = txt$formato$decimal), "%")
}

fmt_mus <- function(x, decimales = 1) {
  paste0(formatC(x / 1000, digits = decimales, format = "f",
                 big.mark = txt$formato$miles, decimal.mark = txt$formato$decimal), " MMUS$")
}

# Variación en puntos porcentuales, con signo
fmt_pp <- function(x, decimales = 1) {
  paste0(ifelse(x >= 0, "+", ""),
         formatC(x, digits = decimales, format = "f", decimal.mark = txt$formato$decimal), " pp")
}

# Índice base 100 sobre el primer valor de la serie
base100 <- function(x) round(x / x[1] * 100, 1)

# MUS$ -> MMUS$ para ejes/series (los datos del cache van en miles de US$)
a_mmus <- function(x) round(x / 1000, 1)

# Colapsa años a rangos legibles: c(2014:2020, 2024) -> "2014-2020, 2024"
rangos <- function(anios) {
  if (length(anios) == 0) return("")
  grupos <- split(anios, cumsum(c(1, diff(anios) != 1)))
  paste(vapply(grupos, function(g) {
    if (length(g) == 1) as.character(g) else paste0(min(g), "-", max(g))
  }, character(1)), collapse = ", ")
}

# Rellena los años faltantes de la serie con NA: un año sin dato debe VERSE
# como hueco en el gráfico (la línea se corta), no desaparecer del eje.
# Serie vacía (empresa sin el indicador) pasa de largo: el módulo la declara.
con_huecos <- function(d) {
  if (nrow(d) == 0) return(d)
  tidyr::complete(d, anio = seq(min(d$anio), max(d$anio))) |> dplyr::arrange(anio)
}

# --- Helper de gráfico base (tema común a todos) ---
# Marcas finas y grilla recesiva (dataviz): línea 2px, grilla al 10% de la
# tinta, sin línea de eje x - el dato manda, el andamiaje se apaga.
grafico_base <- function(categorias, titulo_y = NULL, formato_y = NULL, max_y = NULL) {
  est <- list(color = paleta$tinta_suave,
              fontSize = tipografia$roles$grafico$tamaño,
              fontFamily = tipografia$roles$grafico$familia)
  labels_y <- if (is.null(formato_y)) list(style = est) else list(format = formato_y, style = est)
  grilla <- "rgba(45, 40, 32, 0.10)"
  highchart() |>
    hc_chart(backgroundColor = paleta$fondo,
             style = list(fontFamily = tipografia$roles$grafico$familia)) |>
    hc_xAxis(categories = categorias, title = list(text = NULL),
             lineColor = grilla, tickColor = grilla,
             labels = list(style = est)) |>
    hc_yAxis(title = list(text = titulo_y, style = est), labels = labels_y,
             max = max_y, gridLineColor = grilla) |>
    hc_plotOptions(series = list(
      lineWidth = 2,
      marker = list(enabled = FALSE, symbol = "circle"),
      states = list(hover = list(lineWidth = 3))
    )) |>
    hc_legend(itemStyle = est) |>
    hc_credits(enabled = FALSE)
}

# estilo de texto reutilizable para ejes (mismo que usa grafico_base)
est_eje <- list(color = paleta$tinta_suave,
                fontSize = tipografia$roles$grafico$tamaño,
                fontFamily = tipografia$roles$grafico$familia)

# --- Tooltips JS reutilizables (formato numérico según idioma) ---
tt_pct <- JS(glue(
  "function() {{",
  "  var v = this.y.toFixed(1).replace('.', '{txt$formato$decimal}');",
  "  return '<span style=\"font-size:0.75rem\">' + this.x + '</span><br/><b>' + v + '%</b>';",
  "}}"
))
# Las series de montos se pasan YA convertidas a MMUS$ (a_mmus), asi eje y
# tooltip leen la misma unidad (antes el eje mostraba MUS$ crudos con titulo MMUS$).
tt_mmus <- JS(glue(
  "function() {{",
  "  var v = this.y.toFixed(1).replace('.', '{txt$formato$decimal}');",
  "  return '<span style=\"font-size:0.75rem\">' + this.x + '</span><br/><b>' + this.series.name + ': ' + v + ' MMUS$</b>';",
  "}}"
))
tt_num <- JS(glue(
  "function() {{",
  "  var v = this.y.toFixed(1).replace('.', '{txt$formato$decimal}');",
  "  return '<span style=\"font-size:0.75rem\">' + this.x + '</span><br/><b>' + this.series.name + ': ' + v + '</b>';",
  "}}"
))
