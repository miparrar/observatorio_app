#
# scripts/prepara_deploy.R
# Empaqueta la app para deploy (shinyapps.io / Posit Connect):
# copia SOLO los datos que la app lee (cache/ y YAMLs de src/) DENTRO del
# repo de la app, para que viajen en el bundle.
#
# Uso:
#   Rscript scripts/prepara_deploy.R          # arma cache/ y src locales
#   R -e "rsconnect::deployApp('.', appName='observatorio-mineria')"
#
# cache/ y src/ estan en .gitignore (artefactos de deploy, no fuente).
#

suppressPackageStartupMessages({
  library(yaml); library(fs); library(glue)
})

# Raiz del pipeline (repo hermano)
pipeline_dir <- Sys.getenv("OBS_DATA_DIR", file.path("..", "observatorio_minero"))
src_dir <- file.path(pipeline_dir, "src")
cache_dir <- file.path(pipeline_dir, "cache")

# Empresas publicadas + series que la app lee
publicadas <- yaml::read_yaml(file.path(src_dir, "perfil.yaml"))$empresas
series_emp <- c("tasa_ganancia", "endeudamiento", "distribucion", "hechos_canonicos")
series_cochilco <- c("precio_cobre_anual", "produccion_cobre_empresa_anual")
yamls_src <- c("estetica.yaml", "empresas.yaml", "perfil.yaml", "duckdb.yaml", "chat_documental.yaml")

dst_cache <- "cache"
dst_src   <- "src"

# Limpiar bundle previo
if (dir_exists(dst_cache)) dir_delete(dst_cache)
if (dir_exists(dst_src))   dir_delete(dst_src)
dir_create(dst_cache); dir_create(dst_src)

copiados <- 0
faltan   <- character(0)

copia <- function(orig, destino) {
  if (file_exists(orig)) {
    dir_create(path_dir(destino))
    file_copy(orig, destino, overwrite = TRUE)
    copiados <<- copiados + 1
  } else {
    faltan <<- c(faltan, orig)
  }
}

# 1. Series por empresa publicada
for (e in publicadas) {
  for (s in series_emp) {
    copia(glue("{cache_dir}/{e}/{s}.parquet"), glue("{dst_cache}/{e}/{s}.parquet"))
  }
}

# 2. Series COCHILCO
for (s in series_cochilco) {
  copia(glue("{cache_dir}/cochilco/{s}.parquet"), glue("{dst_cache}/cochilco/{s}.parquet"))
}

# 3. DuckDB financiero + corpus documental
copia(file.path(cache_dir, "observatorio_minero.duckdb"), file.path(dst_cache, "observatorio_minero.duckdb"))

# 4. YAMLs de configuracion
for (y in yamls_src) {
  copia(file.path(src_dir, y), file.path(dst_src, y))
}

message(glue("Bundle listo: {copiados} archivos en cache/ y src/."))
if (length(faltan) > 0) {
  message("Faltantes (no criticos - la app degrada si no estan):")
  for (f in faltan) message("  - ", f)
}
message("\nAntes de deployar, setea: OBS_DATA_DIR=. y OBS_CONFIG_DIR=.")
