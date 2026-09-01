#
# scripts/prepara_deploy.R
# Prepara la app para Posit Connect Cloud.
# La base productiva vive en MotherDuck; el bundle incluye solo configuracion.
#
# Uso:
#   Rscript scripts/prepara_deploy.R
#   R -e "rsconnect::writeManifest(appDir = '.')"
#
# Para el deploy desde GitHub, src/ y manifest.json deben versionarse.
#

suppressPackageStartupMessages({
  library(fs)
  library(glue)
})

pipeline_dir <- Sys.getenv(
  "OBS_DATA_DIR",
  file.path("..", "observatorio_minero")
)
src_dir <- file.path(pipeline_dir, "src")
dst_src <- "src"
dst_cache <- "cache"

yamls_src <- c(
  "estetica.yaml",
  "empresas.yaml",
  "perfil.yaml",
  "duckdb.yaml",
  "chat_documental.yaml"
)

if (dir_exists(dst_src)) dir_delete(dst_src)
if (dir_exists(dst_cache)) dir_delete(dst_cache)
legacy_database <- file.path(
  "data",
  "observatorio_minero_motherduck.duckdb"
)
if (file_exists(legacy_database)) file_delete(legacy_database)
dir_create(dst_src)

copiados <- 0L
faltan <- character()

for (name in yamls_src) {
  source_path <- file.path(src_dir, name)
  target_path <- file.path(dst_src, name)
  if (!file_exists(source_path)) {
    faltan <- c(faltan, source_path)
    next
  }
  file_copy(source_path, target_path, overwrite = TRUE)
  copiados <- copiados + 1L
}

if (length(faltan) > 0L) {
  stop(
    "Faltan configuraciones requeridas: ",
    paste(faltan, collapse = ", "),
    call. = FALSE
  )
}

message(glue("Bundle listo: {copiados} configuraciones copiadas en src/."))
message("La DuckDB local no se incluye. En deploy la app usara MotherDuck.")
message("Configure OBS_DATA_DIR=. y OBS_CONFIG_DIR=./src.")
message("Configure MOTHERDUCK_TOKEN con un token de solo lectura.")
message("Genere manifest.json con rsconnect::writeManifest(appDir = '.').")
