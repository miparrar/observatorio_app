# observatorio_app

App Shiny del Observatorio Minero. Incluye perfil por empresa, comparador,
asistente documental y descarga de datos.

## Arquitectura

```text
observatorio_minero  ->  DuckDB local  ->  MotherDuck
                                              |
observatorio_app     --------------------------+
        |
observatorio_quarto  ->  iframe de la app
```

El pipeline construye y publica la base. La app solo consulta datos procesados.

La selección del backend sigue el patrón de CensoLab:

1. Usa la DuckDB local si el archivo existe.
2. Si el archivo no existe, conecta automáticamente a
   `md:observatorio_minero`.
3. `OBS_DB_TARGET` permite sobrescribir esa selección.

La conexión es de solo lectura y se comparte durante toda la vida del proceso
Shiny.

## Uso local

Desde `observatorio_minero`:

```bash
make duckdb EMPRESA=Codelco
make cochilco
```

Desde este repositorio:

```bash
R -e 'shiny::runApp(".", port=7777)'
```

Para probar explícitamente MotherDuck:

```bash
export MOTHERDUCK_TOKEN=<token_solo_lectura>
OBS_DB_TARGET=md:observatorio_minero \
  R -e 'shiny::runApp(".", port=7777)'
```

## Configuración

La app recibe desde el pipeline los YAML necesarios. `src/duckdb.yaml`
define la ruta local, el nombre de la base MotherDuck, los esquemas y las
tablas.

| Variable | Predeterminado | Uso |
|---|---|---|
| `OBS_DATA_DIR` | `../observatorio_minero` | Raíz del pipeline en desarrollo. |
| `OBS_CONFIG_DIR` | `$OBS_DATA_DIR/src` | Directorio de configuración. |
| `OBS_DB_TARGET` | Automático | Ruta local o `md:<base>`. |
| `OBS_CHAT_DB_TARGET` | Igual a `OBS_DB_TARGET` | Base documental alternativa. |
| `OBS_SITE_URL` | `..` | URL del sitio Quarto. |
| `MOTHERDUCK_TOKEN` | Sin valor | Token de solo lectura para la app. |
| `OPENAI_API_KEY` | Sin valor | Credencial del chat documental. |

En producción se recomienda un token MotherDuck de solo lectura o Read Scaling.
El token de escritura del pipeline no debe configurarse en la app.

## Preparar el deploy en Posit Connect Cloud

```bash
Rscript scripts/prepara_deploy.R
```

El script copia únicamente los YAML de configuración a `src/` y elimina
cualquier `cache/` local del bundle. `src/` se versiona porque Connect Cloud
construye la aplicación desde GitHub. Después de copiar la configuración,
genere el manifiesto de dependencias:

```bash
R -e 'rsconnect::writeManifest(appDir = ".")'
```

`manifest.json` y los YAML de `src/` deben confirmarse en Git. La base DuckDB,
los Parquet y `.Renviron` no deben versionarse.

En Connect Cloud se configuran:

```text
OBS_DATA_DIR=.
OBS_CONFIG_DIR=./src
OBS_DB_TARGET=md:observatorio_minero
MOTHERDUCK_TOKEN=<token_solo_lectura>
OPENAI_API_KEY=<token>
OBS_SITE_URL=<url_del_sitio>
```

`OBS_DB_TARGET` hace explícito el backend productivo. El token de escritura de
MotherDuck se mantiene exclusivamente en el pipeline.

El despliegue se crea en Connect Cloud como contenido Shiny desde el repositorio
GitHub, rama `master`, usando la aplicación ubicada en la raíz. Las
actualizaciones de datos solo requieren ejecutar `make motherduck` en el
pipeline; no requieren republicar la app.
