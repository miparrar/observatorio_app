# observatorio_app

App Shiny del Observatorio Minero. Dashboard interactivo con perfil por empresa,
comparador de indicadores, asistente documental y descarga de datos.

## Arquitectura

```
observatorio_minero (pipeline)  ->  cache/*.duckdb + parquet
                                    |
observatorio_app (esta app)     ->  lee cache/ y src/ del pipeline
observatorio_quarto (sitio)     ->  embebe esta app via iframe
```

En local, la app lee datos y configuracion del repo hermano
`../observatorio_minero/`. Para deploy, `scripts/prepara_deploy.R` empaqueta
cache/ y src/ dentro del bundle.

## Uso

```bash
# Desde observatorio_minero (con cache construido):
make duckdb EMPRESA=codelco
make duckdb EMPRESA=escondida

# Desde este repo:
R -e 'shiny::runApp(".", port=7777)'
```

## Variables de entorno

| Variable | Default | Descripcion |
|---|---|---|
| `OBS_DATA_DIR` | `../observatorio_minero` | Raiz de cache/ del pipeline |
| `OBS_CONFIG_DIR` | `$OBS_DATA_DIR/src` | YAMLs de configuracion |

Para deploy, setear `OBS_DATA_DIR=.` y `OBS_CONFIG_DIR=.` despues de correr
`prepara_deploy.R`.
