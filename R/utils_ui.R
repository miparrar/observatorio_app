# utils_ui.R - piezas de UI compartidas por los módulos del perfil.
# Principio del perfil: VISUALIZAR Y EXPLICAR (docs/perfil_esquema.md §6) -
# los estados vacíos son contenido: declaran qué falta y por qué.

# Encabezado de sección numerado.
section_header <- function(numero, titulo) {
  div(
    class = "section-header",
    span(class = "section-number", numero),
    h2(class = "section-title", titulo)
  )
}

# Puente narrativo entre secciones (frase corta que conecta con la anterior)
section_intro <- function(texto) {
  div(class = "section-intro", texto)
}

# Sub-encabezado dentro de una sección (agrupa gráficos relacionados)
sub_header <- function(titulo) {
  div(class = "sub-header", titulo)
}

# Encabezado compacto para una tarjeta de datos: título + unidad visible.
panel_header <- function(titulo, unidad = NULL) {
  div(
    class = "panel-header",
    span(class = "panel-title", titulo),
    if (!is.null(unidad)) span(class = "panel-unit", unidad)
  )
}

# Tarjeta de pendiente declarado: qué falta, por qué y qué viene
pendiente_card <- function(titulo, ...) {
  div(
    class = "pendiente",
    div(class = "pendiente-titulo", titulo),
    p(class = "mb-0", ...)
  )
}

# Nota editorial al pie de una card (texto chico, tinta suave)
nota_pie <- function(out_id = NULL, texto = NULL) {
  contenido <- if (!is.null(out_id)) textOutput(out_id) else texto
  card_footer(tags$small(class = "kpi-etiqueta", contenido))
}

# Campo de la ficha técnica del hero (label chico arriba, valor abajo)
ficha_campo <- function(label, valor) {
  v <- if (is.null(valor) || length(valor) == 0 || is.na(valor)) "-" else as.character(valor)
  div(
    class = "ficha-campo",
    div(class = "ficha-label", label),
    div(class = "ficha-valor", v)
  )
}
