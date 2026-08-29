# utils_ui.R - piezas de UI compartidas por los módulos del perfil.
# Principio del perfil: VISUALIZAR Y EXPLICAR (docs/perfil_esquema.md §6) -
# los estados vacíos son contenido: declaran qué falta y por qué.

# Encabezado de sección numerado ("01 · Rentabilidad")
section_header <- function(numero, titulo) {
  div(class = "section-header", glue("{numero} · {titulo}"))
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
