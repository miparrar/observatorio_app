ui <- page_navbar(
  id    = "nav_principal",
  # SIN fillable: el perfil es una página que SCROLLEA. Con fillable (default de
  # bslib) el contenido se comprime para caber en la altura de la ventana y los
  # gráficos colapsan a franjas ilegibles en pantallas normales.
  fillable = FALSE,
  title = actionLink(
    "nav_a_inicio",
    label = txt$app_title,
    style = glue(
      "font-family: '{tipografia$roles$marca$familia}';",
      "font-weight: {tipografia$roles$marca$peso};",
      "letter-spacing: {tipografia$roles$marca$tracking};",
      "text-transform: {tipografia$roles$marca$transform};",
      "color: inherit; text-decoration: none;"
    )
  ),
  theme = bs_theme(
    version      = 5,
    bg           = paleta$fondo,
    fg           = paleta$texto,
    primary      = paleta$primario,
    secondary    = paleta$secundario,
    base_font    = font_google("Inter"),
    heading_font = font_google("Rajdhani")
  ),
  header = tags$head(
    # Favicon inline (SVG data-URI): mata el 404 y marca la pestaña con el cobre
    tags$link(
      rel  = "icon",
      type = "image/svg+xml",
      href = paste0(
        "data:image/svg+xml,",
        utils::URLencode(glue(
          '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 32 32">',
          '<rect width="32" height="32" rx="6" fill="{paleta$texto}"/>',
          '<path d="M6 24 L13 10 L18 19 L22 13 L26 24 Z" fill="{paleta$primario}"/>',
          "</svg>"
        ), reserved = TRUE)
      )
    ),
    tags$style(HTML(glue(
      "@import url('https://fonts.googleapis.com/css2?",
      "family=Rajdhani:wght@500;600;700",
      "&family=Inter:wght@400;500;600",
      "&family=IBM+Plex+Mono:wght@400;500",
      "&family=Lora:wght@400;500;600",
      "&family=Space+Grotesk:wght@400;500",
      "&display=swap');",

      ".kpi-valor {{",
      "  font-family: '{tipografia$roles$kpi$familia}';",
      "  font-weight: {tipografia$roles$kpi$peso};",
      "  font-size: {tipografia$roles$kpi$tamaño};",
      "  letter-spacing: {tipografia$roles$kpi$tracking};",
      "  color: {paleta$primario};",
      "  line-height: 1;",
      "}}",

      ".kpi-etiqueta {{",
      "  font-family: '{tipografia$roles$etiqueta$familia}';",
      "  font-size: {tipografia$roles$etiqueta$tamaño};",
      "  letter-spacing: 0.08em;",
      "  text-transform: uppercase;",
      "  color: {paleta$tinta_suave};",
      "  margin-top: 0.3rem;",
      "}}",

      # Chip de variación interanual (▲/▼ + pp) - glifo + número, nunca solo color
      ".kpi-delta {{",
      "  display: inline-block;",
      "  font-family: 'IBM Plex Mono', monospace;",
      "  font-size: 0.85rem;",
      "  padding: 0.1rem 0.6rem;",
      "  border-radius: 999px;",
      "  border: 1px solid {paleta$tinta_suave};",
      "  color: {paleta$texto};",
      "  margin: 0 0.25rem;",
      "}}",

      # Hero de identidad: panel oscuro, la audacia del perfil vive acá
      ".hero-card {{",
      "  background: {paleta$texto};",
      "  color: {paleta$fondo};",
      "  border-radius: 0.5rem;",
      "  padding: 2rem 2.25rem;",
      "  margin-bottom: 1rem;",
      "}}",
      ".hero-nombre {{",
      "  font-family: 'Lora', serif;",
      "  font-size: 2.4rem;",
      "  font-weight: 500;",
      "  line-height: 1.1;",
      "  color: {paleta$fondo};",
      "}}",
      ".hero-razon {{",
      "  font-family: '{tipografia$roles$texto$familia}';",
      "  font-size: 0.9rem;",
      "  color: {paleta$acento_claro};",
      "  margin: 0.25rem 0 0.75rem;",
      "}}",
      ".badge-datos {{",
      "  font-family: 'IBM Plex Mono', monospace;",
      "  font-size: 0.75rem;",
      "  color: {paleta$texto};",
      "  background: {paleta$acento_claro};",
      "  border-radius: 4px;",
      "  padding: 0.15rem 0.6rem;",
      "}}",
      ".hero-editorial {{",
      "  border-left: 3px solid {paleta$primario};",
      "  padding-left: 0.9rem;",
      "  margin-top: 1rem;",
      "  font-size: 0.95rem;",
      "  line-height: 1.55;",
      "  font-style: italic;",
      "  color: #d8cec0;",
      "  max-width: 560px;",
      "}}",
      ".ficha-campo {{ margin-bottom: 0.9rem; }}",
      ".ficha-label {{",
      "  font-size: 0.72rem;",
      "  text-transform: uppercase;",
      "  letter-spacing: 0.08em;",
      "  color: {paleta$acento_claro};",
      "}}",
      ".ficha-valor {{ font-size: 0.9rem; font-weight: 500; color: {paleta$fondo}; }}",

      # KPI strip bajo el hero: tres números, más compactos que el hero viejo
      ".kpi-strip .kpi-valor {{ font-size: 2.1rem; }}",
      ".kpi-strip .card-body {{ text-align: center; padding: 1.1rem; }}",

      # Encabezado de sección numerado (01 · Rentabilidad)
      ".section-header {{",
      "  font-family: '{tipografia$roles$titulo$familia}';",
      "  font-weight: {tipografia$roles$titulo$peso};",
      "  font-size: 1.35rem;",
      "  letter-spacing: 0.04em;",
      "  border-left: 4px solid {paleta$primario};",
      "  padding-left: 0.75rem;",
      "  margin: 2.25rem 0 1rem;",
      "}}",

      # Pendiente declarado: el hueco es contenido (qué falta y por qué)
      ".pendiente {{",
      "  background: rgba(156, 112, 16, 0.07);",
      "  border-left: 4px solid {paleta$datos[[3]]};",
      "  border-radius: 0.35rem;",
      "  padding: 1.1rem 1.3rem;",
      "  font-size: 0.92rem;",
      "  line-height: 1.6;",
      "  color: {paleta$tinta_suave};",
      "}}",
      ".pendiente-titulo {{",
      "  font-family: '{tipografia$roles$titulo$familia}';",
      "  font-weight: {tipografia$roles$titulo$peso};",
      "  color: {paleta$texto};",
      "  margin-bottom: 0.4rem;",
      "}}",

      ".tabla-propiedad {{ font-size: 0.92rem; }}",

      # Cards: borde quieto, sin sombra; headers uniformes (Rajdhani, regla fina)
      ".card {{",
      "  border: 1px solid rgba(45, 40, 32, 0.10);",
      "  box-shadow: none;",
      "}}",
      ".card .card-header {{",
      "  font-family: '{tipografia$roles$titulo$familia}';",
      "  font-weight: {tipografia$roles$titulo$peso};",
      "  font-size: 1.05rem;",
      "  letter-spacing: {tipografia$roles$titulo$tracking};",
      "  background: transparent;",
      "  border-bottom: 1px solid rgba(45, 40, 32, 0.10);",
      "}}",
      ".card .card-footer {{",
      "  background: transparent;",
      "  border-top: 1px dashed rgba(45, 40, 32, 0.15);",
      "  color: {paleta$tinta_suave};",
      "}}",
      ".card .card-footer small {{ text-transform: none; letter-spacing: 0.02em; }}",

      # Landing
      ".landing-panel {{",
      "  background-color: {paleta$texto};",
      "  min-height: calc(100vh - 56px);",
      "  display: flex;",
      "  flex-direction: column;",
      "  justify-content: center;",
      "  padding: 2rem 6rem;",
      "}}",
      ".landing-supertitulo {{",
      "  font-family: '{tipografia$roles$pestaña$familia}';",
      "  font-size: {tipografia$roles$etiqueta$tamaño};",
      "  letter-spacing: 0.15em;",
      "  text-transform: uppercase;",
      "  color: {paleta$acento_claro};",
      "  margin-bottom: 1.5rem;",
      "}}",
      ".landing-titulo {{",
      "  font-family: 'Lora', serif;",
      "  font-weight: 500;",
      "  font-size: 4.5rem;",
      "  line-height: 1.2;",
      "  color: {paleta$fondo};",
      "  margin-bottom: 0;",
      "  max-width: 700px;",
      "}}",
      ".landing-acento {{",
      "  color: {paleta$primario};",
      "}}",
      ".landing-subtitulo {{",
      "  font-family: '{tipografia$roles$texto$familia}';",
      "  font-size: 1.4rem;",
      "  color: #b8a898;",
      "  max-width: 560px;",
      "  margin-top: 1.5rem;",
      "  line-height: 1.6;",
      "}}",
      ".landing-boton {{",
      "  margin-top: 2.5rem;",
      "  padding: 0.75rem 2rem;",
      "  background: transparent;",
      "  color: {paleta$fondo};",
      "  border: 1px solid {paleta$fondo};",
      "  border-radius: 0.4rem;",
      "  font-family: '{tipografia$roles$subtitulo$familia}';",
      "  font-size: 0.95rem;",
      "  font-weight: 500;",
      "  width: fit-content;",
      "  cursor: pointer;",
      "  transition: background 0.2s, color 0.2s;",
      "}}",
      ".landing-boton:hover {{",
      "  background: {paleta$fondo};",
      "  color: {paleta$texto};",
      "}}",
      ".navbar-nav .nav-link[data-value='inicio'] {{",
      "  display: none !important;",
      "}}",
      ".navbar-nav .nav-link {{",
      "  font-family: 'Space Grotesk', sans-serif;",
      "  font-weight: 500;",
      "  font-size: 0.9rem;",
      "}}"
    )))
  ),

  # ── Inicio ────────────────────────────────────────────────────────────────
  nav_panel(
    value = "inicio",
    title = "Inicio",
    div(
      class = "landing-panel",
      div(
        div(
          class = "landing-titulo",
          txt$landing$titulo, " ",
          span(class = "landing-acento", txt$landing$acento)
        ),
        div(class = "landing-subtitulo", txt$landing$subtitulo),
        actionButton(
          "btn_comenzar",
          label  = paste(txt$landing$boton, "→"),
          class  = "landing-boton"
        )
      )
    )
  ),

  # ── Perfil por empresa (módulos curados; docs/perfil_esquema.md §6) ───────
  nav_panel(
    title = txt$nav$perfil,

    layout_columns(
      col_widths = c(3, 9),
      selectInput("sel_empresa", txt$perfil$empresa, choices = choices_empresa),
      NULL
    ),

    mod_hero_ui("hero"),
    mod_sec01_ui("sec01"),
    mod_sec02_ui("sec02"),
    mod_sec03_ui("sec03"),
    mod_sec04_ui("sec04")
  ),

  # Asistente documental: interfaz solamente. El retrieval y el razonamiento
  # viven en el modulo y en el corpus procesado del cache.
  nav_panel(
    title = txt$nav$asistente_documental,
    mod_chat_documental_ui("chat_documental")
  ),

  # ── Datos: descarga abierta de las series (CSV/XLSX, tidy o años en columnas)
  nav_panel(title = txt$nav$datos, mod_datos_ui("datos")),

  # ── Otras pestañas (por desarrollar) ─────────────────────────────────────
  nav_panel(title = txt$nav$comparacion),
  nav_panel(title = txt$nav$metodologia),
  nav_panel(title = txt$nav$quienes_somos)
)
