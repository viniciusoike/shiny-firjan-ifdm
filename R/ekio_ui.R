## EKIO UI helpers ---------------------------------------------------------
# bslib building blocks for the EKIO-branded shell. Mirrors the design system
# in styles.css (shared with shiny-painel-mercado). Functions namespace their
# bslib/shiny calls so this file can be auto-sourced from R/ before app.R.

# Index palette: maps an internal index_type key to a kpi-card colour class
# (defined in styles.css) and to a hex colour for plots.
INDEX_COLOR_CLASS <- c(
  overall = "blue", education = "teal", health = "green", income = "orange"
)
INDEX_HEX <- c(
  overall = "#1E3A5F", education = "#2C7A7B",
  health = "#38A169", income = "#DD6B20"
)

# Same palette keyed by the Portuguese factor labels used in series_data, so
# ggplot scale_*_manual() maps each index to its brand colour by name.
INDEX_PAL_LABELLED <- c(
  "Geral (IFDM)"    = unname(INDEX_HEX["overall"]),
  "Saúde"           = unname(INDEX_HEX["health"]),
  "Emprego & Renda" = unname(INDEX_HEX["income"]),
  "Educação"        = unname(INDEX_HEX["education"])
)

# City vs. benchmark line (EKIO blue + orange accent)
BENCH_PAL <- c("#1E3A5F", "#DD6B20")

## ggplot theme ------------------------------------------------------------

EKIO_INK   <- "#1A202C"
EKIO_GRID  <- "#E2E8F0"
EKIO_MUTED <- "#718096"

# Shared EKIO ggplot theme. base_family left empty so server-side rendering
# (Linux on shinyapps.io) falls back to the default sans rather than failing
# on the Avenir brand font, which is macOS-only.
theme_ekio <- function(base_size = 13) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      text             = ggplot2::element_text(colour = EKIO_INK),
      plot.title       = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.0)),
      plot.subtitle    = ggplot2::element_text(colour = EKIO_MUTED, size = ggplot2::rel(0.82)),
      plot.title.position = "plot",
      axis.title       = ggplot2::element_text(colour = EKIO_MUTED, size = ggplot2::rel(0.85)),
      axis.text        = ggplot2::element_text(colour = EKIO_MUTED),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = EKIO_GRID, linewidth = 0.4),
      strip.text       = ggplot2::element_text(face = "bold", colour = EKIO_INK, hjust = 0),
      legend.position  = "top",
      legend.title     = ggplot2::element_blank()
    )
}

## Layout helpers ----------------------------------------------------------

page_header <- function(title, subtitle = NULL) {
  shiny::div(
    class = "page-header",
    shiny::h2(title),
    if (!is.null(subtitle)) shiny::p(subtitle)
  )
}

ekio_nav_item <- function(value, label, icon, active = FALSE) {
  shiny::tags$a(
    class = paste0("ekio-nav-item", if (active) " active" else ""),
    `data-value` = value,
    role = "link",
    tabindex = "0",
    `aria-current` = if (active) "page" else NULL,
    shiny::tags$span(class = "nav-icon", `aria-hidden` = "true", icon),
    shiny::tags$span(label)
  )
}

ekio_nav_section <- function(label, ...) {
  shiny::div(
    class = "ekio-nav-section",
    if (!is.null(label)) shiny::div(class = "ekio-nav-label", label),
    ...
  )
}

filter_group <- function(label, ..., class = NULL, style = NULL) {
  shiny::div(
    class = paste(c("filter-group", class), collapse = " "),
    style = style,
    if (!is.null(label)) shiny::tags$label(label),
    ...
  )
}

chart_card <- function(title, tag = NULL, ..., full_screen = TRUE) {
  bslib::card(
    full_screen = full_screen,
    bslib::card_header(
      class = "chart-card-header",
      shiny::span(title),
      if (!is.null(tag)) shiny::span(class = "chart-tag", tag)
    ),
    ...
  )
}

about_card <- function(title, text) {
  shiny::div(
    class = "about-card",
    shiny::h4(title),
    shiny::p(text)
  )
}

## Formatting --------------------------------------------------------------

# pt-BR number with comma decimal separator
fmt_ifdm <- function(x, digits = 3) {
  if (length(x) == 0 || is.na(x)) return("—")
  sub("\\.", ",", formatC(x, format = "f", digits = digits))
}

# Signed year-over-year change in IFDM points (e.g. "+0,012")
ifdm_delta_lbl <- function(d) {
  if (length(d) == 0 || is.na(d)) return("—")
  sub("\\.", ",", sprintf("%+.3f", d))
}

pp_dir <- function(d) {
  if (length(d) == 0 || is.na(d)) "neutral" else if (d >= 0) "up" else "down"
}

## KPI cards (Panorama) ----------------------------------------------------

kpi_sparkline <- function(values, n = 12) {
  v <- utils::tail(values[!is.na(values)], n)
  if (length(v) < 2) return(NULL)
  rng <- range(v)
  span <- if (diff(rng) == 0) 1 else diff(rng)
  heights <- 3 + (v - rng[1]) / span * 97
  shiny::div(
    class = "kpi-sparkline",
    lapply(heights, function(h) {
      shiny::div(class = "bar", style = sprintf("height:%.0f%%", h))
    })
  )
}

kpi_card <- function(label, value, delta, period, spark_values,
                     color = "blue", dir = "neutral") {
  shiny::div(
    class = paste("kpi-card", color),
    shiny::div(class = "kpi-label", label),
    shiny::div(class = "kpi-value", value),
    shiny::div(
      class = "kpi-meta",
      shiny::span(class = paste("kpi-delta", dir), delta),
      shiny::span(class = "kpi-period", paste("·", period))
    ),
    kpi_sparkline(spark_values)
  )
}
