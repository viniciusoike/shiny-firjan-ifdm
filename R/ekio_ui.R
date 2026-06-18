## EKIO UI helpers ---------------------------------------------------------
# bslib building blocks for the EKIO-branded shell, with brand colours and the
# ggplot theme sourced from the `ekioplot` package. Mirrors the design system
# in styles.css (shared with shiny-painel-mercado). Functions namespace their
# bslib/shiny calls so this file can be auto-sourced from R/ before app.R.

library(ekioplot)

## Brand palette (from ekioplot) -------------------------------------------

# ekio_accent: blue, orange, teal, amber, purple, red, green, gray
.ekio_accent <- ekioplot::ekio_accent
.ekio_grays <- ekioplot::ekio_gray

# Internal index_type key -> hex, drawn from the ekioplot accent palette so the
# plots match the KPI-card colours defined in styles.css.
INDEX_HEX <- c(
  overall = unname(.ekio_accent[1]), # blue  #1E3A5F
  education = unname(.ekio_accent[3]), # teal  #2C7A7B
  health = unname(.ekio_accent[7]), # green #38A169
  income = unname(.ekio_accent[2]) # orange #DD6B20
)

# Internal index_type key -> kpi-card colour class (defined in styles.css)
INDEX_COLOR_CLASS <- c(
  overall = "blue",
  education = "teal",
  health = "green",
  income = "orange"
)

# Same palette keyed by the Portuguese factor labels used in series_data, so
# ggplot scale_*_manual() maps each index to its brand colour by name.
INDEX_PAL_LABELLED <- c(
  "Geral (IFDM)" = unname(INDEX_HEX["overall"]),
  "Saúde" = unname(INDEX_HEX["health"]),
  "Emprego & Renda" = unname(INDEX_HEX["income"]),
  "Educação" = unname(INDEX_HEX["education"])
)

# City vs. benchmark line (EKIO blue + orange accent)
BENCH_PAL <- ekioplot::ekio_pal("binary")

# Neutral greys from the ekioplot gray ramp (ink / muted text / grid lines)
EKIO_INK <- unname(.ekio_grays[1]) # #1A202C
EKIO_MUTED <- unname(.ekio_grays[4]) # #718096
EKIO_GRID <- unname(.ekio_grays[7]) # #E2E8F0

# The shared ggplot theme is ekioplot::theme_ekio(), attached above; the plot
# builders in R/plot_*.R call it directly (no local definition needed).

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
  if (length(x) == 0 || is.na(x)) {
    return("—")
  }
  sub("\\.", ",", formatC(x, format = "f", digits = digits))
}

# Signed year-over-year change in IFDM points (e.g. "+0,012")
ifdm_delta_lbl <- function(d) {
  if (length(d) == 0 || is.na(d)) {
    return("—")
  }
  sub("\\.", ",", sprintf("%+.3f", d))
}

pp_dir <- function(d) {
  if (length(d) == 0 || is.na(d)) {
    "neutral"
  } else if (d >= 0) {
    "up"
  } else {
    "down"
  }
}

## KPI cards (Panorama) ----------------------------------------------------

kpi_sparkline <- function(values, n = 12) {
  v <- utils::tail(values[!is.na(values)], n)
  if (length(v) < 2) {
    return(NULL)
  }
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

kpi_card <- function(
  label,
  value,
  delta,
  period,
  spark_values,
  color = "blue",
  dir = "neutral"
) {
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
