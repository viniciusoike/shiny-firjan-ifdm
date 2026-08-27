## EKIO UI helpers ---------------------------------------------------------
# bslib building blocks for the EKIO-branded shell, with brand colours and the
# ggplot theme sourced from the `ekioplot` package. Mirrors the design system
# in styles.css (shared with shiny-painel-mercado). Functions namespace their
# bslib/shiny calls so this file can be auto-sourced from R/ before app.R.

library(ekioplot)
## Brand palette (from ekioplot) -------------------------------------------

.ekio_full <- ekioplot::ekio_pal("full")
.ekio_blue <- ekioplot::ekio_pal("blue")
.ekio_gray <- ekioplot::ekio_pal("gray")

EKIO_NAVY <- unname(.ekio_full[1])
EKIO_BLUE <- unname(.ekio_blue[4])
EKIO_TEAL <- unname(.ekio_full[3])
EKIO_ORANGE <- unname(.ekio_full[2])
EKIO_GOLD <- unname(.ekio_full[4])
EKIO_GREEN <- unname(.ekio_full[6])
EKIO_RED <- unname(.ekio_full[5])
EKIO_SURFACE <- "#FFFFFF"

# Internal index_type key -> hex, drawn from the ekioplot accent palette so the
# plots match the KPI-card colours defined in styles.css.
INDEX_HEX <- c(
  overall = EKIO_NAVY,
  education = EKIO_TEAL,
  health = EKIO_BLUE,
  income = EKIO_GOLD
)

# Internal index_type key -> kpi-card colour class (defined in styles.css)
INDEX_COLOR_CLASS <- c(
  overall = "bluedark",
  education = "teal",
  health = "blue",
  income = "amber"
)

# Same palette keyed by the Portuguese factor labels used in series_data, so
# ggplot scale_*_manual() maps each index to its brand colour by name.
INDEX_PAL_LABELLED <- c(
  "Geral (IFDM)" = unname(INDEX_HEX["overall"]),
  "Saúde" = unname(INDEX_HEX["health"]),
  "Emprego & Renda" = unname(INDEX_HEX["income"]),
  "Educação" = unname(INDEX_HEX["education"])
)

# City vs. benchmark line (EKIO navy + orange accent)
BENCH_PAL <- c(EKIO_NAVY, EKIO_ORANGE)

# Neutral greys from the ekioplot gray ramp (ink / muted text / grid lines)
EKIO_INK <- unname(.ekio_gray[1])
EKIO_MUTED <- unname(.ekio_gray[5])
EKIO_GRID <- unname(.ekio_gray[2])

# The shared ggplot theme is ekioplot::theme_ekio(), attached above; the plot
# builders in R/plot_*.R call it directly (no local definition needed).

## echarts4r theme ---------------------------------------------------------

# EKIO styling for echarts4r widgets — the echarts counterpart to theme_ekio().
# Applies the brand colour palette (positional, so series order must match),
# muted axes, dashed grey split lines, and the Avenir font stack. Fonts are
# safe here because echarts renders client-side in the browser.
e_ekio <- function(e, palette = NULL, y_name = NULL, legend = TRUE,
                   y_min = NULL, y_max = NULL) {
  if (!is.null(palette)) {
    e <- echarts4r::e_color(e, unname(palette))
  }
  e |>
    echarts4r::e_grid(top = 48, bottom = 36, left = 52, right = 20) |>
    echarts4r::e_x_axis(
      # `scale` fits the axis to the data extent; without it a value axis forces
      # zero and squashes the 2013–2023 years into a sliver at the right.
      scale = TRUE,
      axisLine = list(lineStyle = list(color = EKIO_GRID)),
      axisTick = list(show = FALSE),
      axisLabel = list(color = EKIO_MUTED, formatter = "{value}"),
      splitLine = list(show = FALSE)
    ) |>
    echarts4r::e_y_axis(
      name = y_name,
      scale = is.null(y_min) && is.null(y_max),
      min = y_min,
      max = y_max,
      nameLocation = "end",
      nameTextStyle = list(color = EKIO_MUTED, align = "left"),
      axisLine = list(show = FALSE),
      axisTick = list(show = FALSE),
      axisLabel = list(color = EKIO_MUTED),
      splitLine = list(
        show = TRUE,
        lineStyle = list(color = EKIO_GRID, type = "dashed")
      )
    ) |>
    echarts4r::e_legend(
      show = legend,
      top = 6,
      textStyle = list(color = EKIO_INK)
    ) |>
    echarts4r::e_text_style(
      fontFamily = "Avenir, 'Helvetica Neue', Arial, sans-serif"
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
