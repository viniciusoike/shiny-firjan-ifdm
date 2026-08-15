prep_benchmark <- function(df, type = "brazil") {
  if (type == "brazil") {
    benchmark <- df |>
      dplyr::select(year, index_type, name_muni_full, hdi) |>
      dplyr::bind_rows(benchmark_bra)
  }
  if (type == "state") {
    benchmark <- df |>
      dplyr::select(year, index_type, name_muni_full, hdi) |>
      dplyr::bind_rows(benchmark_state)
  }
  # Round numbers for better presentation
  benchmark <- dplyr::mutate(benchmark, hdi = round(hdi, 3))
  return(benchmark)
}

prep_series_data <- function(city) {
  df <- series_data |>
    dplyr::filter(name_muni_full == city) |>
    dplyr::mutate(hdi = round(hdi, 3))
  return(df)
}

# pt-BR value formatter for echarts tooltips (3 decimals, comma separator)
.ifdm_value_fmt <- htmlwidgets::JS(
  "function(v){return (v == null) ? '–' :
     v.toLocaleString('pt-BR', {minimumFractionDigits: 3, maximumFractionDigits: 3});}"
)

plot_series <- function(df) {
  # Series are created per index_type group, so colours must follow the factor
  # level order to map each index to its brand colour.
  cols <- unname(INDEX_PAL_LABELLED[levels(df$index_type)])

  # Years are discrete -> use a (sorted) category x-axis so labels render as
  # plain "2013" rather than the value-axis default "2,013".
  df <- df |>
    dplyr::arrange(year) |>
    dplyr::mutate(year = as.character(year))

  df |>
    dplyr::group_by(index_type) |>
    echarts4r::e_charts(year) |>
    echarts4r::e_line(hdi, symbolSize = 7) |>
    e_ekio(palette = cols) |>
    echarts4r::e_tooltip(
      trigger = "axis",
      valueFormatter = .ifdm_value_fmt
    )
}

# One city-vs-benchmark line chart for a single index (`index_label` must match
# an index_type factor label, e.g. "Geral (IFDM)"). Rendered one per tab in the
# Benchmark card so the whole comparison stays compact.
plot_benchmark <- function(df, index_label) {
  # The selected city is plotted against the benchmark ("Média Brasil/Estado");
  # order the factor city-first so BENCH_PAL maps city -> blue, média -> orange.
  is_bench <- startsWith(as.character(df$name_muni_full), "Média")
  bench_name <- unique(as.character(df$name_muni_full[is_bench]))
  city_name <- setdiff(unique(as.character(df$name_muni_full)), bench_name)
  df$name_muni_full <- factor(
    df$name_muni_full,
    levels = c(city_name, bench_name)
  )

  df |>
    dplyr::filter(index_type == index_label) |>
    dplyr::arrange(year) |>
    dplyr::mutate(year = as.character(year)) |>
    dplyr::group_by(name_muni_full) |>
    echarts4r::e_charts(year) |>
    echarts4r::e_line(hdi, symbolSize = 6) |>
    e_ekio(palette = BENCH_PAL, y_min = 0, y_max = 1) |>
    echarts4r::e_tooltip(
      trigger = "axis",
      valueFormatter = .ifdm_value_fmt
    )
}
