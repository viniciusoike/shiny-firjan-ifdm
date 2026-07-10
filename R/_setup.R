library(tmap)
library(tmaptools)
library(ggplot2)
library(echarts4r)
library(dplyr)
library(tidyr)
tmap_mode("view")

# Plot colours come from the EKIO palette in R/ekio_ui.R
# (INDEX_PAL_LABELLED, BENCH_PAL, theme_ekio).

cities <- readr::read_rds("data/cities.rds")
city_list <- unique(cities$name_muni_full)

# Map data
firjan_full <- readr::read_rds("data/firjan_wide.rds")
firjan_full <- firjan_full |>
  dplyr::select(-name_muni_full) |>
  dplyr::left_join(cities, by = "code_muni")

id_muni <- readr::read_csv("data/id_muni.csv")
shp_hdi <- dplyr::select(firjan_full, name_muni_full)

state_border <- sf::st_read("data/shape_state_border.gpkg", quiet = TRUE)

# Series data

series_data <- readr::read_csv(
  here::here("data/firjan_series.csv"),
  show_col_types = FALSE
)

# Labels and levels for factor
lvls <- c("overall", "health", "income", "education")
lbls <- c("Geral (IFDM)", "Saúde", "Emprego & Renda", "Educação")

series_data <- series_data |>
  dplyr::mutate(index_type = factor(index_type, levels = lvls, labels = lbls))

# Compute the average HDI for Brazil (simple average of all cities)
benchmark_bra <- series_data |>
  dplyr::group_by(index_type, year) |>
  dplyr::summarise(avg = mean(hdi, na.rm = TRUE)) |>
  dplyr::ungroup() |>
  dplyr::mutate(name_muni_full = "Média Brasil") |>
  dplyr::rename(hdi = avg)
# Compute the average HDI for States (simple average of all cities)
benchmark_state <- series_data |>
  dplyr::group_by(index_type, abbrev_state, year) |>
  dplyr::summarise(avg = mean(hdi, na.rm = TRUE)) |>
  dplyr::ungroup() |>
  dplyr::mutate(name_muni_full = "Média Estado") |>
  dplyr::rename(hdi = avg)
