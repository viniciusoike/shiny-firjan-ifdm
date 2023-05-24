library(tmap)
library(tmaptools)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
tmap_mode("view")

# Colors from MetBrewer Hokusai1
colors_div <- c("#b75347", "#224b5e")
colors_qual <- c("#6d2f20", "#e09351", "#94b594", "#224b5e")

# map_data <- sf::st_read("data/firjan_hdi.gpkg")
hdi_data <- data.table::fread("data/firjan_hdi.csv")

cities <- readr::read_rds("data/cities.rds")
city_list <- unique(cities$name_muni_full)

# Map data
firjan_full <- readr::read_rds("data/firjan_wide.rds")
firjan_full <- firjan_full |>
  dplyr::select(-name_muni_full) |>
  dplyr::left_join(cities, by = "code_muni")
# firjan_full <- firjan_full |>
#   dplyr::mutate(
#     code_region = as.numeric(substring(code_muni, 1, 1)),
#     code_state  = as.numeric(substring(code_muni, 1, 2)),
#     name_muni = stringr::str_extra
#   )
id_muni <- readr::read_csv("data/id_muni.csv")
shp_hdi <- dplyr::select(firjan_full, name_muni_full)

state_border <- sf::st_read("data/shape_state_border.gpkg", quiet = TRUE)

# Series data
series_data <- data.table::fread(here::here("data/firjan_series.csv"))

# Labels and levels for factor
lvls <- c("overall", "health", "income", "education")
lbls <- c("Geral (IFDM)", "Saúde", "Renda", "Educação")

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
