
series <- readr::read_csv("data/firjan_series.csv")
shp <- sf::st_read("data/firjan_hdi.gpkg")
# Option 1: full shapefile

firjan_full <- series |>
  tidyr::pivot_wider(
    id_cols = c("code_muni", "name_muni_full"),
    names_from = c("index_type", "year"),
    values_from = "hdi"
    )

wide_map <- shp |>
  dplyr::select(code_muni, name_muni_full) |>
  dplyr::left_join(firjan_full, by = c("code_muni", "name_muni_full"))

# Option 2: stacked shapefile (filter)
firjan_stack <- series |>
  dplyr::select(code_muni, name_muni_full, year, hdi)

stacked_map <- shp |>
  dplyr::select(code_muni, name_muni_full) |>
  dplyr::left_join(firjan_stack, by = c("code_muni", "name_muni_full"))

# Option 3: named list
firjan_list <- split(firjan_stack, firjan_stack$year)
names(firjan_list) <- paste0("year_", names(firjan_list))
named_list <- purrr::map(firjan_list, function(df) { dplyr::left_join(shp, df) })

# Export to rds
# 22MB
readr::write_rds(wide_map, "data/firjan_wide.rds")
sf::st_write(wide_map, "data/firjan_wide.gpkg")
# 958 MB
# readr::write_rds(stacked_map, "data/firjan_stacked.rds")
# 1003 MB
# readr::write_rds(named_list, "data/firjan_list.rds")
