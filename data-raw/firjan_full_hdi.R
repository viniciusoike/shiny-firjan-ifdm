## Firjan IFDM wide map data (2013-2023) -----------------------------------
# Builds data/firjan_wide.rds: one sf row per municipio with one column per
# index_type/year ("overall_2013", "education_2013", ...) plus geometry, as
# consumed by R/_setup.R and R/map_hdi.R.
#
# Geometry is reused from the previous build (geobr 2020 municipal borders,
# same 5570 municipios), so no shapefile download is required.

library(dplyr)
library(tidyr)
library(sf)

series <- readr::read_csv("data/firjan_series.csv", show_col_types = FALSE)

# Long -> wide: columns "<index_type>_<year>" to match get_map_variable()
firjan_wide_vals <- series |>
  pivot_wider(
    id_cols = c("code_muni", "name_muni_full"),
    names_from = c("index_type", "year"),
    values_from = "hdi"
  )

# Geometry from the archived build (falls back to the live file if present)
geom_src <- if (file.exists("data/_archive_2005_2016/firjan_wide.rds")) {
  "data/_archive_2005_2016/firjan_wide.rds"
} else {
  "data/firjan_wide.rds"
}
shp <- readr::read_rds(geom_src) |>
  select(code_muni, name_muni_full)

wide_map <- shp |>
  left_join(firjan_wide_vals, by = c("code_muni", "name_muni_full"))

message(sprintf(
  "wide rows: %d | value cols: %d | year span in names: %s",
  nrow(wide_map), ncol(wide_map) - 3,
  paste(range(as.integer(stringr::str_extract(
    grep("_\\d{4}$", names(wide_map), value = TRUE), "\\d{4}"))), collapse = "-")
))

readr::write_rds(wide_map, "data/firjan_wide.rds")
