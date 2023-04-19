# Scrape the Wikipedia table with highest populated cities in Brazil

# library(rvest)
# library(xml2)
# library(janitor)
library(dplyr)

# Read IBGE ids for all cities in Brazil (see shapes.R)
id = readr::read_csv("data/id_muni.csv")

# Url
url = "https://pt.wikipedia.org/wiki/Lista_de_munic%C3%ADpios_do_Brasil_acima_de_cem_mil_habitantes_(2020)"
# Scrape the tables
parsed = xml2::read_html(url)
tables = rvest::html_table(parsed)

# Clean the data
df = tables[[2]]
df = janitor::clean_names(df)
# Join with ids
df_ranked = df |>
  select(name_muni = municipio, name_state = unidade_federativa, rank = pos) |>
  #mutate(name_state = stringr::str_to_title(name_state)) |>
  left_join(id, by = c("name_muni", "name_state"))
# Get all of the other cities and order alphabetically
df_alpha <- id |>
  anti_join(df_ranked, by = "name_muni") %>%
  mutate(rank = nrow(df_ranked) + 1:nrow(.))
# Stack tables
cities <- rbind(df_ranked, df_alpha)
# Order by rank
cities <- cities |>
  mutate(
    name_muni_full = factor(name_muni_full),
    name_muni_full = forcats::fct_reorder(name_muni_full, rank)
  ) |>
  arrange(name_muni_full)
# Export to rds to preserve factor
readr::write_rds(cities, "data/cities.rds")
