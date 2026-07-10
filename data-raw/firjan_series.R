## Firjan IFDM time series ETL (2013-2023) ---------------------------------
# Builds data/firjan_series.csv (long format) from the official FIRJAN
# historical series workbook. Output schema is kept identical to the previous
# (2005-2016) pipeline so the app needs no structural change.
#
# Source: "Serie-Historica-IFDM-2013-a-2023.xlsx"
#   - 4 sheets (Geral, Educacao, Saude, Emprego&Renda), header in row 1.
#   - Columns: COD_MUNIC (6-digit), SIGLA_UF, NOME_MUNIC, then per-year blocks
#     "Ranking Estadual IFDM <comp> <year>", "Ranking IFDM <comp> <year>",
#     "IFDM <comp> <year>".

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

path <- "data-raw/Serie-Historica-IFDM-2013-a-2023.xlsx"

## Helpers -----------------------------------------------------------------

# The workbook carries the 6-digit IBGE code (COD_MUNIC). We recover the
# official 7-digit code by joining on the 6-digit prefix of id_muni rather
# than reconstructing the check digit (which is unreliable for a handful of
# municipios), guaranteeing a match for all 5570 cities.

# Map a sheet name to the internal index_type key used across the app
sheet_index_type <- function(sheet) {
  dplyr::case_when(
    str_detect(sheet, "Geral")                 ~ "overall",
    str_detect(sheet, regex("Educa", TRUE))    ~ "education",
    str_detect(sheet, regex("Sa.de", TRUE))    ~ "health",
    str_detect(sheet, regex("Emprego", TRUE))  ~ "income",
    TRUE ~ NA_character_
  )
}

# Read one sheet and return long (code_muni6, year, hdi, rank, index_type)
import_series_sheet <- function(sheet) {

  raw <- suppressMessages(
    read_excel(path, sheet = sheet, na = c("", "*", "ND", "-"))
  )

  # Value columns: "IFDM ... <year>" (national rank cols start with "Ranking")
  val_cols  <- str_subset(names(raw), "^IFDM\\b.*\\d{4}$")
  rank_cols <- str_subset(names(raw), "^Ranking IFDM\\b.*\\d{4}$")

  vals <- raw |>
    select(code_muni6 = COD_MUNIC, all_of(val_cols)) |>
    pivot_longer(all_of(val_cols), names_to = "name", values_to = "hdi") |>
    mutate(year = as.integer(str_extract(name, "\\d{4}")), name = NULL)

  ranks <- raw |>
    select(code_muni6 = COD_MUNIC, all_of(rank_cols)) |>
    pivot_longer(all_of(rank_cols), names_to = "name", values_to = "rank") |>
    mutate(year = as.integer(str_extract(name, "\\d{4}")), name = NULL)

  vals |>
    left_join(ranks, by = c("code_muni6", "year")) |>
    mutate(index_type = sheet_index_type(sheet))
}

## Import & stack ----------------------------------------------------------

sheets <- excel_sheets(path)
series <- bind_rows(lapply(sheets, import_series_sheet))

## City identifiers --------------------------------------------------------

id <- readr::read_csv("data/id_muni.csv", show_col_types = FALSE)

subid <- id |>
  mutate(
    code_muni6 = as.numeric(substr(as.character(code_muni), 1, 6)),
    name_simplified = stringi::stri_trans_general(name_muni, id = "latin-ascii"),
    name_simplified = paste(abbrev_state, name_simplified),
    name_simplified = str_to_lower(name_simplified),
    name_simplified = str_replace_all(name_simplified, " ", "_")
  ) |>
  select(code_muni6, code_muni, name_simplified, name_region, abbrev_state,
         name_muni, name_state, code_state, name_muni_full)

## Join & arrange to match the legacy schema -------------------------------

series <- series |>
  mutate(code_muni6 = as.numeric(code_muni6)) |>
  left_join(subid, by = "code_muni6") |>
  mutate(hdi = round(as.numeric(hdi), 7), rank = as.integer(rank)) |>
  select(index_type, name_region, abbrev_state, name_muni, year, code_muni,
         name_simplified, name_state, code_state, name_muni_full, hdi, rank) |>
  arrange(index_type, code_muni, year)

## Report & export ---------------------------------------------------------

unmatched <- sum(is.na(series$name_muni_full))
message(sprintf(
  "rows: %d | years: %d-%d | municipios: %d | index_type: %s | unmatched ids: %d",
  nrow(series), min(series$year), max(series$year),
  dplyr::n_distinct(series$code_muni),
  paste(sort(unique(series$index_type)), collapse = ", "), unmatched
))

readr::write_csv(series, "data/firjan_series.csv")
