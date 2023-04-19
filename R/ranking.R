# library(readxl)
# library(dplyr)
# library(tidyr)
# library(ggplot2)
#
# cnames <- c("abbrev_state", "name_muni", "idhm", "idh_r", "idh_e", "idh_s")
#
# firjan <- read_excel(
#   "data-raw/Ranking IFDM - Ordem de pontua__o.xlsx",
#   range = "D11:I5575",
#   col_names = cnames,
#   na = "ND"
#   )
#
# firjan <- firjan |>
#   pivot_longer(
#     cols = starts_with("idh"),
#     names_to = "name_index",
#     values_to = "index"
#     ) |>
#   group_by(name_index) |>
#   mutate(rank_index = rank(-index, na.last = "keep")) |>
#   ungroup()
#
# muni = "Porto Alegre"
# state = "RS"
#
# df <- firjan |>
#   dplyr::filter(abbrev_state == state) |>
#   dplyr::mutate(
#     highlight = ifelse(name_muni == muni, name_muni, NA)
#   ) |>
#   group_by(name_index) |>
#   mutate(rank_state = rank(-index, na.last = "keep")) |>
#   ungroup()
#
# df <- df |>
#   dplyr::mutate(
#     label_index = factor(
#       name_index,
#       levels = c("idhm", "idh_e", "idh_r", "idh_s"),
#       labels = c("Geral (IFDM)", "Saúde", "Renda", "Educação")
#       )
#   )



