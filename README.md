# Dashboard IFDM

[![Abrir app](https://img.shields.io/badge/Shiny-Abrir%20Dashboard-blue?logo=r)](https://viniciusoike-shiny-firjan-ifdm.share.connect.posit.cloud)

Este painel apresenta dados de desenvolvimento dos municípios brasileiros pelo
**Índice FIRJAN de Desenvolvimento Municipal (IFDM)**. O IFDM mede três eixos —
Educação, Saúde e Emprego & Renda — com base em dados públicos oficiais. Sua
leitura é semelhante à do IDH da ONU, mas o IFDM é divulgado anualmente.

Os dados abrangem 2013 a 2023 (IFDM 2025, ano-base 2023, com metodologia
revisada). O painel inclui um mapa coroplético interativo, comparações do
município escolhido com seu estado, região e o Brasil, além da evolução dos
indicadores ao longo do tempo.

[**→ Abrir o dashboard**](https://viniciusoike-shiny-firjan-ifdm.share.connect.posit.cloud)

---

An interactive dashboard for Brazil's FIRJAN Municipal Development Index (IFDM),
with a choropleth map, city-to-state benchmarks, and annual time series. Built
with R Shiny. The app is in Portuguese.

![Dashboard IFDM preview](docs/preview.png)

## Features

- **Choropleth map** — filter by year (2013–2023) and any of the four IFDM
  dimensions (Overall, Education, Health, Employment & Income).
- **Autocomplete search** — search for municipalities, ordered by population.
- **Benchmarks** — ranking, score distribution, and comparison against the
  selected state, region, or national average.
- **Time series** — annual evolution of each indicator for the selected city.
- **Data download** — export the full municipal dataset as CSV or XLSX.
- **About section** — methodology notes, the IFDM reading scale, and contact
  information.

## Built with

R Shiny and [bslib](https://rstudio.github.io/bslib/), with styling defined in
`_brand.yaml` and `styles.css`; interactive maps via
[tmap](https://r-tmap.github.io/tmap/) v4 and time series via
[echarts4r](https://echarts4r.john-coene.com/). Dependencies are pinned with
[renv](https://rstudio.github.io/renv/).

## Data

The FIRJAN source workbooks (from the
[IFDM downloads page](https://www.firjan.com.br/ifdm/downloads/)) live in
`data-raw/`; the scripts there build the processed datasets in `data/`.
