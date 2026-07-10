# Dashboard IFDM

An interactive dashboard that visualizes municipal development across every
city in Brazil, based on the **Índice FIRJAN de Desenvolvimento Municipal
(IFDM)**. The IFDM measures development in three areas — Education, Health, and
Employment & Income — from official public data, with a reading analogous to
the UN's Human Development Index (HDI). Unlike the HDI, the IFDM is published
annually.

The dashboard covers the **2013–2023** series (IFDM 2025, base year 2023,
revised methodology). It offers an interactive choropleth map, a comparison of
a selected city against its state / region / Brazil, and the evolution of each
indicator over time.

[**Open the dashboard →**](https://viniciusoike.shinyapps.io/shiny-firjan-ifdm)
(the app is in Portuguese).

## Built with

R Shiny and [bslib](https://rstudio.github.io/bslib/), themed with the EKIO
brand (`_brand.yml` + `styles.css`); interactive maps via
[tmap](https://r-tmap.github.io/tmap/) v4 and time series via
[plotly](https://plotly.com/r/). Dependencies are pinned with
[renv](https://rstudio.github.io/renv/).

## Data

The FIRJAN source workbooks (from the
[IFDM downloads page](https://www.firjan.com.br/ifdm/downloads/)) live in
`data-raw/`; the scripts there build the processed datasets in `data/`.
