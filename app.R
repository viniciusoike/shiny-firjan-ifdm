# Packages and theme ----------------------------------------------------------

library(shiny)
library(bslib)
library(dplyr)
library(tmap)
library(echarts4r)

# brand.yml is loaded implicitly by bs_theme(brand = TRUE) — referenced here so
# dependency tools (renv) track it.
if (!requireNamespace("brand.yml", quietly = TRUE)) {
  stop("Package 'brand.yml' is required for the EKIO theme (_brand.yml).")
}

# Supporting R/ scripts (_setup.R, ekio_ui.R, map_hdi.R, plot_*.R, utils.R) are
# auto-sourced by Shiny before this file runs.

theme <- bs_theme(version = 5, brand = TRUE) |>
  bs_add_rules(readLines("styles.css"))

# Choices for the index shown on the map / KPIs. Names are display labels;
# values are the keys understood by get_map_variable() (see R/map_hdi.R).
INDEX_CHOICES <- c(
  "Geral" = "IDH",
  "Educação" = "IDH - Educação",
  "Saúde" = "IDH - Saúde",
  "Emprego & Renda" = "IDH - Renda"
)

# KPI cards: internal index_type key, factor label in series_data, display label
KPI_INDICES <- list(
  list(key = "overall", factor = "Geral (IFDM)", label = "IFDM Geral"),
  list(key = "education", factor = "Educação", label = "Educação"),
  list(key = "health", factor = "Saúde", label = "Saúde"),
  list(key = "income", factor = "Emprego & Renda", label = "Emprego & Renda")
)

YEARS <- 2023:2013

# Sidebar ----------------------------------------------------------------------

ekio_sidebar <- sidebar(
  width = 240,
  bg = "#0D1B2A",
  class = "ekio-sidebar",
  div(
    class = "ekio-brand",
    h1("EKIO"),
    p("Desenvolvimento Municipal")
  ),
  tags$nav(
    class = "ekio-nav",
    ekio_nav_section(
      "Painel",
      ekio_nav_item("dashboard", "Dashboard", "◉", active = TRUE)
    ),
    ekio_nav_section(
      "Dados",
      ekio_nav_item("download_data", "Baixar dados", "⤓")
    ),
    ekio_nav_section(
      NULL,
      ekio_nav_item("about", "Sobre", "ⓘ")
    )
  ),
  div(
    class = "ekio-sidebar-footer",
    div(class = "ekio-updated-label", "Fonte"),
    div(class = "ekio-updated-date", "IFDM 2025 · Base 2023")
  )
)

# Sidebar links drive the hidden navset; active state toggles client-side.
nav_js <- "
function ekioActivateNav(el) {
  $('.ekio-nav-item').removeClass('active').removeAttr('aria-current');
  $(el).addClass('active').attr('aria-current', 'page');
  Shiny.setInputValue('sidebar_nav', $(el).data('value'));
}
$(document).on('click', '.ekio-nav-item', function() {
  ekioActivateNav(this);
});
$(document).on('keydown', '.ekio-nav-item', function(e) {
  if (e.key === 'Enter' || e.key === ' ') {
    e.preventDefault();
    ekioActivateNav(this);
  }
});
"

# Pages ------------------------------------------------------------------------

map_options_popover <- popover(
  tags$a(
    class = "chart-tag",
    href = "#",
    style = "cursor:pointer;",
    `aria-label` = "Opções do mapa",
    "⚙ Opções"
  ),
  title = "Opções do mapa",
  selectInput(
    "palette",
    "Paleta de cores",
    choices = names(pals),
    selected = "3 (Vermelho-Azul)"
  ),
  selectInput(
    "style",
    "Tipo de mapa",
    choices = names(styles),
    selected = "Cluster"
  ),
  numericInput(
    "nbreaks",
    "Número de grupos",
    value = 6,
    min = 3,
    max = 10
  )
)

page_dashboard <- tagList(
  page_header(
    "Índice FIRJAN de Desenvolvimento Municipal",
    "Mapa interativo e séries do IFDM para os municípios brasileiros (2013–2023)."
  ),
  div(
    class = "filter-bar",
    filter_group(
      "Cidade",
      selectizeInput(
        "city_sel",
        NULL,
        choices = NULL,
        selected = "São Paulo (SP)",
        width = "240px"
      )
    ),
    filter_group(
      "Índice",
      class = "filter-chips",
      radioButtons(
        "variable",
        NULL,
        inline = TRUE,
        choices = INDEX_CHOICES,
        selected = "IDH"
      )
    ),
    filter_group(
      "Ano",
      selectInput(
        "year_sel",
        NULL,
        choices = YEARS,
        selected = 2023,
        width = "90px"
      )
    ),
    filter_group(
      "Comparação",
      style = "margin-left:auto;",
      selectInput(
        "geo",
        NULL,
        choices = c("Estado", "Região", "Brasil"),
        selected = "Estado",
        width = "120px"
      )
    )
  ),
  uiOutput("kpi_grid"),
  layout_columns(
    col_widths = c(8, 4),
    card(
      full_screen = TRUE,
      card_header(
        class = "chart-card-header",
        span(textOutput("map_title", inline = TRUE)),
        map_options_popover
      ),
      card_body(
        class = "p-0",
        shinycssloaders::withSpinner(
          tmap::tmapOutput("map", width = "100%", height = 560)
        )
      )
    ),
    card(
      full_screen = TRUE,
      card_header(
        class = "chart-card-header",
        span(textOutput("ranking_title", inline = TRUE)),
        span(class = "chart-tag", "ranking")
      ),
      card_body(DT::DTOutput("ranking_table"))
    )
  ),
  layout_columns(
    col_widths = c(6, 6),
    chart_card(
      "Distribuição do IFDM",
      "Ranking relativo (região)",
      plotOutput("plot_histogram", height = "320px")
    ),
    chart_card(
      "Ranking",
      "Ranking relativo (região)",
      plotOutput("plot_ranking", height = "320px")
    )
  ),
  layout_columns(
    col_widths = c(6, 6),
    chart_card(
      "Evolução dos indicadores",
      "Série Histórica",
      echarts4r::echarts4rOutput("plot_series", height = "320px")
    ),
    chart_card(
      "Benchmark",
      "Cidade × Brasil (média)",
      navset_pill(
        nav_panel(
          "Geral",
          echarts4r::echarts4rOutput("plot_bench_overall", height = "270px")
        ),
        nav_panel(
          "Saúde",
          echarts4r::echarts4rOutput("plot_bench_health", height = "270px")
        ),
        nav_panel(
          "Emprego & Renda",
          echarts4r::echarts4rOutput("plot_bench_income", height = "270px")
        ),
        nav_panel(
          "Educação",
          echarts4r::echarts4rOutput("plot_bench_education", height = "270px")
        )
      )
    )
  )
)

page_download <- tagList(
  page_header(
    "Baixar os dados",
    "Série completa do IFDM por município (2013–2023)."
  ),
  div(
    class = "filter-bar",
    downloadButton("download", "Download (csv)", class = "btn-sm"),
    downloadButton("download_xlsx", "Download (xlsx)", class = "btn-sm ms-2")
  ),
  chart_card(
    "Amostra dos dados",
    "csv · primeiras 1.000 linhas",
    full_screen = TRUE,
    card_body(DT::DTOutput("table_preview"))
  ),
  layout_columns(
    col_widths = c(8, 4),
    chart_card(
      "Documentação das colunas",
      NULL,
      full_screen = FALSE,
      card_body(DT::DTOutput("table_docs"))
    ),
    card(
      card_header(class = "chart-card-header", span("Metadados")),
      card_body(DT::DTOutput("table_meta"))
    )
  )
)

page_about <- tagList(
  page_header("Sobre", "Sobre este painel, o IFDM e a EKIO."),
  div(
    class = "about-content",
    h3("O painel"),
    p(HTML(about_app1)),
    p(HTML(about_app2)),
    h3("Sobre o IFDM"),
    div(
      class = "about-grid",
      about_card(
        "Três eixos",
        "O IFDM mede o desenvolvimento municipal em Educação, Saúde e Emprego & Renda."
      ),
      about_card(
        "Cobertura temporal",
        "Série anual de 2013 a 2023, com metodologia revisada (IFDM 2025, ano-base 2023)."
      ),
      about_card(
        "Produtor dos Dados",
        "Firjan (Índice Firjan de Desenvolvimento Municipal). Consultado pela última vez em 05/2026."
      )
    ),
    h3("Classificação"),
    HTML(text_classification),
    h3("Como usar"),
    p(HTML(text_use)),
    h3("Metodologia"),
    HTML(text_methods),
    h3("Autor"),
    p(HTML(aboutme_pt_1)),
    p(HTML(aboutme_pt_2)),
    tags$ul(
      tags$li(tags$a(
        href = "https://github.com/viniciusoike",
        "GitHub"
      )),
      tags$li(tags$a(
        href = "https://www.linkedin.com/in/vinicius-oike-993826a9/",
        "LinkedIn"
      )),
      tags$li(tags$a(
        href = "https://restateinsight.com",
        "Site Pessoal"
      ))
    )
  )
)

# UI ---------------------------------------------------------------------------

ui <- page_sidebar(
  window_title = "Dashboard IFDM — EKIO",
  theme = theme,
  fillable = FALSE,
  sidebar = ekio_sidebar,
  div(
    class = "ekio-pages",
    navset_hidden(
      id = "main_nav",
      nav_panel_hidden("dashboard", page_dashboard),
      nav_panel_hidden("download_data", page_download),
      nav_panel_hidden("about", page_about)
    )
  ),
  tags$script(HTML(nav_js))
)

# Server -----------------------------------------------------------------------

server <- function(input, output, session) {
  # Navigation ----
  observeEvent(input$sidebar_nav, {
    nav_select("main_nav", input$sidebar_nav)
  })

  # Inputs ----
  updateSelectizeInput(
    session,
    "city_sel",
    choices = city_list,
    selected = "São Paulo (SP)",
    server = TRUE
  )

  city <- reactive(input$city_sel)
  year <- reactive(as.integer(input$year_sel))
  geo <- reactive(input$geo)

  # Single lookup of the city's geo context — shared by all prep_* functions
  city_context <- reactive({
    req(city())
    ctx <- dplyr::filter(id_muni, name_muni_full == city())
    list(
      code_state = ctx$code_state,
      code_region = ctx$code_region,
      name_region = ctx$name_region,
      abbrev_state = stringr::str_extract(city(), "(?<=\\()[A-Z]{2}(?=\\))")
    )
  })

  # sf subset recomputed only when city or geo changes, not on palette/style/year
  map_shp <- reactive({
    req(city_context())
    prep_mapdata(city_context(), geo())
  })

  # series_data slice for the selected geo — shared by ranking table, ranking
  # plot, and histogram so the geo filter runs once per city/geo change
  series_geo <- reactive({
    req(city_context())
    switch(
      geo(),
      "Região" = dplyr::filter(
        series_data,
        name_region == city_context()$name_region
      ),
      "Estado" = dplyr::filter(
        series_data,
        code_state == city_context()$code_state
      ),
      "Brasil" = series_data
    )
  })

  # Map ----
  mapborder <- reactive(get_state_border(city_context(), geo()))

  output$map_title <- renderText({
    req(city())
    lbl <- names(INDEX_CHOICES)[match(input$variable, INDEX_CHOICES)]
    paste0(city(), " — ", lbl, " (", year(), ")")
  })

  # Full re-render only when the shape data changes (city or geo).
  # Visual params (year, variable, palette, style, n) are isolated to
  # avoid sending geometry to Leaflet on every click — see observer below.
  output$map <- renderTmap({
    req(map_shp())
    map_hdi(
      shp = map_shp(),
      city = isolate(city()),
      year = isolate(year()),
      variable = isolate(input$variable),
      title = isolate(city()),
      pal = isolate(input$palette),
      style = isolate(input$style),
      n = isolate(input$nbreaks),
      geo = isolate(geo()),
      border = isolate(mapborder())
    )
  })

  # tmapProxy updates only the visual layer (colours, breaks, legend) without
  # resending the geometry.  Triggers on year, index, palette, style or breaks.
  observeEvent(
    c(
      input$year_sel,
      input$variable,
      input$palette,
      input$style,
      input$nbreaks
    ),
    {
      req(map_shp())

      fill_col <- get_map_variable(year(), input$variable)

      popup_vars <- paste(
        c("overall", "education", "income", "health"),
        year(),
        sep = "_"
      )
      names(popup_vars) <- c(
        "IFDM",
        "IFDM - Educação",
        "IFDM - Emprego & Renda",
        "IFDM - Saúde"
      )

      fill_scale <- tm_scale_intervals(
        style = unname(styles[input$style]),
        n = input$nbreaks,
        values = unname(pals[input$palette])
      )

      lbl <- names(INDEX_CHOICES)[match(input$variable, INDEX_CHOICES)]
      title <- paste0(city(), " — ", lbl, " (", year(), ")")

      tmapProxy("map", session, {
        tm_remove_layer(401) +
          tm_shape(map_shp()) +
          tm_polygons(
            fill = fill_col,
            fill.scale = fill_scale,
            fill.legend = tm_legend(title = title),
            fill_alpha = 0.7,
            col = "gray50",
            lwd = 0.5,
            id = "name_muni",
            popup = tm_popup(vars = popup_vars, format = tm_label_format(digits = 3)),
            zindex = 401
          )
      })
    },
    ignoreInit = TRUE
  )

  # Ranking table ----
  ranking_tbl <- reactive({
    req(city(), year(), input$variable, series_geo())
    prep_ranking_table(city(), year(), input$variable, series_geo())
  })

  output$ranking_title <- renderText({
    req(city())
    lbl <- names(INDEX_CHOICES)[match(input$variable, INDEX_CHOICES)]
    paste0("Ranking — ", lbl, " · ", geo(), " (", year(), ")")
  })

  output$ranking_table <- DT::renderDT({
    d <- ranking_tbl()
    sel_muni <- d$name_muni[d$is_city]

    tbl <- d |>
      dplyr::transmute(`#` = rank, `Município` = name_muni, IFDM = hdi)

    dt <- DT::datatable(
      tbl,
      rownames = FALSE,
      selection = "none",
      options = list(
        pageLength = 12,
        dom = "ftip",
        columnDefs = list(
          list(className = "dt-center", targets = c(0, 2))
        ),
        language = list(
          url = "//cdn.datatables.net/plug-ins/1.13.6/i18n/pt-BR.json"
        )
      )
    ) |>
      DT::formatRound("IFDM", digits = 3, mark = ".", dec.mark = ",")

    if (length(sel_muni) == 1) {
      dt <- DT::formatStyle(
        dt,
        "Município",
        target = "row",
        fontWeight = DT::styleEqual(sel_muni, "700"),
        backgroundColor = DT::styleEqual(sel_muni, "#EBF2FA")
      )
    }
    dt
  })

  # KPI cards ----
  output$kpi_grid <- renderUI({
    req(city(), year())
    d <- df_series()

    cards <- lapply(KPI_INDICES, function(ix) {
      s <- d |>
        dplyr::filter(index_type == ix$factor) |>
        dplyr::arrange(year)
      cur <- s$hdi[s$year == year()]
      prev <- s$hdi[s$year == (year() - 1L)]
      delta <- if (length(cur) == 1 && length(prev) == 1) {
        cur - prev
      } else {
        NA_real_
      }
      label_y <- if (year() > min(YEARS)) paste("vs", year() - 1L) else "—"
      kpi_card(
        label = ix$label,
        value = fmt_ifdm(if (length(cur) == 1) cur else NA_real_),
        delta = ifdm_delta_lbl(delta),
        period = label_y,
        spark_values = s$hdi,
        color = INDEX_COLOR_CLASS[[ix$key]],
        dir = pp_dir(delta)
      )
    })

    div(class = "kpi-grid", cards)
  })

  # Plots ----
  output$plot_ranking <- renderPlot(
    {
      req(city(), series_geo())
      plot_ranking(city(), year(), geo(), series_geo())
    },
    res = 96
  )

  output$plot_histogram <- renderPlot(
    {
      req(city(), series_geo())
      plot_histogram(city(), year(), series_geo())
    },
    res = 96
  )

  df_series <- reactive({
    req(city())
    prep_series_data(city())
  })
  df_benchmark <- reactive(prep_benchmark(df_series()))

  output$plot_series <- echarts4r::renderEcharts4r({
    plot_series(df_series())
  })
  # Benchmark: one city-vs-média chart per index, split across pill tabs so the
  # card stays compact. Each tab is an independent echarts widget.
  output$plot_bench_overall <- echarts4r::renderEcharts4r({
    plot_benchmark(df_benchmark(), "Geral (IFDM)")
  })
  output$plot_bench_health <- echarts4r::renderEcharts4r({
    plot_benchmark(df_benchmark(), "Saúde")
  })
  output$plot_bench_income <- echarts4r::renderEcharts4r({
    plot_benchmark(df_benchmark(), "Emprego & Renda")
  })
  output$plot_bench_education <- echarts4r::renderEcharts4r({
    plot_benchmark(df_benchmark(), "Educação")
  })

  # Download ----
  df_download <- reactive({
    series_data |>
      dplyr::select(year, index_type, name_region, code_muni, name_muni, hdi) |>
      dplyr::rename(
        ano = year,
        indicador = index_type,
        nome_regiao = name_region,
        nome_cidade = name_muni,
        ifdm = hdi
      )
  })

  output$table_preview <- DT::renderDT({
    DT::datatable(head(df_download(), 1000), options = list(pageLength = 10))
  })

  output$download <- downloadHandler(
    filename = function() "data_firjan_ifdm.csv",
    content = function(file) write.csv(df_download(), file, row.names = FALSE)
  )

  output$download_xlsx <- downloadHandler(
    filename = function() "data_firjan_ifdm.xlsx",
    content = function(file) writexl::write_xlsx(df_download(), file)
  )

  output$table_docs <- DT::renderDT({
    DT::datatable(
      doc_colunas,
      rownames = FALSE,
      selection = "none",
      options = list(dom = "t", pageLength = 10, ordering = FALSE)
    )
  })

  output$table_meta <- DT::renderDT({
    DT::datatable(
      doc_meta,
      rownames = FALSE,
      selection = "none",
      options = list(dom = "t", pageLength = 10, ordering = FALSE)
    )
  })
}

shinyApp(ui, server)
