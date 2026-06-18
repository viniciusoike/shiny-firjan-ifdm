# Packages and theme ----------------------------------------------------------

library(shiny)
library(bslib)
library(dplyr)
library(tmap)
library(plotly)

# brand.yml is loaded implicitly by bs_theme(brand = TRUE) — referenced here so
# dependency tools (renv) track it.
if (!requireNamespace("brand.yml", quietly = TRUE)) {
  stop("Package 'brand.yml' is required for the EKIO theme (_brand.yml).")
}

# Supporting R/ scripts (_setup.R, ekio_ui.R, map_hdi.R, plot_*.R, utils.R) are
# auto-sourced by Shiny before this file runs.

theme <- bslib::bs_theme(version = 5, brand = TRUE) |>
  bslib::bs_add_rules(readLines("styles.css"))

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

ekio_sidebar <- bslib::sidebar(
  width = 240,
  bg = "#0D1B2A",
  class = "ekio-sidebar",
  shiny::div(
    class = "ekio-brand",
    shiny::h1("EKIO"),
    shiny::p("Desenvolvimento Municipal")
  ),
  shiny::tags$nav(
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
  shiny::div(
    class = "ekio-sidebar-footer",
    shiny::div(class = "ekio-updated-label", "Fonte"),
    shiny::div(class = "ekio-updated-date", "IFDM 2025 · base 2023")
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

map_options_popover <- bslib::popover(
  shiny::tags$a(
    class = "chart-tag",
    href = "#",
    style = "cursor:pointer;",
    `aria-label` = "Opções do mapa",
    "⚙ Opções"
  ),
  title = "Opções do mapa",
  shiny::selectInput(
    "palette",
    "Paleta de cores",
    choices = names(pals),
    selected = "3 (Vermelho-Azul)"
  ),
  shiny::selectInput(
    "style",
    "Tipo de mapa",
    choices = names(styles),
    selected = "Cluster"
  ),
  shiny::numericInput(
    "nbreaks",
    "Número de grupos",
    value = 6,
    min = 3,
    max = 10
  )
)

page_dashboard <- shiny::tagList(
  page_header(
    "Índice FIRJAN de Desenvolvimento Municipal",
    "Mapa interativo e séries do IFDM para os municípios brasileiros (2013–2023)."
  ),
  shiny::div(
    class = "filter-bar",
    filter_group(
      "Cidade",
      shiny::selectizeInput(
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
      shiny::radioButtons(
        "variable",
        NULL,
        inline = TRUE,
        choices = INDEX_CHOICES,
        selected = "IDH"
      )
    ),
    filter_group(
      "Ano",
      shiny::selectInput(
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
      shiny::selectInput(
        "geo",
        NULL,
        choices = c("Estado", "Região", "Brasil"),
        selected = "Estado",
        width = "120px"
      )
    )
  ),
  shiny::uiOutput("kpi_grid"),
  bslib::layout_columns(
    col_widths = c(8, 4),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        class = "chart-card-header",
        shiny::span(shiny::textOutput("map_title", inline = TRUE)),
        map_options_popover
      ),
      bslib::card_body(
        class = "p-0",
        shinycssloaders::withSpinner(
          tmap::tmapOutput("map", width = "100%", height = 560)
        )
      )
    ),
    bslib::navset_card_tab(
      title = "IFDM",
      bslib::nav_panel("Sobre", shiny::HTML(text_about)),
      bslib::nav_panel("Classificação", shiny::HTML(text_classification)),
      bslib::nav_panel("Como usar", shiny::HTML(text_use)),
      bslib::nav_panel("Metodologia", shiny::HTML(text_methods))
    )
  ),
  bslib::layout_columns(
    col_widths = c(6, 6),
    chart_card(
      "Distribuição do IFDM",
      "histograma",
      shiny::plotOutput("plot_histogram", height = "320px")
    ),
    chart_card(
      "Ranking",
      "posição",
      shiny::plotOutput("plot_ranking", height = "320px")
    )
  ),
  bslib::layout_columns(
    col_widths = c(6, 6),
    chart_card(
      "Evolução dos indicadores",
      "série 2013–2023",
      plotly::plotlyOutput("plot_series", height = "320px")
    ),
    chart_card(
      "Benchmark",
      "cidade × média",
      plotly::plotlyOutput("plot_series_facet", height = "320px")
    )
  )
)

page_download <- shiny::tagList(
  page_header(
    "Baixar os dados",
    "Série completa do IFDM por município (2013–2023)."
  ),
  shiny::div(
    class = "filter-bar",
    shiny::downloadButton("download", "Download (csv)", class = "btn-sm")
  ),
  chart_card(
    "Amostra dos dados",
    "csv",
    full_screen = TRUE,
    bslib::card_body(DT::DTOutput("table_preview"))
  )
)

page_about <- shiny::tagList(
  page_header("Sobre", "Sobre este painel, o IFDM e a EKIO."),
  shiny::div(
    class = "about-content",
    shiny::h3("O painel"),
    shiny::p(shiny::HTML(about_app1)),
    shiny::p(shiny::HTML(about_app2)),
    shiny::h3("Sobre o IFDM"),
    shiny::div(
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
        "Fonte",
        "Sistema FIRJAN, a partir de bases públicas oficiais. Leitura análoga à do IDH da ONU."
      )
    ),
    shiny::h3("Autor"),
    shiny::p(shiny::HTML(aboutme_pt)),
    shiny::tags$ul(
      shiny::tags$li(shiny::tags$a(
        href = "https://github.com/viniciusoike",
        "GitHub"
      )),
      shiny::tags$li(shiny::tags$a(
        href = "https://www.linkedin.com/in/vinicius-oike-993826a9/",
        "LinkedIn"
      )),
      shiny::tags$li(shiny::tags$a(
        href = "https://restateinsight.com",
        "Site Pessoal"
      ))
    )
  )
)

# UI ---------------------------------------------------------------------------

ui <- bslib::page_sidebar(
  window_title = "Dashboard IFDM — EKIO",
  theme = theme,
  fillable = FALSE,
  sidebar = ekio_sidebar,
  shiny::div(
    class = "ekio-pages",
    bslib::navset_hidden(
      id = "main_nav",
      bslib::nav_panel_hidden("dashboard", page_dashboard),
      bslib::nav_panel_hidden("download_data", page_download),
      bslib::nav_panel_hidden("about", page_about)
    )
  ),
  shiny::tags$script(shiny::HTML(nav_js))
)

# Server -----------------------------------------------------------------------

server <- function(input, output, session) {
  # Navigation ----
  shiny::observeEvent(input$sidebar_nav, {
    bslib::nav_select("main_nav", input$sidebar_nav)
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

  # Map ----
  mapborder <- reactive(get_state_border(input$city_sel, input$geo))

  output$map_title <- renderText({
    req(city())
    lbl <- names(INDEX_CHOICES)[match(input$variable, INDEX_CHOICES)]
    paste0(city(), " — ", lbl, " (", year(), ")")
  })

  output$map <- renderTmap({
    req(city(), year(), input$palette, geo())
    map_hdi(
      shp = NULL,
      city = city(),
      year = year(),
      variable = input$variable,
      title = city(),
      pal = input$palette,
      style = input$style,
      n = input$nbreaks,
      geo = geo(),
      border = mapborder()
    )
  })

  # KPI cards ----
  output$kpi_grid <- renderUI({
    req(city(), year())
    d <- dplyr::filter(series_data, name_muni_full == city())

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

    shiny::div(class = "kpi-grid", cards)
  })

  # Plots ----
  output$plot_ranking <- renderPlot(
    {
      req(city())
      plot_ranking(city(), year(), geo())
    },
    res = 96
  )

  output$plot_histogram <- renderPlot(
    {
      req(city())
      plot_histogram(city(), year(), geo())
    },
    res = 96
  )

  df_series <- reactive({
    req(city())
    prep_series_data(city())
  })
  df_benchmark <- reactive(prep_benchmark(df_series()))

  output$plot_series <- renderPlotly({
    plot_series(df_series())
  })
  output$plot_series_facet <- renderPlotly({
    plot_series_comparison(df_benchmark())
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
}

shinyApp(ui, server)
