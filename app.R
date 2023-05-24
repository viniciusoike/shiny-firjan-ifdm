library(shiny)
library(shinydashboard)

header <- dashboardHeader(
  title = HTML("Dashboard IFDM - Vinicius Oike"),
  titleWidth = 550,
  dropdownMenu(
    type = "message",
    icon = icon("share-alt"),
    messageItem(
      from = "Twitter",
      message = "",
      icon = icon("twitter")
      ),
    messageItem(
      from = "LinkedIn",
      message = "",
      icon = icon("linkedin")
      )
    )
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", icon = icon("dashboard"), tabName = "dashboard"),
      selectizeInput("city_sel",
        choices = NULL,
        label = "Escolha Cidade",
        selected = "São Paulo (SP)"),
      # selectizeInput("city_sel",
      #              label = "Escolha uma cidade",
      #              choices = city_list,
      #              selected = "São Paulo (SP)",
      #              multiple = FALSE)),
    selectInput("variable",
                label = "Índice para visualizar",
                choices = names(vl),
                selected = "IDH"),
    selectInput("year_sel",
                label = "Ano",
                choices = 2005:2016,
                selected = 2010),
    selectInput("palette",
                label = "Paleta de cores",
                choices = names(pals),
                selected = "3 (Vermelho-Azul)"),
    selectInput("style",
                label = "Tipo de mapa",
                choices = names(styles),
                selected = "Cluster"),
    numericInput("nbreaks",
                 label = "Número de grupos (Não se aplica ao Básico)",
                 value = 6,
                 min = 3,
                 max = 10),
    selectInput("geo",
                label = "Comparação Geográfica",
                choices = c("Estado", "Região", "Brasil"),
                selected = "Estado"),
    menuItem("Baixar dados", icon = icon("file-download"), tabName = "download_data"),
    menuItem("Sobre mim", icon = icon("info-circle"), tabName = "about_me")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem("dashboard",
            # Dashboard content
            fluidRow(
              column(
                12,
                h2("Índice de Desenvolvimento Humano (Firjan)")
              )
            ),
            fluidRow(
              box(
                width = 9,
                solidHeader = TRUE,
                shinycssloaders::withSpinner(
                  tmapOutput("map", width = "100%", height = 700)
                )
              ),
              column(
                3,
                infoBoxOutput("box_hdi", width = NULL),
                infoBoxOutput("box_hdi_educ", width = NULL),
                infoBoxOutput("box_hdi_income", width = NULL),
                infoBoxOutput("box_hdi_health", width = NULL),
                tabBox(
                  title = "",
                  width = NULL,
                  # The id lets us use input$tabset1 on the server to find the current tab
                  id = "tabset1", height = "300px",
                  tabPanel("Sobre", HTML(text_about)),
                  tabPanel("Classificação", HTML(text_classification)),
                  tabPanel("Como usar", HTML(text_use)),
                  tabPanel("Metodologia", HTML(text_methods))
                )
              )
            ),
            fluidRow(
              column(6,
                     box(
                       width = NULL,
                       solidHeader = TRUE,
                       collapsible = TRUE,
                       plotOutput("plot_histogram")
                     )),
              column(6,
                     box(
                       width = NULL,
                       solidHeader = TRUE,
                       collapsible = TRUE,
                       plotOutput("plot_ranking")
                     ))
            ),
            fluidRow(
              column(6, plotlyOutput("plot_series_facet")),
              column(6, plotlyOutput("plot_series"))
            )
    ),
    tabItem("download_data",
      fluidRow(column(12, h2("Baixar os dados"))),
      fluidRow(column(12)),
      fluidRow(column(width = 12, downloadButton("download", "Download (csv)"))),
      box(
        title = "Amostra dos dados",
        width = NULL,
        status = "primary",
        solidHeader = TRUE,
        height = "550px",
        DT::DTOutput("table_preview")
        )
    ),
    tabItem("about_me",
    dashboardBody(
      fluidRow(
        column(4,
               tags$div(
                 class = "container-fluid",
                 tags$h1("Vinicius Oike Reginatto"),
                 tags$h3("Sobre mim"),
                 tags$p(aboutme_pt),
                 tags$h3("About Me"),
                 tags$p(aboutme_en),
                 tags$h5("My links:"),
                 tags$ul(
                   tags$li(tags$a(href = "https://twitter.com/viniciusoike", icon("twitter"), "Twitter")),
                   tags$li(tags$a(href = "https://github.com/viniciusoike", icon("github"), "GitHub")),
                   tags$li(tags$a(href = "https://www.linkedin.com/in/vinicius-oike-993826a9/", icon("linkedin"), "LinkedIn")),
                   tags$li(tags$a(href = "https://www.modelodomundo.com", icon("globe"), "Site Pessoal"))
                 )))
      )

  )))
)

ui <- dashboardPage(
  header,
  sidebar,
  body
)

# Server
server <- function(input, output, session) {

  updateSelectizeInput(session, "city_sel", choices = city_list, server = TRUE)

  city <- reactive({input$city_sel})
  year <- reactive({input$year_sel})
  geo <- reactive({input$geo})

  # Map

  # Prepare the data for the map
  # mapdata <- reactive(prep_mapdata(input$city_sel, input$geo))
  mapborder <- reactive(get_state_border(input$city_sel, input$geo))
  # Output the map
  output$map <- renderTmap({

    req(city())
    req(year())
    req(palette())
    req(geo())

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

  # Plots

  # Plot line ranking
  output$plot_ranking <- renderPlot({
    req(city())
    plot_ranking(city(), year(), geo())
  }, res = 96)

  # Plot histogram
  output$plot_histogram <- renderPlot({
    req(city())
    plot_histogram(city(), year(), geo())
  }, res = 96)

  # Prepare the data for the time-series plots
  df_series <- reactive({
    req(city())
    prep_series_data(city())}
    )
  df_benchmark <- reactive(prep_benchmark(df_series()))

  # Plot Time Series
  output$plot_series <- renderPlotly({
    plot_series(df_series())
  })

  # Plot series comparison
  output$plot_series_facet <- renderPlotly({
    plot_series_comparison(df_benchmark())
  })

  # Boxes (with big numbers)

  # Prepare data
  df_box <- reactive(prep_infobox(city(), year()))

  ## Boxes with key numbers
  output$box_hdi <- renderInfoBox({
    infoBox("IFDM (Geral)", value = df_box()$idhm, icon = icon("compass"), fill = TRUE)
  })
  output$box_hdi_educ <- renderInfoBox({
    infoBox("Educação", value = df_box()$idhm_e, icon = icon("graduation-cap"), fill = TRUE)
  })
  output$box_hdi_income <- renderInfoBox({
    infoBox("Renda", value = df_box()$idhm_r, icon = icon("money-bill-alt"), fill = TRUE)
  })
  output$box_hdi_health <- renderInfoBox({
    infoBox("Saúde", value = df_box()$idhm_s, icon = icon("briefcase-medical"), fill = TRUE)
  })

  # Table output
  output$table_preview <- DT::renderDT({
    DT::datatable(head(df_download(), 1000), options = list(pageLength = 10))
  })

  df_download <- reactive({

    df <- series_data |>
      dplyr::select(
        year, index_type, name_region, code_muni, name_muni, hdi
        ) |>
      dplyr::rename(
        ano = year, indicador = index_type, nome_regiao = name_region,
        nome_cidade = name_muni, ifdm = hdi
      )

    df

  })

  output$download <- downloadHandler(
    filename = function() {
      paste0("data_firjan_ifdm", ".csv")
    },
    content = function(file) {
      write.csv(df_download(), file)
    }
  )
}

# Run the app
shinyApp(ui, server)

