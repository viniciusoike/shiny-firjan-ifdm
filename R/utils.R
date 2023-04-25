
idhm_choices <- c("IDH", "IDH - Educação", "IDH - Renda", "IDH - Saúde")
style_choices <- c("Básico", "Quantis", "Quebras Naturais", "Clusters")

vl <- c(
  "IDH" = "idhm",
  "Educação" = "idhm_e",
  "Renda" = "idhm_r",
  "Saúde" = "idhm_s"
)

styles <- c(
  "Básico" = "pretty",
  "Quantis" = "quantile",
  "Quebras Naturais" = "fisher",
  "Cluster" = "hclust"
)

pals <- c(
  "1 (Viridis)" = "viridis",
  "2 (Marrom-Verde)" = "BrBG",
  "3 (Vermelho-Azul)" = "RdBu",
  "4 (Tons de Azul)" = "Blues",
  "5 (Tons de Verde)" = "Greens"
)

text_about <- "O Índice FIRJAN de Desenvolvimento Municipal (IFDM) – é um indicador anual que mensura o desenvolvimento municipal em três eixos: Emprego & renda, Educação e Saúde. O IFDM utiliza várias bases públicas e tem uma metodologia similar a do IDH da ONU.<br><br>Vale notar que os valores de 2015-16 foram bastante afetados pela recessão econômica."
text_classification <-
  "A leitura do IFDM é similar a do IDH:
   <br>
   <ul>
     <li><b>Alto</b>: 0.8 ou maior</li>
     <li><b>Moderado</b>: maior que 0.6 e menor que 0.8</li>
     <li><b>Regular</b>: maior que 0.4 e menor que 0.6</li>
     <li><b>Baixo</b>: 0.4 ou menor</li>
</ul>  "

text_use <- "Para iniciar selecione o município. A lista está ordenada pela população do município, mas você pode digitar o nome e usar o autocomplete. Os outros campos ajudam a refinar o resultado e alteram tanto o mapa como os gráficos abaixo.<br><br>Vale notar que o mapa de Cluster e de Quebras Naturais pode levar algum tempo para carregar."

text_methods <- "
<p>
<b>IFDM</b>Para mais informações sobre o IFDM consulte o <a href='https://www.firjan.com.br/ifdm/' target='_blank'>site</a>.
</p>
<p>
<b>Tipos de mapa</b>. O método de 'Quebras Naturais' segue o algoritmo de <a href='https://en.wikipedia.org/wiki/Jenks_natural_breaks_optimization' target='_blank'>Jenks</a>, que busca formar grupos homogêneos.
Já a opção 'Cluster' segue um algotirmo de hierarchical clustering.
</p>
"

aboutme_pt <-
  "Meu nome é Vinícius Oike Reginatto, sou economista, mestre em Economia pela Universidade de São Paulo e moro em São Paulo desde 2017. Trabalho com mercado imobiliário e no meu tempo livre faço aplicativos em Shiny."

aboutme_en <-
  "My name is Vinicius Oike Reginatto and I hold a Master's degree in Economics
  from the University of São Paulo (USP), one of the most prestigious universities in Brazil.
  Since graduating, I've gained experience in both the tech and consulting sectors, working primarily in real estate.
  I'm particularly passionate about economics, urbanism, and real estate; I enjoy making apps and data science tools in R to solve real-world problems."

#--------------------------------#

classify_hdi <- function(x) {

  stopifnot(is.numeric(x))

  label <- dplyr::case_when(
    x < 0.4 ~ "Baixo",
    x >= 0.4 & x < 0.6 ~ "Regular",
    x >= 0.6 & x < 0.8 ~ "Moderado",
    x >= 0.8 ~ "Alto"
  )

  glue::glue("{round(x, 3)} ({label})")

}

prep_infobox <- function(city, hdi_year = 2016) {

  new_names <- c("Educação", "Renda", "Geral (IFDM)", "Saúde")
  names(new_names) <- c("idhm_e", "idhm_r", "idhm", "idhm_s")

  df <- series_data |>
    dplyr::filter(name_muni_full == city, year == hdi_year) |>
    tidyr::pivot_wider(
      id_cols = "name_muni_full",
      names_from = "index_type",
      values_from = "hdi"
      ) |>
    dplyr::rename(dplyr::all_of(new_names)) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), classify_hdi))

  return(df)

}
