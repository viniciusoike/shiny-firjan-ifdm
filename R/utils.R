idhm_choices <- c("IDH", "IDH - Educação", "IDH - Renda", "IDH - Saúde")
style_choices <- c("Básico", "Quantis", "Quebras Naturais", "Clusters")

vl <- c(
  "IDH" = "idhm",
  "IDH - Educação" = "idhm_e",
  "IDH - Renda" = "idhm_r",
  "IDH - Saúde" = "idhm_s"
)

styles <- c(
  "Básico" = "pretty",
  "Quantis" = "quantile",
  "Quebras Naturais" = "fisher",
  "Cluster" = "hclust"
)

# cols4all palette names (tmap v4). The old Brewer/viridis names still resolve
# but emit a rename warning; these are the canonical v4 identifiers.
pals <- c(
  "1 (Viridis)" = "viridis",
  "2 (Marrom-Verde)" = "brewer.br_bg",
  "3 (Vermelho-Azul)" = "brewer.rd_bu",
  "4 (Tons de Azul)" = "brewer.blues",
  "5 (Tons de Verde)" = "brewer.greens"
)

text_about <- "O Índice FIRJAN de Desenvolvimento Municipal (IFDM) – é um indicador anual que mensura o desenvolvimento municipal em três eixos: Emprego & renda, Educação e Saúde. O IFDM utiliza várias bases públicas e tem uma metodologia similar a do IDH da ONU."
text_classification <-
  "A leitura do IFDM é similar a do IDH:
   <br>
   <ul>
     <li><b>Alto</b>: 0.8 ou maior</li>
     <li><b>Moderado</b>: maior que 0.6 e menor que 0.8</li>
     <li><b>Regular</b>: maior que 0.4 e menor que 0.6</li>
     <li><b>Baixo</b>: 0.4 ou menor</li>
</ul>  "

text_use <- "Para iniciar selecione o município. A lista está ordenada pela
população do município, mas você pode digitar o nome e usar o autocomplete.
Os outros campos ajudam a refinar o resultado e alteram tanto o mapa como os
gráficos abaixo.<br><br>Vale notar que o mapa de Cluster e de Quebras Naturais
pode levar algum tempo para carregar."

text_methods <- "
<p>
<b>IFDM</b> Para mais informações sobre o IFDM consulte o <a href='https://www.firjan.com.br/ifdm/' target='_blank'>site</a>.
</p>
<p>
<b>Tipos de mapa</b>. O método de 'Quebras Naturais' segue o algoritmo de <a href='https://en.wikipedia.org/wiki/Jenks_natural_breaks_optimization' target='_blank'>Jenks</a>, que busca formar grupos homogêneos.
Já a opção 'Cluster' segue um algotirmo de hierarchical clustering.
</p>
"

aboutme_pt_1 <-
  "Meu nome é Vinícius Oike Reginatto, sou economista, mestre em Economia pela Universidade de São Paulo (USP) e moro em São Paulo desde 2017. Trabalho como consultor em economia e pesquisa, focada em dados. Sou fundador da EKIO, uma consultoria que aplica a inteligência dos dados para transformar projetos e empresas."

aboutme_pt_2 <-
  "Acesse os links abaixo para conhecer mais do meu trabalho ou para entrar em contato."

about_app1 <-
  "Este painel permite analisar o contexto dos municípios brasileiros atráves dos dados do Índice Firjan de Desenvolvimento Municipal (IFDM). O IFDM tem metodologia similar ao popular Índice de Desenvolvimento Humano (IDH) da ONU. O IFDM tem duas vantagens em comparação com o IDH. Primeiro, apesar de analisar as mesmas dimensões do IDH (educação, saúde e renda), o IFDM contempla um maior número de variáveis para compor seu índice. Segundo, o IFDM é calculado anualmente enquanto o IDH é calculado apenas uma vez a cada dez anos."

about_app2 <- "A interpretação do IFDM é bastante simples: quanto maior, melhor. O mapa interativo
permite escolher uma cidade e compará-la com a realidade do seu estado.
Também é possível fazer uma comparação regional ou nacional, alterando o campo
'Opções', no card do mapa, mas note que isto pode levar algum tempo para carregar.
 Os quatro gráficos que aparecem abaixo do mapa ajudam a contextualizar o IFDM da cidade."


# Download page documentation ----

doc_colunas <- data.frame(
  Coluna = c(
    "ano",
    "indicador",
    "nome_regiao",
    "code_muni",
    "nome_cidade",
    "ifdm"
  ),
  Tipo = c("Inteiro", "Texto", "Texto", "Inteiro", "Texto", "Numérico"),
  Descrição = c(
    "Ano de referência",
    "Componente: Geral (IFDM), Educação, Saúde ou Emprego & Renda",
    "Grande região geográfica (Norte, Nordeste, Sudeste, Sul, Centro-Oeste)",
    "Código IBGE do município (7 dígitos)",
    "Nome do município (sem UF)",
    "Valor do índice (escala de 0 a 1; quanto maior, melhor)"
  ),
  stringsAsFactors = FALSE
)

doc_meta <- data.frame(
  Campo = c(
    "Cobertura geográfica",
    "Cobertura temporal",
    "Granularidade",
    "Produtor dos dados"
  ),
  Valor = c(
    "Brasil — 5.570 municípios",
    "2013 a 2023 (anual)",
    "Municipal",
    "Firjan (Índice Firjan de Desenvolvimento Municipal)"
  ),
  stringsAsFactors = FALSE
)

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

prep_infobox <- function(city, hdi_year = 2023) {
  new_names <- c("Educação", "Emprego & Renda", "Geral (IFDM)", "Saúde")
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
