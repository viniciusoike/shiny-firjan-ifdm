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
  "A leitura do IFDM é similar à do IDH.
   <ul>
     <li><b>Alto</b>: 0,8 ou mais</li>
     <li><b>Moderado</b>: de 0,6 a 0,8</li>
     <li><b>Regular</b>: de 0,4 a 0,6</li>
     <li><b>Baixo</b>: abaixo de 0,4</li>
   </ul>"

text_use <- "Comece pelo município. A lista vem ordenada por população, mas
você pode digitar o nome e usar o autocomplete. Os filtros de índice, ano e
comparação mudam tanto o mapa como os gráficos abaixo.<br><br>Os mapas de
Cluster e de Quebras Naturais levam mais tempo para carregar."

text_methods <- "
<p>
<b>IFDM.</b> O <a href='https://www.firjan.com.br/ifdm/' target='_blank'>site da Firjan</a> detalha a metodologia do índice.
</p>
<p>
<b>Tipos de mapa.</b> 'Quebras Naturais' segue o algoritmo de <a href='https://en.wikipedia.org/wiki/Jenks_natural_breaks_optimization' target='_blank'>Jenks</a>, que forma grupos homogêneos.
'Cluster' agrupa os municípios por hierarchical clustering.
</p>
"

aboutme_pt_1 <-
  "Meu nome é Vinícius Oike Reginatto. Sou economista, mestre em Economia pela Universidade de São Paulo (USP), e moro em São Paulo desde 2017. Trabalho como consultor em economia e pesquisa aplicada a dados. Fundei a EKIO, consultoria que usa dados para transformar projetos e empresas."

aboutme_pt_2 <-
  "Acesse os links abaixo para conhecer mais do meu trabalho ou para entrar em contato."

about_app1 <-
  "Este painel analisa os municípios brasileiros com os dados do Índice Firjan de Desenvolvimento Municipal (IFDM). O IFDM segue metodologia parecida com a do Índice de Desenvolvimento Humano (IDH) da ONU e tem duas vantagens sobre ele. Primeiro, cobre as mesmas dimensões (educação, saúde e renda) com um número maior de variáveis. Segundo, sai todo ano, enquanto o IDH municipal sai uma vez a cada dez anos."

about_app2 <- "Quanto maior o IFDM, melhor. O mapa mostra a cidade escolhida
ao lado dos demais municípios do seu estado. O filtro 'Comparação', no topo da
página, troca esse recorte para região ou Brasil; os recortes maiores levam
mais tempo para carregar. Os quatro gráficos abaixo do mapa contextualizam o
IFDM da cidade."


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
