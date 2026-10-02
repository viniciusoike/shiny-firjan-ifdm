styles <- c(
  "Básico" = "pretty",
  "Quantis" = "quantile",
  "Quebras Naturais" = "fisher",
  "Cluster" = "hclust"
)

# cols4all palette names (tmap v4), plus the current EKIO diverging palette.
# Existing Viridis/Brewer choices remain available for users who need them.
pals <- list(
  "1 (Viridis)" = "viridis",
  "2 (Marrom-Verde)" = "brewer.br_bg",
  "3 (Vermelho-Azul)" = "brewer.rd_bu",
  "4 (Tons de Azul)" = "brewer.blues",
  "5 (Tons de Verde)" = "brewer.greens",
  # tmap maps the first colour to the lowest interval. Reverse EKIO's
  # blue-red source palette so low values are red and high values are blue.
  "EKIO (Vermelho–Azul)" = rev(ekioplot::ekio_pal("blue_red"))
)

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
