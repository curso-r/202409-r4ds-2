# r para ciencia de dados 1
## import
## transformação de dados
## análises (estatísticas básicas etc)

# visualização
## ggplot2, extensões

# r para ciencia de dados 2
## mudando a forma da base, pivotagem,
## eficiência: across, etc
## trabalhar com vetores (stringr, forcats, lubridate)
## a grande cola e funções (purrr)


# tidyeval e metaprogramação

library(tidyverse)
library(dados)

diamante |>
  mutate(area = x * y)

diamante |>
  mutate(area_xz = x * z)

diamante |>
  mutate(area_yz = y * z)

# não funciona porque eu não consigo passar um "nome" para dentro
# da função
calcula_area <- function(coluna1, coluna2) {
  diamante |>
    mutate(area = coluna1 * coluna2)
}

calcula_area(x, y)

# no python deveria ser assim
##diamante['area'] = diamante['x'] * diamante['y']

# jeito errado de fazer, mas funciona
diamante |>
  mutate(area = diamante$x * diamante$y)

## 1 de 3: curly-curly
calcula_area <- function(coluna1, coluna2) {
  diamante |>
    mutate(area = {{coluna1}} * {{coluna2}})
}

calcula_area(x, y)

calcula_area(y, z)

calcula_area(x, z)

# bang bang: antigo, superseded

?`!!`

# usando o bang bang (antigo jeito)
calcula_area <- function(coluna1, coluna2) {
  nm1 <- rlang::enquo(coluna1)
  nm2 <- rlang::enquo(coluna2)
  diamante |>
    mutate(area = (!!nm1) * (!!nm2))
}

calcula_area(x, z)

# mais um exemplo: agrupamento

calcular_agrupado <- function(grp) {
  diamante |>
    group_by({{grp}}) |>
    summarise(
      across(c(x, y, z), mean)
    )
}

calcular_agrupado(corte)
calcular_agrupado(cor)
calcular_agrupado(transparencia)

# 2 de 3: agora temos como entrada não o nome, mas uma string

inputs <- read_file("inputs.txt") |>
  str_split(",") |>
  unlist() |>
  str_squish()


calcula_area <- function(coluna1, coluna2) {
  diamante |>
    mutate(area = diamante[[coluna1]] * diamante[[coluna2]])
}

calcula_area(inputs[1], inputs[2])

# diamante$x
# diamante[["x"]]


calcula_maior <- function(coluna1) {
  diamante |>
    group_by(corte) |>
    mutate(maior = max(diamante[[coluna1]]))
}

diamante |>
  group_by(corte) |>
  mutate(maior = max(x))


calcula_maior("x")

# solução: .data[[]]


calcula_maior <- function(coluna1) {
  diamante |>
    group_by(corte) |>
    mutate(maior = max(.data[[coluna1]]))
}

calcula_maior("x")

# para shiny é muito útil

# para ggplot2 é muito útil

faz_grafico <- function(eixo_x, eixo_y) {
  diamante |>
    ggplot(aes(x = .data[[eixo_x]], y = .data[[eixo_y]])) +
    geom_point()
}

faz_grafico("x", "z")
faz_grafico(x, z)

# 3 de 3: operador morsa :=
# serve para criar nomes de colunas
calcula_area <- function(coluna1, coluna2, nome) {
  diamante |>
    mutate({{nome}} := .data[[coluna1]] * .data[[coluna2]])
}

# os dois funcionam
calcula_area("x", "y", "area_xy")
calcula_area("x", "y", area_xy)

# exemplo shiny

library(shiny)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      selectInput(
        "eixo_x", "Selecione o eixo x",
        c("preco", "quilate", "profundidade", "x", "y", "z")
      ),
      selectInput(
        "eixo_y", "Selecione o eixo y",
        c("preco", "quilate", "profundidade", "x", "y", "z")
      )
    ),
    mainPanel = mainPanel(
      plotOutput("grafico")
    )
  )
)

server <- function(input, output, session) {

  output$grafico <- renderPlot({

    diamante |>
      ggplot(aes(x = .data[[input$eixo_x]], y = .data[[input$eixo_y]])) +
      geom_point()

  })

}

shinyApp(ui, server)


# prática -----------------------------------------------------------------

## operador !!! -> splice

# pacote rlang

# expand.grid() do base
expand_grid(
  numero = 1:3,
  texto = c("x", "y"),
  datas = c("2024-10-08", "2024-10-07", "2024-10-06")
)

graficos <- expand_grid(
  eixo_x = c("preco", "quilate", "profundidade", "x", "y", "z"),
  eixo_y = c("preco", "quilate", "profundidade", "x", "y", "z")
) |>
  mutate(
    grafico = map2(eixo_x, eixo_y, faz_grafico)
  )

graficos$grafico[[2]]

parametros <- list(
  eixo_x = c("preco", "quilate", "profundidade", "x", "y", "z"),
  eixo_y = c("preco", "quilate", "profundidade", "x", "y", "z")
)

expand_grid(!!!parametros)

# ISSO NAO FUNCIONA FORA DO TIDYVERSE

expand.grid(
  eixo_x = c("preco", "quilate", "profundidade", "x", "y", "z"),
  eixo_y = c("preco", "quilate", "profundidade", "x", "y", "z")
)

rlang::inject(expand.grid(!!!parametros))

# aplicação ---------------------------------------------------------------


dados <- readr::read_rds(
  "https://github.com/curso-r/202409-r4ds-2/releases/download/data/amostra.rds"
)

dplyr::glimpse(dados)

# jeito antigo: transmute
# seleciona e modifica colunas ao mesmo tempo
da_tidy <- dados |>
  transmute(
    file,
    key,
    datasessao = dmy(datasessao)
  )

# jeito atual
da_tidy <- dados |>
  mutate(
    file,
    key,
    numacordao,
    anoacordao,
    colegiado,
    relator,
    datasessao = dmy(datasessao),
    dtatualizacao = ymd(dtatualizacao),
    urlarquivo,
    acordaosrelacionados,
    .keep = "used"
  )

da_tidy$acordaosrelacionados[[6]]

acordaos_relacionados <- da_tidy |>
  select(file, acordaosrelacionados) |>
  unnest(acordaosrelacionados)

da_tidy$acordaosrelacionados |>
  bind_rows()

acordaos_relacionados <- da_tidy |>
  unnest(
    acordaosrelacionados,
    names_sep = "_",
    keep_empty = TRUE
  )

glimpse(acordaos_relacionados)

variavel <- "minha_variavel"
glue::glue("asdasdas/{variavel}/asdasdas")

## stringr::str_glue() é equivalente

arquivo <- glue::glue("dados/acordaos/{da_tidy$key[1]}.rtf")
download.file(
  da_tidy$urlarquivo[1],
  destfile = arquivo
)
readtext::readtext(arquivo)

# transformando em função
pegar_texto <- function(url, nome) {
  arquivo <- glue::glue("dados/acordaos/{nome}.rtf")
  if (!file.exists(arquivo)) {
    download.file(url, destfile = arquivo)
  }
  readtext::readtext(arquivo)
}

pegar_texto(da_tidy$urlarquivo[1], da_tidy$key[1])

resultados <- map2(
  head(da_tidy$urlarquivo, 20),
  head(da_tidy$key, 20),
  pegar_texto,
  .progress = TRUE
)


## pergunta do Rubens
# duas formas
opcoes <- expand_grid(
  tipo = 1:3,
  ano = c(2022, 2024)
)

map2(opcoes$tipo, opcoes$ano, baixar_tse)

opcoes <- expand_grid(
  tipo = 1:3,
  ano = c(2022, 2024)
) |>
  mutate(arquivo_baixado = map2(
    tipo, ano, baixar_tse
  ))

## voltando

# duas formas de bind_rows()
da_texto <- resultados |>
  bind_rows() |>
  mutate(
    key = str_remove(doc_id, "\\.rtf$")
  ) |>
  inner_join(da_tidy, "key")

# list_rbind()
resultados |>
  list_rbind()

da_tidy |>
  select(key)

da_texto
