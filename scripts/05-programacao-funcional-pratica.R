library(tidyverse)
# https://dadosabertos.camara.leg.br/swagger/api.html?tab=staticfile#staticfile


# Etapa 1: download dos arquivos


# https://dadosabertos.camara.leg.br/arquivos/proposicoes/xlsx/proposicoes-2024.xlsx

baixar_proposicoes <- function(ano = 2024, diretorio = "dados/proposicoes/"){

  # Cria caminho do link para baixar a tabela
  url_baixar <- paste0("https://dadosabertos.camara.leg.br/arquivos/proposicoes/xlsx/proposicoes-", ano, ".xlsx")

  # cria a pasta para baixar (caso ela não exista)
  fs::dir_create(diretorio)

  # criar caminho para o arquivo que será baixado
  arquivo_baixado <- paste0(diretorio, "proposicoes-", ano, ".xlsx")

  # Verificar se o arquivo existe
  arquivo_existe <- fs::file_exists(arquivo_baixado)

  # se o arquivo não existe (!), ou ano for 2024,
  # queremos baixar
  # o ano 2024 queremos que seja baixado sempre para ficar atualizado
  # if (!arquivo_existe | ano == 2024) {
    # baixar o arquivo
    download.file(url = url_baixar, destfile =  arquivo_baixado, mode = "wb")
 #}


}

baixar_proposicoes(2024)

# Baixar vários anos de uma vez!
map(.x = c(2010:2023), .f = baixar_proposicoes, .progress = TRUE)


# Etapa 2: importar os arquivos
# readxl::read_xlsx("dados/proposicoes/proposicoes-2021.xlsx")

# fazendo um vetor com os caminhos dos arquivos
arquivos_proposicoes <- fs::dir_ls("dados/proposicoes", glob = "*.xlsx")


dados_list <- arquivos_proposicoes |>
  # deixando os elementos da lista nomeados
  purrr::set_names() |>
  # executando a função read_xlsx para cada um dos arquivos
  map(readxl::read_xlsx, .progress = TRUE)

# acessando elementos
dados_list$`dados/proposicoes/proposicoes-2020.xlsx`
dados_list[[1]]

# transformando em um data frame
dados_df <- dados_list |>
  purrr::list_rbind(names_to = "arquivo")

# checando se todos os arquivos foram importados
dados_df |>
  dplyr::distinct(arquivo)


# Dúvida: e quando temos estados?
# estado
estados <- unique(geobr::grid_state_correspondence_table$abbrev_state)

# podemos fazer a combinação de cada ano e estado!
combinacoes <- tidyr::expand_grid(anos = c(2010:2024), uf = estados)
