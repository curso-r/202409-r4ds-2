# https://sidra.ibge.gov.br/home/pnadct/nordeste

# Instalar o sidrar
# install.packages("sidrar")

# Carregar o SIDRAR
library(sidrar)

# Buscar tabelas no SIDRA a partir de uma palavra-chave
search_sidra("PNADCT")

# Buscar informações sobre uma tabela específica
info_4092 <- info_sidra(4092)

# Consultando informações sobre a tabela 4092
info_4092$table
# [1] "Tabela 4092: Pessoas de 14 anos ou mais de idade, por condição em relação
# à força de trabalho e condição de ocupação"

info_4092$period
# quebrar isso em um vetor onde cada periodo é um elemento:
periodos_possiveis <- strsplit(info_4092$period, ", ")[[1]]


View(info_4092$geo)

View(info_4092$variable)

View(info_4092$classific_category)

# Importar dados do SIDRA
dados_4092 <- get_sidra(
  # Código da tabela
  x = 4092,
  # Código da variável (consultei em info_4092$variable )
  variable = 1641,
  # Nível territorial (consultei em info_4092$geo )
  geo = "State",
  # Períodos (consultei em info_4092$period )
  period = periodos_possiveis[45:length(periodos_possiveis)]
)

# Vamos trabalhar na organização dessa tabela ---------------------

library(tidyverse)
library(janitor)

names(dados_4092)

# [1] "Nível Territorial (Código)"
#  [2] "Nível Territorial"
#  [3] "Unidade de Medida (Código)"
#  [4] "Unidade de Medida"
#  [5] "Valor"
#  [6] "Unidade da Federação (Código)"
#  [7] "Unidade da Federação"
#  [8] "Trimestre (Código)"
#  [9] "Trimestre"
# [10] "Variável (Código)"
# [11] "Variável"
# [12] "Condição em relação à força de trabalho e condição de ocupação (Código)"
# [13] "Condição em relação à força de trabalho e condição de ocupação"


# Limpar nomes das colunas:
dados_4092_nomes <- dados_4092 |>
  clean_names()

#  [1] "nivel_territorial_codigo"
#  [2] "nivel_territorial"
#  [3] "unidade_de_medida_codigo"
#  [4] "unidade_de_medida"
#  [5] "valor"
#  [6] "unidade_da_federacao_codigo"
#  [7] "unidade_da_federacao"
#  [8] "trimestre_codigo"
#  [9] "trimestre"
# [10] "variavel_codigo"
# [11] "variavel"
# [12] "condicao_em_relacao_a_forca_de_trabalho_e_condicao_de_ocupacao_codigo"
# [13] "condicao_em_relacao_a_forca_de_trabalho_e_condicao_de_ocupacao"

names(dados_4092_nomes)

# Pivot wider! -----

dados_4092_largos <- dados_4092_nomes |>
  # Removendo coluna que apresenta informação repetida
  select(-condicao_em_relacao_a_forca_de_trabalho_e_condicao_de_ocupacao_codigo) |>
  # transformar para formato largo
  pivot_wider(
    # nomes das colunas criadas devem vir da coluna condicao_em_relacao_....
    names_from = condicao_em_relacao_a_forca_de_trabalho_e_condicao_de_ocupacao,
    # valores para preencher as colunas criadas devem vir da coluna valor
    values_from = valor,
    # prefixo para os nomes das colunas criadas
    names_prefix = "mil_pessoas_"
  ) |>
  # limpar novamente os nomes das colunas
  clean_names()


# Podemos praticar voltar para o formato longo
dados_4092_longos <- dados_4092_largos |>
  pivot_longer(
    # colunas que serão transformadas em variáveis
    cols = starts_with("mil_pessoas_")
  )


# Próxima etapa: tratar trimestre código!
