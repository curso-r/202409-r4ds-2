# Funções -------------

# nome <- function(argumento1 = valor_padrao, argumento2 = valor_padrao, ...){
#   # o que será executado quando a gente chamar a função!
# }

multiplicar <- function(x, y){
  x * y
}

multiplicar(5, 10)

multiplicar(x = 5, y = 10)

multiplicar(5)
# Error in multiplicar(5) : argument "y" is missing, with no default

round(5.34234, digits = 1)

multiplicar(y = 3, x = 2)

multiplicar(x = "um", y = "dois")

# Dúvidas
# tem que colocar algo na função para aceitar somente números?
multiplicar_numeros <- function(x, y){

  # verificar se os valores informados são numéricos
  # com base R

  if(class(x) != "numeric"){
    stop("O argumento x precisa ser numérico")
  } else if(class(y) != "numeric"){
    stop("O argumento y precisa ser numérico")
  }

  x * y
}


multiplicar_numeros(1, 2)
multiplicar_numeros("1", 2)
# Error in multiplicar_numeros("1", 2) : O argumento x precisa ser numérico
multiplicar_numeros(1, "2")

# pacote checkmate pode ajudar a fazer a verificação


multiplicar_numeros <- function(x, y){

  # verificar se os valores informados são numéricos
  # com base R

  if(!class(x) %in% c("numeric", "logical")){
    stop("O argumento x precisa ser numérico")
  } else if(class(y) != "numeric"){
    stop("O argumento y precisa ser numérico")
  }

  x * y
}

# ----

multiplicar_numeros <- function(x, y){

  # verificar se os valores informados são numéricos
  # com base R

  if(class(x) != "numeric"){
    stop("O argumento x precisa ser numérico")
  } else if(class(y) != "numeric"){
    stop("O argumento y precisa ser numérico")
  }

  return(x * y)
}

# ----

# Exercicio

# library(tidyverse)
# library(dados)


calcular_sumarizacoes_gapminder <- function(continentes, ano_minimo = 2000){
  dados::dados_gapminder |>
  dplyr::filter(continente %in% continentes, ano >= ano_minimo) |>
  dplyr::group_by(continente, ano) |>
  dplyr::summarise(
    soma_pop_milhoes = sum(populacao) / 1e6,
    media_expectativa_vida = mean(expectativa_de_vida),
    .groups = "drop"
  )
}

calcular_sumarizacoes_gapminder(continentes = c("Europa"))

calcular_sumarizacoes_gapminder(continentes = c("Europa", "Ásia"))


calcular_sumarizacoes_gapminder(continentes = c("Europa", "Ásia"),
                                ano_minimo = 1950)


# outro exemplo ---
readr::parse_date("24/09/2024", format = "%d/%m/%Y")

converter_data_br <- function(data_br){
  readr::parse_date(data_br, format = "%d/%m/%Y")
}

converter_data_br("01/01/2024")


# browser() --------------------
# é bom para desenvolver e resolver erros!

converter_data_br <- function(data_br){

  # browser()
  readr::parse_date(data_br, format = "%d/%m/%Y")

}

converter_data_br("15/02/1993")

# Listas -------------------------------
# vetor - um conjunto de valores com 1 dimensao, eles só podem ter o mesmo tipo
# (ex. character, numerico, etc)
# data.frame - linhas e colunas, cada coluna se comporta como vetor
# matriz - linhas e coluna, apenas aceita números

# lista não nomeada
minha_lista <- list(
  123, # VETOR
  "ABC", # VETOR
  c(TRUE, FALSE), # VETOR
  mtcars,
  list("Beatriz", "Milz", c("Diadema", "Osasco"))
)

# podemos buscar elementos da lista pelo posição
minha_lista[[5]][[3]][[2]]

# lista nomeada --------------------------------


minha_lista_2 <- list(
  dados = mtcars,
  vetor_texto = c("A", "B", "C"),
  vetor_numero = c(1, 2, 3),
  lista = list(nome = "Beatriz", sobrenome = "Milz",
              municipio = c("Diadema", "Osasco"))
)

# podemos buscar elementos da lista pelo nome
minha_lista_2$lista$municipio[2]

# purrr pluck permite acessar elementos da lista pelo nome e posição
library(purrr)

minha_lista_2 |>
  pluck("lista", "municipio", 2)
