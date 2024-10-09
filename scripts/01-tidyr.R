library(tidyverse)

# tidyr -----

# Reorganizando a tabela de treinamento dos cachorros!

treinamento_cao <- tibble(
  cachorro = c("Bacon", "Dexter", "Zip"),
  nota_semana_1 = c(10, 10, 0),
  nota_semana_2 = c(4, 4, 0),
  nota_semana_3 = c(8, 10, 10),
  nota_semana_4 = c(9, 8, 9)

)

treinamento_cao
# Buscamos
# cachorro | semana | nota

# pivot_longer() -------
# `cols` must select at least one column - aceita funções auxiliares do dplyr::select

## selecionando as colunas que serão pivotadas ----
# forma 1 - sendo explicito quais colunas vamos usar
treinamento_cao |>
  pivot_longer(cols = c(nota_semana_1, nota_semana_2, nota_semana_3, nota_semana_4))

# forma 2: removendo as colunas que não queremos
treinamento_cao |>
  # -cachorro : todas as colunas, menos a coluna cachorro!
  pivot_longer(cols = -c(cachorro))

# forma 3: usando função auxiliar do select: starts_with(), ends_with(), contains()
treinamento_cao |>
  pivot_longer(cols = starts_with("nota_"))


# Explorando alguns argumentos do pivot_longer()

treinamento_cao_longo <- treinamento_cao |>
  pivot_longer(
    cols = starts_with("nota_"),
    # nomeando as colunas criadas
    names_to = "semana",
    values_to = "nota",
    # prefixo do texto no nome das colunas que queremos remover
    names_prefix = "nota_semana_"
  ) |>
  mutate(semana = as.numeric(semana))

# exemplo com gráfico

treinamento_cao_longo |>
  ggplot() +
  geom_line(aes(x = semana, y = nota, color = cachorro))

# pivot_wider() ---------------------------
treinamento_cao_longo |>
  pivot_wider(
    # names_from: nome da coluna que tem os nomes que serão usados para nomear as colunas
    names_from = semana,
    # values_from: nome da coluna que contém os valores
    values_from = nota,
    names_prefix = "nota_semana_"
  )


# separate() -----

# Lendo base imdb
imdb <- read_csv("https://raw.githubusercontent.com/curso-r/main-r4ds-1/master/dados/imdb.csv")


# olhando os valores

imdb$generos[1:10]

imdb |> 
  count(generos, sort = TRUE)

# 1 linha por filme, generos em 3 colunas!
generos_colunas <- imdb |> 
  separate(
# qual coluna quero separar?
    col = generos, 
    # qual o separador usado?
    sep = ", ",
    # quais são as colunas que serão criadas
    into = c("genero_1", "genero_2", "genero_3")

    )

generos_longo <- generos_colunas |> 
  pivot_longer(
# quais colunas pivotar
    cols = starts_with("genero_"), 
    # nome da coluna que receberá o nome das colunas das colunas em cols = c(..)
    names_to = "ordem", 
    # nome da coluna que receberá os valores
     values_to = "genero",
     # remover os valores NA que estão em values_to
      values_drop_na = TRUE
    ) 

generos_longo |> 
  count(genero, sort = TRUE) |> 
  slice_max(
  # queremos buscar as linhas com maiores números de certa variável
  # order_by = qual variável usamos para buscar os maiores valores?
    order_by = n, 
    # n = quantos valores queremos?
    n = 15
  ) |> 
  # No futuro, vamos ver como ordenar as barras!
  ggplot() +
  geom_col(aes(x = n, y = genero))



# separate_longer_delim() ----

imdb$elenco[1:10]

elenco_longo <- imdb |> 
  separate_longer_delim(
    cols = c(elenco), delim = ", "
  )

  elenco_longo |> 
    count(elenco, sort = TRUE) |> 
    slice_max(order_by = n, n = 20) 

# unite() --------

generos_colunas |> 
  unite(
    # col: nome da coluna que criaremos
    col = "generos",
    # colunas que vamos unir! podemos usar as funções auxiliares que já usamos
    # no select
    starts_with("genero_"),
    # qual é o separador dos textos?
   sep = "; ",
   # remover os NAs?
    na.rm = TRUE
  )


# Dúvida: Rafael
# Bia, só uma pergunta sobe o pipe, existe alguma diferença entre o |> ou o %>%
# %>% - pipe do tidyverse, pipe do magrittr; requer carregar (library) o tidyverse ou magrittr
## |>  - pipe do R base, pipe nativo


# dplyr -----------------
## across --------------

library(dados)

treinamento_cao <- tibble(
  cachorro = c("Bacon", "Dexter", "Zip"),
  nota_semana_1 = c(10, 10, 0),
  nota_semana_2 = c(4, 4, 0),
  nota_semana_3 = c(8, 10, 10),
  nota_semana_4 = c(9, 8, 9)
)

treinamento_cao |> 
  mutate(
    nota_semana_1 = as.character(nota_semana_1),
    nota_semana_2 = as.character(nota_semana_2),
    # ...
  )

treinamento_cao |> 
  mutate(
    across(
      # .cols = quais colunas queremos usar?
      .cols = starts_with("nota_semana_"),
      # .fns = qual função aplicar (lembrete: não usar o parênteses)
      .fns = as.character),
    )
  )


pinguins |> 
  mutate(across(
    .cols = where(is.numeric),
    .fns = as.character
  ))


# Sugestão do Rafael: Daria para adicionar um valor onde tem NA nos dados?

# versão usando função criada
substituir_por_zero <- function(x){
  replace_na(x, replace = 0)
}

imdb |> 
  mutate(
    across(
      .cols = c(orcamento, receita, receita_eua),
      .fns = substituir_por_zero
    )
  )


# versão usando funções anônimas, é menos claro
  imdb |> 
    mutate(
      across(
        .cols = c(orcamento, receita, receita_eua),
        .fns = ~replace_na(.x, replace = 0)
      )
    )
 
# forma mais recente de fazer função anônima
imdb |> 
  mutate(
    across(
      .cols = c(orcamento, receita, receita_eua),
      # \() - function
      .fns = \(coluna) replace_na(coluna, replace = 0)
    )
  )


# exemplo do across + summarise

pinguins |> 
  group_by(especie, ilha) |> 
  summarise(
    # medias
    media_comprimento_bico = mean(comprimento_bico, na.rm = TRUE),
    media_profundidade_bico = mean(profundidade_bico, na.rm = TRUE),
    media_comprimeiro_nadadeira = mean(comprimento_nadadeira, na.rm = TRUE),
    media_massa_corporal = mean(massa_corporal, na.rm = TRUE)
  )

pinguins |> 
  group_by(especie, ilha) |> 
  summarise(
    across(
    .cols = c(comprimento_bico, profundidade_bico,
    comprimento_nadadeira, massa_corporal),
    .fns = mean
  )
  )


pinguins |> 
    group_by(especie, ilha) |> 
    summarise(
      across(
        .cols = c(comprimento_bico, profundidade_bico,
      comprimento_nadadeira, massa_corporal),
      .fns = mean,
      # colando textos similar à forma do glue
      .names = "media_{.col}"
    )
    )


    pinguins |> 
      group_by(especie, ilha) |> 
      summarise(
        across(
        # colunas que queremos sumarizar
        .cols = c(comprimento_bico, profundidade_bico,
        comprimento_nadadeira, massa_corporal),
        # Calculando a média, removendo os NAs
        .fns = \(coluna) mean(coluna, na.rm = TRUE),
        # padrões do nome
        # colando textos similar à forma do glue
        .names = "media_{.col}"
      )
      )


# dúvida: é como a lista do python? pesquisar, não lembro
exemplo_lista <- list( 
  vetor = c(1, 2, 3),
  valor = "a",
  tabela = pinguins
)



tabela_sumarizada <- pinguins |> 
  group_by(especie, ilha) |> 
  summarise(
    across(
      .cols = c(comprimento_bico, comprimento_nadadeira, profundidade_bico, massa_corporal),
      .fns = list(
        "media" = \(x) mean(x, na.rm = TRUE),
        "mediana" = \(x) median(x, na.rm = TRUE),
        "desvio_padrao" = \(x) sd(x, na.rm = TRUE)  
      ),
      .names = "{.fn}_{.col}"
    )
  ) 

# Dúvida:
# Bia, uma dúvida com o group_by, caso eu necessite resumir meus dados, mas manter os demais valores da tabela
pinguins |> 
  group_by(especie, ilha) |> 
  mutate(
    across(
      .cols = c(comprimento_bico, comprimento_nadadeira, profundidade_bico, massa_corporal),
      .fns = list(
        "media" = \(x) mean(x, na.rm = TRUE),
        "mediana" = \(x) median(x, na.rm = TRUE),
        "desvio_padrao" = \(x) sd(x, na.rm = TRUE)  
      ),
      .names = "{.fn}_{.col}"
    )
  ) |> View()
