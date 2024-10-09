library(tidyverse)
# install.packages("dados")
library(dados)

# Tabela Ames Housing
casas

# Essa tabla tem muitas variáveis. Vamos selecionar algumas para
# usar nas práticas de hoje.
casas_2 <- casas |>
  # selecionar as variáveis que vamos usar
  select(pid, geral_qualidade, vizinhanca) |>
  # transformar textos na coluna geral_qualidade para apenas letras minúsculas
  mutate(geral_qualidade = str_to_lower(geral_qualidade))

# Como o ggplot2 lida com colunas de textos?
# ORDEM ALFABÉTICA T_T
casas_2 |>
  count(geral_qualidade) |>
  ggplot() +
  aes(x = n, y = geral_qualidade) +
  geom_col()


# Podemos mudar a ordem das barras transformando em fator.
# É importante indicar os níveis (levels) seguindo a ordem que faça sentido.
casas_2 |>
  count(geral_qualidade) |>
  mutate(qualidade_fator = fct(
    geral_qualidade,
    levels = c(
      "muito ruim",
      "ruim",
      "abaixo da média",
      "regular",
      "média",
      "acima da média",
      "boa",
      "muito boa",
      "excelente",
      "muito excelente"
    )
  )) |>
  arrange(qualidade_fator) |>
  ggplot() +
  aes(x = n, y = qualidade_fator) +
  geom_col() +
  theme_light()


# explorando uma variável categórica não ordinal -------

# Novamente, o ggplot2 lida com colunas de textos em ordem alfabética.
casas_2 |>
  count(vizinhanca) |>
  ggplot() +
  aes(x = n, y = vizinhanca) +
  geom_col() +
  theme_light()


# Como deixar as barras em ordem de frequência?

casas_2 |>
  count(vizinhanca) |>
  # reordenar as barras segundo a frequência
  # ou valores que temos em outras colunas!
  mutate(vizinhanca_fator = fct_reorder(vizinhanca, n)) |>
  arrange(vizinhanca_fator) |>
  ggplot() +
  aes(x = n, y = vizinhanca_fator) +
  geom_col(aes(fill = vizinhanca_fator), show.legend = FALSE) +
  theme_light()

casas_2 |>
  count(vizinhanca, sort = TRUE)

# Vamos agrupar as vizinhanças com menos de 50 propriedades
# na categoria "Outros"

casas_2 |>
  mutate(
    # vizinha_com_outros = fct_lum(vizinhanca,
    #                                    n = 20,
    #                                    other_level = "Outros (n<50)"),
    vizinha_com_outros = fct_lump_min(vizinhanca, min = 50, other_level = "Outros (n<50)")
  ) |>
  count(vizinha_com_outros) |>
  # reordenar as barras segundo a frequência
  # ou valores que temos em outras colunas!
  mutate(
    vizinhanca_fator = fct_reorder(vizinha_com_outros, n),
    vizinhanca_fator = fct_relevel(vizinhanca_fator, c("Outros (n<50)"), after = 0)
  ) |>
  arrange(vizinhanca_fator) |>
  ggplot() +
  aes(x = n, y = vizinhanca_fator) +
  geom_col(aes(fill = vizinhanca_fator), show.legend = FALSE) +
  labs(caption = "Outros = vizinhanças com menos de 50 propriedades") +
  theme_light()



# Curiosidade: internamente os fatores são salvos como números:
casas_2 |>
  mutate(
    vizinha_com_outros = fct_lump_min(vizinhanca, min = 50, other_level = "Outros (n<50)"),
    vizinhanca_numero_fct = as.numeric(vizinha_com_outros)
  ) |> arrange(vizinhanca_numero_fct)
