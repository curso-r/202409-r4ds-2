library(purrr)

# map ----
# conjunto de elementos
# uma função
# queremos aplicar a função em cada elemento desse conjunto
# for loops

# map(.x = conjunto_de_elementos, .f = funcao)

converter_data_br <- function(data_br){
  readr::parse_date(data_br, format = "%d/%m/%Y")
}

conjunto_datas <- c("24/09/2024", "17/09/2024", "10/09/2024")

# Função já é vetorizada, não precisa do map
converter_data_br(conjunto_datas)


# funcao -------

grafico_star_wars <- function(especie_filtrar){
  # cria um grafico
  grafico <- dados::dados_starwars |>
    dplyr::filter(massa < 1000) |>
    ggplot2::ggplot() +
    ggplot2::aes(x = massa, y = altura) +
    ggplot2::geom_point() +
    gghighlight::gghighlight(especie == especie_filtrar, label_key = nome) +
    ggplot2::theme_light() +
    ggplot2::labs(title = especie_filtrar)

  # salva o gráfico
  ggplot2::ggsave(paste0(especie_filtrar, ".png"), plot = grafico)

  # vetor que retornará
  dados::dados_starwars |>
    dplyr::filter(especie == especie_filtrar) |>
    dplyr::pull(nome)
}


grafico_star_wars(especie_filtrar = "Droide")

# vamos usar o Purrr!

especies_unicas <- unique(dados::dados_starwars$especie)

# repetir a função para cada elemento do vetor especies unicas
lista_resultado <- map(especies_unicas, grafico_star_wars, .progress = TRUE)


lista_resultado_2 <- especies_unicas |>
  purrr::set_names() |>
  map(grafico_star_wars, .progress = TRUE)


lista_resultado_2$Human
