buscar_info_skoob <- function(url_skoob) {

  # browser()
  # Buscar o conteúdo do html
  html_content <- rvest::read_html(url_skoob)

  # título do livro
  titulo <- html_content  |>
    rvest::html_nodes(xpath = '//*[@id="pg-livro-titulo"]')  |>
    rvest::html_text() |>
    stringr::str_trim()

  # Autoria
  # Adicionar depois

  # Buscar a nota média do livro
  rating <- html_content  |>
    rvest::html_nodes(xpath = '//*[@id="pg-livro-box-rating"]')  |>
    rvest::html_text() |>
    readr::parse_number()

  # Buscar o número de avaliações
  number_of_ratings <- html_content  |>
    rvest::html_nodes(xpath = '//*[@id="pg-livro-box-rating-avaliadores-numero"]')  |>
    rvest::html_text() |>
    readr::parse_number(locale = readr::locale(decimal_mark = ",", grouping_mark = "."))

  # Capa do livro
  url_capa <- html_content  |>
    rvest::html_nodes(xpath = '//*[@id="pg-livro-menu-principal-container"]')  |>
    rvest::html_node("img") |>
    rvest::html_attr("src")

  # Criando uma tibble com as informações buscadas
  tibble::tibble(
    titulo = titulo,
    url_capa = url_capa,
    nota_skoob = rating,
    n_avaliacoes_skoob = number_of_ratings,
    atualizacao_nota_skoob = format(Sys.Date(), "%d/%m/%Y"),
    link_skoob = url_skoob
  )
}

# Experimentando a função ---------------------

# buscar_info_skoob(url_skoob = "https://www.skoob.com.br/assassinato-no-expresso-do-oriente-1043ed1110468.html")
#
# buscar_info_skoob("https://www.skoob.com.br/lifelong-learners-11932482ed11927229.html")
#
# buscar_info_skoob("https://www.skoob.com.br/bom-dia-veronica-627428ed12199557.html")
