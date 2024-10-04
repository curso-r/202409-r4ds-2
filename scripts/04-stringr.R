
library(tidyverse)

link_dados <- "https://github.com/jtrecenti/main-cdad2/releases/download/data/camaras.csv"

camaras <- read_csv(link_dados)

glimpse(camaras)

camaras$ementa[2]


# analise exploratoria ----------------------------------------------------

camaras |>
  filter(
    !decisao %in% c("Punibilidade Extinta", "Outros", "Não conhecido")
  ) |>
  count(decisao, sort = TRUE)


camaras |>
  count(camara) |>
  print(n = 100)

camaras |>
  count(camara) |>
  View()

# "str.contains"

camaras |>
  filter(str_detect(camara, "Extraord")) |>
  count(camara)

# usando a função regex
camaras |>
  filter(str_detect(camara, regex("extraord", ignore_case = TRUE))) |>
  count(camara)

camaras |>
  filter(str_detect(camara, "[Ee]xtraord")) |>
  count(camara)


camaras |>
  filter(str_detect(camara, "Extraord") | str_detect(camara, "extraord")) |>
  count(camara)


camaras |>
  mutate(camara = str_to_upper(camara)) |>
  filter(str_detect(camara, "EXTRAORD")) |>
  count(camara)


camaras_filtrado <- camaras |>
  filter(
    !decisao %in% c("Punibilidade Extinta", "Outros", "Não conhecido"),
    !str_detect(camara, "[Ee]xtraord")
  )

camaras_filtrado |>
  count(camara, decisao) |>
  group_by(camara) |>
  mutate(prop = n/sum(n)) |>
  ungroup()

camaras_filtrado |>
  count(camara, decisao) |>
  mutate(prop = n/sum(n), .by = c(camara))


# funções do stringr ------------------------------------------------------


str_length("asdasdl")
nchar("asdasdl")

str_detect("lalala", "la")
grepl("la", "lalala")


# mais usadas pelo julio

str_detect("lalala", "la")

str_extract("banana", "na")

camaras_filtrado |>
  mutate(
    numero_camara = str_extract(camara, "[0123456789]")
  )

camaras_filtrado |>
  mutate(
    numero_camara = str_extract(camara, "[0-9]{2}")
  ) |>
  select(camara, numero_camara)

camaras_filtrado |>
  mutate(
    numero_camara = str_extract(camara, "[\\d]{2}")
  ) |>
  select(camara, numero_camara)

str_extract_all("352374", "[0-9]")

str_replace("Banana", "anana", "atata")


str_replace("12345", "[0-9]", "a")

# parse_number do pacote readr é melhor
str_replace("R$ 12.345,27", "R\\$ ", "") |>
  str_replace("\\.", "") |>
  str_replace(",", ".") |>
  as.numeric()

parse_number(
  "R$ 12.345,27",
  locale = locale(grouping_mark = ".", decimal_mark = ",")
)

str_replace_all("12345", "[0-9]", "8")

str_remove("12.345", "\\.") # equivalente a str_replace(x, rx, "")

str_remove_all()


camaras_filtrado |>
  glimpse()


camaras_filtrado |>
  count(assunto) |>
  print(n = 1000)

camaras_filtrado |>
  mutate(
    elem1 = str_extract(assunto, "[^-]+"),
    elem1 = str_trim(elem1)
  ) |>
  select(elem1, assunto)

## experimentos
# str_extract("DIREITO PENAL - ", "[^-]{1,100}")
# str_extract("DIREITO PENAL - ", "[^-]{1,}")
# str_extract("DIREITO PENAL - ", "[^-]+")
# str_extract(
#   c("DIREITO PENAL - ", "DIREITO PROCESSUAL PENAL - "),
#   "[^-]+"
# )

## str_trim vs str_squish

str_trim("   banana   ")

str_trim("   ba    na    na   ")

str_squish("   ba    na    na   ")

# padronizam strings
str_pad()

dados_exportar <- tibble(
  id = 1:2,
  txt = paste(camaras_filtrado$ementa, collapse = "")
) |>
  mutate(
    truncado = str_trunc(txt, 1000)
  )

dados_exportar$truncado[1]

writexl::write_xlsx(dados_exportar, "teste.xlsx")

dados_exportar$txt |> str_length()

# str_trunc()

c("1", "2", "3", "4", "10") |>
  sort()

str_pad(c("1", "2", "3", "4", "10"), width = 2, side = "left", pad = "0") |>
  sort()

# um pouco de miscelânea

str_split(
  c(
    "0001345-21.2018.8.26.0535",
    "0064728-75.2015.8.26.0050"
  ),
  "[-.]"
)

# considerar tidyr::separate()

abjutils::rm_accent("áááá")

abjutils::rm_accent(
  camaras_filtrado$ementa[1]
)

abjutils::rm_accent

stringi::stri_trans_general("AÁááçççâñü", "Latin-ASCII")

# eu não uso, mas quem sabe você

paste("asda", "333")
paste0("asda", "333")
paste(c("asda", "333"), collapse = "|")

str_c("asda", "333")
str_c(c("asda", "333"), collapse = "|")

# exemplo de como renomear colunas

tibble(
  casa = 1:4,
  apt = 2:5
) |>
  rename(caza = casa)

tibble(
  casa = 1:4,
  apt = 2:5,
  assado = 3:6
) |>
  rename_with(
    \(x) str_replace_all(x, "s", "z"),
    c(casa, assado)
  )


# expressões regulares ----------------------------------------------------

camaras_filtrado$ementa[100]

# quantificadores

# {2}, {1,3}, +

str_view(
  camaras_filtrado$ementa[100],
  "[0-9]+"
)

str_view(
  camaras_filtrado$ementa[100],
  "[0-9]{1,}"
)

str_view(
  "bananas são 10",
  "[0-9]{0,}"
)

str_view(
  "bananas são 10",
  "[0-9]*"
)

str_view(
  c("bananas são 10", "banana é 10"),
  "bananas?"
)

str_view(
  c("bananas são 10", "banana é 10"),
  "bananas{0,1}"
)

# âncoras

camaras_filtrado$assunto[2] |>
  str_extract("- [^-]+$")

camaras_filtrado$assunto[2] |>
  str_extract("- [^-]+ - [^-]+$") |>
  str_extract("- [^-]+")

camaras_filtrado$assunto[2] |>
  str_extract_all("- [^-]+") |>
  purrr::pluck(1) |>
  purrr::pluck(2)

camaras_filtrado$assunto[2] |>
  str_split(" - ") |>
  purrr::pluck(1, 2)


str_extract(
  "maçã Banana bananA",
  regex("^banana", ignore_case = TRUE)
)

## opções/grupos/alternativas


## classificador de empresas
str_detect(
  c(
    "JULIO SATO", "JULIO TRECENTI SA",
    "CURSO-R LTDA", "BEATRIZ DE SA",
    "ITALO SOUSA"
  ),
  "(LTDA|(?<! DE) SA$)"
)

## look ahead e look behind

# (?=xxx) -> seguido por xxx
# (?<=xxx) -> precedido por xxx
# (?!xxx) -> não seguido por xxx
# (?<!xxx) -> não precedido por xxx

str_detect(
  camaras_filtrado$assunto[2],
  "DIREITO PENAL(?= -)"
)

str_detect(
  "DIREITO PENAL",
  "DIREITO PENAL(?= -)"
)

str_detect(
  "- DIREITO PENAL",
  "(?<=- )DIREITO PENAL"
)

str_detect(
  c("- DIREITO PENAL", "* DIREITO PENAL"),
  "(?<!- )DIREITO PENAL"
)

str_detect(
  c("DIREITO PENAL -", "DIREITO PENAL %"),
  "DIREITO PENAL(?! -)"
)

# caracteres especiais

# .
# \n

str_extract(
  c("DIREITO PENAL", "DIREITO CIVEL"),
  "DIREITO .+"
)

str_extract(
  c("Nidorino", "Nidorina"),
  "Nidorin."
)

cat("Julio\nTrecenti")
cat("Julio\tTrecenti")
# \r, \f, ...

str_squish("Julio\tTrecenti")


str_extract(
  c("DIREITO PENAL", "DIREITO CÍVEL"),
  "DIREITO [a-z]+"
)

str_extract(
  c("DIREITO PENAL", "DIREITO CÍVEL"),
  "DIREITO [A-Z]+"
)

str_extract(
  abjutils::rm_accent(c("DIREITO PENAL", "DIREITO CÍVEL")),
  "DIREITO [A-Z]+"
)

str_extract(
  c("DIREITO PENAL", "DIREITO CÍVEL"),
  "DIREITO [A-ZÍ]+"
)

str_extract(
  c("DIREITO PENAL", "DIREITO CÍVEL"),
  "[[:upper:] ]+"
)

str_extract(
  c("DIREITO PENAL", "DIREITO CÍVEL"),
  "[[:alpha:] ]+"
)


str_extract(
  c("DIREITO", "DIREITA"),
  "DIREITO!"
)

# dúvida: ! serve para que?
# "(?!\\!)"

camaras_filtrado$ementa[2]

mascara_cnj <- "\\d{7}-\\d{2}\\.\\d{4}\\.\\d\\.\\d{2}\\.\\d{4}"

str_extract(
  camaras_filtrado$ementa[2],
  mascara_cnj
)