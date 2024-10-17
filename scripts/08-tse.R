# pegando os links pelo CKAN ----------------------------------------------

## não sei usar o ckanr corretamente...
#ckanr::ckanr_setup("https://dadosabertos.tse.jus.br/dataset/resultados-2024-boletim-de-urna")
#ckanr::package_list(url = "https://dadosabertos.tse.jus.br/dataset/")

library(tidyverse)

url_download <- paste0(
  "https://cdn.tse.jus.br/estatistica/sead/eleicoes/eleicoes2024",
  "/buweb/bweb_1t_{uf}_091020241636.zip"
)

uf <- sort(unique(abjData::muni$uf_sigla))

links <- glue::glue(url_download)

fs::dir_create("dados/tse_zip")

baixar_link <- function(link) {
  arquivo <- paste0("dados/tse_zip/", basename(link))
  if (!file.exists(arquivo)) {
    httr::GET(link, httr::write_disk(arquivo))
  }
}

walk(links, baixar_link, .progress = TRUE)


# dezipar -----------------------------------------------------------------

# conseguimos baixar os zips...
# agora precisamos dezipar e colocar o arquivo em uma pasta

arquivos_zip <- fs::dir_ls("dados/tse_zip")

fs::dir_create("dados/tse_csv")

dezipar_arquivo <- function(arquivo_zip) {
  arq_csv <- arquivo_zip |>
    basename() |>
    fs::path_ext_set("csv")
  arq_path <- paste0("dados/tse_csv/", arq_csv)
  if (!file.exists(arq_path)) {
    zip::unzip(
      arquivo_zip,
      files = arq_csv,
      exdir = "dados/tse_csv/"
    )
  }
}

walk(arquivos_zip, dezipar_arquivo, .progress = TRUE)


# leitura dos dados -------------------------------------------------------

arquivos_csv <- fs::dir_ls("dados/tse_csv")
loc <- locale(encoding = "latin1")
da_uf <- read_csv2(arquivos_csv[1], locale = loc) |>
  janitor::clean_names()

da_uf |>
  glimpse()

# da_uf |>
#   transmute()

da_uf |>
  count(ds_tipo_votavel)

dados_urna <- da_uf |>
  mutate(
    dt_geracao = dmy(dt_geracao),
    dt_bu_recebido,
    dt_carga_urna_efetivada,
    dt_emissao_bu,
    sg_uf,
    cd_municipio,
    nm_municipio,
    nr_zona,
    nr_secao,
    nr_local_votacao,
    ds_cargo_pergunta,
    nr_partido,
    sg_partido,
    nm_partido,
    qt_aptos,
    qt_comparecimento,
    qt_abstencoes,
    ds_tipo_votavel,
    nm_votavel,
    qt_votos,
    qt_elei_biom_sem_habilitacao,
    .keep = "used"
  )


# arrumação dos dados -----------------------------------------------------

dados_municipio <- dados_urna |>
  filter(ds_cargo_pergunta == "Prefeito") |>
  group_by(
    sg_uf,
    nm_municipio,
    cd_municipio,
    nr_partido,
    nm_votavel
  ) |>
  summarise(
    across(nr_zona:nr_local_votacao, n_distinct),
    across(c(qt_votos, qt_aptos, qt_comparecimento, qt_abstencoes), sum),
    .groups = "drop"
  )

# não está no cran
# então precisa instalar pelo github
## remotes::install_github("curso-r/munifacil")

dados_municipio_ibge <- dados_municipio |>
  munifacil::limpar_colunas(nm_municipio, sg_uf) |>
  ## correcao manual
  # add_row(
  #   uf_join = "AC",
  #   muni_join = "nao existe"
  # ) |>
  # mutate(muni_join = case_when(
  #   muni_join == "nao existe" ~ "bujari",
  #   .default = muni_join
  # ))
  munifacil::incluir_codigo_ibge() |>
  select(-c(manual:existia_2010)) |>
  mutate(id_municipio = as.numeric(id_municipio))


# análise exploratória ----------------------------------------------------

glimpse(dados_municipio_ibge)

## atenção para código que não faz parte do curso

## utiliza o pacote sf
todos_muni <- geobr::read_municipality(year = 2022)

acre <- todos_muni |>
  filter(abbrev_state == "AC")

dados_municipio_ibge |>
  group_by(nm_votavel) |>
  summarise(n = sum(qt_votos)) |>
  arrange(desc(n)) |>
  print(n = 100)

brancos_nulos <- dados_municipio_ibge |>
  group_by(nm_municipio, id_municipio) |>
  summarise(
    n_bn = sum(qt_votos[nm_votavel %in% c("Branco", "Nulo")]),
    n_total = sum(qt_votos),
    .groups = "drop"
  ) |>
  mutate(prop_bn = n_bn / n_total)

abstencoes <- dados_municipio_ibge |>
  group_by(nm_municipio, id_municipio) |>
  summarise(
    across(qt_aptos:qt_abstencoes, sum),
    .groups = "drop"
  ) |>
  mutate(prop_abs = qt_abstencoes / qt_aptos)

acre |>
  inner_join(
    brancos_nulos,
    # c("id_municipio" = "code_muni") ## sintaxe "antiga"
    join_by(code_muni == id_municipio)
  ) |>
  ggplot(aes(fill = prop_bn)) +
  geom_sf() +
  theme_minimal()

acre |>
  inner_join(
    abstencoes,
    # c("id_municipio" = "code_muni") ## sintaxe "antiga"
    join_by(code_muni == id_municipio)
  ) |>
  ggplot(aes(fill = prop_abs)) +
  geom_sf() +
  theme_minimal()

## install.packages("genderBR")

genderBR::get_gender("julia", prob = TRUE)

aux_genderbr <- dados_municipio_ibge |>
  distinct(nm_votavel) |>
  filter(!nm_votavel %in% c("Branco", "Nulo")) |>
  mutate(
    sexo_presumido = genderBR::get_gender(nm_votavel),
    sexo_presumido = case_when(
      str_detect(nm_votavel, "PROFESSORA") ~ "Female",
      str_detect(nm_votavel, "INHO|DELEGADO|^DR |PROFESSOR |^PADEIRO$|^SERJÃO$") ~ "Male",
      .default = sexo_presumido
    )
  ) |>
  #filter(is.na(sexo_presumido)) |>
  arrange(nm_votavel) |>
  print(n = 100)


# dados candidatos
httr::GET(
  "https://cdn.tse.jus.br/estatistica/sead/odsele/consulta_cand/consulta_cand_2024.zip",
  httr::write_disk("dados/candidatos.zip")
)
zip::unzip(
  "dados/candidatos.zip",
  "consulta_cand_2024_AC.csv",
  exdir = "dados/"
)
candidatos <- read_csv2("dados/consulta_cand_2024_AC.csv", locale = loc) |>
  janitor::clean_names()

candidatos |>
  glimpse()

candidatos_select <- candidatos |>
  filter(ds_cargo == "PREFEITO") |>
  select(
    nm_urna_candidato,
    ds_genero,
    ds_cor_raca
  )

# anti_join
# semi_join
dados_municipio_ibge |>
  distinct(nm_votavel) |>
  anti_join(
    candidatos_select,
    join_by(nm_votavel == nm_urna_candidato)
  )

aux_join_sexo <- dados_municipio_ibge |>
  distinct(nm_votavel) |>
  inner_join(
    candidatos_select,
    join_by(nm_votavel == nm_urna_candidato)
  )

aux_join_sexo |>
  inner_join(aux_genderbr, "nm_votavel") |>
  count(ds_genero, sexo_presumido)

# olhando os erros
aux_join_sexo |>
  inner_join(aux_genderbr, "nm_votavel") |>
  filter(
    ds_genero == "MASCULINO",
    sexo_presumido == "Female"
  )


# gráficos por genero e cor raça ------------------------------------------


por_sexo <- dados_municipio_ibge |>
  left_join(
    aux_join_sexo, "nm_votavel"
  ) |>
  mutate(across(
    c(ds_genero, ds_cor_raca),
    \(x) replace_na(x, "Branco/Nulo")
  )) |>
  # forma alternativa
  # replace_na(list(
  #   ds_genero = 0, ds_cor_raca = 0
  # ))
  group_by(id_municipio, ds_genero) |>
  summarise(
    qt_votos = sum(qt_votos),
    .groups = "drop_last"
  ) |>
  mutate(prop = qt_votos / sum(qt_votos)) |>
  ungroup()




acre |>
  inner_join(
    por_sexo,
    # c("id_municipio" = "code_muni") ## sintaxe "antiga"
    join_by(code_muni == id_municipio)
  ) |>
  ggplot(aes(fill = prop)) +
  geom_sf() +
  facet_wrap(~ds_genero) +
  theme_minimal()


graficos <- acre |>
  inner_join(
    por_sexo,
    # c("id_municipio" = "code_muni") ## sintaxe "antiga"
    join_by(code_muni == id_municipio)
  ) |>
  group_split(ds_genero) |>
  map(\(x) {
    x |>
      ggplot() +
      geom_sf(data = acre) +
      geom_sf(aes(fill = prop)) +
      facet_wrap(~ds_genero) +
      theme_minimal()
  })

patchwork::wrap_plots(graficos)
