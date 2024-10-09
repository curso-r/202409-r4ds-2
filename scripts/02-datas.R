# Definindo o locale da sessão do R:
Sys.setlocale("LC_ALL", "pt_br.utf-8")
Sys.getlocale() # Qual é o locale da sessão do R?

# Funções do R base ------
Sys.Date()
Sys.time()

# class() será usada para verificar o tipo de variável

# "2024-09-17"
# 17/09/2024

class("2024-09-17") # character

class(as.Date("2024-09-17")) # Date

# tidyverse > lubridate ----------------
library(tidyverse)

today() # similar à função Sys.Date()
now() # similar à função Sys.time()

# curiosidade
as.numeric(Sys.Date()) # dias desde 01/01/1970
as.numeric(Sys.time()) # segundos desde 01/01/1970, ano novo de londres

# Convertendo datas -------------------------------
as.Date("17/09/2024") # Data errada: "0017-09-20"
# Cuidado!

as.Date("17/09/2024", format = "%d/%m/%Y")

# %d = dia, com 2 dígitos
# %m = mês, com 2 dígitos
# %Y = ano com 4 dígitos

as.Date("17/09/24", format = "%d/%m/%y")
# %Y = ano com 2 dígitos

# Uma das funções que mais facilitam para datas no padrão usado no Brasil:
dmy("17/09/2024")

# Eu costumo usar parse_date()
parse_date(c("17/09/2024"), format = "%d/%m/%Y")

# ?parse_date
# Year: "%Y" (4 digits). "%y" (2 digits); 00-69 -> 2000-2069, 70-99 -> 1970-1999.
#
# Month: "%m" (2 digits), "%b" (abbreviated name in current locale), "%B" (full name in current locale).
#
# Day: "%d" (2 digits), "%e" (optional leading space), "%a" (abbreviated name in current locale).
#
# Hour: "%H" or "%I" or "%h", use I (and not H) with AM/PM, use h (and not H) if your times represent durations longer than one day.
#
# Minutes: "%M"
#
# Seconds: "%S" (integer seconds), "%OS" (partial seconds)
#
# Time zone: "%Z" (as name, e.g. "America/Chicago"), "%z" (as offset from UTC, e.g. "+0800")
#
# AM/PM indicator: "%p".
#
# Non-digits: "%." skips one non-digit character, "%+" skips one or more non-digit characters, "%*" skips any number of non-digits characters.
#
# Automatic parsers: "%AD" parses with a flexible YMD parser, "%AT" parses with a flexible HMS parser.
#
# Time since the Unix epoch: "%s" decimal seconds since the Unix epoch.
#
# Shortcuts: "%D" = "%m/%d/%y", "%F" = "%Y-%m-%d", "%R" = "%H:%M", "%T" = "%H:%M:%S", "%x" = "%y/%m/%d".


"17 de Setembro de 2024" |>
  dmy(locale = "pt_BR")

"17 de Setembro de 2024" |>
  parse_date(
    format = "%d de %B de %Y",
    locale = locale(date_names = "pt")
  )

# Datas do excel podem ser convertidas para datas do R
library(janitor)
?excel_numeric_to_date

as_date(40000)

excel_numeric_to_date(40000)


# Como podemos usar esse tipo de variável data? --------------------------
library(readr)

mananciais <- read_delim("https://github.com/beatrizmilz/mananciais/raw/master/inst/extdata/mananciais.csv",
  delim = ";", escape_double = FALSE, locale = locale(
    decimal_mark = ",",
    grouping_mark = "."
  ), trim_ws = TRUE
)

glimpse(mananciais)

# funções de arredondamento de números
round(0.9)
round(0.5)
floor(0.9) #  arredonda para o número inteiro menor
ceiling(0.1) # arredonda para o número inteiro maior


mananciais_datas <- mananciais |>
  mutate(
    # extrair elementos de datas
    # singular
    dia = day(data),
    mes = month(data),
    ano = year(data),
    dia_semana = wday(data, label = TRUE),
    # arredondar
    mes_arredondado = floor_date(data, unit = "month"),
    # contas
    trinta_dias_apos = data + days(30),
    tres_meses_apos = data + months(3),
    quatro_anos_depois = data + years(4),
    # onde eu quero que essas colunas novas sejam armazenadas?
    .after = data
  )

# locale(date_names = "pt")

mananciais_media_volume <- mananciais_datas |>
  group_by(mes_arredondado, sistema) |>
  summarise(
    media_volume_porcentagem = mean(volume_porcentagem)
  ) |>
  ungroup()

mananciais_media_volume |>
  ggplot() +
  aes(x = mes_arredondado, y = media_volume_porcentagem) +
  geom_line(aes(color = sistema)) +
  theme_light() +
  # ?scale_x_date
  # personalizar a escala do eixo x para datas
  scale_x_date(
    date_breaks = "3 years",
    date_labels = "%Y"
  )

# fazer subtrações com números
data_ano_passado <- Sys.Date() - years(1)

mananciais_media_volume |>
  filter(mes_arredondado >= data_ano_passado) |>
  ggplot() +
  aes(x = mes_arredondado, y = media_volume_porcentagem) +
  geom_line(aes(color = sistema)) +
  theme_light() +
  # outro exemplo de personalização da escala do eixo x para datas
  scale_x_date(
    date_breaks = "2 month",
    date_labels = "%b/%y"
  )

# ?strftime - string from time -----
# converter data para um texto,
# podemos usar os operadores de datas vistos anteriormente (%d, %m, %Y, etc)

mananciais_datas |>
  mutate(
    data_amigavel = strftime(data, "%d de %b de %Y"),
    mes_amigavel = strftime(data, "%B de %Y")
  ) |>
  View()
