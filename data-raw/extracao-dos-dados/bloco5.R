library("microdatasus")
library("dplyr")
library("janitor")
library("readr")
library(data.table)

# Criando um objeto que recebe os códigos dos municípios que utilizamos no painel
codigos_municipios <- read.csv("data-raw/extracao-dos-dados/databases-antigas/tabela_aux_municipios.csv") |>
  pull(codmunres) |>
  as.character()

# Criando um data.frame auxiliar que possui uma linha para cada combinação de município e ano
df_aux_municipios <- data.frame(codmunres = rep(codigos_municipios, each = length(2012:2023)), ano = 2012:2023)

# Baixando os dados do SINASC de 2012 a 2022
df_sinasc1 <- fetch_datasus(
  year_start = 2012,
  year_end = 2022,
  vars = c("CODMUNRES", "DTNASC", "PESO", "GESTACAO", "SEMAGESTAC", "APGAR5", "IDANOMAL", "CODANOMAL"),
  information_system = "SINASC"
)

sinasc23 <- fread("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SINASC/DNOPEN23.csv", sep = ";")
sinasc23 <- sinasc23 |>
  select(CODMUNRES, DTNASC, PESO, GESTACAO, SEMAGESTAC, APGAR5, IDANOMAL, CODANOMAL)

sinasc23$IDANOMAL <- as.character(sinasc23$IDANOMAL)

df_sinasc <- rbind(df_sinasc1, sinasc23)

unique(df_sinasc$IDANOMAL)
unique(df_sinasc$CODANOMAL)
any(is.na(unique(df_sinasc$CODANOMAL)))  #Existem NAs
any(unique(df_sinasc$CODANOMAL) == '', na.rm = TRUE)  #Não existem ''

# Transformando algumas variáveis e criando as variáveis necessárias
df_bloco5_aux <- df_sinasc |>
  mutate(
    ano = as.numeric(substr(DTNASC, nchar(DTNASC) - 3, nchar(DTNASC))),
    PESO = as.numeric(PESO),
    GESTACAO = as.numeric(GESTACAO),
    SEMAGESTAC = as.numeric(SEMAGESTAC),
    APGAR5 = as.numeric(APGAR5),
    .keep = "unused",
  ) |>
  mutate(
    total_de_nascidos_vivos = 1,
    nascidos_vivos_com_baixo_peso = if_else(PESO < 2500, 1, 0, missing = 0),
    nascidos_vivos_prematuros = if_else(GESTACAO < 5, 1, 0, missing = 0),
    nascidos_vivos_termo_precoce = if_else(SEMAGESTAC %in% c(37, 38), 1, 0, missing = 0),
    nascidos_vivos_peso_menor_1500 = if_else(PESO < 1500, 1, 0, missing = 0),
    nascidos_vivos_peso_1500_a_1999 = if_else(PESO >= 1500 & PESO <= 1999, 1, 0, missing = 0),
    nascidos_vivos_peso_2000_a_2499 = if_else(PESO <= 2499 & PESO >= 2000, 1, 0, missing = 0),
    nascidos_vivos_menos_de_28_semanas = if_else(SEMAGESTAC < 28, 1, 0, missing = 0),
    nascidos_vivos_28_a_32_semanas = if_else(SEMAGESTAC >= 28 & SEMAGESTAC <= 32, 1, 0, missing = 0),
    nascidos_vivos_33_a_34_semanas = if_else(SEMAGESTAC %in% c(33, 34), 1, 0, missing = 0),
    nascidos_vivos_35_a_36_semanas = if_else(SEMAGESTAC %in% c(35, 36), 1, 0, missing = 0),
    nascidos_condicoes_ameacadoras = if_else(PESO < 1500 | (GESTACAO < 4 | SEMAGESTAC < 32) | APGAR5 < 7, 1, 0, missing = 0),
    total_de_nascidos_malformacao = if_else((IDANOMAL == "1" | (!is.na(CODANOMAL) & CODANOMAL != "")), 1, 0, missing = 0),
    .keep = "unused"
  ) |>
  clean_names() |>
  group_by(codmunres, ano) |>
  summarise_at(vars(contains("nascidos")), sum) |>
  ungroup()


## Fazendo um left_join com a base auxiliar de municípios
df_bloco5 <- left_join(df_aux_municipios, df_bloco5_aux, by = c("codmunres", "ano"))

## Preenchendo os valores NAs, gerados após o left_join, com 0
df_bloco5[is.na(df_bloco5)] <- 0

## Verificando se os dados novos e antigos estão batendo
df_bloco5_antigo <- read.csv("data-raw/extracao-dos-dados/databases-antigas/indicadores_bloco5_condicao_de_nascimento_2012-2020.csv") |>
  clean_names()

sum(df_bloco5 |> filter(ano <= 2020) |> pull(total_de_nascidos_vivos)) - sum(df_bloco5_antigo$total_de_nascidos_vivos)
sum(df_bloco5 |> filter(ano <= 2020) |> pull(nascidos_vivos_com_baixo_peso)) - sum(df_bloco5_antigo$nascidos_vivos_com_baixo_peso)
sum(df_bloco5 |> filter(ano <= 2020) |> pull(nascidos_vivos_prematuros)) - sum(df_bloco5_antigo$nascidos_vivos_prematuros)
sum(df_bloco5 |> filter(ano <= 2020) |> pull(nascidos_vivos_termo_precoce)) - sum(df_bloco5_antigo$nascidos_vivos_termo_precoce)

## Exportando os dados
write.csv(df_bloco5, "data-raw/csv/indicadores_bloco5_condicao_de_nascimento_2012_2023.csv", row.names = FALSE)
