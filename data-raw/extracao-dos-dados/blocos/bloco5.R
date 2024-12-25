# Carregando os pacotes necessários
library(microdatasus)
library(dplyr)
library(janitor)
library(RSQLite)
library(glue)
library(tidyr)
library(data.table)
library(readr)

# Criando alguns objetos auxiliares ---------------------------------------
## Criando um objeto que recebe os códigos dos municípios que utilizamos no painel
codigos_municipios <- read_csv("data-raw/extracao-dos-dados/blocos/databases_auxiliares/tabela_aux_municipios.csv") |>
  pull(municipio) |>
  as.numeric()

## Criando um data.frame auxiliar que possui uma linha para cada combinação de município e ano
df_aux_municipios <- data.frame(codmunres = rep(codigos_municipios, each = length(2012:2024)), ano = 2012:2024)


# Para os indicadores provenientes do SINASC ------------------------------
## Baixando os dados consolidados do SINASC de 2012 a 2022 e selecionando as variáveis de interesse
df_sinasc_consolidados <- fetch_datasus(
  year_start = 2012,
  year_end = 2022,
  vars = c("CODMUNRES", "CODMUNNASC", "CODESTAB", "DTNASC", "PESO", "GESTACAO", "SEMAGESTAC", "APGAR5", "IDANOMAL", "CODANOMAL"),
  information_system = "SINASC"
)

## Baixando os dados preliminares do SINASC de 2023 e 2024 e selecionando as variáveis de interesse
options(timeout = 600)

df_sinasc_preliminares23 <- fread("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SINASC/DNOPEN23.csv", sep = ";") |>
  select(CODMUNRES, CODMUNNASC, CODESTAB, DTNASC, PESO, GESTACAO, SEMAGESTAC, APGAR5, IDANOMAL, CODANOMAL) |>
  mutate(IDANOMAL = as.character(IDANOMAL))

df_sinasc_preliminares24 <- fread("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SINASC/DNOPEN24.csv", sep = ";") |>
  select(CODMUNRES, CODMUNNASC, CODESTAB, DTNASC, PESO, GESTACAO, SEMAGESTAC, APGAR5, IDANOMAL, CODANOMAL) |>
  mutate(IDANOMAL = as.character(IDANOMAL))

df_sinasc_consolidados <- df_sinasc_consolidados %>%
  mutate_if(is.character, as.numeric)

df_sinasc_preliminares23 <- df_sinasc_preliminares23 %>%
  mutate_if(is.character, as.numeric)

df_sinasc_preliminares24 <- df_sinasc_preliminares24 %>%
  mutate_if(is.character, as.numeric)

## Juntando os dados consolidados com os dados preliminares
df_sinasc <- full_join(df_sinasc_consolidados, df_sinasc_preliminares23) |>
  full_join(df_sinasc_preliminares24)

## Verificando o que pode ser considerado um dado faltante para CODANOMAL
unique(df_sinasc$CODANOMAL)
any(is.na(unique(df_sinasc$CODANOMAL)))  #Existem NAs
any(unique(df_sinasc$CODANOMAL) == '', na.rm = TRUE)  #Não existem ''

## Transformando algumas variáveis e criando as variáveis necessárias p/ o cálculo dos indicadores
df_bloco5_sinasc <- df_sinasc |>
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
    nascidos_vivos_peso_menor_1000 = if_else(PESO < 1000, 1, 0, missing = 0),
    nascidos_vivos_peso_1000_a_1499 = if_else(PESO >= 1000 & PESO < 1500, 1, 0, missing = 0),
    nascidos_vivos_peso_1500_a_2499 = if_else(PESO < 2500 & PESO >= 1500, 1, 0, missing = 0),
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


# Juntando os dados do SINASC  ------------------------------------

# Juntando dados antigos e dados novos

# bloco5_antigo <- read_csv("data-raw/csv/indicadores_bloco5_condicao_de_nascimento_2012_2023.csv") |>
#   filter(ano <= 2021) |>
#   select(codmunres, ano, total_de_nascidos_vivos, nascidos_vivos_com_baixo_peso,
#   nascidos_vivos_prematuros, nascidos_vivos_termo_precoce, nascidos_vivos_peso_menor_1000,
#   nascidos_vivos_peso_1000_a_1499, nascidos_vivos_peso_1500_a_2499, nascidos_vivos_menos_de_28_semanas,
#   nascidos_vivos_28_a_32_semanas, nascidos_vivos_33_a_34_semanas,
#   nascidos_vivos_35_a_36_semanas, nascidos_condicoes_ameacadoras, total_de_nascidos_malformacao)

#bloco5_novo <- full_join(bloco5_antigo, df_bloco5_sinasc)

## Fazendo um left_join com a base auxiliar de municípios
df_bloco5_aux <- left_join(df_aux_municipios, df_bloco5_sinasc, by = c("codmunres", "ano"))

## Juntando, também, com os dados de asifixia e malformação, baixados no script 'bloco5_asfixia.R'
df_asfixia <- read.csv("data-raw/csv/asfixia_2012_2024.csv", sep = ';') |>
  janitor::clean_names() |>
  dplyr::arrange(codmunres, ano) |>
  dplyr::select(codmunres, ano, nascidos_vivos_asfixia1, total_nascidos) |>
  mutate(codmunres = as.numeric(codmunres))

df_malformacao <- read.csv("data-raw/csv/malformacao_2012_2024.csv", sep = ';') |>
  janitor::clean_names() |>
  dplyr::arrange(codmunres, ano) |>
  dplyr::group_by(ano, codmunres) |>
  dplyr::summarise(nascidos_vivos_anomalia = sum(nascidos_vivos_anomalia)) |>
  ungroup() |>
  mutate(codmunres = as.numeric(codmunres))

df_bloco5 <- left_join(df_bloco5_aux, df_asfixia, by = c("codmunres", "ano")) |>
  left_join(df_malformacao)


# Substituir todos os NAs por 0
df_bloco5[is.na(df_bloco5)] <- 0

## Verificando se os dados novos e antigos estão batendo
# df_bloco5_antigo <- read.csv("data-raw/extracao-dos-dados/databases_auxiliares/indicadores_bloco5_condicao_de_nascimento_2012-2020.csv") |>
#   clean_names()
#
# sum(df_bloco5 |> filter(ano <= 2020) |> pull(total_de_nascidos_vivos)) - sum(df_bloco5_antigo$total_de_nascidos_vivos)
# sum(df_bloco5 |> filter(ano <= 2020) |> pull(nascidos_vivos_com_baixo_peso)) - sum(df_bloco5_antigo$nascidos_vivos_com_baixo_peso)
# sum(df_bloco5 |> filter(ano <= 2020) |> pull(nascidos_vivos_prematuros)) - sum(df_bloco5_antigo$nascidos_vivos_prematuros)
# sum(df_bloco5 |> filter(ano <= 2020) |> pull(nascidos_vivos_termo_precoce)) - sum(df_bloco5_antigo$nascidos_vivos_termo_precoce)

## Exportando os dados
write.csv(df_bloco5, "data-raw/csv/indicadores_bloco5_condicao_de_nascimento_2012_2024.csv", row.names = FALSE)
