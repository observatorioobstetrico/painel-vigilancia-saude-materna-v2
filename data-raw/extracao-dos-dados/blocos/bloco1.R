library(tidyverse)
library(httr)
library(janitor)
library(getPass)
library(repr)
library(data.table)
library(readr)
library(openxlsx)
library(tidyr)
library(microdatasus)

# Criando alguns objetos auxiliares ---------------------------------------
### Criando um vetor com os anos consolidados
anos_consolidados <- 2012:2023

## Criando um objeto que recebe os códigos dos municípios que utilizamos no painel
codigos_municipios <- read.csv("data-raw/extracao-dos-dados/blocos/databases_auxiliares/tabela_aux_municipios.csv") |>
  pull(codmunres) |>
  as.character()

## Criando um data.frame auxiliar que possui uma linha para cada combinação de município e ano
df_aux_municipios <- data.frame(codmunres = rep(codigos_municipios, each = length(2012:2024)), ano = 2012:2024) |>
  mutate_if(is.character, as.numeric)


# Para os indicadores provenientes do SINASC ------------------------------
## Baixando os dados consolidados do SINASC de 2012 a 2022 e selecionando as variáveis de interesse
df_sinasc_consolidados <- fetch_datasus(
  year_start = anos_consolidados[1],
  year_end = anos_consolidados[length(anos_consolidados)],
  vars = c("CODMUNRES", "DTNASC", "IDADEMAE", "RACACORMAE", "ESCMAE"),
  information_system = "SINASC"
)

df_sinasc_consolidados <- df_sinasc_consolidados %>%
  mutate_if(is.character, as.numeric)

## Baixando os dados preliminares do SINASC de 2024 e selecionando as variáveis de interesse
df_sinasc_preliminares_2024 <- fread("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SINASC/DNOPEN24.csv", sep = ";") |>
  select("CODMUNRES", "DTNASC", "IDADEMAE", "RACACORMAE", "ESCMAE")

## Juntando os dados consolidados com os dados preliminares
df_sinasc <- full_join(df_sinasc_consolidados, df_sinasc_preliminares_2024) |>
  clean_names()

## Transformando algumas variáveis e criando as variáveis necessárias p/ o cálculo dos indicadores
df_bloco1_sinasc <- df_sinasc |>
  filter(codmunres %in% codigos_municipios) %>%
  mutate(
    ano = as.numeric(substr(dtnasc, nchar(dtnasc) - 3, nchar(dtnasc))),
    nvm_menor_que_20_anos = if_else(idademae < 20, 1, 0, missing = 0),
    nvm_entre_20_e_34_anos = if_else(idademae >= 20 & idademae < 35, 1, 0, missing = 0),
    nvm_maior_que_34_anos = if_else(idademae >= 35 & idademae <= 55, 1, 0, missing = 0),
    nvm_com_cor_da_pele_branca = if_else(racacormae == 1, 1, 0, missing = 0),
    nvm_com_cor_da_pele_preta = if_else(racacormae == 2, 1, 0, missing = 0),
    nvm_com_cor_da_pele_parda = if_else(racacormae == 4, 1, 0, missing = 0),
    nvm_com_cor_da_pele_amarela = if_else(racacormae == 3, 1, 0, missing = 0),
    nvm_indigenas = if_else(racacormae == 5, 1, 0, missing = 0),
    nvm_com_escolaridade_ate_3 = if_else(escmae == 1 | escmae == 2, 1, 0, missing = 0),
    nvm_com_escolaridade_de_4_a_7 = if_else(escmae == 3, 1, 0, missing = 0),
    nvm_com_escolaridade_de_8_a_11 = if_else(escmae == 4, 1, 0, missing = 0),
    nvm_com_escolaridade_acima_de_11 = if_else(escmae == 5, 1, 0, missing = 0)
  ) %>%
  group_by(codmunres, ano) %>%
  summarise(
    total_de_nascidos_vivos = n(),
    nvm_menor_que_20_anos = sum(nvm_menor_que_20_anos),
    nvm_entre_20_e_34_anos = sum(nvm_entre_20_e_34_anos),
    nvm_maior_que_34_anos = sum(nvm_maior_que_34_anos),
    nvm_com_cor_da_pele_branca = sum(nvm_com_cor_da_pele_branca),
    nvm_com_cor_da_pele_preta = sum(nvm_com_cor_da_pele_preta),
    nvm_com_cor_da_pele_parda = sum(nvm_com_cor_da_pele_parda),
    nvm_com_cor_da_pele_amarela = sum(nvm_com_cor_da_pele_amarela),
    nvm_indigenas = sum(nvm_indigenas),
    nvm_com_escolaridade_ate_3 = sum(nvm_com_escolaridade_ate_3),
    nvm_com_escolaridade_de_4_a_7 = sum(nvm_com_escolaridade_de_4_a_7),
    nvm_com_escolaridade_de_8_a_11 = sum(nvm_com_escolaridade_de_8_a_11),
    nvm_com_escolaridade_acima_de_11 = sum(nvm_com_escolaridade_acima_de_11)
  ) %>%
  mutate_if(is.character, as.numeric)

# Incluindo dados de municipios
df_bloco1_sinasc <- left_join(df_aux_municipios, df_bloco1_sinasc)

# Todos os NA's transformam em 0
df_bloco1_sinasc[is.na(df_bloco1_sinasc)] <- 0


# Para os indicadores provenientes do Tabnet ------------------------------
## Observação: Tabnet foi atualizado até 2021.
## Importando as funções utilizadas para baixar os dados do Tabnet
source("data-raw/extracao-dos-dados/blocos/funcoes_auxiliares.R")

## População feminina de 10 a 49 anos com plano de saúde -------------------
### Baixando os dados de estimativas da população feminina de 10 a 49 anos
df_est_pop_aux <- est_pop_tabnet(
  coluna = "Ano", periodo = as.character(2012:2021)
) |>
  select(!municipio)

### Verificando se existem NAs
if (any(is.na(df_est_pop_aux))) {
  print("existem NAs")
} else {
  print("não existem NAs")
}

### Passando o data.frame para o formato long
df_est_pop <- df_est_pop_aux |>
  pivot_longer(
    !codmunres,
    names_to = "ano",
    values_to = "populacao_feminina_10_a_49"
  ) |>
  mutate(
    ano = as.numeric(ano),
  ) |>
  arrange(codmunres, ano) |>
  filter(codmunres %in% df_aux_municipios$codmunres)

### Baixando os dados de mulheres de 10 a 49 anos beneficíarias de planos de saúde
#### Tive que separar em dois porque estava dando erro baixando o período inteiro
df_beneficiarias_aux1 <- pop_com_plano_saude_tabnet(
  faixa_etaria = c("10 a 14 anos", "15 a 19 anos", "20 a 24 anos", "25 a 29 anos", "30 a 34 anos", "35 a 39 anos", "40 a 44 anos", "45 a 49 anos"),
  periodo = 2012:2016
) |>
  select(!municipio)

df_beneficiarias_aux2 <- pop_com_plano_saude_tabnet(
  faixa_etaria = c("10 a 14 anos", "15 a 19 anos", "20 a 24 anos", "25 a 29 anos", "30 a 34 anos", "35 a 39 anos", "40 a 44 anos", "45 a 49 anos"),
  periodo = 2017:2021
) |>
  select(!municipio)

df_beneficiarias_aux <- full_join(df_beneficiarias_aux1, df_beneficiarias_aux2) |>
  arrange()

#### Verificando se existem NAs
if (any(is.na(df_beneficiarias_aux))) {
  print("existem NAs")
} else {
  print("não existem NAs")
}

#### Aconteceram NAs porque tive que baixar os períodos separadamente
df_beneficiarias_aux[is.na(df_beneficiarias_aux)] <- 0

#### Passando o data.frame para o formato long
df_beneficiarias <- df_beneficiarias_aux |>
  pivot_longer(
    !codmunres,
    names_to = "mes_ano",
    values_to = paste0("beneficiarias_10_a_49")
  ) |>
  mutate(
    mes = substr(mes_ano, start = 1, stop = 3),
    ano = as.numeric(paste0("20", substr(mes_ano, start = 5, stop = 6))),
    .after = mes_ano,
    .keep = "unused"
  ) |>
  arrange(codmunres, ano) |>
  filter(codmunres %in% df_aux_municipios$codmunres) |>
  left_join(df_est_pop) |>
  group_by(codmunres, ano) |>
  filter(beneficiarias_10_a_49 < populacao_feminina_10_a_49) |>
  summarise(
    beneficiarias_10_a_49 = round(median(beneficiarias_10_a_49))
  ) |>
  ungroup()

#### Juntando com os dados de estimativas populacionais
df_beneficiarias_pop <- left_join(df_est_pop, df_beneficiarias)

### Calculando a cobertura suplementar, os limites inferiores e superiores para a consideração de outliers e inputando caso necessário
df_cob_suplementar <- df_beneficiarias_pop |>
  mutate(
    cob_suplementar = round(beneficiarias_10_a_49 / populacao_feminina_10_a_49, 3)
  ) |>
  group_by(codmunres) |>
  mutate(
    q1 = round(quantile(cob_suplementar[which(cob_suplementar < 1 & ano %in% 2014:2021)], 0.25), 3),
    q3 = round(quantile(cob_suplementar[which(cob_suplementar < 1 & ano %in% 2014:2021)], 0.75), 3),
    iiq = q3 - q1,
    lim_inf = round(q1 - 1.5*iiq, 3),
    lim_sup = round(q3 + 1.5*iiq, 3),
    outlier = ifelse((cob_suplementar > 1) | (cob_suplementar < lim_inf | cob_suplementar > lim_sup) | (is.na(q1) & is.na(q3)) | (is.na(cob_suplementar)), 1, 0),
    novo_cob_suplementar = ifelse(
      outlier == 0,
      cob_suplementar,
      round(median(cob_suplementar[which(outlier == 0 & ano %in% 2014:2021)]), 3)
    ),
    novo_beneficiarias_10_a_49 = round(novo_cob_suplementar * populacao_feminina_10_a_49)
  ) |>
  ungroup() |>
  select(codmunres, ano, pop_fem_10_49_com_plano_saude = novo_beneficiarias_10_a_49, populacao_feminina_10_a_49)


## Juntando todos os dados provenientes do Tabnet --------------------------
df_bloco1_tabnet <- df_aux_municipios |>
  left_join(df_cob_suplementar)

### Substituindo os NA's da coluna 'pop_fem_10_49_com_plano_saude' por 0 (gerados após o left_join)
df_bloco1_tabnet$pop_fem_10_49_com_plano_saude[is.na(df_bloco1_tabnet$pop_fem_10_49_com_plano_saude) & df_bloco1_tabnet$ano <= 2021] <- 0


# Para o indicador de cobertura da AB -------------------------------------
## Lendo as bases contendo as variáveis que serão utilizadas e fazendo os tratamentos necessários
### Para os anos de 2012 até 2020
historico_ab_municipios <- read_delim("data-raw/csv/Historico_AB_MUNICIPIOS_2007_202012.csv") |>
  janitor::clean_names() |>
  mutate(
    ano = as.numeric(substr(nu_competencia, 1, 4)),
    qt_cobertura_ab = as.numeric(str_replace_all(qt_cobertura_ab, "\\.", "") |> str_replace_all("\\,", "\\.")),
    qt_populacao = as.numeric(str_replace_all(qt_populacao, "\\.", "") |> str_replace_all("\\,", "\\."))
  ) |>
  select(ano, codmunres = co_municipio_ibge, qt_cobertura_ab, qt_populacao)

### Para os anos de 2021 até set/2023
cobertura_potencial_aps_municipio1 <- read.xlsx("data-raw/csv/cobertura_potencial_aps_municipio3.xlsx") |>
  janitor::clean_names() |>
  select(comp_cnes, codmunres = codigo_ibge, qt_cobertura_ab = qt_capacidade_da_equipe, qt_populacao = populacao) |>
  mutate(
    ano = as.numeric(substr(comp_cnes, 1, 4)),
    qt_cobertura_ab = as.numeric(str_replace_all(qt_cobertura_ab, "\\.", "") |> str_replace_all("\\,", "\\.")),
    qt_populacao = as.numeric(str_replace_all(qt_populacao, "\\.", "") |> str_replace_all("\\,", "\\."))
  ) |>
  select(ano, codmunres, qt_cobertura_ab, qt_populacao)

### Para os meses de out/2023 até abr/2024
cobertura_potencial_aps_municipio2 <- readxl::read_xls("data-raw/csv/cobertura_potencial_aps_municipio4.xls") |>
  janitor::clean_names() |>
  select(competencia_cnes, codmunres = ibge, qt_cobertura_ab = qt_capacidade_da_equipe, qt_populacao = populacao) |>
  mutate(
    ano = as.numeric(substr(competencia_cnes, 4, 7)),
    qt_cobertura_ab = as.numeric(str_replace_all(qt_cobertura_ab, "\\.", "") |> str_replace_all("\\,", "\\.")),
    qt_populacao = as.numeric(str_replace_all(qt_populacao, "\\.", "") |> str_replace_all("\\,", "\\."))
  ) |>
  select(ano, codmunres, qt_cobertura_ab, qt_populacao)

## Juntando os dados de 2021 até abr/2024
cobertura_potencial_aps_municipio <- rbind(cobertura_potencial_aps_municipio1,
                                           cobertura_potencial_aps_municipio2)

## Juntando todas as bases
dados_ab_municipios <- rbind(historico_ab_municipios, cobertura_potencial_aps_municipio)

## Calculando a média anual dos valores de qt_cobertura_ab e qt_populacao
dados_ab_municipios <- dados_ab_municipios |>
  group_by(ano, codmunres) |>
  summarize(
    qt_cobertura_ab = mean(qt_cobertura_ab),
    qt_populacao = mean(qt_populacao)
  )

### Garantindo que qt_cobertura_ab não seja maior que qt_populacao
dados_ab_municipios$qt_cobertura_ab <- pmin(dados_ab_municipios$qt_cobertura_ab, dados_ab_municipios$qt_populacao)

### Fazendo um left_join com a base auxiliar de municípios
df_cobertura_ab <- left_join(df_aux_municipios, dados_ab_municipios |> mutate(codmunres = as.numeric(codmunres)))

### Substituindo os NA's da coluna 'qt_cobertura_ab' por 0 (gerados após o left_join)
df_cobertura_ab$qt_cobertura_ab[is.na(df_cobertura_ab$qt_cobertura_ab) & df_cobertura_ab$ano < 2023] <- 0

### Substituindo os NA's da coluna 'qt_populacao' por 0 (gerados após o left_join)
df_cobertura_ab$qt_populacao[is.na(df_cobertura_ab$qt_populacao) & df_cobertura_ab$ano < 2023] <- 0


# Juntando os dados de todas as bases -------------------------------------
df_bloco1 <- df_bloco1_sinasc |>
  left_join(df_bloco1_tabnet) |>
  left_join(df_cobertura_ab |> rename(media_cobertura_esf = qt_cobertura_ab, populacao_total = qt_populacao))


# Verificando se os dados novos e antigos estão batendo -------------------
## Lendo o arquivo com os dados de 2012 a 2023, que utilizamos no painel original
df_bloco1_antigo <- read.csv("data-raw/csv/indicadores_bloco1_socioeconomicos_2012-2023.csv") |>
  clean_names() |>
  filter(ano <= 2022)

sum(df_bloco1 |> filter(ano < 2023) |> pull(total_de_nascidos_vivos)) - sum(df_bloco1_antigo$total_de_nascidos_vivos)
sum(df_bloco1 |> filter(ano < 2023) |> pull(nvm_menor_que_20_anos)) - sum(df_bloco1_antigo$nvm_menor_que_20_anos)
sum(df_bloco1 |> filter(ano < 2023) |> pull(nvm_entre_20_e_34_anos)) - sum(df_bloco1_antigo$nvm_entre_20_e_34_anos)
sum(df_bloco1 |> filter(ano < 2023) |> pull(nvm_maior_que_34_anos)) - sum(df_bloco1_antigo$nvm_maior_que_34_anos)
sum(df_bloco1 |> filter(ano < 2023) |> pull(nvm_com_cor_da_pele_branca)) - sum(df_bloco1_antigo$nvm_com_cor_da_pele_branca)
sum(df_bloco1 |> filter(ano < 2023) |> pull(nvm_com_cor_da_pele_preta)) - sum(df_bloco1_antigo$nvm_com_cor_da_pele_preta)
sum(df_bloco1 |> filter(ano < 2023) |> pull(nvm_com_cor_da_pele_parda)) - sum(df_bloco1_antigo$nvm_com_cor_da_pele_parda)
sum(df_bloco1 |> filter(ano < 2023) |> pull(nvm_com_cor_da_pele_amarela)) - sum(df_bloco1_antigo$nvm_com_cor_da_pele_amarela)
sum(df_bloco1 |> filter(ano < 2023) |> pull(nvm_indigenas)) - sum(df_bloco1_antigo$nvm_indigenas)
sum(df_bloco1 |> filter(ano < 2023) |> pull(nvm_com_escolaridade_ate_3)) - sum(df_bloco1_antigo$nvm_com_escolaridade_ate_3)
sum(df_bloco1 |> filter(ano < 2023) |> pull(nvm_com_escolaridade_de_4_a_7)) - sum(df_bloco1_antigo$nvm_com_escolaridade_de_4_a_7)
sum(df_bloco1 |> filter(ano < 2023) |> pull(nvm_com_escolaridade_de_8_a_11)) - sum(df_bloco1_antigo$nvm_com_escolaridade_de_8_a_11)
sum(df_bloco1 |> filter(ano < 2023) |> pull(nvm_com_escolaridade_acima_de_11)) - sum(df_bloco1_antigo$nvm_com_escolaridade_acima_de_11)
sum(df_bloco1 |> filter(ano < 2023) |> pull(media_cobertura_esf)) - sum(df_bloco1_antigo$media_cobertura_esf)
sum(df_bloco1 |> filter(ano < 2023) |> pull(populacao_total)) - sum(df_bloco1_antigo$populacao_total)
sum(df_bloco1 |> filter(ano < 2022) |> pull(populacao_feminina_10_a_49)) - sum(df_bloco1_antigo$populacao_feminina_10_a_49, na.rm = T)


# Salvando a base de dados completa na pasta data-raw/csv -----------------
write.csv(df_bloco1, "data-raw/csv/indicadores_bloco1_socioeconomicos_2012-2024.csv", row.names = FALSE)
