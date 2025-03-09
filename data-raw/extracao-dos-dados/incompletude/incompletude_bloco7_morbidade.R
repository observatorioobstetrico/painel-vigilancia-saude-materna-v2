# Carregando os pacotes necessários
library(dplyr)
library(tidyr)
library(janitor)
library(microdatasus)

# Criando alguns objetos auxiliares ----------------------------------------
## Criando um objeto que recebe os códigos dos municípios que utilizamos no painel
codigos_municipios <- read.csv("data-raw/extracao-dos-dados/blocos/databases_auxiliares/tabela_aux_municipios.csv") |>
  pull(codmunres) |>
  as.character()

## Criando um data.frame auxiliar que possui uma linha para cada combinação de município e ano
df_aux_municipios <- data.frame(codmunres = rep(codigos_municipios, each = length(2012:2023)), ano = 2012:2023)


# Baixando os dados do SINASC ----------------------------------------------
## Baixando os dados consolidados do SINASC e selecionando as variáveis de interesse
df_sinasc_aux <- fetch_datasus(
  year_start = 2012,
  year_end = 2023,
  vars = c("CODMUNRES", "DTNASC", "PESO", "GESTACAO", "SEMAGESTAC", "APGAR5"),
  information_system = "SINASC"
)

## Criando a variável de ano e transformando algumas variáveis
df_sinasc <- df_sinasc_aux |>
  mutate(
    ano = as.numeric(substr(DTNASC, nchar(DTNASC) - 3, nchar(DTNASC))),
    .keep = "unused",
    .after = "CODMUNRES"
  ) |>
  clean_names() |>
  mutate(
    peso = as.numeric(peso),
    semagestac = as.numeric(semagestac),
    apgar5 = as.numeric(apgar5)
  )


# Criando as variáveis de incompletude ------------------------------------
## Checando quais os possíveis valores incompletos para cada variável
### Para PESO
sort(unique(df_sinasc$peso), na.last = FALSE)  # Existem NAs
sort(unique(df_sinasc$peso), decreasing = TRUE)
length(df_sinasc$peso[which(df_sinasc$peso == 9999)]) # Existem 107 valores 9999

### Para GESTACAO
sort(unique(df_sinasc$gestacao), na.last = FALSE)  # Existem NAs e valores 9 (ignorado)

### Para SEMAGESTAC
sort(unique(df_sinasc$semagestac), na.last = FALSE)  # Existem NAs

### Para APGAR5
sort(unique(df_sinasc$apgar5), na.last = FALSE)  # Existem NAs e valores 99 (ignorado)

## Criando as variáveis de incompletude
df_incompletude_bloco7_morbidade <- df_sinasc |>
  mutate(
    condicoes_ameacadoras_totais = 1,
    condicoes_ameacadoras_incompletos_intersecao = ifelse(
      (is.na(peso) | peso == 9999) & (is.na(gestacao) & is.na(semagestac)) & (is.na(apgar5) | apgar5 == 99),
      1,
      0
    ),
    condicoes_ameacadoras_incompletos_uniao = ifelse(
      (is.na(peso) | peso == 9999) | (is.na(gestacao) | is.na(semagestac)) | (is.na(apgar5) | apgar5 == 99),
      1,
      0
    ),

  ) |>
  group_by(ano, codmunres) |>
  summarise(
    condicoes_ameacadoras_totais = sum(condicoes_ameacadoras_totais),
    condicoes_ameacadoras_incompletos_intersecao = sum(condicoes_ameacadoras_incompletos_intersecao),
    condicoes_ameacadoras_incompletos_uniao = sum(condicoes_ameacadoras_incompletos_uniao)
  ) |>
  ungroup() |>
  right_join(df_aux_municipios) |>
  mutate(across(everything(), ~replace_na(.x, 0))) |>
  arrange(codmunres, ano)

## Exportando os dados
write.csv(df_incompletude_bloco7_morbidade, 'data-raw/csv/indicadores_incompletude_bloco7_morbidade_2012-2023.csv', row.names = FALSE)
