# Carregando os pacotes necessários
library(tidyverse)
library(httr)
library(janitor)
library(getPass)
library(repr)
library(data.table)
library(readr)
library(openxlsx)
library(microdatasus)
library(future)
library(future.apply)

# Variáveis utilizadas no SINASC
vars <- c("PESO", "GESTACAO", "SEMAGESTAC", "APGAR5")

# Inserir informações:
# Todos os anos a serem baixados
anos <- c(2024:2026)
# Ano baixado pelo opendatasus
ano_opendatasus <- c(2025:2026)
# Links para o ano baixado pelo opendatasus
link_opendatasus_sinasc <- c(
  "2025" = "https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SINASC/csv/SINASC_2025_csv.zip",
  "2026" = "https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SINASC/csv/SINASC_2026_csv.zip"
)

codigos_municipios <- read.csv("data-raw/extracao-dos-dados/blocos/databases_auxiliares/tabela_aux_municipios.csv") |>
  pull(codmunres) |>
  as.character()

df_aux_municipios <- data.frame(codmunres = rep(codigos_municipios, each = length(anos)), ano = anos)

## Criando data.frames que irão receber os dados dos indicadores de causas evitáveis e grupos de causa
df_sinasc_incompletude_aux <- data.frame(codmunres = rep(codigos_municipios, each = length(anos)), ano = anos)

# Baixando os dados do SINASC (com paralelização) -------------------------
## Criando o planejamento dos futures
plan(sequential)
options(timeout = 600)
## Criando uma função que baixa todos os dados necessários para um certo ano
processa_ano <- function(ano) {
  # Carrega os pacotes dentro da worker
  library(microdatasus)
  library(dplyr)
  library(data.table)
  library(stringr)

  # Criando uma função para criar a coluna de "ano" em bases do SIM e SINASC
  extrai_ano <- function(data, n = 4) {
    as.numeric(substr(data, nchar(data) - n + 1, nchar(data)))
  }

  # Criando uma função para insistir várias vezes no download
  fread_retry <- function(url, ..., max_tries = 10, wait_seconds = 10) {
    for (i in seq_len(max_tries)) {
      tryCatch({
        message(sprintf("Tentando baixar: %s (tentativa %d de %d)", url, i, max_tries))
        df <- data.table::fread(url, ...)
        return(df)
      },
      error = function(e) {
        message("Erro ao tentar baixar: ", conditionMessage(e))
        if (i < max_tries) {
          message(sprintf("Aguardando %d segundos para nova tentativa...", wait_seconds))
          Sys.sleep(wait_seconds)
        } else {
          stop("Falha ao baixar após ", max_tries, " tentativas.")
        }
      })
    }
  }

  # Criando uma função genérica para baixar dados pelo microdatasus
  baixa_dados <- function(ano, sistema, data_col, vars = NULL) {
    # Tratamento especial para os dados preliminares
    if (ano %in% ano_opendatasus) {
      switch(sistema,
             "SINASC" = {
               fread_retry(
                 link_opendatasus_sinasc[as.character(ano)],
                 sep = ";"
               ) |>
                 mutate(ano = extrai_ano(.data[[data_col]])) |>
                 select(all_of(c("CODMUNRES", "ano", data_col, vars)))
             }
      )
    } else {
      # Anos consolidados via microdatasus
      dados <- fetch_datasus(
        year_start = ano,
        year_end = ano,
        information_system = sistema,
        vars = c("CODMUNRES", data_col, vars)
      ) |>
        mutate(ano = extrai_ano(.data[[data_col]]))

      dados
    }
  }

  message("Processando ano ", ano)
  list(
    sinasc = baixa_dados(ano, "SINASC", "DTNASC", vars = c(vars))
  )
}


## Baixando todos os dados
resultados <- future_lapply(anos, processa_ano)

df_sinasc <- rbindlist(lapply(resultados, `[[`, "sinasc"), fill = TRUE) |>
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
write.csv(df_incompletude_bloco7_morbidade, 'data-raw/csv/indicadores_incompletude_bloco7_morbidade_2024-2026.csv', row.names = FALSE)
