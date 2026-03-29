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
vars <- c("CONSPRENAT","ESCMAE","GESTACAO","IDADEMAE",
          "MESPRENAT","PARTO", "PESO","QTDPARTCES",
          "QTDPARTNOR","RACACORMAE","SEMAGESTAC","TPROBSON",
          "IDANOMAL")

# Inserir informações:
# Todos os anos a serem baixados
anos <- c(2023:2025)
# Ano baixado pelo opendatasus
ano_opendatasus <- 2025
# Links para o ano baixado pelo opendatasus
link_opendatasus_sinasc <- "https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SINASC/csv/SINASC_2025_csv.zip"

codigos_municipios <- read.csv("data-raw/extracao-dos-dados/blocos/databases_auxiliares/tabela_aux_municipios.csv") |>
  pull(codmunres) |>
  as.character()

## Criando data.frames que irão receber os dados dos indicadores de causas evitáveis e grupos de causa
df_sinasc_incompletude_aux <- data.frame(codmunres = rep(codigos_municipios, each = length(anos)), ano = anos)

# Baixando os dados do SINASC (com paralelização) -------------------------
## Criando o planejamento dos futures
plan(multisession)
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
    if (ano == ano_opendatasus) {
      switch(sistema,
             "SINASC" = {
               fread_retry(link_opendatasus_sinasc, sep = ";") |>
                 mutate(ano = extrai_ano(.data[[data_col]])) |>
                 select(c("CODMUNRES", "ano", data_col, all_of(vars)))
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
  clean_names()


# Fazendo o cálculo da incompletude para cada variável
df_sinasc_incompletude_calculo <- df_sinasc |>
  mutate(
    consprenat_incompletos = case_when(
      is.na(consprenat) ~ 1,
      TRUE ~ 0
    ),
    consprenat_totais = 1,
    escmae_incompletos = case_when(
      is.na(escmae) | as.numeric(escmae) == 9 ~ 1,
      TRUE ~ 0
    ),
    escmae_totais = 1,
    gestacao_incompletos = case_when(
      is.na(gestacao) | as.numeric(gestacao) == 9 ~ 1,
      TRUE ~ 0
    ),
    gestacao_totais = 1,
    idademae_incompletos = case_when(
      is.na(idademae) | as.numeric(idademae) >55 ~1,
      TRUE ~ 0
    ),
    idademae_totais = 1,
    idanomal_incompletos = case_when(
      is.na(idanomal) | as.numeric(idanomal) == 9 ~ 1,
      TRUE ~ 0
    ),
    idanomal_totais = 1,
    mesprenat_incompletos = case_when(
      is.na(mesprenat) ~ 1,
      TRUE ~ 0
    ),
    mesprenat_totais = 1,
    parto_incompletos = case_when(
      is.na(parto) | as.numeric(parto) == 9 ~ 1,
      TRUE ~ 0
    ),
    parto_totais = 1,
    parto_tprobson_incompletos = case_when(
      as.numeric(parto)  == 9 | as.numeric(tprobson) %in% c(11,12) | is.na(parto) | is.na(tprobson) ~ 1,
      TRUE ~ 0
    ),
    parto_tprobson_totais = 1,
    peso_incompletos = case_when(
      is.na(peso) ~ 1,
      TRUE ~ 0
    ),
    peso_totais = 1,
    qtdpartces_incompletos = case_when(
      is.na(qtdpartces) | as.numeric(qtdpartces) == 99 ~ 1,
      TRUE ~ 0
    ),
    qtdpartces_totais = 1,
    qtdpartnor_incompletos = case_when(
      is.na(qtdpartnor) | as.numeric(qtdpartnor) == 99 ~ 1,
      TRUE ~ 0
    ),
    qtdpartnor_totais = 1,
    racacormae_incompletos = case_when(
      is.na(racacormae) | is.na(racacormae) == 9 ~ 1,
      TRUE ~ 0
    ),
    racacormae_totais = 1,
    semagestac_incompletos = case_when(
      is.na(semagestac) ~ 1,
      TRUE ~ 0
    ),
    semagestac_totais = 1,
    tprobson_incompletos = case_when(
      as.numeric(tprobson) %in% c(11,12) | is.na(tprobson) ~ 1,
      TRUE ~ 0
    ),
    tprobson_totais = 1
  ) |>
  group_by(ano, codmunres) |>
  summarise(
    consprenat_incompletos = sum(consprenat_incompletos),
    consprenat_totais = sum(consprenat_totais),
    escmae_incompletos = sum(escmae_incompletos),
    escmae_totais = sum(escmae_totais),
    gestacao_incompletos = sum(gestacao_incompletos),
    gestacao_totais = sum(gestacao_totais),
    idademae_incompletos = sum(idademae_incompletos),
    idademae_totais = sum(idademae_totais),
    idanomal_incompletos = sum(idanomal_incompletos),
    idanomal_totais = sum(idanomal_totais),
    mesprenat_incompletos = sum(mesprenat_incompletos),
    mesprenat_totais = sum(mesprenat_totais),
    parto_incompletos = sum(parto_incompletos),
    parto_totais = sum(parto_totais),
    parto_tprobson_incompletos = sum(parto_tprobson_incompletos),
    parto_tprobson_totais = sum(parto_tprobson_totais),
    peso_incompletos = sum(peso_incompletos),
    peso_totais = sum(peso_totais),
    qtdpartces_incompletos = sum(qtdpartces_incompletos),
    qtdpartces_totais = sum(qtdpartces_totais),
    qtdpartnor_incompletos = sum(qtdpartnor_incompletos),
    qtdpartnor_totais = sum(qtdpartnor),
    racacormae_incompletos = sum(racacormae_incompletos),
    racacormae_totais = sum(racacormae_totais),
    semagestac_incompletos = sum(semagestac_incompletos),
    semagestac_totais = sum(semagestac_totais),
    tprobson_incompletos = sum(tprobson_incompletos),
    tprobson_totais = sum(tprobson_totais)
  )

df_sinasc_incompletude <- left_join(df_sinasc_incompletude_aux, df_sinasc_incompletude_calculo)

write.csv(df_sinasc_incompletude, "data-raw/csv/incompletude_variaveis_sinasc_2023_2025.csv")

