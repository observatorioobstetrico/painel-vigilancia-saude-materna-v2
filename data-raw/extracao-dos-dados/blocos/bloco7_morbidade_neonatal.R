library(microdatasus)
library(dplyr)
library(janitor)
library(data.table)
library(stringr)
library(tidyr)
library(data.table)
library(future)
library(future.apply)
library(readxl)

# Baixando os dados do SINASC (com paralelização) -------------------------
## Criando o planejamento dos futures
plan(multisession)

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
    if (ano == 2024) {
      switch(sistema,
             "SIM-DOINF" = {
               fread_retry("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SIM/DO24OPEN.csv") |>
                 mutate(ano = extrai_ano(.data[[data_col]])) |>
                 pre_processa_doinf()
             },
             "SINASC" = {
               fread_retry("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SINASC/csv/SINASC_2024.csv", sep = ";") |>
                 mutate(ano = extrai_ano(.data[[data_col]])) |>
                 select(CODMUNRES, ano, PESO)
             }
      )
    } else {
      # Anos consolidados via microdatasus
      dados <- fetch_datasus(
        year_start = ano,
        year_end = ano,
        information_system = sistema,
        vars = vars
      ) |>
        mutate(ano = extrai_ano(.data[[data_col]]))

      if (sistema == "SINASC") {
        dados <- dados |> select(CODMUNRES, ano, PESO)
      } else if (sistema == "SIM-DOINF") {
        dados <- pre_processa_doinf(dados)
      }

      dados
    }
  }

  message("Processando ano ", ano)

  list(
    sinasc = baixa_dados(ano, "SINASC", "DTNASC", vars = c("CODMUNRES", "DTNASC", "PESO", "GESTACAO", "SEMAGESTAC", "APGAR5"))
  )
}

## Criando um vetor com os anos a serem baixados
anos <- 2012:2024

## Baixando todos os dados
resultados <- future_lapply(anos, processa_ano)

## Separando-os em objetos diferentes
df_sinasc <- rbindlist(lapply(resultados, `[[`, "sinasc"), fill = TRUE)

## Criando uma função para salvar os arquivos
salva_csv_gz <- function(df, nome) {
  path <- paste0("data-raw/extracao-dos-dados/blocos/databases_auxiliares/", nome, "_2012_2024.csv.gz")
  write.csv(df, gzfile(path), row.names = FALSE)
}

## Salvando os arquivos
salva_csv_gz(df_sinasc, "df_sinasc")

# Para os indicadores de nascidos vivos com condições ameaçadoras à vida -----
## Lendo o arquivo com os nascimentos no período de 2012-2024
df_sinasc <- fread("data-raw/extracao-dos-dados/blocos/databases_auxiliares/df_sinasc_2012_2024.csv.gz") |>
  mutate(CODMUNRES = as.character(CODMUNRES))

## Criando um objeto que recebe os códigos dos municípios que utilizamos no painel
codigos_municipios <- read.csv("data-raw/extracao-dos-dados/blocos/databases_auxiliares/tabela_aux_municipios.csv") |>
  pull(codmunres) |>
  as.character()

## Criando um data.frame auxiliar que possui uma linha para cada combinação de município e ano
df_aux_municipios <- data.frame(codmunres = rep(codigos_municipios, each = length(2012:2024)), ano = 2012:2024)

## Criando variáveis necessárias
df_indicadores_sinasc <- df_sinasc |>
  clean_names() |>
  mutate(
    # Garantindo que as variáveis são do tipo correto
    peso = as.numeric(peso)
  ) |>
  mutate(
    total_de_nascidos_vivos = 1,
    nv_peso_menos_1000 = if_else(
      peso < 1000, 1, 0, missing = 0
    ),
    nv_peso_1000_1499 = if_else(
      (peso >= 1000 & peso < 1500), 1, 0, missing = 0
    ),
    nv_peso_1500_2499 = if_else(
      (peso >= 1500 & peso < 2500), 1, 0, missing = 0
    ),
    nv_peso_2500_mais = if_else(
      peso >= 2500, 1, 0, missing = 0
    ),
    nv_peso_sem_info = if_else(
      is.na(peso), 1, 0, missing = 0
    )
  ) |>
  group_by(codmunres, ano) |>
  summarise_at(vars(starts_with("total_") | starts_with("nv")), sum) |>
  ungroup()

## Para os dados do SIM, criando variáveis de óbitos neonatais por faixa de peso e momento do óbito
df_nasc_ameacadoras <- df_sinasc |>
  clean_names() |>
  mutate(
    peso = as.numeric(peso),
    gestacao = as.numeric(gestacao),
    semagestac = as.numeric(semagestac),
    apgar5 = as.numeric(apgar5),
    .keep = "unused",
  ) |>
  mutate(
    total_de_nascidos_vivos = 1,
    nascidos_condicoes_ameacadoras = if_else(peso < 1500 | (gestacao < 4 | semagestac < 32) | apgar5 < 7, 1, 0, missing = 0),
    .keep = "unused"
  ) |>
  group_by(codmunres, ano) |>
  summarise_at(vars(starts_with("nascidos") | starts_with("total")), sum) |>
  ungroup() |>
  # Juntando com a base auxiliar de municípios
  right_join(df_aux_municipios, by = join_by(codmunres, ano)) |>
  mutate(across(everything(), ~replace_na(., 0))) |>
  dplyr::select(
    codmunres,
    ano,
    total_de_nascidos_vivos,
    starts_with("nascidos"),
  ) |>
  arrange(codmunres, ano)


# Para os indicadores provenientes do SIH ---------------------------------
## Criando um vetor com os anos considerados
anos <- c(2012:2024)

## Criando um vetor com as siglas de todos os estados do Brasil
estados <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA",
  "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", "RO",
  "RR", "SC", "SP", "SE", "TO"
)

procedimentos_parto <- c("0310010012", "0310010039", "0310010047",
                         "0310010055", "0411010026", "0411010034",
                         "0411010042")

## Baixando os dados do SIH-RD
for (estado in estados) {
  # Criando data.frames que guardarão as bases do estado
  df_sih_rd_menores_28_uf <- data.frame()
  df_sih_rd_partos_uf <- data.frame()

  for (ano in anos) {

    erro_rd <- TRUE
    while (erro_rd) {
      erro_rd <- tryCatch({
        # Baixando os dados do SIH-RD para o dado ano e UF
        df_sih_rd_aux <- fetch_datasus(
          year_start = ano,
          year_end = ano,
          uf = estado,
          month_start = 1,
          month_end = 12,
          information_system = "SIH-RD",
          timeout = 500,
          stop_on_error = TRUE,
          vars = c(
            "CNES", "CEP", "MUNIC_RES", "MUNIC_MOV", "ANO_CMPT", "COD_IDADE", "IDADE", "NASC",
            "DT_INTER", "DT_SAIDA", "COBRANCA", "N_AIH", "DIAG_PRINC", "PROC_REA",
            "US_TOT", "UTI_MES_TO"
          )
        )

        # Criando um data.frame que contém apenas as internações de menores de 28 dias
        df_sih_rd_aux_menores_28 <- df_sih_rd_aux |>
          mutate(idade_dias = as.numeric(as.Date(DT_INTER, format = "%Y%m%d") - as.Date(NASC, format = "%Y%m%d"))) |>
          dplyr::filter(
            idade_dias < 28
          )

        # Criando um data.frame que contém apenas os partos
        df_sih_rd_aux_partos <- df_sih_rd_aux |>
          dplyr::filter(
            ((DIAG_PRINC >= "O32" & DIAG_PRINC <= "O36") | (DIAG_PRINC >= "O60" & DIAG_PRINC <= "O69") |
               (DIAG_PRINC >= "O75" & DIAG_PRINC < "O76") | (DIAG_PRINC >= "O80" & DIAG_PRINC <= "O84") |
               DIAG_PRINC == "P95") | (PROC_REA %in% procedimentos_parto)
          )

        erro_rd <- FALSE
      },
      warning = function(cond) return(TRUE)
      )
    }

    # Juntando com os dados dos anos anteriores para a dada UF
    df_sih_rd_menores_28_uf <- bind_rows(df_sih_rd_menores_28_uf, df_sih_rd_aux_menores_28)
    df_sih_rd_partos_uf <- bind_rows(df_sih_rd_partos_uf, df_sih_rd_aux_partos)


    # Limpando a memória
    rm(df_sih_rd_aux_menores_28,
       df_sih_rd_aux_partos)
    gc()
  }

  # Salvando as bases da UF
  write.csv2(
    df_sih_rd_menores_28_uf,
    gzfile(glue("data-raw/extracao-dos-dados/blocos/databases_auxiliares/internacoes_menores_28_dias/SIH/{estado}_sih_rd_menores_28_dias_{anos[1]}_{anos[length(anos)]}.csv.gz")),
    row.names = FALSE
  )

  write.csv2(
    df_sih_rd_partos_uf,
    gzfile(glue("data-raw/extracao-dos-dados/blocos/databases_auxiliares/internacoes_menores_28_dias/SIH/{estado}_sih_rd_partos_{anos[1]}_{anos[length(anos)]}.csv.gz")),
    row.names = FALSE
  )

  # Limpando a memória
  rm(df_sih_rd_uf)
  gc()
}

### Criando os data.frames que guardarão as bases finais
df_sih_rd_menores_28 <- data.frame()
df_sih_rd_partos <- data.frame()

for (estado in estados) {
  df_sih_rd_menores_28_aux <- fread(
    glue("data-raw/extracao-dos-dados/blocos/databases_auxiliares/internacoes_menores_28_dias/SIH/{estado}_sih_rd_menores_28_dias_2024_2024.csv.gz"),
    sep = ";"
  )
  df_sih_rd_menores_28 <- bind_rows(df_sih_rd_menores_28, df_sih_rd_menores_28_aux)

  rm(df_sih_rd_menores_28_aux)
  gc()
}

for (estado in estados) {
  df_sih_rd_partos_aux <- fread(
    glue("data-raw/extracao-dos-dados/blocos/databases_auxiliares/internacoes_menores_28_dias/SIH/{estado}_sih_rd_partos_2024_2024.csv.gz"),
    sep = ";"
  )
  df_sih_rd_partos <- bind_rows(df_sih_rd_partos, df_sih_rd_partos_aux)

  rm(df_sih_rd_partos_aux)
  gc()
}

## Salvando as bases completas
write.csv2(
  df_sih_rd_menores_28,
  glue("data-raw/extracao-dos-dados/blocos/databases_auxiliares/internacoes_menores_28_dias/SIH/BR_sih_rd_menores_28_dias_{anos[1]}_{anos[length(anos)]}.csv"),
  row.names = FALSE
)

write.csv2(
  df_sih_rd_partos,
  glue("data-raw/extracao-dos-dados/blocos/databases_auxiliares/internacoes_menores_28_dias/SIH/BR_sih_rd_partos_{anos[1]}_{anos[length(anos)]}.csv"),
  row.names = FALSE
)


## Para os numeradores dos indicadores (número de internações/internações em UTI em menores de 28 dias) ----
### Lendo uma base com informações auxiliares dos municípios
df_infos_municipios <- read.csv("data-raw/extracao-dos-dados/blocos/databases_auxiliares/df_aux_municipios.csv") |>
  mutate_if(is.numeric, as.character)

### Rodando o algoritmo da Claudia na base completa de internações em menores de 28 dias
#### Criando um vetor que contém o diretório original do projeto
diretorio_original <- getwd()

#### Criando um vetor que contém o diretório das bases brutas do SIH-RD
diretorio_bases_brutas <- glue("{getwd()}/data-raw/extracao-dos-dados/blocos/databases_auxiliares/internacoes_menores_28_dias/SIH")

#### Mudando o diretório para a pasta que contém o algoritmo em C++
setwd("data-raw/extracao-dos-dados/blocos/databases_auxiliares/internacoes_menores_28_dias/algorithm_episode_of_care/")

#### Rodando o algoritmo em C++ na base de internações
system(glue("./processaih {diretorio_bases_brutas}/BR_sih_rd_menores_28_dias_2024_2024.csv"))

#### Voltando para o diretório original do projeto
setwd(diretorio_original)

#### Criando a conexão com o arquivo .sqlite gerado como resultado do algoritmo em C++
con <- dbConnect(SQLite(), "data-raw/extracao-dos-dados/blocos/databases_auxiliares/internacoes_menores_28_dias/algorithm_episode_of_care/work.sqlite")

#### Selecionando a tabela "aih" com todas as suas variáveis, ordenadas por AIHREF e DT_INTER
df_aih_internacoes_aux <- dbGetQuery(con, "select * from aih order by AIHREF, DT_INTER")
dbDisconnect(con)

### Adicionando variáveis que estão no SIH-RD, mas que não são devolvidas na base gerada pelo algoritmo
df_aih_internacoes <- left_join(
  df_aih_internacoes_aux,
  df_sih_rd_menores_28 |>
    select(ANO_CMPT, DT_INTER, DT_SAIDA, N_AIH, MUNIC_MOV, idade_dias) |>
    mutate_at(vars(c(DT_INTER, DT_SAIDA, N_AIH, MUNIC_MOV)), as.character)
)

### Passando os casos para o formato wide (cada linha corresponderá a uma pessoa única)
df_aih_internacoes_wide <- df_aih_internacoes |>
  mutate(
    DT_INTER = as.Date(DT_INTER, format = "%Y%m%d"),
    DT_SAIDA = as.Date(DT_SAIDA, format = "%Y%m%d"),
    NASC = as.Date(NASC, format = "%Y%m%d")
  ) |>
  group_by(AIHREF) |>  # Agrupando pelo N_AIH comum para todas as linhas de um episódio de cuidado completo
  summarise(
    ANO_CMPT = last(ANO_CMPT),  # Ano de processamento do SIH da última internação
    CNES = first(CNES),  # CNES do estabelecimento da primeira internação
    MUNIC_RES = first(MUNIC_RES),  # Município de residência da primeira internação
    MUNIC_MOV = first(MUNIC_MOV),  # Município do estabelecimento da primeira internação
    idade_dias = first(idade_dias),  # Idade, em dias, na data da primeira internação
    SOMA_UTI = sum(as.integer(UTI_MES_TO)),  # Total de dias na UTI
    PDIAG = first(DIAG_PRINC),  # Diagnóstico principal da primeira internação
    PPROC = first(PROC_REA)
  ) |>
  ungroup() |>
  select(ano = ANO_CMPT, codmunres = MUNIC_RES, causabas = PDIAG, codmunocor = MUNIC_MOV, cnes = CNES, aihref = AIHREF, idade_dias, soma_uti_mes_to = SOMA_UTI) |>
  # Filtrando apenas pelos casos em que os municípios de residência e ocorrência são considerados no painel
  filter(codmunres %in% df_infos_municipios$codmunres & codmunocor %in% df_infos_municipios$codmunres)

### Adicionando as indicadores de internação em UTI e de internação na macrorregião de residência
df_aih_internacoes_wide_macros <- df_aih_internacoes_wide |>
  left_join(df_infos_municipios |> select(codmunres, macro_r_saude_res = macro_r_saude), by = join_by(codmunres == codmunres)) |>
  left_join(df_infos_municipios |> select(codmunres, macro_r_saude_ocor = macro_r_saude), by = join_by(codmunocor == codmunres)) |>
  mutate(
    idade_cat = case_when(
      idade_dias == 0 ~ "0_dias",
      idade_dias >= 1 & idade_dias < 7 ~ "1_a_6_dias",
      idade_dias >= 7 & idade_dias <= 27 ~ "7_a_27_dias",
      TRUE ~ NA_character_
    )        #ifelse(idade_dias < 7, "menores_7_dias", "7_a_27_dias"),
    ,
    indicadora_mesma_macro = ifelse(macro_r_saude_res == macro_r_saude_ocor, "na_macro", "fora_macro"),
    indicadora_uti = ifelse(soma_uti_mes_to > 0, "internado_uti", "nao_internado_uti")
  ) |>
  select(!c(cnes, codmunocor, aihref, idade_dias, soma_uti_mes_to, macro_r_saude_res, macro_r_saude_ocor))

### Passando a base para o formato wide (um município por linha) e criando as variáveis necessárias

df_aih_internacoes_wide_macros$codmunres <-as.numeric(df_aih_internacoes_wide_macros$codmunres)

df_bloco7_sih_internacoes <- df_aih_internacoes_wide_macros |>
  group_by_all() |>
  summarise(num_internacoes = n()) |>
  ungroup() |>
  pivot_wider(
    names_from = c(indicadora_mesma_macro, idade_cat, indicadora_uti),
    values_from = num_internacoes,
    values_fill = 0,
    names_prefix = "internacoes_"
  ) |>
  right_join(data.frame(codmunres = rep(as.numeric(df_infos_municipios$codmunres), each = length(2012:2024)), ano = 2012:2024)) |>
  arrange(codmunres, ano) |>
  mutate(across(.cols = -c(codmunres, ano, causabas), .fns = ~ replace_na(., 0))) |>
  rowwise() |>
  mutate(
    # Para o indicador de internações geral, os nomes das variáveis seguem o padrão "internacoes_local-do-parto_idade-do-bebe"
    internacoes_na_macro_7_a_27_dias = sum(c_across(contains("na_macro_7_a_27_dias"))),
    internacoes_fora_macro_7_a_27_dias = sum(c_across(contains("fora_macro_7_a_27_dias"))),
    internacoes_na_macro_1_a_6_dias = sum(c_across(contains("na_macro_1_a_6_dias"))),
    internacoes_fora_macro_1_a_6_dias = sum(c_across(contains("fora_macro_1_a_6_dias"))),
    internacoes_na_macro_0_dias = sum(c_across(contains("na_macro_0_dias"))),
    internacoes_fora_macro_0_dias = sum(c_across(contains("fora_macro_0_dias"))),
    internacoes_geral_7_a_27_dias = internacoes_na_macro_7_a_27_dias + internacoes_fora_macro_7_a_27_dias,
    internacoes_geral_1_a_6_dias = internacoes_na_macro_1_a_6_dias + internacoes_fora_macro_1_a_6_dias,
    internacoes_geral_0_dias = internacoes_na_macro_0_dias + internacoes_fora_macro_0_dias,
    internacoes_na_macro_geral = internacoes_na_macro_7_a_27_dias + internacoes_na_macro_1_a_6_dias + internacoes_na_macro_0_dias,
    internacoes_fora_macro_geral = internacoes_fora_macro_7_a_27_dias + internacoes_fora_macro_1_a_6_dias + internacoes_fora_macro_0_dias,
    internacoes_geral_geral = internacoes_geral_7_a_27_dias + internacoes_geral_1_a_6_dias + internacoes_geral_0_dias,
    # Para o indicador de internações em UTI, os nomes das variáveis seguem o padrão "internacoes_local-do-parto_idade-do-bebe_internado_uti"
    internacoes_na_macro_geral_internado_uti = sum(c_across(contains("na_macro") & contains("dias_internado"))),
    internacoes_fora_macro_geral_internado_uti = sum(c_across(contains("fora_macro") & contains("dias_internado"))),
    internacoes_geral_7_a_27_dias_internado_uti = sum(c_across(contains("7_a_27_dias_internado"))),
    internacoes_geral_1_a_6_dias_internado_uti = sum(c_across(contains("1_a_6_dias_internado"))),
    internacoes_geral_0_dias_internado_uti = sum(c_across(contains("0_dias_internado"))),
    internacoes_geral_geral_internado_uti = internacoes_geral_7_a_27_dias_internado_uti + internacoes_geral_1_a_6_dias_internado_uti + internacoes_geral_0_dias_internado_uti
  ) |>
  select(ano, codmunres, ends_with("_6_dias"), ends_with("_27_dias"), ends_with("_0_dias"), ends_with("_geral"), ends_with("internado_uti") & !ends_with("nao_internado_uti"))

### Verificando se o total de internações equivale ao número de linhas das bases df_aih_wide/df_aih_wide_macros
sum(df_bloco7_sih_internacoes$internacoes_geral_geral) == nrow(df_aih_internacoes_wide_macros)

sum(df_bloco7_sih_internacoes$internacoes_geral_geral_internado_uti) == nrow(df_aih_internacoes_wide[df_aih_internacoes_wide$soma_uti_mes_to > 0, ])

sum(df_bloco7_sih_internacoes$internacoes_na_macro_geral_internado_uti, df_bloco7_sih_internacoes$internacoes_fora_macro_geral_internado_uti) ==
  sum(df_bloco7_sih_internacoes$internacoes_geral_7_a_27_dias_internado_uti, df_bloco7_sih_internacoes$internacoes_geral_1_a_6_dias_internado_uti, df_bloco7_sih_internacoes$internacoes_geral_0_dias_internado_uti)

sum(df_bloco7_sih_internacoes$internacoes_geral_7_a_27_dias_internado_uti, df_bloco7_sih_internacoes$internacoes_geral_1_a_6_dias_internado_uti, df_bloco7_sih_internacoes$internacoes_geral_0_dias_internado_uti) ==
  sum(df_bloco7_sih_internacoes$internacoes_geral_geral_internado_uti)


## Para o denominador dos indicadores (total de partos públicos) -----------
### Rodando o algoritmo da Claudia na base completa de partos
#### Mudando o diretório para a pasta que contém o algoritmo em C++
setwd("data-raw/extracao-dos-dados/blocos/databases_auxiliares/internacoes_menores_28_dias/algorithm_episode_of_care/")

#### Rodando o algoritmo em C++ na base de partos
system(glue("./processaih {diretorio_bases_brutas}/BR_sih_rd_partos_2024_2024.csv"))

#### Voltando para o diretório original do projeto
setwd(diretorio_original)

#### Criando a conexão com o arquivo .sqlite gerado como resultado do algoritmo em C++
con <- dbConnect(SQLite(), "data-raw/extracao-dos-dados/blocos/databases_auxiliares/internacoes_menores_28_dias/algorithm_episode_of_care/work.sqlite")

#### Selecionando a tabela "aih" com todas as suas variáveis, ordenadas por AIHREF e DT_INTER
df_aih_partos_aux <- dbGetQuery(con, "select * from aih order by AIHREF, DT_INTER")
dbDisconnect(con)

### Adicionando variáveis que estão no SIH-RD, mas que não são devolvidas na base gerada pelo algoritmo
df_aih_partos <- left_join(
  df_aih_partos_aux,
  df_sih_rd_partos |>
    select(ANO_CMPT, DT_INTER, DT_SAIDA, N_AIH) |>
    mutate_at(vars(c(DT_INTER, DT_SAIDA, N_AIH)), as.character)
)

### Passando as casos para o formato wide (cada linha corresponderá a uma pessoa única)
df_bloco7_sih_partos <- df_aih_partos |>
  group_by(AIHREF) |>  # Agrupando pelo N_AIH comum para todas as linhas de um episódio de cuidado completo
  summarise(
    ANO_CMPT = last(ANO_CMPT),  # Ano de processamento do SIH da última internação
    MUNIC_RES = first(MUNIC_RES),  # Município de residência da primeira internação
  ) |>
  ungroup() |>
  select(ano = ANO_CMPT, codmunres = MUNIC_RES, aihref = AIHREF) |>
  filter(codmunres %in% df_infos_municipios$codmunres) |>
  mutate(nascidos_estabelecimentos_publicos_sih = 1) |>
  clean_names() |>
  group_by(codmunres, ano) |>
  # Criando a variável que contém o número nascimentos em hospitais públicos em cada município
  summarise(nascidos_estabelecimentos_publicos_sih = sum(nascidos_estabelecimentos_publicos_sih)) |>
  ungroup()

### Removendo arquivos já utilizados e que são maiores que o limite de 100 mb
file.remove(c(
  "data-raw/extracao-dos-dados/blocos/databases_auxiliares/internacoes_menores_28_dias/SIH/BR_sih_rd_menores_28_dias_2024_2024.csv",
  "data-raw/extracao-dos-dados/blocos/databases_auxiliares/internacoes_menores_28_dias/SIH/BR_sih_rd_partos_2024_2024.csv",
  "data-raw/extracao-dos-dados/blocos/databases_auxiliares/internacoes_menores_28_dias/algorithm_episode_of_care/aihperm.csv",
  "data-raw/extracao-dos-dados/blocos/databases_auxiliares/internacoes_menores_28_dias/algorithm_episode_of_care/aihpermtransf.csv",
  "data-raw/extracao-dos-dados/blocos/databases_auxiliares/internacoes_menores_28_dias/algorithm_episode_of_care/work.sqlite"

))

## Criando um objeto que recebe os códigos dos municípios que utilizamos no painel
codigos_municipios <- read.csv("data-raw/extracao-dos-dados/blocos/databases_auxiliares/tabela_aux_municipios.csv") |>
  pull(codmunres) |>
  as.character()

## Criando um data.frame auxiliar que possui uma linha para cada combinação de município e ano
df_aux_municipios <- data.frame(codmunres = rep(codigos_municipios, each = length(2012:2024)), ano = 2012:2024)

df_bloco7_sih_partos$codmunres <- as.character(df_bloco7_sih_partos$codmunres)
df_bloco7_sih_internacoes$codmunres <- as.character(df_bloco7_sih_internacoes$codmunres)
df_nasc_ameacadoras$codmunres <- as.character(df_nasc_ameacadoras$codmunres)

df_bloco7_morbidade_neonatal <- left_join(df_aux_municipios, df_nasc_ameacadoras, by = c("codmunres", "ano")) |>
  left_join(df_bloco7_sih_partos) |>
  left_join(df_bloco7_sih_internacoes)

## Preenchendo os valores NAs, gerados após o left_join, com 0 (MENOS PARA 2023 PARA AS COLUNAS QUE VEM DO SIH, QUE AINDA NÃO FORAM ATUALIZADAS)
internacoes_cols <- grep("^internacoes|sih$", names(df_bloco7_morbidade_neonatal), value = TRUE)

for (col in names(df_bloco7_morbidade_neonatal)) {
  if (col %in% internacoes_cols) {
    # Nas colunas que começam com "internacoes" ou que terminam com "sih", substituir os NAs por 0 apenas se o ano não for 2023
    df_bloco7_morbidade_neonatal[[col]][is.na(df_bloco7_morbidade_neonatal[[col]])] <- 0 #& df_bloco7_morbidade_neonatal$ano != 2023
  } else {
    # Nas outras colunas, substituir todos os NAs por 0
    df_bloco7_morbidade_neonatal[[col]][is.na(df_bloco7_morbidade_neonatal[[col]])] <- 0
  }
}

write.csv(df_bloco7_morbidade_neonatal, 'data-raw/csv/indicadores_bloco7_morbidade_neonatal_2012-2024.csv', sep = ",", dec = ".", row.names = FALSE)


############ Dados para a distribuição de internações neonatais
cids_internacoes_neonatais <- read_excel("data-raw/extracao-dos-dados/blocos/databases_auxiliares/cids_internacoes_neonatais.xlsx") |>
  select(causabas = `CID`,
         grupos = `CONSIDERAR ESTA  COLUNA -REVISÃO COM CINTIA E TATIANE EM 1/10/2024 Grupo da rede interagencial moficado para causa de internação neonatal (fluxograma na aba 'orientacoes')`)

for(i in unique(cids_internacoes_neonatais$grupos)){
  nome_variavel <- tolower(i)
  nome_variavel <- gsub(" ", "_", nome_variavel)
  assign(nome_variavel, filter(cids_internacoes_neonatais, grupos == i)$causabas)
}

df_internacoes_neonatais_totais <- df_aih_internacoes_wide |>
  mutate(
    causabas = causabas,
    causabas2 = substr(causabas, 1 , 3)
  ) |>
  filter(!(causabas %in% excluir | causabas2 %in% excluir)) |>
  select(codmunres, ano) |>
  mutate(internacoes_neonatais_totais = 1) |>
  group_by(across(!internacoes_neonatais_totais)) |>
  summarise(internacoes_neonatais_totais = sum(internacoes_neonatais_totais)) |>
  ungroup()


internacoes_neonatais_grupos <- df_aih_internacoes_wide |>
  mutate(
    causabas = causabas,
    causabas2 = substr(causabas, 1 , 3)
  ) |>
  mutate(
    grupo_cid = case_when(
      causabas %in% infecções | causabas2 %in% infecções | causabas %in% infecção | causabas2 %in% infecção ~ "morbidade_neonatal_grupos_infeccoes",
      causabas %in% `afecções_respiratórias_do_recém-nascido` | causabas2 %in% `afecções_respiratórias_do_recém-nascido` | causabas %in% `afecções_respiratórias_dos_recém-nascidos` | causabas2 %in% `afecções_respiratórias_do_recém-nascido` ~ "morbidade_neonatal_grupos_afeccoes_respiratorias",
      causabas %in% fatores_maternos_relacionados_à_gravidez | causabas2 %in% fatores_maternos_relacionados_à_gravidez~ "morbidade_neonatal_grupos_fatores_maternos",
      causabas %in% `asfixia_/_hipóxia` | causabas2 %in% `asfixia_/_hipóxia` ~ "morbidade_neonatal_grupos_asfixia",
      causabas %in% prematuridade | causabas2 %in% prematuridade ~ "morbidade_neonatal_grupos_prematuridade",
      causabas %in% afecções_não_especificadas_do_período_perinatal | causabas %in% afecções_originais_no_período_perinatal| causabas %in% afecções_não_especificadas_originadas_no_período_perinatal |  causabas2 %in% afecções_não_especificadas_do_período_perinatal | causabas2 %in% afecções_originais_no_período_perinatal | causabas2 %in% causabas %in% afecções_não_especificadas_originadas_no_período_perinatal ~ "morbidade_neonatal_grupos_afeccoes_perinatal",
      causabas %in% transtornos_cardíacos_originados_no_período_perinatal | causabas2 %in% transtornos_cardíacos_originados_no_período_perinatal ~ "morbidade_neonatal_grupos_cardiacos_perinatal",
      causabas %in% icterícia_neonatal | causabas2 %in% icterícia_neonatal ~ "morbidade_neonatal_grupos_ictericia",
      causabas %in% `transtornos_endócrinos_e_metabólicos_transitórios_específicos_do_feto_e_do_recém-nascido`| causabas2 %in% `transtornos_endócrinos_e_metabólicos_transitórios_específicos_do_feto_e_do_recém-nascido` ~ "morbidade_neonatal_grupos_endocrinos",
      causabas %in% problemas_de_alimentação_do_rn | causabas2 %in% problemas_de_alimentação_do_rn ~ "morbidade_neonatal_grupos_alimentacao",
      causabas %in% má_formação_congênita | causabas2 %in% má_formação_congênita ~ "morbidade_neonatal_grupos_ma_formacao",
      causabas %in% mal_definidas | causabas2 %in% mal_definidas ~ "morbidade_neonatal_grupos_mal_definidas",
      TRUE ~ "morbidade_neonatal_grupos_outros"
    )
  ) |>
  select(codmunres, ano, grupo_cid) |>
  mutate(internacoes = 1) |>
  group_by(across(!internacoes)) |>
  summarise(internacoes = sum(internacoes)) |>
  ungroup() |>
  pivot_wider(
    names_from = grupo_cid,
    values_from = internacoes,
    values_fill = 0
  ) |>
  right_join(df_aux_municipios) |>
  arrange(codmunres)

internacoes_neonatais_grupos[is.na(internacoes_neonatais_grupos)] <- 0


internacoes_neonatais_grupos_0_dias <- df_aih_internacoes_wide|>
  filter(idade_dias == 0) |>
  mutate(
    causabas = causabas,
    causabas2 = substr(causabas, 1 , 3)
  ) |>
  mutate(
    grupo_cid = case_when(
      causabas %in% infecções | causabas2 %in% infecções | causabas %in% infecção | causabas2 %in% infecção ~ "morbidade_neonatal_grupos_0_dias_infeccoes",
      causabas %in% `afecções_respiratórias_do_recém-nascido` | causabas2 %in% `afecções_respiratórias_do_recém-nascido` | causabas %in% `afecções_respiratórias_dos_recém-nascidos` | causabas2 %in% `afecções_respiratórias_do_recém-nascido` ~ "morbidade_neonatal_grupos_0_dias_afeccoes_respiratorias",
      causabas %in% fatores_maternos_relacionados_à_gravidez | causabas2 %in% fatores_maternos_relacionados_à_gravidez~ "morbidade_neonatal_grupos_0_dias_fatores_maternos",
      causabas %in% `asfixia_/_hipóxia` | causabas2 %in% `asfixia_/_hipóxia` ~ "morbidade_neonatal_grupos_0_dias_asfixia",
      causabas %in% prematuridade | causabas2 %in% prematuridade ~ "morbidade_neonatal_grupos_0_dias_prematuridade",
      causabas %in% afecções_não_especificadas_do_período_perinatal | causabas %in% afecções_originais_no_período_perinatal| causabas %in% afecções_não_especificadas_originadas_no_período_perinatal |  causabas2 %in% afecções_não_especificadas_do_período_perinatal | causabas2 %in% afecções_originais_no_período_perinatal | causabas2 %in% causabas %in% afecções_não_especificadas_originadas_no_período_perinatal ~ "morbidade_neonatal_grupos_0_dias_afeccoes_perinatal",
      causabas %in% transtornos_cardíacos_originados_no_período_perinatal | causabas2 %in% transtornos_cardíacos_originados_no_período_perinatal ~ "morbidade_neonatal_grupos_0_dias_cardiacos_perinatal",
      causabas %in% icterícia_neonatal | causabas2 %in% icterícia_neonatal ~ "morbidade_neonatal_grupos_0_dias_ictericia",
      causabas %in% `transtornos_endócrinos_e_metabólicos_transitórios_específicos_do_feto_e_do_recém-nascido`| causabas2 %in% `transtornos_endócrinos_e_metabólicos_transitórios_específicos_do_feto_e_do_recém-nascido` ~ "morbidade_neonatal_grupos_0_dias_endocrinos",
      causabas %in% problemas_de_alimentação_do_rn | causabas2 %in% problemas_de_alimentação_do_rn ~ "morbidade_neonatal_grupos_0_dias_alimentacao",
      causabas %in% má_formação_congênita | causabas2 %in% má_formação_congênita ~ "morbidade_neonatal_grupos_0_dias_ma_formacao",
      causabas %in% mal_definidas | causabas2 %in% mal_definidas ~ "morbidade_neonatal_grupos_0_dias_mal_definidas",
      TRUE ~ "morbidade_neonatal_grupos_0_dias_outros"
    )
  ) |>
  select(codmunres, ano, grupo_cid) |>
  mutate(internacoes = 1) |>
  group_by(across(!internacoes)) |>
  summarise(internacoes = sum(internacoes)) |>
  ungroup() |>
  pivot_wider(
    names_from = grupo_cid,
    values_from = internacoes,
    values_fill = 0
  ) |>
  right_join(df_aux_municipios) |>
  arrange(codmunres)

internacoes_neonatais_grupos_0_dias[is.na(internacoes_neonatais_grupos_0_dias)] <- 0


internacoes_neonatais_grupos_7_27_dias <- df_aih_internacoes_wide |>
  filter(idade_dias >= 7 & idade_dias <= 27)|>
  mutate(
    causabas = causabas,
    causabas2 = substr(causabas, 1 , 3)
  ) |>
  mutate(
    grupo_cid = case_when(
      causabas %in% infecções | causabas2 %in% infecções | causabas %in% infecção | causabas2 %in% infecção ~ "morbidade_neonatal_grupos_7_27_dias_infeccoes",
      causabas %in% `afecções_respiratórias_do_recém-nascido` | causabas2 %in% `afecções_respiratórias_do_recém-nascido` | causabas %in% `afecções_respiratórias_dos_recém-nascidos` | causabas2 %in% `afecções_respiratórias_do_recém-nascido` ~ "morbidade_neonatal_grupos_7_27_dias_afeccoes_respiratorias",
      causabas %in% fatores_maternos_relacionados_à_gravidez | causabas2 %in% fatores_maternos_relacionados_à_gravidez~ "morbidade_neonatal_grupos_7_27_dias_fatores_maternos",
      causabas %in% `asfixia_/_hipóxia` | causabas2 %in% `asfixia_/_hipóxia` ~ "morbidade_neonatal_grupos_7_27_dias_asfixia",
      causabas %in% prematuridade | causabas2 %in% prematuridade ~ "morbidade_neonatal_grupos_7_27_dias_prematuridade",
      causabas %in% afecções_não_especificadas_do_período_perinatal | causabas %in% afecções_originais_no_período_perinatal| causabas %in% afecções_não_especificadas_originadas_no_período_perinatal |  causabas2 %in% afecções_não_especificadas_do_período_perinatal | causabas2 %in% afecções_originais_no_período_perinatal | causabas2 %in% causabas %in% afecções_não_especificadas_originadas_no_período_perinatal ~ "morbidade_neonatal_grupos_7_27_dias_afeccoes_perinatal",
      causabas %in% transtornos_cardíacos_originados_no_período_perinatal | causabas2 %in% transtornos_cardíacos_originados_no_período_perinatal ~ "morbidade_neonatal_grupos_7_27_dias_cardiacos_perinatal",
      causabas %in% icterícia_neonatal | causabas2 %in% icterícia_neonatal ~ "morbidade_neonatal_grupos_7_27_dias_ictericia",
      causabas %in% `transtornos_endócrinos_e_metabólicos_transitórios_específicos_do_feto_e_do_recém-nascido`| causabas2 %in% `transtornos_endócrinos_e_metabólicos_transitórios_específicos_do_feto_e_do_recém-nascido` ~ "morbidade_neonatal_grupos_7_27_dias_endocrinos",
      causabas %in% problemas_de_alimentação_do_rn | causabas2 %in% problemas_de_alimentação_do_rn ~ "morbidade_neonatal_grupos_7_27_dias_alimentacao",
      causabas %in% má_formação_congênita | causabas2 %in% má_formação_congênita ~ "morbidade_neonatal_grupos_7_27_dias_ma_formacao",
      causabas %in% mal_definidas | causabas2 %in% mal_definidas ~ "morbidade_neonatal_grupos_7_27_dias_mal_definidas",
      TRUE ~ "morbidade_neonatal_grupos_7_27_dias_outros"
    )
  ) |>
  select(codmunres, ano, grupo_cid) |>
  mutate(internacoes = 1) |>
  group_by(across(!internacoes)) |>
  summarise(internacoes = sum(internacoes)) |>
  ungroup() |>
  pivot_wider(
    names_from = grupo_cid,
    values_from = internacoes,
    values_fill = 0
  ) |>
  right_join(df_aux_municipios) |>
  arrange(codmunres)

internacoes_neonatais_grupos_7_27_dias[is.na(internacoes_neonatais_grupos_7_27_dias)] <- 0

internacoes_neonatais_grupos_1_6_dias <- df_aih_internacoes_wide |>
  filter(idade_dias >= 1 & idade_dias <= 6)|>
  mutate(
    causabas = causabas,
    causabas2 = substr(causabas, 1 , 3)
  ) |>
  mutate(
    grupo_cid = case_when(
      causabas %in% infecções | causabas2 %in% infecções | causabas %in% infecção | causabas2 %in% infecção ~ "morbidade_neonatal_grupos_1_6_dias_infeccoes",
      causabas %in% `afecções_respiratórias_do_recém-nascido` | causabas2 %in% `afecções_respiratórias_do_recém-nascido` | causabas %in% `afecções_respiratórias_dos_recém-nascidos` | causabas2 %in% `afecções_respiratórias_do_recém-nascido` ~ "morbidade_neonatal_grupos_1_6_dias_afeccoes_respiratorias",
      causabas %in% fatores_maternos_relacionados_à_gravidez | causabas2 %in% fatores_maternos_relacionados_à_gravidez~ "morbidade_neonatal_grupos_1_6_dias_fatores_maternos",
      causabas %in% `asfixia_/_hipóxia` | causabas2 %in% `asfixia_/_hipóxia` ~ "morbidade_neonatal_grupos_1_6_dias_asfixia",
      causabas %in% prematuridade | causabas2 %in% prematuridade ~ "morbidade_neonatal_grupos_1_6_dias_prematuridade",
      causabas %in% afecções_não_especificadas_do_período_perinatal | causabas %in% afecções_originais_no_período_perinatal| causabas %in% afecções_não_especificadas_originadas_no_período_perinatal |  causabas2 %in% afecções_não_especificadas_do_período_perinatal | causabas2 %in% afecções_originais_no_período_perinatal | causabas2 %in% causabas %in% afecções_não_especificadas_originadas_no_período_perinatal ~ "morbidade_neonatal_grupos_1_6_dias_afeccoes_perinatal",
      causabas %in% transtornos_cardíacos_originados_no_período_perinatal | causabas2 %in% transtornos_cardíacos_originados_no_período_perinatal ~ "morbidade_neonatal_grupos_1_6_dias_cardiacos_perinatal",
      causabas %in% icterícia_neonatal | causabas2 %in% icterícia_neonatal ~ "morbidade_neonatal_grupos_1_6_dias_ictericia",
      causabas %in% `transtornos_endócrinos_e_metabólicos_transitórios_específicos_do_feto_e_do_recém-nascido`| causabas2 %in% `transtornos_endócrinos_e_metabólicos_transitórios_específicos_do_feto_e_do_recém-nascido` ~ "morbidade_neonatal_grupos_1_6_dias_endocrinos",
      causabas %in% problemas_de_alimentação_do_rn | causabas2 %in% problemas_de_alimentação_do_rn ~ "morbidade_neonatal_grupos_1_6_dias_alimentacao",
      causabas %in% má_formação_congênita | causabas2 %in% má_formação_congênita ~ "morbidade_neonatal_grupos_1_6_dias_ma_formacao",
      causabas %in% mal_definidas | causabas2 %in% mal_definidas ~ "morbidade_neonatal_grupos_1_6_dias_mal_definidas",
      TRUE ~ "morbidade_neonatal_grupos_1_6_dias_outros"
    )
  ) |>
  select(codmunres, ano, grupo_cid) |>
  mutate(internacoes = 1) |>
  group_by(across(!internacoes)) |>
  summarise(internacoes = sum(internacoes)) |>
  ungroup() |>
  pivot_wider(
    names_from = grupo_cid,
    values_from = internacoes,
    values_fill = 0
  ) |>
  right_join(df_aux_municipios) |>
  arrange(codmunres)

internacoes_neonatais_grupos_1_6_dias[is.na(internacoes_neonatais_grupos_1_6_dias)] <- 0

############ Juntandos os dados para a aba de morbidade
df_distribuicao_morbidade <- left_join(internacoes_neonatais_grupos, internacoes_neonatais_grupos_0_dias, by=c("codmunres", "ano"))|>
  left_join(internacoes_neonatais_grupos_1_6_dias, by=c("codmunres", "ano")) |>
  left_join(internacoes_neonatais_grupos_7_27_dias, by=c("codmunres", "ano")) |>
  left_join(df_internacoes_neonatais_totais, by = c("codmunres", "ano"))

df_distribuicao_morbidade[is.na(df_distribuicao_morbidade)] <- 0

write.csv(df_distribuicao_morbidade, 'data-raw/csv/indicadores_bloco7_distribuicao_morbidade_neonatal_2012-2024.csv', sep = ",", dec = ".", row.names = FALSE)


