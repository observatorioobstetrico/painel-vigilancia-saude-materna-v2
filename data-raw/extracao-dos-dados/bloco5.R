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
codigos_municipios <- read.csv("data-raw/extracao-dos-dados/databases-antigas/tabela_aux_municipios.csv") |>
  pull(codmunres) |>
  as.character()

## Criando um data.frame auxiliar que possui uma linha para cada combinação de município e ano
df_aux_municipios <- data.frame(codmunres = rep(codigos_municipios, each = length(2012:2023)), ano = 2012:2023)


# Para os indicadores provenientes do SINASC ------------------------------
## Baixando os dados consolidados do SINASC de 2012 a 2022 e selecionando as variáveis de interesse
df_sinasc_consolidados <- fetch_datasus(
  year_start = 2012,
  year_end = 2022,
  vars = c("CODMUNRES", "CODMUNNASC", "CODESTAB", "DTNASC", "PESO", "GESTACAO", "SEMAGESTAC", "APGAR5", "IDANOMAL", "CODANOMAL"),
  information_system = "SINASC"
)

## Baixando os dados preliminares do SINASC de 2023 e selecionando as variáveis de interesse
df_sinasc_preliminares <- fread("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SINASC/DNOPEN23.csv", sep = ";") |>
  select(CODMUNRES, CODMUNNASC, CODESTAB, DTNASC, PESO, GESTACAO, SEMAGESTAC, APGAR5, IDANOMAL, CODANOMAL) |>
  mutate(IDANOMAL = as.character(IDANOMAL))

## Juntando os dados consolidados com os dados preliminares
df_sinasc <- full_join(df_sinasc_consolidados, df_sinasc_preliminares)

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


# Para os indicadores provenientes do SIH ---------------------------------
## Criando um vetor com os anos considerados
anos <- c(2012:2022)

## Criando um vetor com as siglas de todos os estados do Brasil
estados <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA",
             "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN",
             "RS", "RO", "RR", "SC", "SP", "SE", "TO")

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
    rm(df_sih_rd_aux_menores_28, df_sih_rd_aux_partos)
    gc()
  }

  # Salvando as bases da UF
  write.csv2(
    df_sih_rd_menores_28_uf,
    gzfile(glue("data-raw/extracao-dos-dados/databases-antigas/internacoes_menores_28_dias/SIH/{estado}_sih_rd_menores_28_dias_{anos[1]}_{anos[length(anos)]}.csv.gz")),
    row.names = FALSE
  )

  write.csv2(
    df_sih_rd_partos_uf,
    gzfile(glue("data-raw/extracao-dos-dados/databases-antigas/internacoes_menores_28_dias/SIH/{estado}_sih_rd_partos_{anos[1]}_{anos[length(anos)]}.csv.gz")),
    row.names = FALSE
  )

  # Limpando a memória
  rm(df_sih_rd_uf)
  gc()
}

## Criando os data.frames que guardarão as bases finais
df_sih_rd_menores_28 <- data.frame()
df_sih_rd_partos <- data.frame()

for (estado in estados) {
  df_sih_rd_menores_28_aux <- fread(
    glue("data-raw/extracao-dos-dados/databases-antigas/internacoes_menores_28_dias/SIH/{estado}_sih_rd_menores_28_dias_2012_2022.csv.gz"),
    sep = ";"
  )
  df_sih_rd_menores_28 <- bind_rows(df_sih_rd_menores_28, df_sih_rd_menores_28_aux)

  rm(df_sih_rd_menores_28_aux)
  gc()
}

for (estado in estados) {
  df_sih_rd_partos_aux <- fread(
    glue("data-raw/extracao-dos-dados/databases-antigas/internacoes_menores_28_dias/SIH/{estado}_sih_rd_partos_2012_2022.csv.gz"),
    sep = ";"
  )
  df_sih_rd_partos <- bind_rows(df_sih_rd_partos, df_sih_rd_partos_aux)

  rm(df_sih_rd_partos_aux)
  gc()
}

## Salvando as bases completas
write.csv2(
  df_sih_rd_menores_28,
  glue("data-raw/extracao-dos-dados/databases-antigas/internacoes_menores_28_dias/SIH/BR_sih_rd_menores_28_dias_{anos[1]}_{anos[length(anos)]}.csv"),
  row.names = FALSE
)

write.csv2(
  df_sih_rd_partos,
  glue("data-raw/extracao-dos-dados/databases-antigas/internacoes_menores_28_dias/SIH/BR_sih_rd_partos_{anos[1]}_{anos[length(anos)]}.csv"),
  row.names = FALSE
)


## Para os numeradores dos indicadores (número de internações/internações em UTI em menores de 28 dias) ----
### Lendo uma base com informações auxiliares dos municípios
df_infos_municipios <- read.csv("data-raw/extracao-dos-dados/databases-antigas/df_aux_municipios.csv") |>
  mutate_if(is.numeric, as.character)

### Rodando o algoritmo da Claudia na base completa de internações em menores de 28 dias
#### Criando um vetor que contém o diretório original do projeto
diretorio_original <- getwd()

#### Criando um vetor que contém o diretório das bases brutas do SIH-RD
diretorio_bases_brutas <- glue("{getwd()}/data-raw/extracao-dos-dados/databases-antigas/internacoes_menores_28_dias/SIH")

#### Mudando o diretório para a pasta que contém o algoritmo em C++
setwd("data-raw/extracao-dos-dados/databases-antigas/internacoes_menores_28_dias/algorithm_episode_of_care/")

#### Rodando o algoritmo em C++ na base de internações
system(glue("./processaih {diretorio_bases_brutas}/BR_sih_rd_menores_28_dias_2012_2022.csv"))

#### Voltando para o diretório original do projeto
setwd(diretorio_original)

#### Criando a conexão com o arquivo .sqlite gerado como resultado do algoritmo em C++
con <- dbConnect(SQLite(), "data-raw/extracao-dos-dados/databases-antigas/internacoes_menores_28_dias/algorithm_episode_of_care/work.sqlite")

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
    SOMA_UTI = sum(as.integer(UTI_MES_TO))  # Total de dias na UTI
  ) |>
  ungroup() |>
  select(ano = ANO_CMPT, codmunres = MUNIC_RES, codmunocor = MUNIC_MOV, cnes = CNES, aihref = AIHREF, idade_dias, soma_uti_mes_to = SOMA_UTI) |>
  # Filtrando apenas pelos casos em que os municípios de residência e ocorrência são considerados no painel
  filter(codmunres %in% df_infos_municipios$codmunres & codmunocor %in% df_infos_municipios$codmunres)

### Adicionando as indicadoras de internação em UTI e de internação na macrorregião de residência
df_aih_internacoes_wide_macros <- df_aih_internacoes_wide |>
  left_join(df_infos_municipios |> select(codmunres, macro_r_saude_res = macro_r_saude), by = join_by(codmunres == codmunres)) |>
  left_join(df_infos_municipios |> select(codmunres, macro_r_saude_ocor = macro_r_saude), by = join_by(codmunocor == codmunres)) |>
  mutate(
    idade_cat = ifelse(idade_dias < 7, "menores_7_dias", "7_a_27_dias"),
    indicadora_mesma_macro = ifelse(macro_r_saude_res == macro_r_saude_ocor, "na_macro", "fora_macro"),
    indicadora_uti = ifelse(soma_uti_mes_to > 0, "internado_uti", "nao_internado_uti")
  ) |>
  select(!c(cnes, codmunocor, aihref, idade_dias, soma_uti_mes_to, macro_r_saude_res, macro_r_saude_ocor))

### Passando a base para o formato wide (um município por linha) e criando as variáveis necessárias
df_bloco5_sih_internacoes <- df_aih_internacoes_wide_macros |>
  group_by_all() |>
  summarise(num_internacoes = n()) |>
  ungroup() |>
  pivot_wider(
    names_from = c(indicadora_mesma_macro, idade_cat, indicadora_uti),
    values_from = num_internacoes,
    values_fill = 0,
    names_prefix = "internacoes_"
  ) |>
  right_join(data.frame(codmunres = rep(df_infos_municipios$codmunres, each = length(2012:2022)), ano = 2012:2022)) |>
  arrange(codmunres, ano) |>
  mutate(across(.cols = -c(codmunres, ano), .fns = ~ replace_na(., 0))) |>
  rowwise() |>
  mutate(
    # Para o indicador de internações geral, os nomes das variáveis seguem o padrão "internacoes_local-do-parto_idade-do-bebe"
    internacoes_na_macro_7_a_27_dias = sum(c_across(contains("na_macro_7_a_27_dias"))),
    internacoes_fora_macro_7_a_27_dias = sum(c_across(contains("fora_macro_7_a_27_dias"))),
    internacoes_na_macro_menores_7_dias = sum(c_across(contains("na_macro_menores_7_dias"))),
    internacoes_fora_macro_menores_7_dias = sum(c_across(contains("fora_macro_menores_7_dias"))),
    internacoes_geral_7_a_27_dias = internacoes_na_macro_7_a_27_dias + internacoes_fora_macro_7_a_27_dias,
    internacoes_geral_menores_7_dias = internacoes_na_macro_menores_7_dias + internacoes_fora_macro_menores_7_dias,
    internacoes_na_macro_geral = internacoes_na_macro_7_a_27_dias + internacoes_na_macro_menores_7_dias,
    internacoes_fora_macro_geral = internacoes_fora_macro_7_a_27_dias + internacoes_fora_macro_menores_7_dias,
    internacoes_geral_geral = internacoes_geral_7_a_27_dias + internacoes_geral_menores_7_dias,
    # Para o indicador de internações em UTI, os nomes das variáveis seguem o padrão "internacoes_local-do-parto_idade-do-bebe_internado_uti"
    internacoes_na_macro_geral_internado_uti = sum(c_across(contains("na_macro") & contains("dias_internado"))),
    internacoes_fora_macro_geral_internado_uti = sum(c_across(contains("fora_macro") & contains("dias_internado"))),
    internacoes_geral_7_a_27_dias_internado_uti = sum(c_across(contains("7_a_27_dias_internado"))),
    internacoes_geral_menores_7_dias_internado_uti = sum(c_across(contains("menores_7_dias_internado"))),
    internacoes_geral_geral_internado_uti = internacoes_geral_7_a_27_dias_internado_uti + internacoes_geral_menores_7_dias_internado_uti
  ) |>
  select(ano, codmunres, ends_with("_7_dias"), ends_with("_27_dias"), ends_with("_geral"), ends_with("internado_uti") & !ends_with("nao_internado_uti"))

### Verificando se o total de internações equivale ao número de linhas das bases df_aih_wide/df_aih_wide_macros
sum(df_bloco5_sih_internacoes$internacoes_geral_geral) == nrow(df_aih_internacoes_wide_macros)

sum(df_bloco5_sih_internacoes$internacoes_geral_geral_internado_uti) == nrow(df_aih_internacoes_wide[df_aih_internacoes_wide$soma_uti_mes_to > 0, ])

sum(df_bloco5_sih_internacoes$internacoes_na_macro_geral_internado_uti, df_bloco5_sih_internacoes$internacoes_fora_macro_geral_internado_uti) ==
  sum(df_bloco5_sih_internacoes$internacoes_geral_7_a_27_dias_internado_uti, df_bloco5_sih_internacoes$internacoes_geral_menores_7_dias_internado_uti)

sum(df_bloco5_sih_internacoes$internacoes_geral_7_a_27_dias_internado_uti, df_bloco5_sih_internacoes$internacoes_geral_menores_7_dias_internado_uti) ==
  sum(df_bloco5_sih_internacoes$internacoes_geral_geral_internado_uti)


## Para o denominador dos indicadores (total de partos públicos) -----------
### Rodando o algoritmo da Claudia na base completa de partos
#### Mudando o diretório para a pasta que contém o algoritmo em C++
setwd("data-raw/extracao-dos-dados/databases-antigas/internacoes_menores_28_dias/algorithm_episode_of_care/")

#### Rodando o algoritmo em C++ na base de partos
system(glue("./processaih {diretorio_bases_brutas}/BR_sih_rd_partos_2012_2022.csv"))

#### Voltando para o diretório original do projeto
setwd(diretorio_original)

#### Criando a conexão com o arquivo .sqlite gerado como resultado do algoritmo em C++
con <- dbConnect(SQLite(), "data-raw/extracao-dos-dados/databases-antigas/internacoes_menores_28_dias/algorithm_episode_of_care/work.sqlite")

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
df_bloco5_sih_partos <- df_aih_partos |>
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
  "data-raw/extracao-dos-dados/databases-antigas/internacoes_menores_28_dias/SIH/BR_sih_rd_menores_28_dias_2012_2022.csv",
  "data-raw/extracao-dos-dados/databases-antigas/internacoes_menores_28_dias/SIH/BR_sih_rd_partos_2012_2022.csv",
  "data-raw/extracao-dos-dados/databases-antigas/internacoes_menores_28_dias/algorithm_episode_of_care/aihperm.csv",
  "data-raw/extracao-dos-dados/databases-antigas/internacoes_menores_28_dias/algorithm_episode_of_care/aihpermtransf.csv",
  "data-raw/extracao-dos-dados/databases-antigas/internacoes_menores_28_dias/algorithm_episode_of_care/work.sqlite",

))

# Juntando os dados do SINASC e do SIH ------------------------------------
## Fazendo um left_join de ambas as bases com a base auxiliar de municípios
df_bloco5_aux <- left_join(df_aux_municipios, df_bloco5_sinasc, by = c("codmunres", "ano")) |>
  left_join(df_bloco5_sih_partos) |>
  left_join(df_bloco5_sih_internacoes)

## Juntando, também, com os dados de asifixia e malformação, baixados no script 'bloco5_asfixia.R'
df_asfixia <- read.csv("data-raw/csv/asfixia_2012_2023.csv", sep = ';') |>
  janitor::clean_names() |>
  dplyr::arrange(codmunres, ano) |>
  dplyr::select(codmunres, ano, nascidos_vivos_asfixia1, total_nascidos) |>
  mutate(codmunres = as.character(codmunres))

df_malformacao <- read.csv("data-raw/csv/malformacao_2012_2023.csv", sep = ';') |>
  janitor::clean_names() |>
  dplyr::arrange(codmunres, ano) |>
  dplyr::group_by(ano, codmunres) |>
  dplyr::summarise(nascidos_vivos_anomalia = sum(nascidos_vivos_anomalia)) |>
  ungroup() |>
  mutate(codmunres = as.character(codmunres))

df_bloco5 <- left_join(df_bloco5_aux, df_asfixia, by = c("codmunres", "ano")) |>
  left_join(df_malformacao)

## Preenchendo os valores NAs, gerados após o left_join, com 0 (MENOS PARA 2023 PARA AS COLUNAS QUE VEM DO SIH, QUE AINDA NÃO FORAM ATUALIZADAS)
internacoes_cols <- grep("^internacoes|sih$", names(df_bloco5), value = TRUE)

for (col in names(df_bloco5)) {
  if (col %in% internacoes_cols) {
    # Nas colunas que começam com "internacoes" ou que terminam com "sih", substituir os NAs por 0 apenas se o ano não for 2023
    df_bloco5[[col]][is.na(df_bloco5[[col]]) & df_bloco5$ano != 2023] <- 0
  } else {
    # Nas outras colunas, substituir todos os NAs por 0
    df_bloco5[[col]][is.na(df_bloco5[[col]])] <- 0
  }
}

## Verificando se os dados novos e antigos estão batendo
df_bloco5_antigo <- read.csv("data-raw/extracao-dos-dados/databases-antigas/indicadores_bloco5_condicao_de_nascimento_2012-2020.csv") |>
  clean_names()

sum(df_bloco5 |> filter(ano <= 2020) |> pull(total_de_nascidos_vivos)) - sum(df_bloco5_antigo$total_de_nascidos_vivos)
sum(df_bloco5 |> filter(ano <= 2020) |> pull(nascidos_vivos_com_baixo_peso)) - sum(df_bloco5_antigo$nascidos_vivos_com_baixo_peso)
sum(df_bloco5 |> filter(ano <= 2020) |> pull(nascidos_vivos_prematuros)) - sum(df_bloco5_antigo$nascidos_vivos_prematuros)
sum(df_bloco5 |> filter(ano <= 2020) |> pull(nascidos_vivos_termo_precoce)) - sum(df_bloco5_antigo$nascidos_vivos_termo_precoce)

## Exportando os dados
write.csv(df_bloco5, "data-raw/csv/indicadores_bloco5_condicao_de_nascimento_2012_2023.csv", row.names = FALSE)
