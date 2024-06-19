library(microdatasus)
library(dplyr)
library(janitor)
library(RSQLite)
library(glue)
library(tidyr)
library(data.table)
library(readr)

# Criando um objeto que recebe os códigos dos municípios que utilizamos no painel
codigos_municipios <- read.csv("data-raw/extracao-dos-dados/databases-antigas/tabela_aux_municipios.csv") |>
  pull(codmunres) |>
  as.character()

# Criando um data.frame auxiliar que possui uma linha para cada combinação de município e ano
df_aux_municipios <- data.frame(codmunres = rep(codigos_municipios, each = length(2012:2023)), ano = 2012:2023)

# Criando um vetor com os anos considerados (2012 a 2022) (p/ a parte do CNES e do SIH)
anos <- c(2012:2022)

# Criando um vetor com as siglas de todos os estados do Brasil (p/ a parte do SIH)
estados <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA",
             "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN",
             "RS", "RO", "RR", "SC", "SP", "SE", "TO")

# Baixando os dados do SINASC de 2012 a 2022
df_sinasc1 <- fetch_datasus(
  year_start = 2012,
  year_end = 2022,
  vars = c("CODMUNRES", "CODMUNNASC", "CODESTAB", "DTNASC", "PESO", "GESTACAO", "SEMAGESTAC", "APGAR5", "IDANOMAL", "CODANOMAL"),
  information_system = "SINASC"
)

sinasc23 <- fread("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SINASC/DNOPEN23.csv", sep = ";")
sinasc23 <- sinasc23 |>
  select(CODMUNRES, CODMUNNASC, CODESTAB, DTNASC, PESO, GESTACAO, SEMAGESTAC, APGAR5, IDANOMAL, CODANOMAL)

sinasc23$IDANOMAL <- as.character(sinasc23$IDANOMAL)

df_sinasc <- rbind(df_sinasc1, sinasc23)

unique(df_sinasc$IDANOMAL)
unique(df_sinasc$CODANOMAL)
any(is.na(unique(df_sinasc$CODANOMAL)))  #Existem NAs
any(unique(df_sinasc$CODANOMAL) == '', na.rm = TRUE)  #Não existem ''

# Baixando os dados do CNES-ST para discriminar entre estabelecimentos com ou sem vínculo com o SUS
for (ano in anos) {
  erro <- TRUE
  while (erro) {
    erro <- tryCatch({
      # Baixando os dados do CNES-ST para o dado ano
      df_cnes_st <- fetch_datasus(
        year_start = ano,
        year_end = ano,
        month_start = 1,
        month_end = 12,
        information_system = "CNES-ST",
        timeout = 500,
        stop_on_error = TRUE,
        vars = c("CNES", "CODUFMUN", "COMPETEN", "VINC_SUS")
      ) |>
        mutate(ano = as.numeric(substr(COMPETEN, 1, 4)), mes = as.numeric(substr(COMPETEN, 5, 6)))

      erro <- FALSE
    },
    warning = function(cond) return(TRUE)
    )
  }

  write.csv(df_cnes_st, gzfile(glue("data-raw/extracao-dos-dados/databases-antigas/internacoes_menores_28_dias/CNES/dados_brutos/df_cnes_st_{ano}.csv.gz")), row.names = FALSE)

  # Classificando estabelecimentos entre públicos e não públicos
  df_estabelecimentos <- df_cnes_st |>
    group_by(CNES, CODUFMUN, ano) |>
    summarise(vinc_sus = ifelse(all(VINC_SUS == 1), 1, 0)) |>
    select(cnes = CNES, codmunnasc = CODUFMUN, ano, vinc_sus)

  write.csv(
    df_estabelecimentos,
    gzfile(glue("data-raw/extracao-dos-dados/databases-antigas/internacoes_menores_28_dias/CNES/estabelecimentos_class/df_estabelecimentos_publicos_{ano}.csv.gz")),
    row.names = FALSE
  )
}

# Criando o data.frame que guardará a base final
df_estabelecimentos_publicos <- data.frame()

for (ano in anos) {
  df_estabelecimentos_aux <- fread(
    glue("data-raw/extracao-dos-dados/databases-antigas/internacoes_menores_28_dias/CNES/estabelecimentos_class/df_estabelecimentos_publicos_{ano}.csv.gz"),
    colClasses = c("character", "character", "numeric", "numeric"),
    sep = ","
  )

  df_estabelecimentos_publicos <- bind_rows(df_estabelecimentos_publicos, df_estabelecimentos_aux)

  rm(df_estabelecimentos_aux)
  gc()
}

# Salvando a base completa
write.csv(
  df_estabelecimentos_publicos,
  gzfile("data-raw/extracao-dos-dados/databases-antigas/internacoes_menores_28_dias/CNES/estabelecimentos_class/df_estabelecimentos_publicos_2012_2022.csv.gz"),
  row.names = FALSE
)

df_estabelecimentos_publicos <- fread(
  "data-raw/extracao-dos-dados/databases-antigas/internacoes_menores_28_dias/CNES/estabelecimentos_class/df_estabelecimentos_publicos_2012_2022.csv.gz",
  colClasses = c("character", "character", "numeric", "numeric")
  ) |>
  mutate(codmunnasc = as.character(codmunnasc))


# Transformando algumas variáveis e criando as variáveis necessárias
df_bloco5_sinasc <- df_sinasc |>
  mutate(
    ano = as.numeric(substr(DTNASC, nchar(DTNASC) - 3, nchar(DTNASC))),
    PESO = as.numeric(PESO),
    GESTACAO = as.numeric(GESTACAO),
    SEMAGESTAC = as.numeric(SEMAGESTAC),
    APGAR5 = as.numeric(APGAR5),
    .keep = "unused",
  ) |>
  left_join(df_estabelecimentos_publicos, by = join_by(CODESTAB == cnes, CODMUNNASC == codmunnasc, ano)) |>
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
    nascidos_estabelecimentos_sus = if_else(vinc_sus == 1, 1, 0, missing = 0),
    .keep = "unused"
  ) |>
  clean_names() |>
  group_by(codmunres, ano) |>
  summarise_at(vars(contains("nascidos")), sum) |>
  ungroup()

# Para o indicador de porcentagem de internações em menores de 28 dias
## Baixando os dados do SIH-RD e do SIH-SP de internações em menores de 28 dias
for (estado in estados) {
  # Criando o data.frame que guardará a base final do estado
  df_sih_rd_uf <- data.frame()

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
        ) |>
          # Filtrando apenas por internações de menores de 28 dias
          mutate(idade_dias = as.numeric(as.Date(DT_INTER, format = "%Y%m%d") - as.Date(NASC, format = "%Y%m%d"))) |>
          dplyr::filter(
            idade_dias < 28
          )

        erro_rd <- FALSE
      },
      warning = function(cond) return(TRUE)
      )
    }

    # Juntando com os dados dos anos anteriores para a dada UF
    df_sih_rd_uf <- bind_rows(df_sih_rd_uf, df_sih_rd_aux)

    # Limpando a memória
    rm(df_sih_rd_aux)
    gc()
  }

  # Salvando as bases da UF
  write.csv2(
    df_sih_rd_uf,
    gzfile(glue("data-raw/extracao-dos-dados/databases-antigas/internacoes_menores_28_dias/SIH/{estado}_sih_rd_menores_28_dias_{anos[1]}_{anos[length(anos)]}.csv.gz")),
    row.names = FALSE
  )

  # Limpando a memória
  rm(df_sih_rd_uf)
  gc()
}

## Criando o data.frame que guardará a base final
df_sih_rd <- data.frame()

for (estado in estados) {
  df_rd_aux <- fread(
    glue("data-raw/extracao-dos-dados/databases-antigas/internacoes_menores_28_dias/SIH/{estado}_sih_rd_menores_28_dias_2012_2022.csv.gz"),
    sep = ";",
    dec = ","
  )
  df_sih_rd <- bind_rows(df_sih_rd, df_rd_aux)

  rm(df_rd_aux)
  gc()
}

## Salvando a base completa
write.csv2(
  df_sih_rd,
  glue("data-raw/extracao-dos-dados/databases-antigas/internacoes_menores_28_dias/SIH/BR_sih_rd_menores_28_dias_{anos[1]}_{anos[length(anos)]}.csv"),
  row.names = FALSE
)

## Rodando o algoritmo da Claudia na base completa do SIH-RD
### Criando um vetor que contém o diretório original do projeto
diretorio_original <- getwd()

### Criando um vetor que contém o diretório das bases brutas do SIH-RD
diretorio_bases_brutas <- glue("{getwd()}/data-raw/extracao-dos-dados/databases-antigas/internacoes_menores_28_dias/SIH")

### Mudando o diretório para a pasta que contém o algoritmo em C++
setwd("data-raw/extracao-dos-dados/databases-antigas/internacoes_menores_28_dias/algorithm_episode_of_care/")

### Rodando o algoritmo em C++
system(glue("./processaih {diretorio_bases_brutas}/BR_sih_rd_menores_28_dias_2012_2022.csv"))

## Voltando para o diretório original do projeto
setwd(diretorio_original)

### Criando a conexão com o arquivo .sqlite gerado como resultado do algoritmo em C++
con <- dbConnect(SQLite(), "data-raw/extracao-dos-dados/databases-antigas/internacoes_menores_28_dias/algorithm_episode_of_care/work.sqlite")

### Selecionando a tabela "aih" com todas as suas variáveis, ordenadas por AIHREF e DT_INTER
df_aih <- dbGetQuery(con, "select * from aih order by AIHREF, DT_INTER")
dbDisconnect(con)

### Adicionando variáveis que estão no SIH-RD, mas que não são devolvidas na base gerada pelo algoritmo
df_aih_completo <- left_join(
  df_aih,
  df_sih_rd |>
    select(ANO_CMPT, DT_INTER, DT_SAIDA, N_AIH, MUNIC_MOV, idade_dias) |>
    mutate_at(vars(c(DT_INTER, DT_SAIDA, N_AIH, MUNIC_MOV)), as.character)
)

## Lendo uma base com informações auxiliares dos municípios
df_infos_municipios <- read.csv("data-raw/extracao-dos-dados/databases-antigas/df_aux_municipios.csv") |>
  mutate_if(is.numeric, as.character)

## Passando a base para o formato wide (cada linha corresponderá a uma pessoa única)
df_aih_wide <- df_aih_completo |>
  mutate(
    DT_INTER = as.Date(DT_INTER, format = "%Y%m%d"),
    DT_SAIDA = as.Date(DT_SAIDA, format = "%Y%m%d"),
    NASC = as.Date(NASC, format = "%Y%m%d")
  ) |>
  group_by(AIHREF) |>  # Agrupando pelo N_AIH comum para todas as linhas de um episódio de cuidado completo
  summarise(
    ANO_CMPT = last(ANO_CMPT),  # Ano de processamento do SIH da última internação
    CNES = first(CNES),
    MUNIC_RES = first(MUNIC_RES),  # Município de residência da primeira internação
    MUNIC_MOV = first(MUNIC_MOV),  # Município do estabelecimento da primeira internação
    idade_dias = first(idade_dias)  # Idade, em dias, na data da primeira internação
  ) |>
  ungroup() |>
  select(ano = ANO_CMPT, codmunres = MUNIC_RES, codmunocor = MUNIC_MOV, cnes = CNES, aihref = AIHREF, idade_dias) |>
  filter(codmunres %in% df_infos_municipios$codmunres & codmunocor %in% df_infos_municipios$codmunres)

## Adicionando a informação sobre a macrorregião dos municípios de residência e ocorrência
df_aih_wide_macros <- df_aih_wide |>
  left_join(df_infos_municipios |> select(codmunres, macro_r_saude_res = macro_r_saude), by = join_by(codmunres == codmunres)) |>
  left_join(df_infos_municipios |> select(codmunres, macro_r_saude_ocor = macro_r_saude), by = join_by(codmunocor == codmunres)) |>
  mutate(
    idade_cat = ifelse(idade_dias < 7, "menores_7_dias", "7_a_27_dias"),
    indicadora_mesma_macro = ifelse(macro_r_saude_res == macro_r_saude_ocor, "na_macro", "fora_macro")
  ) |>
  select(!c(cnes, codmunocor, aihref, idade_dias, macro_r_saude_res, macro_r_saude_ocor))

## Passando a base para o formato wide (um município por linha), criando uma linha para cada município e ano e preenchendo os NAs gerados com 0
df_bloco5_sih <- df_aih_wide_macros |>
  group_by_all() |>
  summarise(num_internacoes = n()) |>
  ungroup() |>
  pivot_wider(
    names_from = c(indicadora_mesma_macro, idade_cat),
    values_from = num_internacoes,
    values_fill = 0,
    names_prefix = "internacoes_"
  ) |>
  arrange(codmunres, ano) |>
  right_join(data.frame(codmunres = rep(df_infos_municipios$codmunres, each = length(2012:2022)), ano = 2012:2022)) |>
  mutate(across(.cols = -c(codmunres, ano), .fns = ~ replace_na(., 0))) |>
  mutate(
    internacoes_geral_7_a_27_dias = internacoes_na_macro_7_a_27_dias + internacoes_fora_macro_7_a_27_dias,
    internacoes_geral_menores_7_dias = internacoes_na_macro_menores_7_dias + internacoes_fora_macro_menores_7_dias,
    internacoes_na_macro_geral = internacoes_na_macro_menores_7_dias + internacoes_na_macro_7_a_27_dias,
    internacoes_fora_macro_geral = internacoes_fora_macro_menores_7_dias + internacoes_fora_macro_7_a_27_dias,
    internacoes_geral_geral = internacoes_geral_7_a_27_dias + internacoes_geral_menores_7_dias
  ) |>
  select(ano, codmunres, ends_with("_7_dias"), ends_with("_27_dias"), ends_with("_geral"))

## Verificando se o total de internações equivale ao número de linhas das bases df_aih_wide/df_aih_wide_macros
sum(df_bloco5_sih$internacoes_geral_geral) == nrow(df_aih_wide)

## Removendo arquivos já utilizados
file.remove(c(
  "data-raw/extracao-dos-dados/databases-antigas/internacoes_menores_28_dias/SIH/BR_sih_rd_menores_28_dias_2012_2022.csv",
  "data-raw/extracao-dos-dados/databases-antigas/internacoes_menores_28_dias/algorithm_episode_of_care/aihperm.csv",
  "data-raw/extracao-dos-dados/databases-antigas/internacoes_menores_28_dias/algorithm_episode_of_care/aihpermtransf.csv",
  "data-raw/extracao-dos-dados/databases-antigas/internacoes_menores_28_dias/algorithm_episode_of_care/work.sqlite"
))

# Fazendo um left_join de ambas as bases com a base auxiliar de municípios
df_bloco5 <- left_join(df_aux_municipios, df_bloco5_sinasc, by = c("codmunres", "ano")) |>
  left_join(df_bloco5_sih)


# adicionar dados de asfixia e malformação a base df_bloco5
# lembre-se de rodar o script bloco5_asfixia.R para obter os dados em caso de atualização


asfixia <- read.csv("data-raw/csv/asfixia_2012_2023.csv", sep = ';') |>
  janitor::clean_names() |>
  dplyr::arrange(codmunres, ano) |>
  dplyr::select(codmunres, ano, nascidos_vivos_asfixia1, total_nascidos)

malformacao <- read.csv("data-raw/csv/malformacao_2012_2023.csv", sep = ';') |>
  janitor::clean_names() |>
  dplyr::arrange(codmunres, ano) |>
  select(codmunres, ano, anomalia, grupo_de_anomalias_congenitas, descricao, nascidos_vivos_anomalia)


df_bloco5 <- left_join(df_bloco5, asfixia, by = c("codmunres", "ano")) |>
  left_join(malformacao)


# Preenchendo os valores NAs, gerados após o left_join, com 0 (MENOS PARA 2023 PARA AS COLUNAS QUE VEM DO SIH)
internacoes_cols <- grep("^internacoes", names(df_bloco5), value = TRUE)

for (col in names(df_bloco5)) {
  if (col %in% internacoes_cols) {
    # Nas colunas que começam com "internacoes", substituir os NAs por 0 apenas se o ano não for 2023
    df_bloco5[[col]][is.na(df_bloco5[[col]]) & df_bloco5$ano != 2023] <- 0
  } else {
    # Nas outras colunas, substituir todos os NAs por 0
    df_bloco5[[col]][is.na(df_bloco5[[col]])] <- 0
  }
}

# Verificando se os dados novos e antigos estão batendo
df_bloco5_antigo <- read.csv("data-raw/extracao-dos-dados/databases-antigas/indicadores_bloco5_condicao_de_nascimento_2012-2020.csv") |>
  clean_names()

sum(df_bloco5 |> filter(ano <= 2020) |> pull(total_de_nascidos_vivos)) - sum(df_bloco5_antigo$total_de_nascidos_vivos)
sum(df_bloco5 |> filter(ano <= 2020) |> pull(nascidos_vivos_com_baixo_peso)) - sum(df_bloco5_antigo$nascidos_vivos_com_baixo_peso)
sum(df_bloco5 |> filter(ano <= 2020) |> pull(nascidos_vivos_prematuros)) - sum(df_bloco5_antigo$nascidos_vivos_prematuros)
sum(df_bloco5 |> filter(ano <= 2020) |> pull(nascidos_vivos_termo_precoce)) - sum(df_bloco5_antigo$nascidos_vivos_termo_precoce)

# Exportando os dados
write.csv(df_bloco5, "data-raw/csv/indicadores_bloco5_condicao_de_nascimento_2012_2023.csv", row.names = FALSE)













