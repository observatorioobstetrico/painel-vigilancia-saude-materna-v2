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
library(ckanr)
library(purrr)

# OBS.: Ainda pretendo melhorar a separação entre dados consolidados e dados preliminares


# Para os dados consolidados --------------------------------------------------
## Criando um vetor que contém o diretório original do projeto
diretorio_original <- getwd()

## Mudando o diretório para a pasta de extração dos dados
setwd("data-raw/extracao-dos-dados/blocos/")

## Criando alguns objetos auxiliares ------------------------------------------
### Criando um objeto que recebe os códigos dos municípios que utilizamos no painel
codigos_municipios <- read.csv("databases_auxiliares/tabela_aux_municipios.csv") |>
  pull(codmunres) |>
  as.character()

### Criando um data.frame auxiliar que possui uma linha para cada combinação de município e ano
df_aux_municipios <- data.frame(codmunres = rep(codigos_municipios, each = length(2012:2022)), ano = 2012:2022)

### Importando funções que baixam dados do Tabnet
source("funcoes_auxiliares.R")

## Para as variáveis provenientes do SINASC -----------------------------------
### Baixando os dados consolidados do SINASC de 2012 a 2022 e selecionando as variáveis de interesse
df_sinasc <- fetch_datasus(
  year_start = 2012,
  year_end = 2022,
  vars = c("CODMUNRES", "DTNASC", "IDADEMAE", "QTDPARTNOR", "QTDPARTCES"),
  information_system = "SINASC"
)

### Transformando algumas variáveis e criando as variáveis necessárias p/ o cálculo dos indicadores
df_bloco2_sinasc <- df_sinasc |>
  mutate(
    ano = as.numeric(substr(DTNASC, nchar(DTNASC) - 3, nchar(DTNASC))),
    IDADEMAE = as.numeric(IDADEMAE),
    QTDPARTNOR = as.numeric(QTDPARTNOR),
    QTDPARTCES = as.numeric(QTDPARTCES),
    .keep = "unused",
  ) |>
  mutate(
    total_de_nascidos_vivos = 1,
    nvm_menor_que_20 = if_else(IDADEMAE < 20, 1, 0, missing = 0),
    mulheres_com_mais_de_tres_partos_anteriores = if_else(
      QTDPARTNOR > 3 | (QTDPARTNOR > 2 & QTDPARTCES > 0) |
        (QTDPARTNOR > 1  &  QTDPARTCES > 1) |
        (QTDPARTNOR > 0  &  QTDPARTCES > 2) |
        QTDPARTCES > 3,
      1, 0, missing = 0
    ),
    .keep = "unused"
  ) |>
  clean_names() |>
  group_by(codmunres, ano) |>
  summarise_at(vars(-group_cols()), sum) |>
  ungroup() |>
  # Juntando com a base aulixiar de municípios
  right_join(df_aux_municipios)

### Substituindo todos os NAs, gerados após o right_join, por 0
df_bloco2_sinasc[is.na(df_bloco2_sinasc)] <- 0


## Para as variáveis provenientes do Tabnet -----------------------------------
### Estimativas populacionais de mulheres de 10 a 19 anos ---------------------
#### Baixando os dados de 2012 a 2021
df_est_pop_fem_10_19 <- est_pop_tabnet(
  coluna = "Ano",
  periodo = as.character(2012:2021),
  faixa_etaria = c("10 a 14 anos", "15 a 19 anos")
)
head(df_est_pop_fem_10_19)

#### Passando os dados para o formato long e juntando com a base auxiliar de municípios
df_est_pop_fem_10_19_long <- df_est_pop_fem_10_19 |>
  select(!municipio) |>
  pivot_longer(
    cols = !c(codmunres),
    names_to = "ano",
    values_to = "pop_feminina_10_a_19"
  ) |>
  mutate(codmunres = as.character(codmunres)) |>
  mutate_at(vars(ano, pop_feminina_10_a_19), as.numeric) |>
  right_join(df_aux_municipios)

head(df_est_pop_fem_10_19_long)

#### Substituindo todos os NAs, gerados após o right_join, por 0 (menos para o ano de 2022)
df_est_pop_fem_10_19_long[is.na(df_est_pop_fem_10_19_long) & df_est_pop_fem_10_19_long$ano != 2022] <- 0

### Estimativas populacionais de mulheres de 10 a 49 anos ---------------------
#### Baixando os dados de 2012 a 2021
df_est_pop_fem_10_49 <- est_pop_tabnet(coluna = "Ano", periodo = as.character(2012:2021))
head(df_est_pop_fem_10_49)

#### Passando os dados para o formato long e juntando com a base auxiliar de municípios
df_est_pop_fem_10_49_long <- df_est_pop_fem_10_49 |>
  select(!municipio) |>
  pivot_longer(
    cols = !c(codmunres),
    names_to = "ano",
    values_to = "pop_fem_10_49"
  ) |>
  mutate(codmunres = as.character(codmunres)) |>
  mutate_at(vars(ano, pop_fem_10_49), as.numeric) |>
  right_join(df_aux_municipios)

head(df_est_pop_fem_10_49_long)

#### Substituindo todos os NAs, gerados após o right_join, por 0 (menos para o ano de 2022)
df_est_pop_fem_10_49_long[is.na(df_est_pop_fem_10_49_long) & df_est_pop_fem_10_49_long$ano != 2022] <- 0

### Mulheres de 10 a 49 anos beneficiárias de planos de saúde -----------------
#### Baixando os dados de mulheres de 2012 a 2021
df_beneficiarias_10_49 <- {
  full_join(
    pop_com_plano_saude_tabnet(periodo = 2012:2016) |> select(!municipio),
    pop_com_plano_saude_tabnet(periodo = 2017:2021) |> select(!municipio)
  ) |>
    mutate(codmunres = as.character(codmunres))
}
head(df_beneficiarias_10_49)

#### Passando o data.frame para o formato long e calculando o número mediano de beneficiárias para cada município e ano
df_beneficiarias_10_49_long <- df_beneficiarias_10_49 |>
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
  right_join(df_est_pop_fem_10_49_long) |>
  group_by(codmunres, ano) |>
  summarise(
    beneficiarias_10_a_49 = round(median(beneficiarias_10_a_49[beneficiarias_10_a_49 < pop_fem_10_49]))
  ) |>
  ungroup()

#### Substituindo todos os NAs, gerados após o right_join, por 0 (menos para o ano de 2022)
df_beneficiarias_10_49_long[is.na(df_beneficiarias_10_49_long) & df_beneficiarias_10_49_long$ano != 2022] <- 0

### Juntando todos os dados --------------------------------------------------
df_bloco2_tabnet <- full_join(
  full_join(df_est_pop_fem_10_19_long, df_est_pop_fem_10_49_long),
  df_beneficiarias_10_49_long
) |>
  arrange(codmunres, ano)


## Para os indicadores de aborto ----------------------------------------------
### Para os numeradores -------------------------------------------------------
#### Criando um vetor com os anos considerados
anos <- c(2015:2022)

#### Criando um vetor com as siglas de todos os estados do Brasil
estados <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA",
             "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN",
             "RS", "RO", "RR", "SC", "SP", "SE", "TO")

#### Definindo as CIDs de aborto
cids_aborto <- c("O03", "O030", "O031", "O032", "O033", "O034", "O035", "O036", "O037", "O038", "O039",
                 "O04", "O040", "O041", "O042", "O043", "O044", "O045", "O046", "O047", "O048", "O049",
                 "O05", "O050", "O051", "O052", "O053", "O054", "O055", "O056", "O057", "O058", "O059",
                 "O06", "O060", "O061", "O062", "O063", "O064", "O065", "O066", "O067", "O068", "O069",
                 "O07", "O070", "O071", "O072", "O073", "O074", "O075", "O076", "O077", "O078", "O079",
                 "O08", "O080", "O081", "O082", "O083", "O084", "O085", "O086", "O087", "O088", "O089")

#### Definindo as CIDs de não aborto
cids_nao_aborto <- c("O00", "O000", "O001", "O002", "O008", "O009",
                     "O01", "O010", "O011", "O019",
                     "O02", "O020", "O021", "O028", "O029")

#### Para os dados da ANS -----------------------------------------------------
##### Criando pastas temporárias para armazenar os arquivos baixados
dir.create("databases_auxiliares/ANS/arquivos_base_consolidada", showWarnings = FALSE, recursive = TRUE)
dir.create("databases_auxiliares/ANS/arquivos_base_detalhada", showWarnings = FALSE, recursive = TRUE)

##### Baixando e descompactando os arquivos da base consolidada para cada ano, UF e mês
for (ano in anos) {
  base_url <- paste0("https://dadosabertos.ans.gov.br/FTP/PDA/TISS/HOSPITALAR/", ano, "/")
  for (uf in estados) {
    for (mes in 1:12) {
      mes_str <- sprintf("%02d", mes)
      url <- paste0(base_url, uf, "/", uf, "_", ano, mes_str, "_HOSP_CONS.zip")
      filename <- paste0("databases_auxiliares/ANS/arquivos_base_consolidada/", uf, "_", ano, mes_str, "_HOSP_CONS.zip")
      download.file(url, filename)
      unzip(filename, exdir = "databases_auxiliares/ANS/arquivos_base_consolidada")
      file.remove(filename)
    }
  }
}

##### Criando uma lista com o nome de todos os arquivos baixados
arquivos <- list.files("databases_auxiliares/ANS/arquivos_base_consolidada/", full.names = TRUE, pattern = "\\.csv", recursive = TRUE)

##### Lendo e concatenando os arquivos baixados
dados_concatenados_hosp <- data.frame()

for (arquivo in arquivos) {
  # Lendo o arquivo CSV
  df <- read_delim(arquivo, delim = ";", escape_double = FALSE, trim_ws = TRUE)

  # Extraindo a UF a partir do nome do arquivo
  uf <- gsub("^.+/([A-Z]+)_\\d{6}_HOSP_CONS\\.csv", "\\1", arquivo)

  # Extraindo o ano e mês a partir do nome do arquivo
  ano <- gsub(".+_(\\d{4})(\\d{2})_HOSP_CONS\\.csv", "\\1", arquivo)
  mes <- gsub(".+_(\\d{4})(\\d{2})_HOSP_CONS\\.csv", "\\2", arquivo)

  # Adicionando colunas de UF, ano e mês
  df1 <- df |>
    mutate(
      Unidade_Federativa = uf,
      ano = ano,
      mes = mes
    )

  # Juntando com o restante dos dados
  dados_concatenados_hosp <- bind_rows(dados_concatenados_hosp, df1)

  # Limpando a memória
  rm(df, df1)
  gc()
}

##### Exportando os dados concatenados da base consolidada
write.csv(
  dados_concatenados_hosp,
  "databases_auxiliares/ANS/arquivos_base_consolidada/dados_ANS_hosp_2015_2022.csv",
  row.names = FALSE
)

##### Filtrando pelas faixas etárias de interesse e pelas indicadoras de aborto ou não aborto
dados_concatenados_hosp_filtrados <- dados_concatenados_hosp |>
  mutate(
    fet = ifelse(
      FAIXA_ETARIA %in% c("50 a 59", "60 a 69", "70 a 79", "80 ou mais"),
      ">=50",
      FAIXA_ETARIA
    ),
    cid_aborto = ifelse(if_any(starts_with("CID_"), ~ . %in% cids_aborto), 1, 0),
    cid_nao_aborto = ifelse(if_any(starts_with("CID_"), ~ . %in% cids_nao_aborto), 1, 0)
  ) |>
  filter(
    fet %in% c("10 a 14", "15 a 19", "20 a 29", "30 a 39", "40 a 49", ">=50"),
    cid_aborto == 1 | cid_nao_aborto == 1
  )

##### Exportando os dados da base consolidada filtrados
write.csv(
  dados_concatenados_hosp_filtrados,
  "databases_auxiliares/ANS/arquivos_base_consolidada/dados_ANS_hosp_filtrados_2015_2022.csv",
  row.names = FALSE
)

##### Limpando a memória
rm(dados_concatenados_hosp)
gc()

##### Baixando e descompactando os arquivos da base detalhada para cada ano, UF e mês
for (ano in anos) {
  base_url <- paste0("https://dadosabertos.ans.gov.br/FTP/PDA/TISS/HOSPITALAR/", ano, "/")
  for (uf in estados) {
    for (mes in 1:12) {
      mes_str <- sprintf("%02d", mes)
      url <- paste0(base_url, uf, "/", uf, "_", ano, mes_str, "_HOSP_DET.zip")
      filename <- paste0("databases_auxiliares/ANS/arquivos_base_detalhada/", uf, "_", ano, mes_str, "_HOSP_DET.zip")
      download.file(url, filename)
      unzip(filename, exdir = "databases_auxiliares/ANS/arquivos_base_detalhada")
      file.remove(filename)
    }
  }
}

##### Lendo e concatenando os arquivos baixados
for (ano in anos) {
  # Criando o data.frame que guardará os dados do ano
  dados_concatenados_hosp_det_ano <- data.frame()

  for (uf in estados) {
    # Criando uma lista com o nome de todos os arquivos baixados
    arquivos <- list.files("databases_auxiliares/ANS/arquivos_base_detalhada", full.names = TRUE, pattern = paste0("^", uf, ".*", ano, ".*\\.csv$"), recursive = TRUE)

    # Criando o data.frame que guardará os dados da UF
    dados_det_uf <- data.frame()

    for (arquivo in arquivos) {
      # Lendo o arquivo CSV
      df <- read_delim(arquivo, delim = ";", escape_double = FALSE, trim_ws = TRUE)
      # Extraindo a unidade federativa a partir do nome do arquivo
      uf <- gsub("^.+/([A-Z]+)_\\d{6}_HOSP_DET\\.csv", "\\1", arquivo)

      # Extrair o ano e mês a partir do nome do arquivo
      ano <- gsub(".+_(\\d{4})(\\d{2})_HOSP_DET\\.csv", "\\1", arquivo)
      mes <- gsub(".+_(\\d{4})(\\d{2})_HOSP_DET\\.csv", "\\2", arquivo)

      # Adicionando colunas de unidade federativa, ano e mês
      df1 <- df |>
        mutate(
          Unidade_Federativa = uf,
          ano = ano,
          mes = mes,
          QT_ITEM_EVENTO_INFORMADO = as.numeric(QT_ITEM_EVENTO_INFORMADO),
          VL_ITEM_EVENTO_INFORMADO = as.numeric(VL_ITEM_EVENTO_INFORMADO),
          VL_ITEM_PAGO_FORNECEDOR = as.numeric(VL_ITEM_PAGO_FORNECEDOR),
          IND_PACOTE = as.numeric(IND_PACOTE),
          IND_TABELA_PROPRIA = as.numeric(IND_TABELA_PROPRIA),
          CD_TABELA_REFERENCIA = as.numeric(CD_TABELA_REFERENCIA)
        )

      # Juntando com o restante dos dados
      dados_det_uf <- bind_rows(dados_det_uf, df1)

      # Limpando a memória
      rm(df, df1)
      gc()
    }

    # Juntando com o restante dos dados
    dados_concatenados_hosp_det_ano <- rbind(dados_concatenados_hosp_det_ano, dados_det_uf)

    # Limpando a memória
    rm(dados_det_uf)
    gc()

  }

  # Exportando os dados concatenados de todas as UFs para o dado ano
  write.csv(
    dados_concatenados_hosp_det_ano,
    paste0("databases_auxiliares/ANS/arquivos_base_detalhada/dados_ANS_hosp_det_", ano, ".csv"),
    row.names = FALSE
  )

  # Limpando a memória
  rm(dados_concatenados_hosp_det_ano)
  gc()
}

##### Fazendo todas as manipulações necessárias
###### Lendo o arquivo com os dados concatenados da base consolidada e criando outra variável de faixa etária
dados_concatenados_hosp <- fread("databases_auxiliares/ANS/arquivos_base_consolidada/dados_ANS_hosp_2015_2022.csv") |>
  mutate(
    fet = ifelse(
      FAIXA_ETARIA %in% c("50 a 59", "60 a 69", "70 a 79", "80 ou mais"),
      ">=50",
      FAIXA_ETARIA
    )
  )

###### Lendo o arquivo com os dados concatenados da base consolidada filtrados e excluindo os casos em que cid_nao_aborto == 1
dados_filtrados_cid <- fread("databases_auxiliares/ANS/arquivos_base_consolidada/dados_ANS_hosp_filtrados_2015_2022.csv") |>
  filter(cid_nao_aborto == 0 | is.na(cid_nao_aborto)) |>
  select(
    ID_EVENTO_ATENCAO_SAUDE, Unidade_Federativa, ano, mes, SEXO,
    CD_MUNICIPIO_BENEFICIARIO, fet, cid_aborto_cid = cid_aborto,
    cid_nao_aborto_cid = cid_nao_aborto
  )

###### Criando o data.frame que guardará a base completa
df_numerador_aborto_ans <- data.frame()

for (year in anos){
  # Lendo o arquivo com os dados concatenados das UFs da base detalhada para o dado ano
  dados_concatenados_hosp_det_ano <- fread(
    paste0("databases_auxiliares/ANS/arquivos_base_detalhada/dados_ANS_hosp_det_", year, ".csv")
  )

  # Filtrando os procedimentos que indicam aborto
  dados_det_aborto <- dados_concatenados_hosp_det_ano |>
    filter(CD_PROCEDIMENTO == '31309020' | CD_PROCEDIMENTO == '31309062')
  ## 31309020 - Aspiração manual intra-uterina (AMIU) pós-abortamento
  ## 31309062 - Curetagem pós-abortamento

  # Juntando os dados das bases detalhada e consolidada
  dados_det_cons <- left_join(
    dados_det_aborto,
    dados_concatenados_hosp |> filter(ano == year),
    by = c('ID_EVENTO_ATENCAO_SAUDE')
  )

  # Criando indicadoras de aborto ou não aborto e excluindo os casos em que cid_nao_aborto == 1
  dados_aborto_det_cons <- dados_det_cons |>
    mutate(
      cid_aborto = ifelse(if_any(starts_with("CID_"), ~ . %in% cids_aborto), 1, 0),
      cid_nao_aborto = ifelse(if_any(starts_with("CID_"), ~ . %in% cids_nao_aborto), 1, 0)
    ) |>
    filter(cid_nao_aborto == 0 | is.na(cid_nao_aborto))

  # Considerando apenas um caso por ID_EVENTO_ATENCAO_SAUDE
  dados_aborto_det_cons_unico <- dados_aborto_det_cons |>
    distinct(ID_EVENTO_ATENCAO_SAUDE, .keep_all = TRUE) |>
    select(
      ID_EVENTO_ATENCAO_SAUDE, CD_PROCEDIMENTO, Unidade_Federativa = Unidade_Federativa.x,
      ano = ano.x, mes = mes.x, SEXO, CD_MUNICIPIO_BENEFICIARIO, fet, cid_aborto, cid_nao_aborto
    )

  # Juntando as duas bases
  dados_aborto_completo_aux <- full_join(
    dados_aborto_det_cons_unico |> mutate(ID_EVENTO_ATENCAO_SAUDE = as.numeric(ID_EVENTO_ATENCAO_SAUDE)),
    dados_filtrados_cid |>
      filter(ano == year) |>
      mutate(ID_EVENTO_ATENCAO_SAUDE = as.numeric(ID_EVENTO_ATENCAO_SAUDE)),
    by = c('ID_EVENTO_ATENCAO_SAUDE', 'ano', 'mes', 'Unidade_Federativa', 'CD_MUNICIPIO_BENEFICIARIO' ,'SEXO', 'fet')
  )

  # Fazendo algumas manipulações e agrupando os dados por município de residência
  dados_aborto_completo <- dados_aborto_completo_aux |>
    mutate(
      CD_PROCEDIMENTO = ifelse(is.na(CD_PROCEDIMENTO), 0, CD_PROCEDIMENTO),
      SEXO = ifelse(is.na(SEXO), "ignorado", SEXO),
      fet = case_when(
        fet %in% c("10 a 14", "15 a 19", "20 a 29") ~ "abortos_ans_menor_30",
        fet == "30 a 39" ~ "abortos_ans_30_a_39",
        fet == "40 a 49" ~ "abortos_ans_40_a_49",
        .default = fet
      )
    ) |>
    filter(
      fet %in% c("abortos_ans_menor_30", "abortos_ans_30_a_39", "abortos_ans_40_a_49"),
      SEXO == "Feminino"
    ) |>
    group_by(CD_MUNICIPIO_BENEFICIARIO, ano, fet) |>
    summarise(cont = n()) |>
    ungroup()

  # Passando os dados para o formato wide
  dados_aborto_completo_wide <- dados_aborto_completo |>
    pivot_wider(names_from = fet, values_from = cont, values_fill = 0) |>
    rename(codmunres = CD_MUNICIPIO_BENEFICIARIO)

  # Juntando com os dados dos outros anos
  df_numerador_aborto_ans <- bind_rows(df_numerador_aborto_ans, dados_aborto_completo_wide)

  # Limpando a memória
  rm(
    dados_aborto_completo, dados_aborto_completo_aux, dados_aborto_completo_wide,
    dados_det_aborto, dados_det_cons, dados_concatenados_hosp_det_ano,
    dados_aborto_det_cons, dados_aborto_det_cons_unico
  )
  gc()
}

##### Limpando a memória
rm(dados_concatenados_hosp, dados_filtrados_cid)
gc()

##### Juntando com a base aulixiar de municípios
df_numerador_aborto_ans <- left_join(
  df_aux_municipios,
  df_numerador_aborto_ans |> mutate(codmunres = as.character(codmunres))
)

##### Substituindo todos os NAs, gerados após o right_join, por 0
df_numerador_aborto_ans[is.na(df_numerador_aborto_ans)] <- 0


#### Para os dados do SIH -----------------------------------------------------
##### Criando a base que guardará os dados de todos os anos
df_numerador_aborto_sus <- data.frame()

for (ano in anos) {
  # Baixando os dados do dado ano e criando uma coluna de ano
  dados <- fetch_datasus(
    year_start = ano,
    year_end = ano,
    month_start = 1,
    month_end = 12,
    information_system = "SIH-RD",
    vars = c(
      "ANO_CMPT", "MUNIC_RES", "DIAG_PRINC", "DIAG_SECUN", "CID_ASSO", "CID_NOTIF", "CID_MORTE",
      "DIAGSEC1", "DIAGSEC2", "DIAGSEC3", "DIAGSEC4", "DIAGSEC5", "DIAGSEC6",
      "DIAGSEC7", "DIAGSEC8", "DIAGSEC9", "SEXO", "COD_IDADE", "IDADE", "DT_INTER", "DT_SAIDA", "IDENT"
    )
  )

  # Criando as variáveis de ano e faixa etária e filtrando mulheres de 10 a 49 anos com IDENT != 5
  dados_filtrados <- dados |>
    mutate(
      ano = substr(DT_INTER, 1, 4),
      fet = case_when(
        COD_IDADE == 4 & IDADE >= 10 & IDADE <= 29 ~ "abortos_sus_menor_30",
        COD_IDADE == 4 & IDADE >= 30 & IDADE <= 39 ~ "abortos_sus_30_a_39",
        COD_IDADE == 4 & IDADE >= 40 & IDADE <= 49 ~ "abortos_sus_40_a_49",
        TRUE ~ "não" # Caso não caia em nenhuma das faixas
      )
    ) |>
    filter(
      fet != "não",
      SEXO == 3 | SEXO == 2,
      IDENT != 5 | !is.na(IDENT)
    ) |>
    mutate()

  # Filtrando casos com CID de aborto em alguma das colunas CID_1, CID_2, CID_3 ou CID_4
  dados_filtrados1 <- dados_filtrados |>
    filter(if_any(starts_with("CID_"), ~ . %in% cids_aborto) |
             if_any(starts_with("DIAG_"), ~ . %in% cids_aborto) |
             if_any(starts_with("DIAG"), ~ . %in% cids_aborto))

  # Excluir mulheres com CID de não aborto em alguma das colunas CID_1, CID_2, CID_3 ou CID_4
  dados_filtrados2 <- dados_filtrados1 |>
    filter(!if_any(starts_with("CID_"), ~ . %in% cids_nao_aborto))

  # Excluir mulheres com CID de não aborto em alguma das colunas CID_1, CID_2, CID_3 ou CID_4
  dados_filtrados3 <- dados_filtrados2 |>
    filter(!if_any(starts_with("DIAG"), ~ . %in% cids_nao_aborto))

  # Agrupando por município e passando para o formato wide
  dados_filtrados_wide <- dados_filtrados3 |>
    group_by(MUNIC_RES, ano, fet) |>
    summarise(cont = n()) |>
    ungroup() |>
    pivot_wider(names_from = fet, values_from = cont, values_fill = 0) |>
    rename(codmunres = MUNIC_RES)

  # Juntando com o restante dos dados
  df_numerador_aborto_sus <- bind_rows(df_numerador_aborto_sus, dados_filtrados_wide) |>
    group_by(codmunres, ano) |>
    summarise_at(starts_with("aborto"), sum) |>
    ungroup()

  # Limpando a memória
  rm(dados, dados_filtrados1, dados_filtrados2, dados_filtrados3, dados_filtrados_wide)
  gc()
}

##### Juntando com a base aulixiar de municípios
df_numerador_aborto_sus <- left_join(
  df_aux_municipios,
  df_numerador_aborto_sus |> mutate(codmunres = as.character(codmunres), ano = as.numeric(ano))
)

##### Substituindo todos os NAs, gerados após o right_join, por 0
df_numerador_aborto_sus[is.na(df_numerador_aborto_sus)] <- 0


#### Juntando os dados das duas bases -----------------------------------------
df_numerador_aborto <- full_join(df_numerador_aborto_ans, df_numerador_aborto_sus)


### Para os denominadores -----------------------------------------------------
#### Calculando a cobertura suplementar, os limites inferiores e superiores para a consideração de outliers e inputando caso necessário
df_cob_suplementar <- df_bloco2_tabnet |>
  mutate(
    cob_suplementar = round(beneficiarias_10_a_49 / pop_fem_10_49, 3)
  ) |>
  group_by(codmunres) |>
  mutate(
    q1 = round(quantile(cob_suplementar[which(cob_suplementar < 1 & ano %in% 2014:2021)], 0.25), 3),
    q3 = round(quantile(cob_suplementar[which(cob_suplementar < 1 & ano %in% 2014:2021)], 0.75), 3),
    iiq = q3 - q1,
    lim_inf = round(q1 - 1.5*iiq, 3),
    lim_sup = round(q3 + 1.5*iiq, 3),
    outlier = ifelse(
      # Obs.: estou desconsiderando 2022, aqui, porque não temos as estimativas populacionais ainda
      ano < 2022 & ((cob_suplementar > 1) | (cob_suplementar < lim_inf | cob_suplementar > lim_sup) | (is.na(q1) & is.na(q3)) | (is.na(cob_suplementar))),
      1,
      0
    ),
    novo_cob_suplementar = ifelse(
      outlier == 0,
      cob_suplementar,
      round(median(cob_suplementar[which(outlier == 0 & ano %in% 2014:2021)]), 3)
    ),
    novo_beneficiarias_10_a_49 = round(novo_cob_suplementar * pop_fem_10_49)
  ) |>
  ungroup() |>
  select(codmunres, ano, pop_fem_10_49, cob_suplementar = novo_cob_suplementar)

#### Juntando com os dados de nascidos vivos do SINASC e criando as variáveis exclusivas do SUS ou da ANS
df_denominador_aborto <- left_join(df_bloco2_sinasc, df_cob_suplementar) |>
  mutate(
    pop_fem_ans_10_49 = round(pop_fem_10_49 * cob_suplementar),
    pop_fem_sus_10_49 = pop_fem_10_49 - pop_fem_ans_10_49,
    total_de_nascidos_vivos_ans = round(total_de_nascidos_vivos * cob_suplementar),
    total_de_nascidos_vivos_sus = total_de_nascidos_vivos - total_de_nascidos_vivos_ans
  ) |>
  select(codmunres, ano, pop_fem_ans_10_49, pop_fem_sus_10_49, total_de_nascidos_vivos_ans, total_de_nascidos_vivos_sus)

### Juntando os dados dos numeradores e dos denominadores ---------------------
df_bloco2_aborto <- full_join(df_numerador_aborto, df_denominador_aborto)


## Juntando todos os dados baixados -------------------------------------------
df_bloco2 <- full_join(
  full_join(df_bloco2_sinasc, df_bloco2_tabnet),
  df_bloco2_aborto
)


## Verificando se os dados novos e antigos estão batendo ----------------------
df_bloco2_antigo <- read_delim(
  "databases_auxiliares/indicadores_bloco2_planejamento_reprodutivo_SUS_ANS_2012_2020.csv",
  delim = ";", escape_double = FALSE, trim_ws = TRUE
) |>
  clean_names() |>
  filter(codmunres %in% codigos_municipios)

sum(df_bloco2 |> filter(ano < 2021) |> pull(total_de_nascidos_vivos)) - sum(df_bloco2_antigo$total_de_nascidos_vivos)
sum(df_bloco2 |> filter(ano < 2021) |> pull(nvm_menor_que_20)) - sum(df_bloco2_antigo$nvm_menor_que_20)
sum(df_bloco2 |> filter(ano < 2021) |> pull(pop_feminina_10_a_19)) - sum(df_bloco2_antigo$pop_feminina_10_a_19)
sum(df_bloco2 |> filter(ano < 2021) |> pull(mulheres_com_mais_de_tres_partos_anteriores)) - sum(df_bloco2_antigo$mulheres_com_mais_de_tres_partos_anteriores)
sum(df_bloco2 |> filter(ano < 2021) |> pull(pop_fem_10_49)) - sum(df_bloco2_antigo$pop_fem_10_49)
sum(df_numerador_aborto_sus |> filter(ano < 2021) |> pull(abortos_sus_30_a_39)) - sum(df_bloco2_antigo$abortos_sus_30_a_39)


## Salvando a base de dados consolidada ---------------------------------------
write.csv(
  df_bloco2,
  "databases_consolidadas/indicadores_bloco2_planejamento_reprodutivo_SUS_ANS_2012_2022.csv",
  row.names = FALSE
)

### Excluindo todos os arquivos baixados
unlink("ANS/", recursive = TRUE)

### Voltando para o diretório original do projeto
setwd(diretorio_original)


# Para os dados preliminares --------------------------------------------------
## Para as variáveis provenientes do SINASC -----------------------------------
### Baixando os dados preliminares de 2023 e selecionando as variáveis de interesse
df_sinasc_preliminares_2023 <- fread("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SINASC/DNOPEN23.csv", sep = ";") |>
  select(CODMUNRES, DTNASC, IDADEMAE, QTDPARTNOR, QTDPARTCES)

### Baixando os dados preliminares de 2024 e selecionando as variáveis de interesse
df_sinasc_preliminares_2024 <- fread("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SINASC/DNOPEN24.csv", sep = ";") |>
  select(CODMUNRES, DTNASC, IDADEMAE, QTDPARTNOR, QTDPARTCES)

### Juntando os dados preliminares em uma única base
df_sinasc_preliminares <- full_join(df_sinasc_preliminares_2023, df_sinasc_preliminares_2024) |>
  mutate(CODMUNRES = as.character(CODMUNRES))

### Removendo arquivos já utilizados e limpando a memória
rm(df_sinasc_preliminares_2023, df_sinasc_preliminares_2024)
gc()

### Transformando algumas variáveis e criando as variáveis necessárias p/ o cálculo dos indicadores
df_bloco2_sinasc_preliminares <- df_sinasc_preliminares |>
  mutate(
    ano = as.numeric(substr(DTNASC, nchar(DTNASC) - 3, nchar(DTNASC))),
    IDADEMAE = as.numeric(IDADEMAE),
    QTDPARTNOR = as.numeric(QTDPARTNOR),
    QTDPARTCES = as.numeric(QTDPARTCES),
    .keep = "unused",
  ) |>
  mutate(
    total_de_nascidos_vivos = 1,
    nvm_menor_que_20 = if_else(IDADEMAE < 20, 1, 0, missing = 0),
    mulheres_com_mais_de_tres_partos_anteriores = if_else(
      QTDPARTNOR > 3 | (QTDPARTNOR > 2 & QTDPARTCES > 0) |
        (QTDPARTNOR > 1  &  QTDPARTCES > 1) |
        (QTDPARTNOR > 0  &  QTDPARTCES > 2) |
        QTDPARTCES > 3,
      1, 0, missing = 0
    ),
    .keep = "unused"
  ) |>
  clean_names() |>
  group_by(codmunres, ano) |>
  summarise_at(vars(-group_cols()), sum) |>
  ungroup() |>
  # Juntando com a base aulixiar de municípios
  right_join(df_aux_municipios)

### Substituindo todos os NAs, gerados após o right_join, por 0
df_bloco2_sinasc_preliminares[is.na(df_bloco2_sinasc_preliminares)] <- 0


## Para as variáveis provenientes do Tabnet -----------------------------------
### Ainda não estão disponíveis para 2023 e 2024. Assim, essa parte é apenas um placeholder
df_bloco2_tabnet_preliminares <- df_aux_municipios |>
  mutate(
    pop_feminina_10_a_19 = NA,
    pop_fem_10_49 = NA,
    beneficiarias_10_a_49 = NA
  )


## Para os indicadores de aborto ----------------------------------------------
### Para os numeradores -------------------------------------------------------
#### Criando um vetor com os anos considerados
anos <- c(2023:2024)

#### Criando um vetor com as siglas de todos os estados do Brasil
estados <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA",
             "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN",
             "RS", "RO", "RR", "SC", "SP", "SE", "TO")

#### Definindo as CIDs de aborto
cids_aborto <- c("O03", "O030", "O031", "O032", "O033", "O034", "O035", "O036", "O037", "O038", "O039",
                 "O04", "O040", "O041", "O042", "O043", "O044", "O045", "O046", "O047", "O048", "O049",
                 "O05", "O050", "O051", "O052", "O053", "O054", "O055", "O056", "O057", "O058", "O059",
                 "O06", "O060", "O061", "O062", "O063", "O064", "O065", "O066", "O067", "O068", "O069",
                 "O07", "O070", "O071", "O072", "O073", "O074", "O075", "O076", "O077", "O078", "O079",
                 "O08", "O080", "O081", "O082", "O083", "O084", "O085", "O086", "O087", "O088", "O089")

#### Definindo as CIDs de não aborto
cids_nao_aborto <- c("O00", "O000", "O001", "O002", "O008", "O009",
                     "O01", "O010", "O011", "O019",
                     "O02", "O020", "O021", "O028", "O029")

#### Para os dados da ANS -----------------------------------------------------
##### Ainda não estão disponíveis para 2023 e 2024
df_numerador_aborto_ans <- df_aux_municipios |>
  mutate(
    abortos_ans_menor_30 = NA,
    abortos_ans_30_a_39 = NA,
    abortos_ans_40_a_49 = NA
  )


#### Para os dados do SIH -----------------------------------------------------
##### Criando a base que guardará os dados de todos os anos
df_numerador_aborto_sus <- data.frame()

for (ano in anos) {
  # Baixando os dados do dado ano e criando uma coluna de ano
  dados <- fetch_datasus(
    year_start = ano,
    year_end = ano,
    month_start = 1,
    month_end = 12,
    information_system = "SIH-RD",
    vars = c(
      "ANO_CMPT", "MUNIC_RES", "DIAG_PRINC", "DIAG_SECUN", "CID_ASSO", "CID_NOTIF", "CID_MORTE",
      "DIAGSEC1", "DIAGSEC2", "DIAGSEC3", "DIAGSEC4", "DIAGSEC5", "DIAGSEC6",
      "DIAGSEC7", "DIAGSEC8", "DIAGSEC9", "SEXO", "COD_IDADE", "IDADE", "DT_INTER", "DT_SAIDA", "IDENT"
    )
  )

  # Criando as variáveis de ano e faixa etária e filtrando mulheres de 10 a 49 anos com IDENT != 5
  dados_filtrados <- dados |>
    mutate(
      ano = substr(DT_INTER, 1, 4),
      fet = case_when(
        COD_IDADE == 4 & IDADE >= 10 & IDADE <= 29 ~ "abortos_sus_menor_30",
        COD_IDADE == 4 & IDADE >= 30 & IDADE <= 39 ~ "abortos_sus_30_a_39",
        COD_IDADE == 4 & IDADE >= 40 & IDADE <= 49 ~ "abortos_sus_40_a_49",
        TRUE ~ "não" # Caso não caia em nenhuma das faixas
      )
    ) |>
    filter(
      fet != "não",
      SEXO == 3 | SEXO == 2,
      IDENT != 5 | !is.na(IDENT)
    ) |>
    mutate()

  # Filtrando casos com CID de aborto em alguma das colunas CID_1, CID_2, CID_3 ou CID_4
  dados_filtrados1 <- dados_filtrados |>
    filter(if_any(starts_with("CID_"), ~ . %in% cids_aborto) |
             if_any(starts_with("DIAG_"), ~ . %in% cids_aborto) |
             if_any(starts_with("DIAG"), ~ . %in% cids_aborto))

  # Excluir mulheres com CID de não aborto em alguma das colunas CID_1, CID_2, CID_3 ou CID_4
  dados_filtrados2 <- dados_filtrados1 |>
    filter(!if_any(starts_with("CID_"), ~ . %in% cids_nao_aborto))

  # Excluir mulheres com CID de não aborto em alguma das colunas CID_1, CID_2, CID_3 ou CID_4
  dados_filtrados3 <- dados_filtrados2 |>
    filter(!if_any(starts_with("DIAG"), ~ . %in% cids_nao_aborto))

  # Agrupando por município e passando para o formato wide
  dados_filtrados_wide <- dados_filtrados3 |>
    group_by(MUNIC_RES, ano, fet) |>
    summarise(cont = n()) |>
    ungroup() |>
    pivot_wider(names_from = fet, values_from = cont, values_fill = 0) |>
    rename(codmunres = MUNIC_RES)

  # Juntando com o restante dos dados
  df_numerador_aborto_sus <- bind_rows(df_numerador_aborto_sus, dados_filtrados_wide)

  # Limpando a memória
  rm(dados, dados_filtrados1, dados_filtrados2, dados_filtrados3, dados_filtrados_wide)
  gc()
}

##### Juntando com a base aulixiar de municípios
df_numerador_aborto_sus <- left_join(
  df_aux_municipios,
  df_numerador_aborto_sus |> mutate(codmunres = as.character(codmunres), ano = as.numeric(ano))
)

##### Substituindo todos os NAs, gerados após o right_join, por 0
df_numerador_aborto_sus[is.na(df_numerador_aborto_sus)] <- 0


#### Juntando os dados das duas bases -----------------------------------------
df_numerador_aborto_preliminares <- full_join(df_numerador_aborto_ans, df_numerador_aborto_sus)


### Para os denominadores -----------------------------------------------------
#### Como dependem do Tabnet, ainda não estão disponíveis para 2023 e 2024
df_denominador_aborto_preliminares <- df_aux_municipios |>
  mutate(
    pop_fem_ans_10_49 = NA,
    pop_fem_sus_10_49 = NA,
    total_de_nascidos_vivos_ans = NA,
    total_de_nascidos_vivos_sus = NA
  ) |>
  select(codmunres, ano, pop_fem_ans_10_49, pop_fem_sus_10_49, total_de_nascidos_vivos_ans, total_de_nascidos_vivos_sus)


### Juntando os dados dos numeradores e dos denominadores ---------------------
df_bloco2_aborto_preliminares <- full_join(df_numerador_aborto_preliminares, df_denominador_aborto_preliminares)


## Juntando todos os dados baixados -------------------------------------------
df_bloco2_preliminares <- full_join(
  full_join(df_bloco2_sinasc_preliminares, df_bloco2_tabnet_preliminares),
  df_bloco2_aborto_preliminares
)


## Juntando os dados preliminares com os dados consolidados -------------------
### Lendo o arquivo com os dados consolidados
df_bloco2_consolidados <- read.csv(
  "databases_consolidadas/indicadores_bloco2_planejamento_reprodutivo_SUS_ANS_2012_2022.csv"
)

### Juntando as duas bases
df_bloco2_final <- full_join(
  df_bloco2_consolidados |> mutate(codmunres = as.character(codmunres)),
  df_bloco2_preliminares
) |>
  arrange(codmunres, ano)


## Salvando a base de dados final ---------------------------------------------
### Excluindo todos os arquivos baixados
unlink("ANS/", recursive = TRUE)

### Voltando para o diretório original do projeto
setwd(diretorio_original)

### Exportando os dados
write.csv(
  df_bloco2_final,
  "data-raw/csv/indicadores_bloco2_planejamento_reprodutivo_SUS_ANS_2012_2024.csv",
  row.names = FALSE
)

