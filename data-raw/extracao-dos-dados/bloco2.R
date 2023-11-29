library(tidyverse)
library(httr)
library(janitor)
library(getPass)
library(repr)
library(data.table)
library(readr)
library(openxlsx)
library(microdatasus)

token = getPass()  #Token de acesso à API da PCDaS (todos os arquivos gerados se encontram na pasta "Databases", no Google Drive)

url_base = "https://bigdata-api.fiocruz.br"

convertRequestToDF <- function(request){
  variables = unlist(content(request)$columns)
  variables = variables[names(variables) == "name"]
  column_names <- unname(variables)
  values = content(request)$rows
  df <- as.data.frame(do.call(rbind,lapply(values,function(r) {
    row <- r
    row[sapply(row, is.null)] <- NA
    rbind(unlist(row))
  } )))
  names(df) <- column_names
  return(df)
}

estados <- c('RO','AC','AM','RR','PA','AP','TO','MA','PI','CE','RN','PB','PE','AL','SE','BA','MG','ES','RJ','SP','PR','SC','RS','MS','MT','GO','DF')
endpoint <- paste0(url_base,"/","sql_query")

#Criando um objeto que recebe os códigos dos municípios que utilizamos no painel
codigos_municipios <- read.csv("data-raw/extracao-dos-dados/databases-antigas/tabela_aux_municipios.csv") |>
  pull(codmunres)

#Criando um data.frame auxiliar que possui uma linha para cada combinação de município e ano
df_aux_municipios <- data.frame(codmunres = rep(codigos_municipios, each = length(2012:2021)), ano = 2012:2021)

#Lendo o arquivo com os dados de 2012 a 2020, que utilizamos no painel original
df_bloco2_antigo <- read.csv2("data-raw/extracao-dos-dados/databases-antigas/indicadores_bloco2_planejamento_reprodutivo_SUS_ANS_2012_2020.csv") |>
  clean_names() |>
  mutate_if(is.character, as.numeric) |>
  right_join(df_aux_municipios |> filter(ano < 2021))

#Criando o data.frame que irá receber todos os dados do bloco 1
df_bloco2 <- data.frame()


# Total de nascidos vivos -------------------------------------------------
df <- dataframe <- data.frame()

for (estado in estados){

  params = paste0('{
      "token": {
        "token": "',token,'"
      },
      "sql": {
        "sql": {"query": "SELECT CODMUNRES, ano_nasc, COUNT(1)',
                  ' FROM \\"datasus-sinasc\\"',
                  ' WHERE (res_SIGLA_UF = \'',estado,'\' AND ano_nasc >= 2012)',
                  ' GROUP BY CODMUNRES, ano_nasc",
                        "fetch_size": 65000}
      }
    }')

  request <- POST(url = endpoint, body = params, encode = "form")
  dataframe <- convertRequestToDF(request)
  names(dataframe) <- c('codmunres', 'ano', 'total_de_nascidos_vivos')
  df <- rbind(df, dataframe)

  repeat {

    cursor <- content(request)$cursor

    params = paste0('{
          "token": {
            "token": "',token,'"
          },
          "sql": {
            "sql": {"query":"SELECT CODMUNRES, ano_nasc, COUNT(1)',
                    ' FROM \\"datasus-sinasc\\"',
                    ' WHERE (res_SIGLA_UF = \'',estado,'\' AND ano_nasc >= 2012)',
                    ' GROUP BY CODMUNRES, ano_nasc",
                           "fetch_size": 65000, "cursor": "',cursor,'"}
          }
        }')


    request <- POST(url = endpoint, body = params, encode = "form")

    if (length(content(request)$rows) == 0)
      break
    else print("oi")

    dataframe <- convertRequestToDF(request)
    names(dataframe) <- c('codmunres', 'ano', 'total_de_nascidos_vivos')
    df <- rbind(df, dataframe)
  }
}
head(df)

##Transformando as colunas que estão em caracter para numéricas
df <- df |> mutate_if(is.character, as.numeric)

##Fazendo um left_join da base auxiliar de municípios com o data.frame que contém o total de nascidos vivos
df_bloco2 <- left_join(df_aux_municipios, df)

##Substituindo os NA's da coluna 'total_de_nascidos_vivos' por 0 (os NA's surgem quando um município não apresentou nascidos vivos num dado ano)
df_bloco2$total_de_nascidos_vivos[is.na(df_bloco2$total_de_nascidos_vivos)] <- 0


# Proporção de nascidos vivos de mulheres com idade inferior a 20 anos (gestação na adolescência) ----------------------
##Os dados da PCDaS e do microdatasus diferem; optamos por deixar os do microdatasus nos dois
df_microdatasus_aux <- fetch_datasus(
  year_start = 2012,
  year_end = 2021,
  vars = c("CODMUNRES", "DTNASC", "IDADEMAE"),
  information_system = "SINASC"
) |>
  clean_names()

df_microdatasus <- df_microdatasus_aux |>
  filter(codmunres %in% df_municipios_aux$codmunres) |>
  mutate(
    ano = as.numeric(substr(dtnasc, 5, 8)),
    nvm_menor_que_20 = 1,
    .keep = "unused"
  ) |>
  filter(idademae < 20) |>
  group_by(codmunres, ano) |>
  summarise(nvm_menor_que_20 = sum(nvm_menor_que_20))

##Juntando com o restante da base do bloco 2
df_bloco2 <- left_join(df_bloco2, df_microdatasus)

##Substituindo os NA's da coluna 'nvm_menor_que_20' por 0 (gerados após o left_join)
df_bloco2$nvm_menor_que_20[is.na(df_bloco2$nvm_menor_que_20)] <- 0


# População feminina de 10 a 19 anos --------------------------------------
##Criando a função que utiliza web scrapping para pegar dados de estimativas populacionais do Tabnet DATASUS
est_pop_tabnet <- function (linha = "Município", coluna = "Não ativa", conteudo = 1, periodo = 2021, regiao = "Todas as categorias",
                            unidade_da_federacao = "Todas as categorias", sexo = "Feminino", faixa_etaria = c("10 a 14 anos", "15 a 19 anos", "20 a 29 anos", "30 a 39 anos", "40 a 49 anos"),
                            faixa_etaria_reajuste = "Todas as categorias")
{
  page <- xml2::read_html("http://tabnet.datasus.gov.br/cgi/tabcgi.exe?ibge/cnv/popsvsbr.def")

  linha.df <- data.frame(id = page |> rvest::html_elements("#L option") |>
                           rvest::html_text() |> trimws(), value = page |> rvest::html_elements("#L option") |>
                           rvest::html_attr("value"))
  linha.df[] <- lapply(linha.df, as.character)
  coluna.df <- data.frame(id = page |> rvest::html_elements("#C option") |>
                            rvest::html_text() |> trimws(), value = page |> rvest::html_elements("#C option") |>
                            rvest::html_attr("value"))
  coluna.df[] <- lapply(coluna.df, as.character)

  conteudo.df <- data.frame(id1 = 1, id2 = "População_residente", value = "População_residente")

  periodo.df <- data.frame(id = page |> rvest::html_elements("#A option") |>
                             rvest::html_text() |> as.numeric(), value = page |>
                             rvest::html_elements("#A option") |> rvest::html_attr("value"))
  regiao.df <- suppressWarnings(data.frame(id = page |> rvest::html_elements("#S1 option") |>
                                             rvest::html_text() |> readr::parse_number(), value = page |>
                                             rvest::html_elements("#S1 option") |> rvest::html_attr("value")))
  unidade_da_federacao.df <- suppressWarnings(data.frame(id = page |>
                                                           rvest::html_elements("#S2 option") |> rvest::html_text() |>
                                                           trimws(), value = page |> rvest::html_elements("#S2 option") |>
                                                           rvest::html_attr("value")))
  unidade_da_federacao.df[] <- lapply(unidade_da_federacao.df,
                                      as.character)
  sexo.df <- data.frame(id = page |> rvest::html_elements("#S16 option") |>
                          rvest::html_text() |> trimws(), value = page |> rvest::html_elements("#S16 option") |>
                          rvest::html_attr("value"))
  sexo.df[] <- lapply(sexo.df, as.character)
  faixa_etaria.df <- data.frame(id = page |> rvest::html_elements("#S17 option") |>
                                  rvest::html_text() |> trimws(), value = page |> rvest::html_elements("#S17 option") |>
                                  rvest::html_attr("value"))
  faixa_etaria.df[] <- lapply(faixa_etaria.df, as.character)
  faixa_etaria_reajuste.df <- data.frame(id = page |> rvest::html_elements("#S18 option") |>
                                           rvest::html_text() |> trimws(), value = page |> rvest::html_elements("#S18 option") |>
                                           rvest::html_attr("value"))
  faixa_etaria_reajuste.df[] <- lapply(faixa_etaria_reajuste.df, as.character)
  regiao.df$id[1] <- unidade_da_federacao.df$id[1] <- "Todas as categorias"
  sexo.df$id[1] <- faixa_etaria.df$id[1] <- faixa_etaria_reajuste.df$id[1] <- "Todas as categorias"
  if (linha != "Unidade da Federação") {
    if (!is.character(linha))
      stop("The 'linha' argument must be a character element")
    if (length(linha) != 1)
      stop("The 'linha' argument must have only one element")
    if (!(all(linha %in% linha.df$id))) {
      if (!(all(linha %in% linha.df$value))) {
        stop("The 'linha' argument is misspecified")
      }
    }
  }
  if (coluna != "Não ativa") {
    if (!is.character(coluna))
      stop("The 'coluna' argument must be a character element")
    if (length(coluna) != 1)
      stop("The 'coluna' argument must have only one element")
    if (!(all(coluna %in% coluna.df$id))) {
      if (!(all(coluna %in% coluna.df$value))) {
        stop("The 'coluna' argument is misspecified")
      }
    }
  }
  if (conteudo != 1 & conteudo != 2 & conteudo != 3) {
    if (is.numeric(conteudo))
      stop("The only numeric elements allowed are 1 or 2")
    if (length(conteudo) != 1)
      stop("The 'coluna' argument must have only one element")
    if (!(all(conteudo %in% conteudo.df$id2))) {
      if (!(all(conteudo %in% conteudo.df$value))) {
        stop("The 'conteudo' argument is misspecified")
      }
    }
  }
  if (periodo[1] != "last") {
    if (is.character(periodo)) {
      periodo <- as.numeric(periodo)
    }
    if (!(all(periodo %in% periodo.df$id)))
      stop("The 'periodo' argument is misspecified")
  }
  if (any(regiao != "Todas as categorias")) {
    regiao <- as.character(regiao)
    if (!(all(regiao %in% regiao.df$id)))
      stop("Some element in 'regiao' argument is wrong")
  }
  if (any(unidade_da_federacao != "Todas as categorias")) {
    unidade_da_federacao <- as.character(unidade_da_federacao)
    if (!(all(unidade_da_federacao %in% unidade_da_federacao.df$id)))
      stop("Some element in 'unidade_da_federacao' argument is wrong")
  }
  if (any(faixa_etaria != "Todas as categorias")) {
    if (!(all(faixa_etaria %in% faixa_etaria.df$id))) {
      faixa_etaria <- as.character(faixa_etaria)
      if (!(all(faixa_etaria %in% faixa_etaria.df$value))) {
        stop("Some element in 'faixa_etaria' argument is wrong")
      }
    }
  }
  if (any(faixa_etaria_reajuste != "Todas as categorias")) {
    if (!(all(faixa_etaria_reajuste %in% faixa_etaria_reajuste.df$id))) {
      faixa_etaria_reajuste <- as.character(faixa_etaria_reajuste)
      if (!(all(faixa_etaria_reajuste %in% faixa_etaria_reajuste.df$value))) {
        stop("Some element in 'faixa_etaria_reajuste' argument is wrong")
      }
    }
  }
  if (any(sexo != "Todas as categorias")) {
    if (!(all(sexo %in% sexo.df$id))) {
      sexo <- as.character(sexo)
      if (!(all(sexo %in% sexo.df$value))) {
        stop("Some element in 'sexo' argument is wrong")
      }
    }
  }
  if (linha %in% linha.df$id) {
    linha <- dplyr::filter(linha.df, linha.df$id %in% linha)
    linha <- linha$value
  }
  if (!stringi::stri_enc_isascii(linha)) {
    form_linha <- paste0("Linha=", stringi::stri_escape_unicode(linha))
  } else {
    form_linha <- paste0("Linha=", linha)
  }
  if (coluna %in% coluna.df$id) {
    coluna <- dplyr::filter(coluna.df, coluna.df$id %in%
                              coluna)
    coluna <- coluna$value
  }
  if (!stringi::stri_enc_isascii(coluna)) {
    form_coluna <- paste0("Coluna=", stringi::stri_escape_unicode(coluna))
  } else {
    form_coluna <- paste0("Coluna=", coluna)
  }
  form_conteudo <- conteudo.df$value[conteudo]
  if (!stringi::stri_enc_isascii(form_conteudo)) {
    form_conteudo <- paste0("Incremento=", stringi::stri_escape_unicode(form_conteudo))
  } else {
    form_conteudo <- paste0("Incremento=", form_conteudo)
  }
  form_periodo <- dplyr::filter(periodo.df, periodo.df$id %in%
                                  periodo)
  form_periodo <- paste0("Arquivos=", form_periodo$value, collapse = "&")
  form_pesqmes1 <- "pesqmes1=Digite+o+texto+e+ache+f%E1cil"
  form_regiao <- dplyr::filter(regiao.df, regiao.df$id %in%
                                 regiao)
  form_regiao <- paste0("SRegi%E3o=", form_regiao$value, collapse = "&")
  form_pesqmes2 <- "pesqmes2=Digite+o+texto+e+ache+f%E1cil"
  form_unidade_da_federacao <- dplyr::filter(unidade_da_federacao.df,
                                             unidade_da_federacao.df$id %in% unidade_da_federacao)
  form_unidade_da_federacao <- paste0("SUnidade_da_Federa%E7%E3o=",
                                      form_unidade_da_federacao$value, collapse = "&")
  form_pesqmes3 <- "pesqmes3=Digite+o+texto+e+ache+f%E1cil"
  form_faixa_etaria <- dplyr::filter(faixa_etaria.df, faixa_etaria.df$id %in%
                                       faixa_etaria)
  form_faixa_etaria <- paste0("SFaixa_Et%E1ria_1=", form_faixa_etaria$value,
                              collapse = "&")
  form_faixa_etaria_reajuste <- dplyr::filter(faixa_etaria_reajuste.df, faixa_etaria_reajuste.df$id %in%
                                                faixa_etaria_reajuste)
  form_faixa_etaria_reajuste <- paste0("SFaixa_Et%E1ria_2=", form_faixa_etaria_reajuste$value,
                                       collapse = "&")
  form_sexo <- dplyr::filter(sexo.df, sexo.df$id %in% sexo)
  form_sexo <- paste0("SSexo=", form_sexo$value, collapse = "&")
  form_data <- paste(form_linha, form_coluna, form_conteudo,
                     form_periodo, form_pesqmes1, form_regiao, form_pesqmes2,
                     form_unidade_da_federacao, form_pesqmes3, form_sexo,
                     form_faixa_etaria, form_faixa_etaria_reajuste, "formato=table&mostre=Mostra",
                     sep = "&")
  form_data <- gsub("\\\\u00", "%", form_data)
  site <- httr::POST(url = "http://tabnet.datasus.gov.br/cgi/tabcgi.exe?ibge/cnv/popsvsbr.def",
                     body = form_data)
  tabdados <- httr::content(site, encoding = "Latin1") |>
    rvest::html_elements(".tabdados tbody td") |> rvest::html_text() |>
    trimws()
  col_tabdados <- httr::content(site, encoding = "Latin1") |>
    rvest::html_elements("th") |> rvest::html_text() |> trimws()
  f1 <- function(x) x <- gsub("\\.", "", x)
  f2 <- function(x) x <- as.numeric(as.character(x))
  tabela_final <- as.data.frame(matrix(data = tabdados, nrow = length(tabdados)/length(col_tabdados),
                                       ncol = length(col_tabdados), byrow = TRUE))
  names(tabela_final) <- col_tabdados
  tabela_final[-1] <- lapply(tabela_final[-1], f1)
  tabela_final[-1] <- suppressWarnings(lapply(tabela_final[-1],
                                              f2))
  tabela_final
}

df_est_pop_fem_10_19 <- est_pop_tabnet(
  coluna = "Ano",
  periodo = as.character(2012:2021),
  faixa_etaria = c("10 a 14 anos", "15 a 19 anos")
)
head(df_est_pop_fem_10_19)

##Retirando a linha de "TOTAL"
df_est_pop_fem_10_19 <- df_est_pop_fem_10_19[-1, ]
head(df_est_pop_fem_10_19)

##Passando os dados para o formato long
df_est_pop_fem_10_19_long <- df_est_pop_fem_10_19 |>
  rename(
    municipio = Município
  ) |>
  mutate(
    codmunres = substr(municipio, start = 1, stop = 6),
    .before = municipio
  ) |>
  select(!municipio) |>
  pivot_longer(
    cols = !c(codmunres),
    names_to = "ano",
    values_to = "pop_feminina_10_a_19"
  ) |>
  mutate_if(is.character, as.numeric)

head(df_est_pop_fem_10_19_long)

##Juntando com o restante da base do bloco 2
df_bloco2 <- left_join(df_bloco2, df_est_pop_fem_10_19_long)


# Proporção de nascidos vivos de mulheres com mais de 3 partos anteriores ----------------------
df <- dataframe <- data.frame()

for (estado in estados){

  params = paste0('{
      "token": {
        "token": "',token,'"
      },
      "sql": {
        "sql": {"query": "SELECT CODMUNRES, ano_nasc, COUNT(1)',
                  ' FROM \\"datasus-sinasc\\"',
                  ' WHERE (res_SIGLA_UF = \'',estado,'\' AND ano_nasc >= 2012)',
                  ' AND (QTDPARTNOR > 3 OR (QTDPARTNOR > 2 AND QTDPARTCES > 0) ',
                  ' OR (QTDPARTNOR > 1 AND QTDPARTCES > 1) ',
                  ' OR (QTDPARTNOR > 0 AND QTDPARTCES > 2) ',
                  ' OR QTDPARTCES > 3)',
                  ' GROUP BY CODMUNRES, ano_nasc",
                        "fetch_size": 65000}
      }
    }')

  request <- POST(url = endpoint, body = params, encode = "form")
  dataframe <- convertRequestToDF(request)
  names(dataframe) <- c('codmunres', 'ano', 'mulheres_com_mais_de_tres_partos_anteriores')
  df <- rbind(df, dataframe)

  repeat {

    cursor <- content(request)$cursor

    params = paste0('{
          "token": {
            "token": "',token,'"
          },
          "sql": {
            "sql": {"query":"SELECT CODMUNRES, ano_nasc, COUNT(1)',
                    ' FROM \\"datasus-sinasc\\"',
                    ' WHERE (res_SIGLA_UF = \'',estado,'\' AND ano_nasc >= 2012)',
                    ' AND (QTDPARTNOR > 3 OR (QTDPARTNOR > 2 AND QTDPARTCES > 0) ',
                    ' OR (QTDPARTNOR > 1 AND QTDPARTCES > 1) ',
                    ' OR (QTDPARTNOR > 0 AND QTDPARTCES > 2) ',
                    ' OR QTDPARTCES > 3)',
                    ' GROUP BY CODMUNRES, ano_nasc",
                           "fetch_size": 65000, "cursor": "',cursor,'"}
          }
        }')


    request <- POST(url = endpoint, body = params, encode = "form")

    if (length(content(request)$rows) == 0)
      break
    else print("oi")

    dataframe <- convertRequestToDF(request)
    names(dataframe) <- c('codmunres', 'ano', 'mulheres_com_mais_de_tres_partos_anteriores')
    df <- rbind(df, dataframe)
  }
}
head(df)

##Transformando as colunas que estão em caracter para numéricas
df <- df |> mutate_if(is.character, as.numeric)

##Juntando com o restante da base do bloco 2
df_bloco2 <- left_join(df_bloco2, df)

##Substituindo os NA's da coluna 'mulheres_com_mais_de_tres_partos_anteriores' por 0 (gerados após o left_join)
df_bloco2$mulheres_com_mais_de_tres_partos_anteriores[is.na(df_bloco2$mulheres_com_mais_de_tres_partos_anteriores)] <- 0


# População feminina de 10 a 49 anos --------------------------------------
df_est_pop_fem_10_49 <- est_pop_tabnet(coluna = "Ano", periodo = as.character(2012:2021))
head(df_est_pop_fem_10_49)

##Retirando a linha de "TOTAL"
df_est_pop_fem_10_49 <- df_est_pop_fem_10_49[-1, ]
head(df_est_pop_fem_10_49)

##Passando os dados para o formato long
df_est_pop_fem_10_49_long <- df_est_pop_fem_10_49 |>
  rename(
    municipio = Município
  ) |>
  mutate(
    codmunres = substr(municipio, start = 1, stop = 6),
    .before = municipio
  ) |>
  select(!municipio) |>
  pivot_longer(
    cols = !c(codmunres),
    names_to = "ano",
    values_to = "pop_fem_10_49"
  ) |>
  mutate_if(is.character, as.numeric)

##Juntando com o restante da base do bloco 2
df_bloco2 <- left_join(df_bloco2, df_est_pop_fem_10_49_long)


# Abortos SUS menor 30 -------------------
##Ainda não disponíveis para o ano de 2021
df_abortos_sus_menor_30 <- df_bloco2_antigo |>
  select(codmunres, ano, abortos_sus_menor_30)

##Juntando com o restante da base do bloco 2
df_bloco2 <- left_join(df_bloco2, df_abortos_sus_menor_30)


# Abortos SUS 30 a 39 -------------------
##Ainda não disponíveis para o ano de 2021
df_abortos_sus_30_a_39 <- df_bloco2_antigo |>
  select(codmunres, ano, abortos_sus_30_a_39)

##Juntando com o restante da base do bloco 2
df_bloco2 <- left_join(df_bloco2, df_abortos_sus_30_a_39)


# Abortos SUS 40 a 49 -------------------
##Ainda não disponíveis para o ano de 2021
df_abortos_sus_40_a_49 <- df_bloco2_antigo |>
  select(codmunres, ano, abortos_sus_40_a_49)

##Juntando com o restante da base do bloco 2
df_bloco2 <- left_join(df_bloco2, df_abortos_sus_40_a_49)


# Abortos ANS menor 30 -------------------
##Ainda não disponíveis para o ano de 2021
df_abortos_ans_menor_30 <- df_bloco2_antigo |>
  select(codmunres, ano, abortos_ans_menor_30)

##Juntando com o restante da base do bloco 2
df_bloco2 <- left_join(df_bloco2, df_abortos_ans_menor_30)


# Abortos ANS 30 a 39 -------------------
##Ainda não disponíveis para o ano de 2021
df_abortos_ans_30_a_39 <- df_bloco2_antigo |>
  select(codmunres, ano, abortos_ans_30_a_39)

##Juntando com o restante da base do bloco 2
df_bloco2 <- left_join(df_bloco2, df_abortos_ans_30_a_39)


# Abortos ANS 40 a 49 -------------------
##Ainda não disponíveis para o ano de 2021
df_abortos_ans_40_a_49 <- df_bloco2_antigo |>
  select(codmunres, ano, abortos_ans_40_a_49)

##Juntando com o restante da base do bloco 2
df_bloco2 <- left_join(df_bloco2, df_abortos_ans_40_a_49)


# Verificando se os dados novos e antigos estão batendo -------------------
sum(df_bloco2 |> filter(ano < 2021) |> pull(total_de_nascidos_vivos)) - sum(df_bloco2_antigo$total_de_nascidos_vivos)
sum(df_bloco2 |> filter(ano < 2021) |> pull(nvm_menor_que_20)) - sum(df_bloco2_antigo$nvm_menor_que_20)
sum(df_bloco2 |> filter(ano < 2021) |> pull(pop_feminina_10_a_19)) - sum(df_bloco2_antigo$pop_feminina_10_a_19)
sum(df_bloco2 |> filter(ano < 2021) |> pull(mulheres_com_mais_de_tres_partos_anteriores)) - sum(df_bloco2_antigo$mulheres_com_mais_de_tres_partos_anteriores)
sum(df_bloco2 |> filter(ano < 2021) |> pull(pop_fem_10_49)) - sum(df_bloco2_antigo$pop_fem_10_49)
sum(df_bloco2 |> filter(ano < 2021) |> pull(abortos_sus_menor_30)) - sum(df_bloco2_antigo$abortos_sus_menor_30)
sum(df_bloco2 |> filter(ano < 2021) |> pull(abortos_sus_30_a_39)) - sum(df_bloco2_antigo$abortos_sus_30_a_39)
sum(df_bloco2 |> filter(ano < 2021) |> pull(abortos_sus_40_a_49)) - sum(df_bloco2_antigo$abortos_sus_40_a_49)
sum(df_bloco2 |> filter(ano < 2021) |> pull(abortos_ans_menor_30)) - sum(df_bloco2_antigo$abortos_ans_menor_30)
sum(df_bloco2 |> filter(ano < 2021) |> pull(abortos_ans_30_a_39)) - sum(df_bloco2_antigo$abortos_ans_30_a_39)
sum(df_bloco2 |> filter(ano < 2021) |> pull(abortos_ans_40_a_49)) - sum(df_bloco2_antigo$abortos_ans_40_a_49)


# Salvando a base de dados completa na pasta data-raw/csv -----------------
write.csv(df_bloco2, "data-raw/csv/indicadores_bloco2_planejamento_reprodutivo_SUS_ANS_2012_2021.csv", row.names = FALSE)
