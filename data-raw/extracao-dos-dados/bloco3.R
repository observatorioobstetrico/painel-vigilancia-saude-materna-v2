library(tidyverse)
library(httr)
library(janitor)
library(getPass)
library(repr)
library(data.table)
library(readr)
library(openxlsx)
library(tidyr)
library(readxl)

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

#Lendo o arquivo com os dados de 2012 a 2020, que utilizamos no painel original
df_bloco3_antigo <- read.csv("data-raw/extracao-dos-dados/databases-antigas/indicadores_bloco3_assistencia_pre-natal_2012-2020.csv") |>
  clean_names()

#Criando um objeto que recebe os códigos dos municípios que utilizamos no painel
codigos_municipios <- read.csv("data-raw/extracao-dos-dados/databases-antigas/tabela_aux_municipios.csv") |>
  pull(codmunres)

#Criando um data.frame auxiliar que possui uma linha para cada combinação de município e ano
df_aux_municipios <- data.frame(codmunres = rep(codigos_municipios, each = length(2012:2021)), ano = 2012:2021)

#Criando o data.frame que irá receber todos os dados do bloco 3
df_bloco3 <- data.frame()

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
df_bloco3 <- left_join(df_aux_municipios, df)

##Substituindo os NA's da coluna 'total_de_nascidos_vivos' por 0 (os NA's surgem quando um município não apresentou nascidos vivos num dado ano)
df_bloco3$total_de_nascidos_vivos[is.na(df_bloco3$total_de_nascidos_vivos)] <- 0


# Cobertura de assistência Pré-natal ----------------------
df <- dataframe <- data.frame()

for (estado in estados){

  params = paste0('{
      "token": {
        "token": "',token,'"
      },
      "sql": {
        "sql": {"query": "SELECT res_codigo_adotado, ano_nasc, COUNT(1)',
                  ' FROM \\"datasus-sinasc\\"',
                  ' WHERE (res_SIGLA_UF = \'',estado,'\' AND ano_nasc >= 2012)',
                  ' AND (CONSPRENAT >= 1) ',
                  ' GROUP BY res_codigo_adotado, ano_nasc",
                        "fetch_size": 65000}
      }
    }')

  request <- POST(url = endpoint, body = params, encode = "form")
  dataframe <- convertRequestToDF(request)
  names(dataframe) <- c('codmunres', 'ano', 'mulheres_com_pelo_menos_uma_consulta_prenatal')
  df <- rbind(df, dataframe)

  repeat {

    cursor <- content(request)$cursor

    params = paste0('{
          "token": {
            "token": "',token,'"
          },
          "sql": {
            "sql": {"query":"SELECT res_codigo_adotado, ano_nasc, COUNT(1)',
                    ' FROM \\"datasus-sinasc\\"',
                    ' WHERE (res_SIGLA_UF = \'',estado,'\' AND ano_nasc >= 2012)',
                    ' AND (CONSPRENAT >= 1) ',
                    ' GROUP BY res_codigo_adotado, ano_nasc",
                           "fetch_size": 65000, "cursor": "',cursor,'"}
          }
        }')


    request <- POST(url = endpoint, body = params, encode = "form")

    if (length(content(request)$rows) == 0)
      break
    else print("oi")

    dataframe <- convertRequestToDF(request)
    names(dataframe) <- c('codmunres', 'ano', 'mulheres_com_pelo_menos_uma_consulta_prenatal')
    df <- rbind(df, dataframe)
  }
}

head(df)

##Transformando as colunas que estão em caracter para numéricas
df <- df |> mutate_if(is.character, as.numeric)

##Juntando com o restante da base do bloco 3
df_bloco3 <- left_join(df_bloco3, df)

##Substituindo os NA's da coluna 'mulheres_com_pelo_menos_uma_consulta_prenatal' por 0 (gerados após o left_join)
df_bloco3$mulheres_com_pelo_menos_uma_consulta_prenatal[is.na(df_bloco3$mulheres_com_pelo_menos_uma_consulta_prenatal)] <- 0


# Proporção de mulheres com início precoce do pré-natal (até o 3 mês de gestação) ---------------
df <- dataframe <- data.frame()

for (estado in estados){

  params = paste0('{
      "token": {
        "token": "',token,'"
      },
      "sql": {
        "sql": {"query": "SELECT res_codigo_adotado, ano_nasc, COUNT(1)',
                  ' FROM \\"datasus-sinasc\\"',
                  ' WHERE (res_SIGLA_UF = \'',estado,'\' AND ano_nasc >= 2012)',
                  ' AND (MESPRENAT =  1 OR MESPRENAT =2 OR MESPRENAT =  3)  ',
                  ' GROUP BY res_codigo_adotado, ano_nasc",
                        "fetch_size": 65000}
      }
    }')

  request <- POST(url = endpoint, body = params, encode = "form")
  dataframe <- convertRequestToDF(request)
  names(dataframe) <- c('codmunres', 'ano', 'mulheres_com_inicio_precoce_do_prenatal')
  df <- rbind(df, dataframe)

  repeat {

    cursor <- content(request)$cursor

    params = paste0('{
          "token": {
            "token": "',token,'"
          },
          "sql": {
            "sql": {"query":"SELECT res_codigo_adotado, ano_nasc, COUNT(1)',
                    ' FROM \\"datasus-sinasc\\"',
                    ' WHERE (res_SIGLA_UF = \'',estado,'\' AND ano_nasc >=2012)',
                    ' AND (MESPRENAT =  1 OR MESPRENAT =2 OR MESPRENAT =  3) ',
                    ' GROUP BY res_codigo_adotado, ano_nasc",
                           "fetch_size": 65000, "cursor": "',cursor,'"}
          }
        }')


    request <- POST(url = endpoint, body = params, encode = "form")

    if (length(content(request)$rows) == 0)
      break
    else print("oi")

    dataframe <- convertRequestToDF(request)
    names(dataframe) <- c('codmunres', 'ano', 'mulheres_com_inicio_precoce_do_prenatal')
    df <- rbind(df, dataframe)
  }
}

head(df)

##Transformando as colunas que estão em caracter para numéricas
df <- df |> mutate_if(is.character, as.numeric)

##Juntando com o restante da base do bloco 3
df_bloco3 <- left_join(df_bloco3, df)

##Substituindo os NA's da coluna 'mulheres_com_inicio_precoce_do_prenatal' por 0 (gerados após o left_join)
df_bloco3$mulheres_com_inicio_precoce_do_prenatal[is.na(df_bloco3$mulheres_com_inicio_precoce_do_prenatal)] <- 0


# Proporção de mulheres com mais de sete consultas de pré-natal ------------------
df <- dataframe <- data.frame()

for (estado in estados){

  params = paste0('{
      "token": {
        "token": "',token,'"
      },
      "sql": {
        "sql": {"query": "SELECT res_codigo_adotado, ano_nasc, COUNT(1)',
                  ' FROM \\"datasus-sinasc\\"',
                  ' WHERE (res_SIGLA_UF = \'',estado,'\' AND ano_nasc >= 2012)',
                  ' AND (CONSPRENAT > 7)  ',
                  ' GROUP BY res_codigo_adotado, ano_nasc",
                        "fetch_size": 65000}
      }
    }')

  request <- POST(url = endpoint, body = params, encode = "form")
  dataframe <- convertRequestToDF(request)
  names(dataframe) <- c('codmunres', 'ano', 'mulheres_com_mais_de_sete_consultas_prenatal')
  df <- rbind(df, dataframe)

  repeat {

    cursor <- content(request)$cursor

    params = paste0('{
          "token": {
            "token": "',token,'"
          },
          "sql": {
            "sql": {"query":"SELECT res_codigo_adotado, ano_nasc, COUNT(1)',
                    ' FROM \\"datasus-sinasc\\"',
                    ' WHERE (res_SIGLA_UF = \'',estado,'\' AND ano_nasc >= 2012)',
                    ' AND (CONSPRENAT > 7 ) ',
                    ' GROUP BY res_codigo_adotado, ano_nasc",
                           "fetch_size": 65000, "cursor": "',cursor,'"}
          }
        }')


    request <- POST(url = endpoint, body = params, encode = "form")

    if (length(content(request)$rows) == 0)
      break
    else print("oi")

    dataframe <- convertRequestToDF(request)
    names(dataframe) <- c('codmunres', 'ano', 'mulheres_com_mais_de_sete_consultas_prenatal')
    df <- rbind(df, dataframe)
  }
}

head(df)

##Transformando as colunas que estão em caracter para numéricas
df <- df |> mutate_if(is.character, as.numeric)

##Juntando com o restante da base do bloco 3
df_bloco3 <- left_join(df_bloco3, df)

##Substituindo os NA's da coluna 'mulheres_com_mais_de_sete_consultas_prenatal' por 0 (gerados após o left_join)
df_bloco3$mulheres_com_mais_de_sete_consultas_prenatal[is.na(df_bloco3$mulheres_com_mais_de_sete_consultas_prenatal)] <- 0


# Porcentagem de mulheres com número de consultas de pré-natal adequado ---------------------------
df <- dataframe <- data.frame()

for (estado in estados){

  params = paste0('{
      "token": {
        "token": "',token,'"
      },
      "sql": {
        "sql": {"query":" SELECT CODMUNRES, ano_nasc, CONSPRENAT, SEMAGESTAC, COUNT(1)',
                  ' FROM \\"datasus-sinasc\\"',
                  ' WHERE (res_SIGLA_UF = \'',estado,'\' AND ano_nasc >= 2012 AND ano_nasc <= 2021 AND CONSPRENAT < 99) ',
                  ' GROUP BY CODMUNRES, ano_nasc, CONSPRENAT, SEMAGESTAC",
                        "fetch_size": 65000}
      }
    }')

  request <- POST(url = endpoint, body = params, encode = "form")
  dataframe <- convertRequestToDF(request)
  names(dataframe) <- c("codmunres", "ano", "consprenat", "semagestac", "nascidos")
  df <- rbind(df, dataframe)

  repeat {

    cursor <- content(request)$cursor

    params = paste0('{
          "token": {
            "token": "',token,'"
          },
          "sql": {
            "sql": {"query":" SELECT CODMUNRES, ano_nasc, CONSPRENAT, SEMAGESTAC, COUNT(1)',
                    ' FROM \\"datasus-sinasc\\"',
                    ' WHERE (res_SIGLA_UF = \'',estado,'\' AND ano_nasc >= 2012 AND ano_nasc <= 2021 AND CONSPRENAT < 99)',
                    ' GROUP BY CODMUNRES, ano_nasc, CONSPRENAT, SEMAGESTAC",
                            "fetch_size": 65000, "cursor": "',cursor,'"}
                            }
                    }')


    request <- POST(url = endpoint, body = params, encode = "form")

    if (length(content(request)$rows) == 0)
      break
    else print("oi")

    request <- POST(url = endpoint, body = params, encode = "form")
    dataframe <- convertRequestToDF(request)
    names(dataframe) <- c("codmunres", "ano", "consprenat", "semagestac", "nascidos")
    df <- rbind(df, dataframe)
  }
}

head(df)

##Transformando as colunas que estão em caracter para numéricas e criando a variável de consultas adequadas
df <- df |>
  mutate_if(is.character, as.numeric) |>
  mutate(
    mulheres_com_consultas_prenatal_adequadas = dplyr::if_else(
      (semagestac < 20 & consprenat >= 1) |
        (semagestac >= 20 & semagestac < 26 & consprenat >= 2) |
        (semagestac >= 26 & semagestac < 30 & consprenat >= 3) |
        (semagestac >= 30 & semagestac < 34 & consprenat >= 4) |
        (semagestac >= 34 & semagestac < 36 & consprenat >= 5) |
        (semagestac >= 36 & semagestac < 38 & consprenat >= 6) |
        (semagestac >= 38 & semagestac < 40 & consprenat >= 7) |
        (semagestac >= 40 & consprenat >= 8),
      nascidos,
      0
    )
  ) |>
  group_by(codmunres, ano) |>
  summarise(mulheres_com_consultas_prenatal_adequadas = sum(mulheres_com_consultas_prenatal_adequadas, na.rm = TRUE)) |>
  ungroup()

##Juntando com o restante da base do bloco 3
df_bloco3 <- left_join(df_bloco3, df)

##Substituindo os NA's da coluna 'mulheres_com_consultas_prenatal_adequadas' por 0 (gerados após o left_join)
df_bloco3$mulheres_com_consultas_prenatal_adequadas[is.na(df_bloco3$mulheres_com_consultas_prenatal_adequadas)] <- 0


# Incidência de sífilis congênita por mil nascidos vivos ------------------
##Lendo a base de dados obtida pelo site http://indicadoressifilis.aids.gov.br/
df_sifilis_excel <- read_excel(
  "data-raw/extracao-dos-dados/databases-antigas/dados_painel_sifilis_2022.xlsx",
  sheet = "DADOS CONTINUAÇÃO 2"
)

#Corrigindo os nomes das colunas e filtrando pelos municípios que utilizamos no painel
names(df_sifilis_excel) <- as.character(df_sifilis_excel[1,])

df_sifilis <- df_sifilis_excel[-1, ] |>
  clean_names() |>
  select(
    codmunres = codigo,
    starts_with("sifilis_congenita_em_menores_de_um_ano_2")
  ) |>
  rename_with(
    str_sub, start = -4, starts_with("sifilis_congenita_em_menores_de_um_ano_2")
  ) |>
  filter(codmunres %in% df_aux_municipios$codmunres) |>
  mutate_if(is.character, as.numeric)

#Passando para o formato long
df_sifilis_long <- df_sifilis |>
  pivot_longer(
    cols = !codmunres,
    names_to = "ano",
    values_to = "casos_sc"
  ) |>
  filter(ano <= 2021) |>
  mutate_if(is.character, as.numeric)

##Juntando com o restante da base do bloco 3
df_bloco3 <- left_join(df_bloco3, df_sifilis_long)

##Substituindo os NA's da coluna 'casos_sc' por 0 (gerados após o left_join)
df_bloco3$casos_sc[is.na(df_bloco3$casos_sc)] <- 0


# Verificando se os dados novos e antigos estão batendo -------------------
sum(df_bloco3 |> filter(ano < 2021) |> pull(total_de_nascidos_vivos)) - sum(df_bloco3_antigo$total_de_nascidos_vivos)
sum(df_bloco3 |> filter(ano < 2021) |> pull(mulheres_com_pelo_menos_uma_consulta_prenatal )) - sum(df_bloco3_antigo$mulheres_com_pelo_menos_uma_consulta_prenatal )
sum(df_bloco3 |> filter(ano < 2021) |> pull(mulheres_com_inicio_precoce_do_prenatal)) - sum(df_bloco3_antigo$mulheres_com_inicio_precoce_do_prenatal)
sum(df_bloco3 |> filter(ano < 2021) |> pull(mulheres_com_mais_de_sete_consultas_prenatal )) - sum(df_bloco3_antigo$mulheres_com_mais_de_sete_consultas_prenatal )
sum(df_bloco3 |> filter(ano < 2021) |> pull(casos_sc)) - sum(df_bloco3_antigo$casos_sc)
##Os dados de sífilis não batem, mas acreditamos que seja porque a base foi atualizada após o cálculo desse
##indicador na versão v1 do painel


# Salvando a base de dados completa na pasta data-raw/csv -----------------
write.csv(df_bloco3, "data-raw/csv/indicadores_bloco3_assistencia_pre-natal_2012-2021.csv", row.names = FALSE)
