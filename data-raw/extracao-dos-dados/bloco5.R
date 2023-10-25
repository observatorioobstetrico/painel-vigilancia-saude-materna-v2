library(tidyverse)
library(httr)
library(janitor)
library(getPass)
library(repr)
library(data.table)
library(readr)
library(openxlsx)
library(tidyr)

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
df_bloco5_antigo <- read.csv("data-raw/extracao-dos-dados/databases-antigas/indicadores_bloco5_condicao_de_nascimento_2012-2020.csv") |>
  clean_names()

#Criando um objeto que recebe os códigos dos municípios que utilizamos no painel
codigos_municipios <- read.csv("data-raw/extracao-dos-dados/databases-antigas/tabela_aux_municipios.csv") |>
  pull(codmunres)

#Criando um data.frame auxiliar que possui uma linha para cada combinação de município e ano
df_aux_municipios <- data.frame(codmunres = rep(codigos_municipios, each = length(2012:2021)), ano = 2012:2021)

#Criando o data.frame que irá receber todos os dados do bloco 5
df_bloco5 <- data.frame()

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
df_bloco5 <- left_join(df_aux_municipios, df)

##Substituindo os NA's da coluna 'total_de_nascidos_vivos' por 0 (os NA's surgem quando um município não apresentou nascidos vivos num dado ano)
df_bloco5$total_de_nascidos_vivos[is.na(df_bloco5$total_de_nascidos_vivos)] <- 0

# Proporção de baixo peso ao nascer ----------------------
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
                  ' AND (PESO < 2500 ) ',
                  ' GROUP BY res_codigo_adotado, ano_nasc",
                        "fetch_size": 65000}
      }
    }')

  request <- POST(url = endpoint, body = params, encode = "form")
  dataframe <- convertRequestToDF(request)
  names(dataframe) <- c('codmunres', 'ano', 'nascidos_vivos_com_baixo_peso')
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
                    ' AND (PESO < 2500 ) ',
                    ' GROUP BY res_codigo_adotado, ano_nasc",
                           "fetch_size": 65000, "cursor": "',cursor,'"}
          }
        }')


    request <- POST(url = endpoint, body = params, encode = "form")

    if (length(content(request)$rows) == 0)
      break
    else print("oi")

    dataframe <- convertRequestToDF(request)
    names(dataframe) <- c('codmunres', 'ano', 'nascidos_vivos_com_baixo_peso')
    df <- rbind(df, dataframe)
  }
}


head(df)

##Transformando as colunas que estão em caracter para numéricas
df <- df |> mutate_if(is.character, as.numeric)

##Juntando com o restante da base do bloco 5
df_bloco5 <- left_join(df_bloco5, df)

##Substituindo os NA's da coluna 'nascidos_vivos_com_baixo_peso' por 0 (gerados após o left_join)
df_bloco5$nascidos_vivos_com_baixo_peso[is.na(df_bloco5$nascidos_vivos_com_baixo_peso)] <- 0


# Proporção de nascimentos prematuros -----------------
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
                  ' AND  (GESTACAO < 5 ) ',
                  ' GROUP BY res_codigo_adotado, ano_nasc",
                        "fetch_size": 65000}
      }
    }')

  request <- POST(url = endpoint, body = params, encode = "form")
  dataframe <- convertRequestToDF(request)
  names(dataframe) <- c('codmunres', 'ano', 'nascidos_vivos_prematuros')
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
                    ' AND (GESTACAO < 5 ) ',
                    ' GROUP BY res_codigo_adotado, ano_nasc",
                           "fetch_size": 65000, "cursor": "',cursor,'"}
          }
        }')


    request <- POST(url = endpoint, body = params, encode = "form")

    if (length(content(request)$rows) == 0)
      break
    else print("oi")

    dataframe <- convertRequestToDF(request)
    names(dataframe) <- c('codmunres', 'ano', 'nascidos_vivos_prematuros')
    df <- rbind(df, dataframe)
  }
}


head(df)

##Transformando as colunas que estão em caracter para numéricas
df <- df |> mutate_if(is.character, as.numeric)

##Juntando com o restante da base do bloco 5
df_bloco5 <- left_join(df_bloco5, df)

##Substituindo os NA's da coluna 'nascidos_vivos_prematuros' por 0 (gerados após o left_join)
df_bloco5$nascidos_vivos_prematuros[is.na(df_bloco5$nascidos_vivos_prematuros)] <- 0

# Proporção de nascimentos termo precoce  --------------------
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
                  ' AND (SEMAGESTAC = 37 OR SEMAGESTAC = 38 ) ',
                  ' GROUP BY res_codigo_adotado, ano_nasc",
                        "fetch_size": 65000}
      }
    }')

  request <- POST(url = endpoint, body = params, encode = "form")
  dataframe <- convertRequestToDF(request)
  names(dataframe) <- c('codmunres', 'ano', 'nascidos_vivos_termo_precoce')
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
                    ' AND (SEMAGESTAC = 37 OR SEMAGESTAC = 38 ) ',
                    ' GROUP BY res_codigo_adotado, ano_nasc",
                           "fetch_size": 65000, "cursor": "',cursor,'"}
          }
        }')


    request <- POST(url = endpoint, body = params, encode = "form")

    if (length(content(request)$rows) == 0)
      break
    else print("oi")

    dataframe <- convertRequestToDF(request)
    names(dataframe) <- c('codmunres', 'ano', 'nascidos_vivos_termo_precoce')
    df <- rbind(df, dataframe)
  }
}

head(df)

##Transformando as colunas que estão em caracter para numéricas
df <- df |> mutate_if(is.character, as.numeric)

##Juntando com o restante da base do bloco 5
df_bloco5 <- left_join(df_bloco5, df)

##Substituindo os NA's da coluna 'nascidos_vivos_termo_precoce' por 0 (gerados após o left_join)
df_bloco5$nascidos_vivos_termo_precoce[is.na(df_bloco5$nascidos_vivos_termo_precoce)] <- 0

# proporcao de nascidos vivos com menos de 1500g ------------------------
df <- dataframe <- data.frame()

for (estado in estados){

  params = paste0('{
      "token": {
        "token": "',token,'"
      },
      "sql": {
        "sql": {"query": "SELECT res_codigo_adotado, ano_nasc, COUNT(1)',
                  ' FROM \\"datasus-sinasc\\"',
                  ' WHERE (res_SIGLA_UF = \'',estado,'\' AND (ano_nasc >= 2012))',
                  ' AND (PESO < 1500) ',
                  ' GROUP BY res_codigo_adotado, ano_nasc",
                        "fetch_size": 65000}
      }
    }')

  request <- POST(url = endpoint, body = params, encode = "form")
  dataframe <- convertRequestToDF(request)
  names(dataframe) <- c('codmunres', 'ano', 'nascidos_vivos_peso_menor_1500')
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
                    ' WHERE (res_SIGLA_UF = \'',estado,'\' AND (ano_nasc >= 2012))',
                    ' AND (PESO < 1500 ) ',
                    ' GROUP BY res_codigo_adotado, ano_nasc",
                           "fetch_size": 65000, "cursor": "',cursor,'"}
          }
        }')


    request <- POST(url = endpoint, body = params, encode = "form")

    if (length(content(request)$rows) == 0)
      break
    else print("oi")

    dataframe <- convertRequestToDF(request)
    names(dataframe) <- c('codmunres', 'ano', 'nascidos_vivos_peso_menor_1500')
    df <- rbind(df, dataframe)
  }
}

head(df)

##Transformando as colunas que estão em caracter para numéricas
df <- df |> mutate_if(is.character, as.numeric)

##Juntando com o restante da base do bloco 5
df_bloco5 <- left_join(df_bloco5, df)

##Substituindo os NA's da coluna 'nascidos_vivos_peso_menor_1500' por 0 (gerados após o left_join)
df_bloco5$nascidos_vivos_peso_menor_1500[is.na(df_bloco5$nascidos_vivos_peso_menor_1500)] <- 0

# proporcao de nascidos vivos ENTRE 1500 E 1999g -----------------------
df <- dataframe <- data.frame()

for (estado in estados){

  params = paste0('{
      "token": {
        "token": "',token,'"
      },
      "sql": {
        "sql": {"query": "SELECT res_codigo_adotado, ano_nasc, COUNT(1)',
                  ' FROM \\"datasus-sinasc\\"',
                  ' WHERE (res_SIGLA_UF = \'',estado,'\' AND (ano_nasc >= 2012))',
                  ' AND (PESO >= 1500  AND PESO <= 1999) ',
                  ' GROUP BY res_codigo_adotado, ano_nasc",
                        "fetch_size": 65000}
      }
    }')

  request <- POST(url = endpoint, body = params, encode = "form")
  dataframe <- convertRequestToDF(request)
  names(dataframe) <- c('codmunres', 'ano', 'nascidos_vivos_peso_1500_a_1999')
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
                    ' WHERE (res_SIGLA_UF = \'',estado,'\' AND (ano_nasc >= 2012))',
                    ' AND (PESO >= 1500  AND PESO <= 1999) ',
                    ' GROUP BY res_codigo_adotado, ano_nasc",
                           "fetch_size": 65000, "cursor": "',cursor,'"}
          }
        }')


    request <- POST(url = endpoint, body = params, encode = "form")

    if (length(content(request)$rows) == 0)
      break
    else print("oi")

    dataframe <- convertRequestToDF(request)
    names(dataframe) <- c('codmunres', 'ano', 'nascidos_vivos_peso_1500_a_1999')
    df <- rbind(df, dataframe)
  }
}

head(df)

##Transformando as colunas que estão em caracter para numéricas
df <- df |> mutate_if(is.character, as.numeric)

##Juntando com o restante da base do bloco 5
df_bloco5 <- left_join(df_bloco5, df)

##Substituindo os NA's da coluna 'nascidos_vivos_peso_1500_a_1999' por 0 (gerados após o left_join)
df_bloco5$nascidos_vivos_peso_1500_a_1999[is.na(df_bloco5$nascidos_vivos_peso_1500_a_1999)] <- 0


# proporcao de nascidos vivos ENTRE 2000 E 2500g --------------------
df <- dataframe <- data.frame()

for (estado in estados){

  params = paste0('{
      "token": {
        "token": "',token,'"
      },
      "sql": {
        "sql": {"query": "SELECT res_codigo_adotado, ano_nasc, COUNT(1)',
                  ' FROM \\"datasus-sinasc\\"',
                  ' WHERE (res_SIGLA_UF = \'',estado,'\' AND (ano_nasc >= 2012))',
                  ' AND (PESO <= 2499 AND PESO >= 2000) ',
                  ' GROUP BY res_codigo_adotado, ano_nasc",
                        "fetch_size": 65000}
      }
    }')

  request <- POST(url = endpoint, body = params, encode = "form")
  dataframe <- convertRequestToDF(request)
  names(dataframe) <- c('codmunres', 'ano', 'nascidos_vivos_peso_2000_a_2499')
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
                    ' WHERE (res_SIGLA_UF = \'',estado,'\' AND (ano_nasc >= 2012))',
                    ' AND (PESO <= 2499 AND PESO >= 2000) ',
                    ' GROUP BY res_codigo_adotado, ano_nasc",
                           "fetch_size": 65000, "cursor": "',cursor,'"}
          }
        }')


    request <- POST(url = endpoint, body = params, encode = "form")

    if (length(content(request)$rows) == 0)
      break
    else print("oi")

    dataframe <- convertRequestToDF(request)
    names(dataframe) <- c('codmunres', 'ano', 'nascidos_vivos_peso_2000_a_2499')
    df <- rbind(df, dataframe)
  }
}

head(df)

##Transformando as colunas que estão em caracter para numéricas
df <- df |> mutate_if(is.character, as.numeric)

##Juntando com o restante da base do bloco 5
df_bloco5 <- left_join(df_bloco5, df)

##Substituindo os NA's da coluna 'nascidos_vivos_peso_2000_a_2499' por 0 (gerados após o left_join)
df_bloco5$nascidos_vivos_peso_2000_a_2499[is.na(df_bloco5$nascidos_vivos_peso_2000_a_2499)] <- 0


# proporcao de nascidos vivos menos de 28 semanas --------------
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
                  ' AND (SEMAGESTAC < 28) ',
                  ' GROUP BY res_codigo_adotado, ano_nasc",
                        "fetch_size": 65000}
      }
    }')

  request <- POST(url = endpoint, body = params, encode = "form")
  dataframe <- convertRequestToDF(request)
  names(dataframe) <- c('codmunres', 'ano', 'nascidos_vivos_menos_de_28_semanas')
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
                    ' AND (SEMAGESTAC < 28) ',
                    ' GROUP BY res_codigo_adotado, ano_nasc",
                           "fetch_size": 65000, "cursor": "',cursor,'"}
          }
        }')


    request <- POST(url = endpoint, body = params, encode = "form")

    if (length(content(request)$rows) == 0)
      break
    else print("oi")

    dataframe <- convertRequestToDF(request)
    names(dataframe) <- c('codmunres', 'ano', 'nascidos_vivos_menos_de_28_semanas')
    df <- rbind(df, dataframe)
  }
}

head(df)

##Transformando as colunas que estão em caracter para numéricas
df <- df |> mutate_if(is.character, as.numeric)

##Juntando com o restante da base do bloco 5
df_bloco5 <- left_join(df_bloco5, df)

##Substituindo os NA's da coluna 'nascidos_vivos_menos_de_28_semanas' por 0 (gerados após o left_join)
df_bloco5$nascidos_vivos_menos_de_28_semanas[is.na(df_bloco5$nascidos_vivos_menos_de_28_semanas)] <- 0

# proporcao de nascidos vivos ENTRE 28 e 32 semanas ---------------------------
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
                  ' AND (SEMAGESTAC <= 32 AND SEMAGESTAC >= 28) ',
                  ' GROUP BY res_codigo_adotado, ano_nasc",
                        "fetch_size": 65000}
      }
    }')

  request <- POST(url = endpoint, body = params, encode = "form")
  dataframe <- convertRequestToDF(request)
  names(dataframe) <- c('codmunres', 'ano', 'nascidos_vivos_28_a_32_semanas')
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
                    ' AND (SEMAGESTAC <= 32 AND SEMAGESTAC >= 28) ',
                    ' GROUP BY res_codigo_adotado, ano_nasc",
                           "fetch_size": 65000, "cursor": "',cursor,'"}
          }
        }')


    request <- POST(url = endpoint, body = params, encode = "form")

    if (length(content(request)$rows) == 0)
      break
    else print("oi")

    dataframe <- convertRequestToDF(request)
    names(dataframe) <- c('codmunres', 'ano', 'nascidos_vivos_28_a_32_semanas')
    df <- rbind(df, dataframe)
  }
}


head(df)

##Transformando as colunas que estão em caracter para numéricas
df <- df |> mutate_if(is.character, as.numeric)

##Juntando com o restante da base do bloco 5
df_bloco5 <- left_join(df_bloco5, df)

##Substituindo os NA's da coluna 'nascidos_vivos_28_a_32_semanas' por 0 (gerados após o left_join)
df_bloco5$nascidos_vivos_28_a_32_semanas[is.na(df_bloco5$nascidos_vivos_28_a_32_semanas)] <- 0

# proporcao de nascidos vivos ENTRE 33 e 34 semanas ----------------------
df <- dataframe <- data.frame()

for (estado in estados){

  params = paste0('{
      "token": {
        "token": "',token,'"
      },
      "sql": {
        "sql": {"query": "SELECT res_codigo_adotado, ano_nasc, COUNT(1)',
                  ' FROM \\"datasus-sinasc\\"',
                  ' WHERE (res_SIGLA_UF = \'',estado,'\' AND ano_nasc >= 2012  ) ',
                  ' AND (SEMAGESTAC <= 34 AND SEMAGESTAC >= 33) ',
                  ' GROUP BY res_codigo_adotado, ano_nasc",
                        "fetch_size": 65000}
      }
    }')

  request <- POST(url = endpoint, body = params, encode = "form")
  dataframe <- convertRequestToDF(request)
  names(dataframe) <- c('codmunres', 'ano', 'nascidos_vivos_33_a_34_semanas')
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
                    ' AND (SEMAGESTAC <= 34 AND SEMAGESTAC >= 33) ',
                    ' GROUP BY res_codigo_adotado, ano_nasc",
                           "fetch_size": 65000, "cursor": "',cursor,'"}
          }
        }')


    request <- POST(url = endpoint, body = params, encode = "form")

    if (length(content(request)$rows) == 0)
      break
    else print("oi")

    dataframe <- convertRequestToDF(request)
    names(dataframe) <- c('codmunres', 'ano', 'nascidos_vivos_33_a_34_semanas')
    df <- rbind(df, dataframe)
  }
}

head(df)

##Transformando as colunas que estão em caracter para numéricas
df <- df |> mutate_if(is.character, as.numeric)

##Juntando com o restante da base do bloco 5
df_bloco5 <- left_join(df_bloco5, df)

##Substituindo os NA's da coluna 'nascidos_vivos_33_a_34_semanas' por 0 (gerados após o left_join)
df_bloco5$nascidos_vivos_33_a_34_semanas[is.na(df_bloco5$nascidos_vivos_33_a_34_semanas)] <- 0


# proporcao de nascidos vivos ENTRE 35 e 36 semanas ----------------------
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
                  ' AND (SEMAGESTAC <= 36 AND SEMAGESTAC >= 35) ',
                  ' GROUP BY res_codigo_adotado, ano_nasc",
                        "fetch_size": 65000}
      }
    }')

  request <- POST(url = endpoint, body = params, encode = "form")
  dataframe <- convertRequestToDF(request)
  names(dataframe) <- c('codmunres', 'ano', 'nascidos_vivos_35_a_36_semanas')
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
                    ' AND (SEMAGESTAC <= 36 AND SEMAGESTAC >= 35) ',
                    ' GROUP BY res_codigo_adotado, ano_nasc",
                           "fetch_size": 65000, "cursor": "',cursor,'"}
          }
        }')


    request <- POST(url = endpoint, body = params, encode = "form")

    if (length(content(request)$rows) == 0)
      break
    else print("oi")

    dataframe <- convertRequestToDF(request)
    names(dataframe) <- c('codmunres', 'ano', 'nascidos_vivos_35_a_36_semanas')
    df <- rbind(df, dataframe)
  }
}

head(df)

##Transformando as colunas que estão em caracter para numéricas
df <- df |> mutate_if(is.character, as.numeric)

##Juntando com o restante da base do bloco 5
df_bloco5 <- left_join(df_bloco5, df)

##Substituindo os NA's da coluna 'nascidos_vivos_35_a_36_semanas' por 0 (gerados após o left_join)
df_bloco5$nascidos_vivos_35_a_36_semanas[is.na(df_bloco5$nascidos_vivos_35_a_36_semanas)] <- 0


# Verificando se os dados novos e antigos estão batendo -------------------
sum(df_bloco5 |> filter(ano < 2021) |> pull(total_de_nascidos_vivos)) - sum(df_bloco5_antigo$total_de_nascidos_vivos)
sum(df_bloco5 |> filter(ano < 2021) |> pull(nascidos_vivos_com_baixo_peso )) - sum(df_bloco5_antigo$nascidos_vivos_com_baixo_peso )
sum(df_bloco5 |> filter(ano < 2021) |> pull(nascidos_vivos_prematuros)) - sum(df_bloco5_antigo$nascidos_vivos_prematuros)
sum(df_bloco5 |> filter(ano < 2021) |> pull(nascidos_vivos_termo_precoce )) - sum(df_bloco5_antigo$nascidos_vivos_termo_precoce )
sum(df_bloco5 |> filter(ano < 2021) |> pull(nascidos_vivos_peso_menor_1500 )) +
sum(df_bloco5 |> filter(ano < 2021) |> pull(nascidos_vivos_peso_1500_a_1999)) +
sum(df_bloco5 |> filter(ano < 2021) |> pull(nascidos_vivos_peso_2000_a_2499 )) - sum(df_bloco5_antigo$nascidos_vivos_com_baixo_peso )
#Esse vai ter uma porcentagem de incompletude mesmo
sum(df_bloco5 |> filter(ano < 2021) |> pull(nascidos_vivos_menos_de_28_semanas )) +
sum(df_bloco5 |> filter(ano < 2021) |> pull(nascidos_vivos_28_a_32_semanas)) +
sum(df_bloco5 |> filter(ano < 2021) |> pull(nascidos_vivos_33_a_34_semanas )) +
sum(df_bloco5 |> filter(ano < 2021) |> pull(nascidos_vivos_35_a_36_semanas )) - sum(df_bloco5_antigo$nascidos_vivos_prematuros )

# Salvando a base de dados completa na pasta data-raw/csv -----------------
write.csv(df_bloco5, "data-raw/csv/indicadores_bloco5_condicao_de_nascimento_2012-2021.csv", row.names = FALSE)
