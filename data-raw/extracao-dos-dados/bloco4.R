library(tidyverse)
library(httr)
library(janitor)
library(getPass)
library(repr)
library(data.table)
library(readr)
library(openxlsx)
library(tidyr)


token = getPass() #Token de acesso à API da PCDaS (todos os arquivos gerados se encontram na pasta "Databases", no Google Drive)


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

df_bloco4_antigo <- read.csv("Databases/indicadores_bloco4_assistencia_ao_parto_2012-2020.csv") |>
  clean_names()

#Criando um objeto que recebe os códigos dos municípios que utilizamos no painel
codigos_municipios <- read.csv("Databases/tabela_aux_municipios.csv") |>
  pull(codmunres)

#Criando um data.frame auxiliar que possui uma linha para cada combinação de município e ano
df_aux_municipios <- data.frame(codmunres = rep(codigos_municipios, each = length(2012:2021)), ano = 2012:2021)

#Criando o data.frame que irá receber todos os dados do bloco 4
df_bloco4 <- data.frame()


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
df_bloco4 <- left_join(df_aux_municipios, df)

##Substituindo os NA's da coluna 'total_de_nascidos_vivos' por 0 (os NA's surgem quando um município não apresentou nascidos vivos num dado ano)
df_bloco4$total_de_nascidos_vivos[is.na(df_bloco4$total_de_nascidos_vivos)] <- 0


### Proporção de nascimentos por cesariana
df <- dataframe <- data.frame()

for (estado in estados){
  
  params = paste0('{
      "token": {
        "token": "',token,'"
      },
      "sql": {
        "sql": {"query":"SELECT res_codigo_adotado, ano_nasc, COUNT(1)',
                  ' FROM \\"datasus-sinasc\\"',
                  ' WHERE (res_SIGLA_UF = \'',estado,'\' AND ano_nasc >= 2012 AND PARTO = 2)',
                  ' GROUP BY res_codigo_adotado, ano_nasc",
                       "fetch_size": 65000}
      }
    }')
  
  request <- POST(url = endpoint, body = params, encode = "form")
  dataframe <- convertRequestToDF(request)
  names(dataframe) <- c('codmunres', 'ano', 'mulheres_com_parto_cesariana')
  df <- rbind(df, dataframe)
  
  repeat {
    
    cursor <- content(request)$cursor
    
    params = paste0('{
          "token": {
            "token": "',token,'"
          },
          "sql": {
            "sql": {"query":"res_codigo_adotado, ano_nasc, COUNT(1)',
                    ' FROM \\"datasus-sinasc\\"',
                    ' WHERE (res_SIGLA_UF = \'',estado,'\' AND ano_nasc >= 2012 AND PARTO = 2)',
                    ' GROUP BY res_codigo_adotado, ano_nasc",
                           "fetch_size": 65000, "cursor": "',cursor,'"}
          }
        }')
    
    
    request <- POST(url = endpoint, body = params, encode = "form")
    
    if (length(content(request)$rows) == 0)
      break
    else print("oi")
    
    dataframe <- convertRequestToDF(request)
    names(dataframe) <- c('codmunres', 'ano', 'mulheres_com_parto_cesariana')
    df <- rbind(df, dataframe)
  }
}

head(df)

##Transformando as colunas que estão em caracter para numéricas
df <- df |> mutate_if(is.character, as.numeric)

##Juntando com o restante da base do bloco 4
df_bloco4 <- left_join(df_bloco4, df)

##Substituindo os NA's da coluna 'mulheres_com_parto_cesariana' por 0 (gerados após o left_join)
df_bloco4$mulheres_com_parto_cesariana[is.na(df_bloco4$mulheres_com_parto_cesariana)] <- 0

# Número de nascidos vivos de mulheres pertencentes ao grupo 1 de Robson
df <- dataframe <- data.frame()

for (estado in estados){
  
  params = paste0('{
      "token": {
        "token": "',token,'"
      },
      "sql": {
        "sql": {"query":"SELECT res_codigo_adotado, ano_nasc, COUNT(1)',
                  ' FROM \\"datasus-sinasc\\"',
                  ' WHERE (res_SIGLA_UF = \'',estado,'\' AND ano_nasc >= 2012 AND TPROBSON = 1)',
                  ' GROUP BY res_codigo_adotado, ano_nasc",
                       "fetch_size": 65000}
      }
    }')
  
  request <- POST(url = endpoint, body = params, encode = "form")
  dataframe <- convertRequestToDF(request)
  names(dataframe) <- c('codmunres', 'ano', 'mulheres_dentro_do_grupo_de_robson_1')
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
                    ' WHERE (res_SIGLA_UF = \'',estado,'\' AND ano_nasc >= 2012 AND TPROBSON = 1)',
                    ' GROUP BY res_codigo_adotado, ano_nasc",
                           "fetch_size": 65000, "cursor": "',cursor,'"}
          }
        }')
    
    
    request <- POST(url = endpoint, body = params, encode = "form")
    
    if (length(content(request)$rows) == 0)
      break
    else print("oi")
    
    dataframe <- convertRequestToDF(request)
    names(dataframe) <- c('uf', 'municipio', 'codmunres', 'ano', 'mulheres_dentro_do_grupo_de_robson_1')
    df <- rbind(df, dataframe)
  }
}

head(df) 

##Transformando as colunas que estão em caracter para numéricas
df <- df |> mutate_if(is.character, as.numeric)

##Juntando com o restante da base do bloco 4
df_bloco4 <- left_join(df_bloco4, df)

##Substituindo os NA's da coluna 'mulheres_dentro_do_grupo_de_robson_1' por 0 (gerados após o left_join)
df_bloco4$mulheres_dentro_do_grupo_de_robson_1[is.na(df_bloco4$mulheres_dentro_do_grupo_de_robson_1)] <- 0


# Número de nascidos vivos de mulheres pertencentes ao grupo 2 de Robson
df <- dataframe <- data.frame()

for (estado in estados){
  
  params = paste0('{
      "token": {
        "token": "',token,'"
      },
      "sql": {
        "sql": {"query":"SELECT res_codigo_adotado, ano_nasc, COUNT(1)',
                  ' FROM \\"datasus-sinasc\\"',
                  ' WHERE (res_SIGLA_UF = \'',estado,'\' AND ano_nasc >= 2012 AND TPROBSON = 2)',
                  ' GROUP BY res_codigo_adotado, ano_nasc",
                       "fetch_size": 65000}
      }
    }')
  
  request <- POST(url = endpoint, body = params, encode = "form")
  dataframe <- convertRequestToDF(request)
  names(dataframe) <- c('codmunres', 'ano', 'mulheres_dentro_do_grupo_de_robson_2')
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
                    ' WHERE (res_SIGLA_UF = \'',estado,'\' AND ano_nasc >= 2012 AND TPROBSON = 2)',
                    ' GROUP BY res_codigo_adotado, ano_nasc",
                           "fetch_size": 65000, "cursor": "',cursor,'"}
          }
        }')
    
    
    request <- POST(url = endpoint, body = params, encode = "form")
    
    if (length(content(request)$rows) == 0)
      break
    else print("oi")
    
    dataframe <- convertRequestToDF(request)
    names(dataframe) <- c('codmunres', 'ano', 'mulheres_dentro_do_grupo_de_robson_2')
    df <- rbind(df, dataframe)
  }
}

head(df)

##Transformando as colunas que estão em caracter para numéricas
df <- df |> mutate_if(is.character, as.numeric)

##Juntando com o restante da base do bloco 4
df_bloco4 <- left_join(df_bloco4, df)

##Substituindo os NA's da coluna 'mulheres_dentro_do_grupo_de_robson_2' por 0 (gerados após o left_join)
df_bloco4$mulheres_dentro_do_grupo_de_robson_2[is.na(df_bloco4$mulheres_dentro_do_grupo_de_robson_2)] <- 0



# Número de nascidos vivos de mulheres pertencentes ao grupo 3 de Robson
df <- dataframe <- data.frame()

for (estado in estados){
  
  params = paste0('{
      "token": {
        "token": "',token,'"
      },
      "sql": {
        "sql": {"query":"SELECT res_codigo_adotado, ano_nasc, COUNT(1)',
                  ' FROM \\"datasus-sinasc\\"',
                  ' WHERE (res_SIGLA_UF = \'',estado,'\' AND ano_nasc >= 2012 AND TPROBSON = 3)',
                  ' GROUP BY res_codigo_adotado, ano_nasc",
                       "fetch_size": 65000}
      }
    }')
  
  request <- POST(url = endpoint, body = params, encode = "form")
  dataframe <- convertRequestToDF(request)
  names(dataframe) <- c('codmunres', 'ano', 'mulheres_dentro_do_grupo_de_robson_3')
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
                    ' WHERE (res_SIGLA_UF = \'',estado,'\' AND ano_nasc >= 2012 AND TPROBSON = 3)',
                    ' GROUP BY res_codigo_adotado, ano_nasc",
                           "fetch_size": 65000, "cursor": "',cursor,'"}
          }
        }')
    
    
    request <- POST(url = endpoint, body = params, encode = "form")
    
    if (length(content(request)$rows) == 0)
      break
    else print("oi")
    
    dataframe <- convertRequestToDF(request)
    names(dataframe) <- c('codmunres', 'ano', 'mulheres_dentro_do_grupo_de_robson_3')
    df <- rbind(df, dataframe)
  }
}

head(df)

##Transformando as colunas que estão em caracter para numéricas
df <- df |> mutate_if(is.character, as.numeric)

##Juntando com o restante da base do bloco 4
df_bloco4 <- left_join(df_bloco4, df)

##Substituindo os NA's da coluna 'mulheres_dentro_do_grupo_de_robson_3' por 0 (gerados após o left_join)
df_bloco4$mulheres_dentro_do_grupo_de_robson_3[is.na(df_bloco4$mulheres_dentro_do_grupo_de_robson_3)] <- 0



# Número de nascidos vivos de mulheres pertencentes ao grupo 4 de Robson
df <- dataframe <- data.frame()

for (estado in estados){
  
  params = paste0('{
      "token": {
        "token": "',token,'"
      },
      "sql": {
        "sql": {"query":"SELECT res_codigo_adotado, ano_nasc, COUNT(1)',
                  ' FROM \\"datasus-sinasc\\"',
                  ' WHERE (res_SIGLA_UF = \'',estado,'\' AND ano_nasc >= 2012 AND TPROBSON = 4)',
                  ' GROUP BY res_codigo_adotado, ano_nasc",
                       "fetch_size": 65000}
      }
    }')
  
  request <- POST(url = endpoint, body = params, encode = "form")
  dataframe <- convertRequestToDF(request)
  names(dataframe) <- c('codmunres', 'ano', 'mulheres_dentro_do_grupo_de_robson_4')
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
                    ' WHERE (res_SIGLA_UF = \'',estado,'\' AND ano_nasc >= 2012 AND TPROBSON = 4)',
                    ' GROUP BY res_codigo_adotado, ano_nasc",
                           "fetch_size": 65000, "cursor": "',cursor,'"}
          }
        }')
    
    
    request <- POST(url = endpoint, body = params, encode = "form")
    
    if (length(content(request)$rows) == 0)
      break
    else print("oi")
    
    dataframe <- convertRequestToDF(request)
    names(dataframe) <- c('codmunres', 'ano', 'mulheres_dentro_do_grupo_de_robson_4')
    df <- rbind(df, dataframe)
  }
}

head(df)

##Transformando as colunas que estão em caracter para numéricas
df <- df |> mutate_if(is.character, as.numeric)

##Juntando com o restante da base do bloco 4
df_bloco4 <- left_join(df_bloco4, df)

##Substituindo os NA's da coluna 'mulheres_dentro_do_grupo_de_robson_4' por 0 (gerados após o left_join)
df_bloco4$mulheres_dentro_do_grupo_de_robson_4[is.na(df_bloco4$mulheres_dentro_do_grupo_de_robson_4)] <- 0


# Número de nascidos vivos de mulheres pertencentes ao grupo 5 de Robson
df <- dataframe <- data.frame()

for (estado in estados){
  
  params = paste0('{
      "token": {
        "token": "',token,'"
      },
      "sql": {
        "sql": {"query":"SELECT res_codigo_adotado, ano_nasc, COUNT(1)',
                  ' FROM \\"datasus-sinasc\\"',
                  ' WHERE (res_SIGLA_UF = \'',estado,'\' AND ano_nasc >= 2012 AND TPROBSON = 5)',
                  ' GROUP BY res_codigo_adotado, ano_nasc",
                       "fetch_size": 65000}
      }
    }')
  
  request <- POST(url = endpoint, body = params, encode = "form")
  dataframe <- convertRequestToDF(request)
  names(dataframe) <- c('codmunres', 'ano', 'mulheres_dentro_do_grupo_de_robson_5')
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
                    ' WHERE (res_SIGLA_UF = \'',estado,'\' AND ano_nasc >= 2012 AND TPROBSON = 5)',
                    ' GROUP BY res_codigo_adotado, ano_nasc",
                           "fetch_size": 65000, "cursor": "',cursor,'"}
          }
        }')
    
    
    request <- POST(url = endpoint, body = params, encode = "form")
    
    if (length(content(request)$rows) == 0)
      break
    else print("oi")
    
    dataframe <- convertRequestToDF(request)
    names(dataframe) <- c('uf', 'municipio', 'codmunres', 'ano', 'mulheres_dentro_do_grupo_de_robson_5')
    df <- rbind(df, dataframe)
  }
}

head(df)

##Transformando as colunas que estão em caracter para numéricas
df <- df |> mutate_if(is.character, as.numeric)

##Juntando com o restante da base do bloco 4
df_bloco4 <- left_join(df_bloco4, df)

##Substituindo os NA's da coluna 'mulheres_dentro_do_grupo_de_robson_5' por 0 (gerados após o left_join)
df_bloco4$mulheres_dentro_do_grupo_de_robson_5[is.na(df_bloco4$mulheres_dentro_do_grupo_de_robson_5)] <- 0


# Número de nascidos vivos de mulheres pertencentes ao grupo  de Robson de 6 ao 9
df <- dataframe <- data.frame()

for (estado in estados){
  
  params = paste0('{
      "token": {
        "token": "',token,'"
      },
      "sql": {
        "sql": {"query":"SELECT res_codigo_adotado, ano_nasc, COUNT(1)',
                  ' FROM \\"datasus-sinasc\\"',
                  ' WHERE (res_SIGLA_UF = \'',estado,'\' AND ano_nasc >= 2012 AND TPROBSON >= 6 AND TPROBSON <= 9)',
                  ' GROUP BY res_codigo_adotado, ano_nasc",
                       "fetch_size": 65000}
      }
    }')
  
  request <- POST(url = endpoint, body = params, encode = "form")
  dataframe <- convertRequestToDF(request)
  names(dataframe) <- c('codmunres', 'ano', 'mulheres_dentro_do_grupo_de_robson_6_ao_9')
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
                    ' WHERE (res_SIGLA_UF = \'',estado,'\' AND ano_nasc >= 2012 AND TPROBSON >= 6 AND TPROBSON <= 9)',
                    ' GROUP BY res_codigo_adotado, ano_nasc",
                           "fetch_size": 65000, "cursor": "',cursor,'"}
          }
        }')
    
    
    request <- POST(url = endpoint, body = params, encode = "form")
    
    if (length(content(request)$rows) == 0)
      break
    else print("oi")
    
    dataframe <- convertRequestToDF(request)
    names(dataframe) <- c('uf', 'municipio', 'codmunres', 'ano', 'mulheres_dentro_do_grupo_de_robson_6_ao_9')
    df <- rbind(df, dataframe)
  }
}

head(df)

##Transformando as colunas que estão em caracter para numéricas
df <- df |> mutate_if(is.character, as.numeric)

##Juntando com o restante da base do bloco 4
df_bloco4 <- left_join(df_bloco4, df)

##Substituindo os NA's da coluna 'mulheres_dentro_do_grupo_de_robson_6_ao_9' por 0 (gerados após o left_join)
df_bloco4$mulheres_dentro_do_grupo_de_robson_6_ao_9[is.na(df_bloco4$mulheres_dentro_do_grupo_de_robson_6_ao_9)] <- 0


# Número de nascidos vivos de mulheres pertencentes ao grupo 10 de Robson
df <- dataframe <- data.frame()

for (estado in estados){
  
  params = paste0('{
      "token": {
        "token": "',token,'"
      },
      "sql": {
        "sql": {"query":"SELECT res_codigo_adotado, ano_nasc, COUNT(1)',
                  ' FROM \\"datasus-sinasc\\"',
                  ' WHERE (res_SIGLA_UF = \'',estado,'\' AND ano_nasc >= 2012 AND TPROBSON = 10)',
                  ' GROUP BY res_codigo_adotado, ano_nasc",
                       "fetch_size": 65000}
      }
    }')
  
  request <- POST(url = endpoint, body = params, encode = "form")
  dataframe <- convertRequestToDF(request)
  names(dataframe) <- c('codmunres', 'ano', 'mulheres_dentro_do_grupo_de_robson_10')
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
                    ' WHERE (res_SIGLA_UF = \'',estado,'\' AND ano_nasc >= 2012 AND TPROBSON = 10)',
                    ' GROUP BY res_codigo_adotado, ano_nasc",
                           "fetch_size": 65000, "cursor": "',cursor,'"}
          }
        }')
    
    
    request <- POST(url = endpoint, body = params, encode = "form")
    
    if (length(content(request)$rows) == 0)
      break
    else print("oi")
    
    dataframe <- convertRequestToDF(request)
    names(dataframe) <- c('uf', 'municipio', 'codmunres', 'ano', 'mulheres_dentro_do_grupo_de_robson_10')
    df <- rbind(df, dataframe)
  }
}

head(df)

##Transformando as colunas que estão em caracter para numéricas
df <- df |> mutate_if(is.character, as.numeric)

##Juntando com o restante da base do bloco 4
df_bloco4 <- left_join(df_bloco4, df)

##Substituindo os NA's da coluna 'mulheres_dentro_do_grupo_de_robson_10' por 0 (gerados após o left_join)
df_bloco4$mulheres_dentro_do_grupo_de_robson_10[is.na(df_bloco4$mulheres_dentro_do_grupo_de_robson_10)] <- 0


# Contribuição relativa de cada grupo de Robson na taxa global de cesariana
## Número de nascidos vivos de mães do grupo 1
df <- dataframe <- data.frame()

for (estado in estados){
  
  params = paste0('{
      "token": {
        "token": "',token,'"
      },
      "sql": {
        "sql": {"query":" SELECT res_codigo_adotado, ano_nasc, COUNT(1)',
                  ' FROM \\"datasus-sinasc\\"',
                  ' WHERE (res_SIGLA_UF = \'',estado,'\' AND ano_nasc >= 2012 ',
                  ' AND TPROBSON = 1 AND PARTO = 2)',
                  ' GROUP BY res_codigo_adotado, ano_nasc",
                        "fetch_size": 65000}
      }
    }')
  
  request <- POST(url = endpoint, body = params, encode = "form")
  dataframe <- convertRequestToDF(request)
  names(dataframe) <- c('codmunres', 'ano', 'total_cesariana_grupo_robson_1')
  df <- rbind(df, dataframe)
  
  repeat {
    
    cursor <- content(request)$cursor
    
    params = paste0('{
          "token": {
            "token": "',token,'"
          },
          "sql": {
            "sql": {"query":" SELECT res_codigo_adotado, ano_nasc, COUNT(1)',
                    ' FROM \\"datasus-sinasc\\"',
                    ' WHERE (res_SIGLA_UF = \'',estado,'\' AND ano_nasc >= 2012 ',
                    ' AND TPROBSON = 1 AND PARTO = 2)',
                    ' GROUP BY res_codigo_adotado, ano_nasc",
                            "fetch_size": 65000, "cursor": "',cursor,'"}
          }
        }')
    
    
    request <- POST(url = endpoint, body = params, encode = "form")
    
    if (length(content(request)$rows) == 0)
      break
    else print("oi")
    
    dataframe <- convertRequestToDF(request)
    names(dataframe) <- c('codmunres', 'ano', 'total_cesariana_grupo_robson_1')
    df <- rbind(df, dataframe)
  }
}

head(df)

##Transformando as colunas que estão em caracter para numéricas
df <- df |> mutate_if(is.character, as.numeric)

##Juntando com o restante da base do bloco 4
df_bloco4 <- left_join(df_bloco4, df)

##Substituindo os NA's da coluna 'total_cesariana_grupo_robson_1' por 0 (gerados após o left_join)
df_bloco4$total_cesariana_grupo_robson_1[is.na(df_bloco4$total_cesariana_grupo_robson_1)] <- 0


## Número de nascidos vivos de mães do grupo 2
df <- dataframe <- data.frame()

for (estado in estados){
  
  params = paste0('{
      "token": {
        "token": "',token,'"
      },
      "sql": {
        "sql": {"query":" SELECT res_codigo_adotado, ano_nasc, COUNT(1)',
                  ' FROM \\"datasus-sinasc\\"',
                  ' WHERE (res_SIGLA_UF = \'',estado,'\' AND ano_nasc >= 2012 ',
                  ' AND TPROBSON = 2 AND PARTO = 2)',
                  ' GROUP BY res_codigo_adotado, ano_nasc",
                        "fetch_size": 65000}
      }
    }')
  
  request <- POST(url = endpoint, body = params, encode = "form")
  dataframe <- convertRequestToDF(request)
  names(dataframe) <- c('codmunres', 'ano', 'total_cesariana_grupo_robson_2')
  df <- rbind(df, dataframe)
  
  repeat {
    
    cursor <- content(request)$cursor
    
    params = paste0('{
          "token": {
            "token": "',token,'"
          },
          "sql": {
            "sql": {"query":" SELECT res_codigo_adotado, ano_nasc, COUNT(1)',
                    ' FROM \\"datasus-sinasc\\"',
                    ' WHERE (res_SIGLA_UF = \'',estado,'\' AND ano_nasc >= 2012 ',
                    ' AND TPROBSON = 2 AND PARTO = 2)',
                    ' GROUP BY res_codigo_adotado, ano_nasc",
                            "fetch_size": 65000, "cursor": "',cursor,'"}
          }
        }')
    
    
    request <- POST(url = endpoint, body = params, encode = "form")
    
    if (length(content(request)$rows) == 0)
      break
    else print("oi")
    
    dataframe <- convertRequestToDF(request)
    names(dataframe) <- c('codmunres', 'ano', 'total_cesariana_grupo_robson_2')
    df <- rbind(df, dataframe)
  }
}

head(df)

##Transformando as colunas que estão em caracter para numéricas
df <- df |> mutate_if(is.character, as.numeric)

##Juntando com o restante da base do bloco 4
df_bloco4 <- left_join(df_bloco4, df)

##Substituindo os NA's da coluna 'total_cesariana_grupo_robson_2' por 0 (gerados após o left_join)
df_bloco4$total_cesariana_grupo_robson_2[is.na(df_bloco4$total_cesariana_grupo_robson_2)] <- 0



## Número de nascidos vivos de mães do grupo 3
df <- dataframe <- data.frame()

for (estado in estados){
  
  params = paste0('{
      "token": {
        "token": "',token,'"
      },
      "sql": {
        "sql": {"query":" SELECT res_codigo_adotado, ano_nasc, COUNT(1)',
                  ' FROM \\"datasus-sinasc\\"',
                  ' WHERE (res_SIGLA_UF = \'',estado,'\' AND ano_nasc >= 2012 ',
                  ' AND TPROBSON = 3 AND PARTO = 2)',
                  ' GROUP BY res_codigo_adotado, ano_nasc",
                        "fetch_size": 65000}
      }
    }')
  
  request <- POST(url = endpoint, body = params, encode = "form")
  dataframe <- convertRequestToDF(request)
  names(dataframe) <- c('codmunres', 'ano', 'total_cesariana_grupo_robson_3')
  df <- rbind(df, dataframe)
  
  repeat {
    
    cursor <- content(request)$cursor
    
    params = paste0('{
          "token": {
            "token": "',token,'"
          },
          "sql": {
            "sql": {"query":" SELECT res_codigo_adotado, ano_nasc, COUNT(1)',
                    ' FROM \\"datasus-sinasc\\"',
                    ' WHERE (res_SIGLA_UF = \'',estado,'\' AND ano_nasc >= 2012 ',
                    ' AND TPROBSON = 3 AND PARTO = 2)',
                    ' GROUP BY res_codigo_adotado, ano_nasc",
                            "fetch_size": 65000, "cursor": "',cursor,'"}
          }
        }')
    
    
    request <- POST(url = endpoint, body = params, encode = "form")
    
    if (length(content(request)$rows) == 0)
      break
    else print("oi")
    
    dataframe <- convertRequestToDF(request)
    names(dataframe) <- c('codmunres', 'ano', 'total_cesariana_grupo_robson_3')
    df <- rbind(df, dataframe)
  }
}

head(df)

##Transformando as colunas que estão em caracter para numéricas
df <- df |> mutate_if(is.character, as.numeric)

##Juntando com o restante da base do bloco 4
df_bloco4 <- left_join(df_bloco4, df)

##Substituindo os NA's da coluna 'total_cesariana_grupo_robson_3' por 0 (gerados após o left_join)
df_bloco4$total_cesariana_grupo_robson_3[is.na(df_bloco4$total_cesariana_grupo_robson_3)] <- 0



## Número de nascidos vivos de mães do grupo 4
df <- dataframe <- data.frame()

for (estado in estados){
  
  params = paste0('{
      "token": {
        "token": "',token,'"
      },
      "sql": {
        "sql": {"query":" SELECT res_codigo_adotado, ano_nasc, COUNT(1)',
                  ' FROM \\"datasus-sinasc\\"',
                  ' WHERE (res_SIGLA_UF = \'',estado,'\' AND ano_nasc >= 2012 ',
                  ' AND TPROBSON = 4 AND PARTO = 2)',
                  ' GROUP BY res_codigo_adotado, ano_nasc",
                        "fetch_size": 65000}
      }
    }')
  
  request <- POST(url = endpoint, body = params, encode = "form")
  dataframe <- convertRequestToDF(request)
  names(dataframe) <- c('codmunres', 'ano', 'total_cesariana_grupo_robson_4')
  df <- rbind(df, dataframe)
  
  repeat {
    
    cursor <- content(request)$cursor
    
    params = paste0('{
          "token": {
            "token": "',token,'"
          },
          "sql": {
            "sql": {"query":" SELECT res_codigo_adotado, ano_nasc, COUNT(1)',
                    ' FROM \\"datasus-sinasc\\"',
                    ' WHERE (res_SIGLA_UF = \'',estado,'\' AND ano_nasc >= 2012 ',
                    ' AND TPROBSON = 4 AND PARTO = 2)',
                    ' GROUP BY res_codigo_adotado, ano_nasc",
                            "fetch_size": 65000, "cursor": "',cursor,'"}
          }
        }')
    
    
    request <- POST(url = endpoint, body = params, encode = "form")
    
    if (length(content(request)$rows) == 0)
      break
    else print("oi")
    
    dataframe <- convertRequestToDF(request)
    names(dataframe) <- c('codmunres', 'ano', 'total_cesariana_grupo_robson_4')
    df <- rbind(df, dataframe)
  }
}

head(df)

##Transformando as colunas que estão em caracter para numéricas
df <- df |> mutate_if(is.character, as.numeric)

##Juntando com o restante da base do bloco 4
df_bloco4 <- left_join(df_bloco4, df)

##Substituindo os NA's da coluna 'total_cesariana_grupo_robson_4' por 0 (gerados após o left_join)
df_bloco4$total_cesariana_grupo_robson_4[is.na(df_bloco4$total_cesariana_grupo_robson_4)] <- 0



## Número de nascidos vivos de mães do grupo 5
df <- dataframe <- data.frame()

for (estado in estados){
  
  params = paste0('{
      "token": {
        "token": "',token,'"
      },
      "sql": {
        "sql": {"query":" SELECT res_codigo_adotado, ano_nasc, COUNT(1)',
                  ' FROM \\"datasus-sinasc\\"',
                  ' WHERE (res_SIGLA_UF = \'',estado,'\' AND ano_nasc >= 2012 ',
                  ' AND TPROBSON = 5 AND PARTO = 2)',
                  ' GROUP BY res_codigo_adotado, ano_nasc",
                        "fetch_size": 65000}
      }
    }')
  
  request <- POST(url = endpoint, body = params, encode = "form")
  dataframe <- convertRequestToDF(request)
  names(dataframe) <- c('codmunres', 'ano', 'total_cesariana_grupo_robson_5')
  df <- rbind(df, dataframe)
  
  repeat {
    
    cursor <- content(request)$cursor
    
    params = paste0('{
          "token": {
            "token": "',token,'"
          },
          "sql": {
            "sql": {"query":" SELECT res_codigo_adotado, ano_nasc, COUNT(1)',
                    ' FROM \\"datasus-sinasc\\"',
                    ' WHERE (res_SIGLA_UF = \'',estado,'\' AND ano_nasc >= 2012 ',
                    ' AND TPROBSON = 5 AND PARTO = 2)',
                    ' GROUP BY res_codigo_adotado, ano_nasc",
                            "fetch_size": 65000, "cursor": "',cursor,'"}
          }
        }')
    
    
    request <- POST(url = endpoint, body = params, encode = "form")
    
    if (length(content(request)$rows) == 0)
      break
    else print("oi")
    
    dataframe <- convertRequestToDF(request)
    names(dataframe) <- c('codmunres', 'ano', 'total_cesariana_grupo_robson_5')
    df <- rbind(df, dataframe)
  }
}

head(df)

##Transformando as colunas que estão em caracter para numéricas
df <- df |> mutate_if(is.character, as.numeric)

##Juntando com o restante da base do bloco 4
df_bloco4 <- left_join(df_bloco4, df)

##Substituindo os NA's da coluna 'total_cesariana_grupo_robson_5' por 0 (gerados após o left_join)
df_bloco4$total_cesariana_grupo_robson_5[is.na(df_bloco4$total_cesariana_grupo_robson_5)] <- 0


## Número de nascidos vivos de mães dos grupos 6 a 9 conjuntamente
df <- dataframe <- data.frame()

for (estado in estados){
  
  params = paste0('{
      "token": {
        "token": "',token,'"
      },
      "sql": {
        "sql": {"query":" SELECT res_codigo_adotado, ano_nasc, COUNT(1)',
                  ' FROM \\"datasus-sinasc\\"',
                  ' WHERE (res_SIGLA_UF = \'',estado,'\' AND ano_nasc >= 2012 ',
                  ' AND TPROBSON >= 6 AND TPROBSON <= 9 AND PARTO = 2)',
                  ' GROUP BY res_codigo_adotado, ano_nasc",
                        "fetch_size": 65000}
      }
    }')
  
  request <- POST(url = endpoint, body = params, encode = "form")
  dataframe <- convertRequestToDF(request)
  names(dataframe) <- c('codmunres', 'ano', 'total_cesariana_grupo_robson_6_ao_9')
  df <- rbind(df, dataframe)
  
  repeat {
    
    cursor <- content(request)$cursor
    
    params = paste0('{
          "token": {
            "token": "',token,'"
          },
          "sql": {
            "sql": {"query":" SELECT res_codigo_adotado, ano_nasc, COUNT(1)',
                    ' FROM \\"datasus-sinasc\\"',
                    ' WHERE (res_SIGLA_UF = \'',estado,'\' AND ano_nasc >= 2012 ',
                    ' AND TPROBSON >= 6 AND TPROBSON <= 9 AND PARTO = 2)',
                    ' GROUP BY res_codigo_adotado, ano_nasc",
                            "fetch_size": 65000, "cursor": "',cursor,'"}
          }
        }')
    
    
    request <- POST(url = endpoint, body = params, encode = "form")
    
    if (length(content(request)$rows) == 0)
      break
    else print("oi")
    
    dataframe <- convertRequestToDF(request)
    names(dataframe) <- c('codmunres', 'ano', 'total_cesariana_grupo_robson_6_ao_9')
    df <- rbind(df, dataframe)
  }
}

head(df)

##Transformando as colunas que estão em caracter para numéricas
df <- df |> mutate_if(is.character, as.numeric)

##Juntando com o restante da base do bloco 4
df_bloco4 <- left_join(df_bloco4, df)

##Substituindo os NA's da coluna 'total_cesariana_grupo_robson_6_ao_9' por 0 (gerados após o left_join)
df_bloco4$total_cesariana_grupo_robson_6_ao_9[is.na(df_bloco4$total_cesariana_grupo_robson_6_ao_9)] <- 0


## Número de nascidos vivos de mães do grupo 10
df <- dataframe <- data.frame()

for (estado in estados){
  
  params = paste0('{
      "token": {
        "token": "',token,'"
      },
      "sql": {
        "sql": {"query":" SELECT res_codigo_adotado, ano_nasc, COUNT(1)',
                  ' FROM \\"datasus-sinasc\\"',
                  ' WHERE (res_SIGLA_UF = \'',estado,'\' AND ano_nasc >= 2012 ',
                  ' AND TPROBSON = 10 AND PARTO = 2)',
                  ' GROUP BY res_codigo_adotado, ano_nasc",
                        "fetch_size": 65000}
      }
    }')
  
  request <- POST(url = endpoint, body = params, encode = "form")
  dataframe <- convertRequestToDF(request)
  names(dataframe) <- c('codmunres', 'ano', 'total_cesariana_grupo_robson_10')
  df <- rbind(df, dataframe)
  
  repeat {
    
    cursor <- content(request)$cursor
    
    params = paste0('{
          "token": {
            "token": "',token,'"
          },
          "sql": {
            "sql": {"query":" SELECT res_codigo_adotado, ano_nasc, COUNT(1)',
                    ' FROM \\"datasus-sinasc\\"',
                    ' WHERE (res_SIGLA_UF = \'',estado,'\' AND ano_nasc >= 2012 ',
                    ' AND TPROBSON = 10 AND PARTO = 2)',
                    ' GROUP BY res_codigo_adotado, ano_nasc",
                            "fetch_size": 65000, "cursor": "',cursor,'"}
          }
        }')
    
    
    request <- POST(url = endpoint, body = params, encode = "form")
    
    if (length(content(request)$rows) == 0)
      break
    else print("oi")
    
    dataframe <- convertRequestToDF(request)
    names(dataframe) <- c('codmunres', 'ano', 'total_cesariana_grupo_robson_10')
    df <- rbind(df, dataframe)
  }
}

head(df)

##Transformando as colunas que estão em caracter para numéricas
df <- df |> mutate_if(is.character, as.numeric)

##Juntando com o restante da base do bloco 4
df_bloco4 <- left_join(df_bloco4, df)

##Substituindo os NA's da coluna 'total_cesariana_grupo_robson_10' por 0 (gerados após o left_join)
df_bloco4$total_cesariana_grupo_robson_10[is.na(df_bloco4$total_cesariana_grupo_robson_10)] <- 0



# Verificando se os dados novos e antigos estão batendo -------------------
sum(df_bloco4 |> filter(ano < 2021) |> pull(total_de_nascidos_vivos)) - sum(df_bloco4_antigo$total_de_nascidos_vivos)
sum(df_bloco4 |> filter(ano < 2021) |> pull(mulheres_com_parto_cesariana )) - sum(df_bloco4_antigo$mulheres_com_parto_cesariana )
sum(df_bloco4 |> filter(ano < 2021) |> pull(mulheres_dentro_do_grupo_de_robson_1)) - sum(df_bloco4_antigo$mulheres_dentro_do_grupo_de_robson_1)
sum(df_bloco4 |> filter(ano < 2021) |> pull(mulheres_dentro_do_grupo_de_robson_2)) - sum(df_bloco4_antigo$mulheres_dentro_do_grupo_de_robson_2)
sum(df_bloco4 |> filter(ano < 2021) |> pull(mulheres_dentro_do_grupo_de_robson_3)) - sum(df_bloco4_antigo$mulheres_dentro_do_grupo_de_robson_3)
sum(df_bloco4 |> filter(ano < 2021) |> pull(mulheres_dentro_do_grupo_de_robson_4)) - sum(df_bloco4_antigo$mulheres_dentro_do_grupo_de_robson_4)
sum(df_bloco4 |> filter(ano < 2021) |> pull(mulheres_dentro_do_grupo_de_robson_5)) - sum(df_bloco4_antigo$mulheres_dentro_do_grupo_de_robson_5)
sum(df_bloco4 |> filter(ano < 2021) |> pull(mulheres_dentro_do_grupo_de_robson_6_ao_9)) - sum(df_bloco4_antigo$mulheres_dentro_do_grupo_de_robson_6_ao_9)
sum(df_bloco4 |> filter(ano < 2021) |> pull(mulheres_dentro_do_grupo_de_robson_10)) - sum(df_bloco4_antigo$mulheres_dentro_do_grupo_de_robson_10)
sum(df_bloco4 |> filter(ano < 2021) |> pull(total_cesariana_grupo_robson_1)) - sum(df_bloco4_antigo$total_cesariana_grupo_robson_1 )
sum(df_bloco4 |> filter(ano < 2021) |> pull(total_cesariana_grupo_robson_2)) - sum(df_bloco4_antigo$total_cesariana_grupo_robson_2 )
sum(df_bloco4 |> filter(ano < 2021) |> pull(total_cesariana_grupo_robson_3)) - sum(df_bloco4_antigo$total_cesariana_grupo_robson_3 )
sum(df_bloco4 |> filter(ano < 2021) |> pull(total_cesariana_grupo_robson_4)) - sum(df_bloco4_antigo$total_cesariana_grupo_robson_4 )
sum(df_bloco4 |> filter(ano < 2021) |> pull(total_cesariana_grupo_robson_5)) - sum(df_bloco4_antigo$total_cesariana_grupo_robson_5 )
sum(df_bloco4 |> filter(ano < 2021) |> pull(total_cesariana_grupo_robson_6_ao_9)) - sum(df_bloco4_antigo$total_cesariana_grupo_robson_6_ao_9 )
sum(df_bloco4 |> filter(ano < 2021) |> pull(total_cesariana_grupo_robson_10)) - sum(df_bloco4_antigo$total_cesariana_grupo_robson_10 )




# Salvando a base de dados completa na pasta data-raw/csv -----------------
write.csv(df_bloco4, "data-raw/csv/indicadores_bloco4_condicao_de_nascimento_2012-2021.csv", row.names = FALSE)






