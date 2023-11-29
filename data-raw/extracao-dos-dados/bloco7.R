library(dplyr)
library(httr)
library(getPass)
library(repr)
library(readr)
library(tidyverse)
library(janitor)
library(microdatasus)

############## Informações dos municípios

tabela_aux_municipios <- read_csv("data-raw/csv/tabela_aux_municipios.csv") |>
  select(
    codmunres = municipio,
    municipio = regiao,
  )

############## Função para extrair dados PCDas

token = getPass()

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

endpoint <- paste0(url_base,"/","sql_query")

############## Mortalidade Neonatal

########## Dados de óbitos fetais de 2012 a 2021

df_fetais1 <- fetch_datasus(
  year_start = 2012,
  year_end = 2021,
  information_system = "SIM-DOFET"
)

df_fetais2 <- process_sim(df_fetais1, municipality_data = TRUE)

########### Filtrando para maior ou igual a 22 semanas e peso>=500g

dados_fetais <- df_fetais2 |>
  select(
    codmunres = CODMUNRES,
    DTOBITO,
    PESO,
    GESTACAO,
    SEMAGESTAC,
    OBITOPARTO
  ) |>
  filter(
    ((GESTACAO != "Menos de 22 semanas" & !is.na(GESTACAO)) | (is.na(GESTACAO) & SEMAGESTAC >= 22 & SEMAGESTAC != 99)) | (PESO >= 500)
  )

########## Número de óbitos fetais necessários para os indicadores

df_fetais <- dados_fetais |>
  select(
    DTOBITO,
    PESO,
    codmunres,
    OBITOPARTO
  ) |>
  mutate(
    ano = substr(DTOBITO, 1, 4),
    .after = DTOBITO
  ) |>
  mutate(
    obitos=1,
    peso_menos_1500 = case_when(
      PESO < 1500 ~ 1,
      !(PESO < 1500) ~ 0
    ),
    peso_1500_1999 = case_when(
      (PESO >= 1500 & PESO < 2000) ~ 1,
      !(PESO >= 1500 & PESO < 2000) ~ 0
    ),
    peso_2000_2499 = case_when(
      (PESO >= 2000 & PESO < 2500) ~ 1,
      !(PESO >= 2000 & PESO < 2500) ~ 0
    ),
    peso_mais_2500 = case_when(
      (PESO >= 2500) ~ 1,
      !(PESO >=2500) ~ 0
    ),
    antes = case_when(
      OBITOPARTO == "Antes" ~ 1,
      !(OBITOPARTO == "Antes") ~ 0
    ),
    durante = case_when(
      (OBITOPARTO == "Durante") ~ 1,
      !(OBITOPARTO == "Durante") ~ 0
    ),
    depois = case_when(
      (OBITOPARTO == "Depois") ~ 1,
      !(OBITOPARTO == "Depois") ~ 0
    ),
    antes = case_when(
      (OBITOPARTO == "Antes") ~ 1,
      !(OBITOPARTO == "Antes") ~ 0
    ),

    antes_peso_menos_1500 = case_when(
      (OBITOPARTO == "Antes" & PESO < 1500) ~ 1,
      !(OBITOPARTO == "Antes" & PESO < 1500) ~ 0
    ),
    antes_peso_1500_1999 = case_when(
      (OBITOPARTO == "Antes" & PESO >= 1500 & PESO < 2000) ~ 1,
      !(OBITOPARTO == "Antes" & PESO >= 1500 & PESO < 2000) ~ 0
    ),

    antes_peso_2000_2499 = case_when(
      (OBITOPARTO == "Antes" & PESO >= 2000 & PESO < 2500) ~ 1,
      !(OBITOPARTO == "Antes" & PESO >= 2000 & PESO < 2500) ~ 0
    ),

    antes_peso_mais_2500 = case_when(
      (OBITOPARTO == "Antes" & PESO >= 2500) ~ 1,
      !(OBITOPARTO == "Antes" & PESO >= 2500) ~ 0
    ),

    durante = case_when(
      (OBITOPARTO == "Durante") ~ 1,
      !(OBITOPARTO == "Durante") ~ 0
    ),
    durante_peso_menos_1500 = case_when(
      (OBITOPARTO == "Durante" & PESO < 1500) ~ 1,
      !(OBITOPARTO == "Durante" & PESO < 1500) ~ 0
    ),
    durante_peso_1500_1999 = case_when(
      (OBITOPARTO == "Durante" & PESO >= 1500 & PESO < 2000) ~ 1,
      !(OBITOPARTO == "Durante" & PESO >= 1500 & PESO < 2000) ~ 0
    ),

    durante_peso_2000_2499 = case_when(
      (OBITOPARTO == "Durante" & PESO >= 2000 & PESO < 2500) ~ 1,
      !(OBITOPARTO == "Durante" & PESO >= 2000 & PESO < 2500) ~ 0
    ),

    durante_peso_mais_2500 = case_when(
      (OBITOPARTO == "Durante" & PESO >= 2500) ~ 1,
      !(OBITOPARTO == "Durante" & PESO >= 2500) ~ 0
    ),

    depois = case_when(
      (OBITOPARTO == "Depois") ~ 1,
      !(OBITOPARTO == "Depois") ~ 0
    ),

    depois_peso_menos_1500 = case_when(
      (OBITOPARTO == "Depois" & PESO < 1500) ~ 1,
      !(OBITOPARTO == "Depois" & PESO < 1500) ~ 0
    ),
    depois_peso_1500_1999 = case_when(
      (OBITOPARTO == "Depois" & PESO >= 1500 & PESO < 2000) ~ 1,
      !(OBITOPARTO == "Depois" & PESO >= 1500 & PESO < 2000) ~ 0
    ),

    depois_peso_2000_2499 = case_when(
      (OBITOPARTO == "Depois" & PESO >= 2000 & PESO < 2500) ~ 1,
      !(OBITOPARTO == "Depois" & PESO >= 2000 & PESO < 2500) ~ 0
    ),

    depois_peso_mais_2500 = case_when(
      (OBITOPARTO == "Depois" & PESO >= 2500) ~ 1,
      !(OBITOPARTO == "Depois" & PESO >= 2500) ~ 0
    )

  ) |>
  select(!c(DTOBITO, PESO)) |>
  group_by(ano, codmunres) |>
  summarise(
    obitos_fetais_mais_22sem = sum(obitos),
    fetal_peso_menos_1500 = sum(peso_menos_1500, na.rm = T),
    fetal_peso_1500_1999 = sum(peso_1500_1999, na.rm = T),
    fetal_peso_2000_2499 = sum(peso_2000_2499, na.rm = T),
    fetal_peso_mais_2500 = sum(peso_mais_2500, na.rm = T),
    fetal_antes = sum(antes, na.rm = T),
    fetal_durante = sum(durante, na.rm = T),
    fetal_depois = sum(depois, na.rm = T),
    fetal_antes_peso_menos_1500 = sum(antes_peso_menos_1500, na.rm = T),
    fetal_antes_peso_1500_1999 = sum(antes_peso_1500_1999, na.rm = T),
    fetal_antes_peso_2000_2499 = sum(antes_peso_2000_2499, na.rm = T),
    fetal_antes_peso_mais_2500 = sum(antes_peso_mais_2500, na.rm = T),
    fetal_durante_peso_menos_1500 = sum(durante_peso_menos_1500, na.rm = T),
    fetal_durante_peso_1500_1999 = sum(durante_peso_1500_1999, na.rm = T),
    fetal_durante_peso_2000_2499 = sum(durante_peso_2000_2499, na.rm = T),
    fetal_durante_peso_mais_2500 = sum(durante_peso_mais_2500, na.rm = T),
    fetal_depois_peso_menos_1500 = sum(depois_peso_menos_1500, na.rm = T),
    fetal_depois_peso_1500_1999 = sum(depois_peso_1500_1999, na.rm = T),
    fetal_depois_peso_2000_2499 = sum(depois_peso_2000_2499, na.rm = T),
    fetal_depois_peso_mais_2500 = sum(depois_peso_mais_2500, na.rm = T)
    ) |> ungroup()


########### Número de nascidos vivos

# Nascidos vivos por município de 2012 a 2021

df_nascidos <- data.frame()


params = paste0('{
       "token": {
         "token": "',token,'"
       },
       "sql": {
         "sql": {"query": "SELECT res_codigo_adotado, ano_nasc, COUNT(1)',
                ' FROM \\"datasus-sinasc\\"',
                ' WHERE (ano_nasc >= 2012 AND ano_nasc<=2021)',
                ' GROUP BY res_codigo_adotado, ano_nasc",
                         "fetch_size": 65000}
       }
     }')

request <- POST(url = endpoint, body = params, encode = "form")
df_mun4 <- convertRequestToDF(request)
names(df_mun4) <- c('codmunres', 'ano', 'nascidos')
df_nascidos <- rbind(df_nascidos, df_mun4)

######## Juntando as bases

df_nascidos$codmunres <- as.numeric(df_nascidos$codmunres)
df_fetais$codmunres <- as.numeric(df_fetais$codmunres)

juncao1 <- left_join(tabela_aux_municipios, df_nascidos, by=c("codmunres"))
juncao2 <- left_join(juncao1, df_fetais, by=c("ano", "codmunres"))

df_obitos_fetais <- juncao2 |>
  select(
    codmunres,
    municipio,
    ano,
    nascidos,
    obitos_fetais_mais_22sem,
    fetal_peso_menos_1500,
    fetal_peso_1500_1999,
    fetal_peso_2000_2499,
    fetal_peso_mais_2500,
    fetal_antes,
    fetal_durante,
    fetal_depois,
    fetal_antes_peso_menos_1500,
    fetal_antes_peso_1500_1999,
    fetal_antes_peso_2000_2499,
    fetal_antes_peso_mais_2500,
    fetal_durante_peso_menos_1500,
    fetal_durante_peso_1500_1999,
    fetal_durante_peso_2000_2499,
    fetal_durante_peso_mais_2500,
    fetal_depois_peso_menos_1500,
    fetal_depois_peso_1500_1999,
    fetal_depois_peso_2000_2499,
    fetal_depois_peso_mais_2500
  )

df_obitos_fetais$obitos_fetais_mais_22sem[is.na(df_obitos_fetais$obitos_fetais_mais_22sem)] <- 0
df_obitos_fetais$nascidos[is.na(df_obitos_fetais$nascidos)] <- 0
df_obitos_fetais$fetal_peso_menos_1500[is.na(df_obitos_fetais$fetal_peso_menos_1500)] <- 0
df_obitos_fetais$fetal_peso_1500_1999[is.na(df_obitos_fetais$fetal_peso_1500_1999)] <- 0
df_obitos_fetais$fetal_peso_2000_2499[is.na(df_obitos_fetais$fetal_peso_2000_2499)] <- 0
df_obitos_fetais$fetal_peso_mais_2500[is.na(df_obitos_fetais$fetal_peso_mais_2500)] <- 0
df_obitos_fetais$fetal_antes[is.na(df_obitos_fetais$fetal_antes)] <- 0
df_obitos_fetais$fetal_durante[is.na(df_obitos_fetais$fetal_durante)] <- 0
df_obitos_fetais$fetal_depois[is.na(df_obitos_fetais$fetal_depois)] <- 0
df_obitos_fetais$fetal_antes_peso_menos_1500[is.na(df_obitos_fetais$fetal_antes_peso_menos_1500)] <- 0
df_obitos_fetais$fetal_antes_peso_1500_1999[is.na(df_obitos_fetais$fetal_antes_peso_1500_1999)] <- 0
df_obitos_fetais$fetal_antes_peso_2000_2499[is.na(df_obitos_fetais$fetal_antes_peso_2000_2499)] <- 0
df_obitos_fetais$fetal_antes_peso_mais_2500[is.na(df_obitos_fetais$fetal_antes_peso_mais_2500)] <- 0
df_obitos_fetais$fetal_durante_peso_menos_1500[is.na(df_obitos_fetais$fetal_durante_peso_menos_1500)] <- 0
df_obitos_fetais$fetal_durante_peso_1500_1999[is.na(df_obitos_fetais$fetal_durante_peso_1500_1999)] <- 0
df_obitos_fetais$fetal_durante_peso_2000_2499[is.na(df_obitos_fetais$fetal_durante_peso_2000_2499)] <- 0
df_obitos_fetais$fetal_durante_peso_mais_2500[is.na(df_obitos_fetais$fetal_durante_peso_mais_2500)] <- 0
df_obitos_fetais$fetal_depois_peso_menos_1500[is.na(df_obitos_fetais$fetal_depois_peso_menos_1500)] <- 0
df_obitos_fetais$fetal_depois_peso_1500_1999[is.na(df_obitos_fetais$fetal_depois_peso_1500_1999)] <- 0
df_obitos_fetais$fetal_depois_peso_2000_2499[is.na(df_obitos_fetais$fetal_depois_peso_2000_2499)] <- 0
df_obitos_fetais$fetal_depois_peso_mais_2500[is.na(df_obitos_fetais$fetal_depois_peso_mais_2500)] <- 0

#write.table(df_obitos_fetais, 'indicadores_bloco7_mortalidade_fetal_2012-2021.csv', sep = ",", dec = ".", row.names = FALSE)



#################### Mortalidade perinatal

# Tabela com o número de óbitos fetais de idade gestacional maior ou igual a 28 semanas por ano e município

df_fetais_28sem <- df_fetais2 |>
  select(
    codmunres = CODMUNRES,
    DTOBITO,
    PESO,
    GESTACAO,
    SEMAGESTAC
  ) |>
  filter(
    ((GESTACAO != "Menos de 22 semanas" & !is.na(GESTACAO) & GESTACAO != "22 a 27 semanas") | (is.na(GESTACAO) & SEMAGESTAC >= 28 & SEMAGESTAC != 99)) | (PESO >= 1000)
  ) |>
  mutate(
    ano = substr(DTOBITO, 1, 4),
    .after = DTOBITO
  ) |>
  mutate(
    peso_menos_1500 = case_when(
      PESO < 1500 ~ 1,
      !(PESO < 1500) ~ 0
    ),
    peso_1500_1999 = case_when(
      (PESO >= 1500 & PESO < 2000) ~ 1,
      !(PESO >= 1500 & PESO < 2000) ~ 0
    ),
    peso_2000_2499 = case_when(
      (PESO >= 2000 & PESO < 2500) ~ 1,
      !(PESO >= 2000 & PESO < 2500) ~ 0
    ),
    peso_mais_2500 = case_when(
      (PESO >= 2500) ~ 1,
      !(PESO >=2500) ~ 0
    ),
    obitos = 1,
    .after = PESO
  )|>
  group_by(ano, codmunres) |>
  summarise(obitos_fetais_mais_28sem = sum(obitos),
            peso_menos_1500_mais_28sem = sum(peso_menos_1500, na.rm=T),
            peso_1500_1999_mais_28sem = sum(peso_1500_1999, na.rm=T),
            peso_2000_2499_mais_28sem = sum(peso_2000_2499, na.rm=T),
            peso_mais_2500_mais_28sem = sum(peso_mais_2500, na.rm=T)) |>
  ungroup()

tabela_aux_municipios$codmunres <- as.character(tabela_aux_municipios$codmunres)

juncao <- left_join(tabela_aux_municipios, df_fetais_28sem, by=c("codmunres"))

juncao$obitos_fetais_mais_28sem[is.na(juncao$obitos_fetais_mais_28sem)] <- 0
juncao$peso_menos_1500_mais_28sem[is.na(juncao$peso_menos_1500_mais_28sem)] <- 0
juncao$peso_1500_1999_mais_28sem[is.na(juncao$peso_1500_1999_mais_28sem)] <- 0
juncao$peso_2000_2499_mais_28sem[is.na(juncao$peso_2000_2499_mais_28sem)] <- 0
juncao$peso_mais_2500_mais_28sem[is.na(juncao$peso_mais_2500_mais_28sem)] <- 0

df_obitos_perinatais <- juncao %>%
  select(
    codmunres,
    municipio,
    ano,
    obitos_fetais_mais_28sem,
    peso_menos_1500_mais_28sem,
    peso_1500_1999_mais_28sem,
    peso_2000_2499_mais_28sem,
    peso_mais_2500_mais_28sem
  )


#write.table(df_obitos_perinatais, 'indicadores_bloco7_mortalidade_perinatal_2012-2021.csv', sep = ",", dec = ".", row.names = FALSE)

#################### Mortalidade neonatal


######## Número de óbitos até 27 dias geral

df_27dias <- data.frame()


params = paste0('{
      "token": {
        "token": "',token,'"
      },
      "sql": {
        "sql": {"query": "SELECT res_codigo_adotado, ano_obito, COUNT(1)',
                ' FROM \\"datasus-sim\\"',
                ' WHERE (ano_obito >= 2012) AND (idade_obito_mins <= 59 OR idade_obito_horas <= 23 OR idade_obito_dias <= 27)',
                ' GROUP BY res_codigo_adotado, ano_obito",
                        "fetch_size": 65000}
      }
    }')

request <- POST(url = endpoint, body = params, encode = "form")
df_mun <- convertRequestToDF(request)
names(df_mun) <- c('codmunres', 'ano', 'obitos_27dias')
df_27dias <- rbind(df_27dias, df_mun)

head(df_27dias)

######## Número de óbitos até 27 dias com menos de 1500 g

df_27dias_menos1500 <- data.frame()


params = paste0('{
      "token": {
        "token": "',token,'"
      },
      "sql": {
        "sql": {"query": "SELECT res_codigo_adotado, ano_obito, COUNT(1)',
                ' FROM \\"datasus-sim\\"',
                ' WHERE (ano_obito >= 2012) AND (idade_obito_mins <= 59 OR idade_obito_horas <= 23 OR idade_obito_dias <= 27) AND (PESO < 1500)',
                ' GROUP BY res_codigo_adotado, ano_obito",
                        "fetch_size": 65000}
      }
    }')

request <- POST(url = endpoint, body = params, encode = "form")
df_mun <- convertRequestToDF(request)
names(df_mun) <- c('codmunres', 'ano', 'obitos_27dias_menos1500')
df_27dias_menos1500 <- rbind(df_27dias_menos1500, df_mun)

head(df_27dias_menos1500)

######## Número de óbitos até 27 dias com menos de 1500 a 1999

df_27dias_1500_1999 <- data.frame()


params = paste0('{
      "token": {
        "token": "',token,'"
      },
      "sql": {
        "sql": {"query": "SELECT res_codigo_adotado, ano_obito, COUNT(1)',
                ' FROM \\"datasus-sim\\"',
                ' WHERE (ano_obito >= 2012) AND (idade_obito_mins <= 59 OR idade_obito_horas <= 23 OR idade_obito_dias <= 27) AND (PESO >= 1500 AND PESO <= 1999)',
                ' GROUP BY res_codigo_adotado, ano_obito",
                        "fetch_size": 65000}
      }
    }')

request <- POST(url = endpoint, body = params, encode = "form")
df_mun <- convertRequestToDF(request)
names(df_mun) <- c('codmunres', 'ano', 'obitos_27dias_1500_1999')
df_27dias_1500_1999 <- rbind(df_27dias_1500_1999, df_mun)

head(df_27dias_1500_1999)

######## Número de óbitos até 27 dias  de 2000 a 2499g

df_27dias_2000_2499 <- data.frame()


params = paste0('{
      "token": {
        "token": "',token,'"
      },
      "sql": {
        "sql": {"query": "SELECT res_codigo_adotado, ano_obito, COUNT(1)',
                ' FROM \\"datasus-sim\\"',
                ' WHERE (ano_obito >= 2012) AND (idade_obito_mins <= 59 OR idade_obito_horas <= 23 OR idade_obito_dias <= 27) AND (PESO >= 2000 AND PESO <= 2499)',
                ' GROUP BY res_codigo_adotado, ano_obito",
                        "fetch_size": 65000}
      }
    }')

request <- POST(url = endpoint, body = params, encode = "form")
df_mun <- convertRequestToDF(request)
names(df_mun) <- c('codmunres', 'ano', 'obitos_27dias_2000_2499')
df_27dias_2000_2499 <- rbind(df_27dias_2000_2499, df_mun)

head(df_27dias_2000_2499)

######## Número de óbitos até 27 dias com mais ou igual a 2500g

df_27dias_mais2500 <- data.frame()


params = paste0('{
      "token": {
        "token": "',token,'"
      },
      "sql": {
        "sql": {"query": "SELECT res_codigo_adotado, ano_obito, COUNT(1)',
                ' FROM \\"datasus-sim\\"',
                ' WHERE (ano_obito >= 2012) AND (idade_obito_mins <= 59 OR idade_obito_horas <= 23 OR idade_obito_dias <= 27) AND (PESO >= 2500)',
                ' GROUP BY res_codigo_adotado, ano_obito",
                        "fetch_size": 65000}
      }
    }')

request <- POST(url = endpoint, body = params, encode = "form")
df_mun <- convertRequestToDF(request)
names(df_mun) <- c('codmunres', 'ano', 'obitos_27dias_mais2500')
df_27dias_mais2500 <- rbind(df_27dias_mais2500, df_mun)

head(df_27dias_mais2500)

######## Número de óbitos de 0 a 6 dias geral

df_6dias <- data.frame()


params = paste0('{
      "token": {
        "token": "',token,'"
      },
      "sql": {
        "sql": {"query": "SELECT res_codigo_adotado, ano_obito, COUNT(1)',
                ' FROM \\"datasus-sim\\"',
                ' WHERE (ano_obito >= 2012) AND (idade_obito_mins <= 59 OR idade_obito_horas <= 23 OR idade_obito_dias <= 6)',
                ' GROUP BY res_codigo_adotado, ano_obito",
                        "fetch_size": 65000}
      }
    }')

request <- POST(url = endpoint, body = params, encode = "form")
df_mun2 <- convertRequestToDF(request)
names(df_mun2) <- c('codmunres', 'ano', 'obitos_6dias')
df_6dias <- rbind(df_6dias, df_mun2)



head(df_6dias)

######## Número de óbitos de 0 a 6 dias com menos de 1500g

df_6dias_menos1500 <- data.frame()


params = paste0('{
      "token": {
        "token": "',token,'"
      },
      "sql": {
        "sql": {"query": "SELECT res_codigo_adotado, ano_obito, COUNT(1)',
                ' FROM \\"datasus-sim\\"',
                ' WHERE (ano_obito >= 2012) AND (idade_obito_mins <= 59 OR idade_obito_horas <= 23 OR idade_obito_dias <= 6) AND (PESO < 1500)',
                ' GROUP BY res_codigo_adotado, ano_obito",
                        "fetch_size": 65000}
      }
    }')

request <- POST(url = endpoint, body = params, encode = "form")
df_mun2 <- convertRequestToDF(request)
names(df_mun2) <- c('codmunres', 'ano', 'obitos_6dias_menos1500')
df_6dias_menos1500 <- rbind(df_6dias_menos1500, df_mun2)



head(df_6dias_menos1500)

######## Número de óbitos de 0 a 6 dias de 1500g a 1999g

df_6dias_1500_1999 <- data.frame()


params = paste0('{
      "token": {
        "token": "',token,'"
      },
      "sql": {
        "sql": {"query": "SELECT res_codigo_adotado, ano_obito, COUNT(1)',
                ' FROM \\"datasus-sim\\"',
                ' WHERE (ano_obito >= 2012) AND (idade_obito_mins <= 59 OR idade_obito_horas <= 23 OR idade_obito_dias <= 6) AND (PESO >= 1500 AND PESO <= 1999)',
                ' GROUP BY res_codigo_adotado, ano_obito",
                        "fetch_size": 65000}
      }
    }')

request <- POST(url = endpoint, body = params, encode = "form")
df_mun2 <- convertRequestToDF(request)
names(df_mun2) <- c('codmunres', 'ano', 'obitos_6dias_1500_1999')
df_6dias_1500_1999 <- rbind(df_6dias_1500_1999, df_mun2)



head(df_6dias_1500_1999)

######## Número de óbitos de 0 a 6 dias de 2000g a 2499g

df_6dias_2000_2499 <- data.frame()


params = paste0('{
      "token": {
        "token": "',token,'"
      },
      "sql": {
        "sql": {"query": "SELECT res_codigo_adotado, ano_obito, COUNT(1)',
                ' FROM \\"datasus-sim\\"',
                ' WHERE (ano_obito >= 2012) AND (idade_obito_mins <= 59 OR idade_obito_horas <= 23 OR idade_obito_dias <= 6) AND (PESO >= 2000 AND PESO <= 2499)',
                ' GROUP BY res_codigo_adotado, ano_obito",
                        "fetch_size": 65000}
      }
    }')

request <- POST(url = endpoint, body = params, encode = "form")
df_mun2 <- convertRequestToDF(request)
names(df_mun2) <- c('codmunres', 'ano', 'obitos_6dias_2000_2499')
df_6dias_2000_2499 <- rbind(df_6dias_2000_2499, df_mun2)



head(df_6dias_2000_2499)

######## Número de óbitos de 0 a 6 dias com mais de 2500g

df_6dias_mais2500 <- data.frame()


params = paste0('{
      "token": {
        "token": "',token,'"
      },
      "sql": {
        "sql": {"query": "SELECT res_codigo_adotado, ano_obito, COUNT(1)',
                ' FROM \\"datasus-sim\\"',
                ' WHERE (ano_obito >= 2012) AND (idade_obito_mins <= 59 OR idade_obito_horas <= 23 OR idade_obito_dias <= 6) AND (PESO >= 2500)',
                ' GROUP BY res_codigo_adotado, ano_obito",
                        "fetch_size": 65000}
      }
    }')

request <- POST(url = endpoint, body = params, encode = "form")
df_mun2 <- convertRequestToDF(request)
names(df_mun2) <- c('codmunres', 'ano', 'obitos_6dias_mais2500')
df_6dias_mais2500 <- rbind(df_6dias_mais2500, df_mun2)



head(df_6dias_mais2500)

######## Número de óbitos de 7 a 27 dias geral

df_7_27dias <- data.frame()


params = paste0('{
      "token": {
        "token": "',token,'"
      },
      "sql": {
        "sql": {"query": "SELECT res_codigo_adotado, ano_obito, COUNT(1)',
                ' FROM \\"datasus-sim\\"',
                ' WHERE (ano_obito >= 2012) AND (idade_obito_dias <= 27 AND idade_obito_dias >= 7)',
                ' GROUP BY res_codigo_adotado, ano_obito",
                        "fetch_size": 65000}
      }
    }')

request <- POST(url = endpoint, body = params, encode = "form")
df_mun3 <- convertRequestToDF(request)
names(df_mun3) <- c('codmunres', 'ano', 'obitos_7_27dias')
df_7_27dias <- rbind(df_7_27dias, df_mun3)



head(df_7_27dias)

######## Número de óbitos de 7 a 27 dias abaixo de 1500g

df_7_27dias_menos1500 <- data.frame()


params = paste0('{
      "token": {
        "token": "',token,'"
      },
      "sql": {
        "sql": {"query": "SELECT res_codigo_adotado, ano_obito, COUNT(1)',
                ' FROM \\"datasus-sim\\"',
                ' WHERE (ano_obito >= 2012) AND (idade_obito_dias <= 27 AND idade_obito_dias >= 7) AND (PESO < 1500)',
                ' GROUP BY res_codigo_adotado, ano_obito",
                        "fetch_size": 65000}
      }
    }')

request <- POST(url = endpoint, body = params, encode = "form")
df_mun3 <- convertRequestToDF(request)
names(df_mun3) <- c('codmunres', 'ano', 'obitos_7_27dias_menos1500')
df_7_27dias_menos1500 <- rbind(df_7_27dias_menos1500, df_mun3)



head(df_7_27dias_menos1500)

######## Número de óbitos de 7 a 27 dias de 1500 a 1999g

df_7_27dias_1500_1999 <- data.frame()


params = paste0('{
      "token": {
        "token": "',token,'"
      },
      "sql": {
        "sql": {"query": "SELECT res_codigo_adotado, ano_obito, COUNT(1)',
                ' FROM \\"datasus-sim\\"',
                ' WHERE (ano_obito >= 2012) AND (idade_obito_dias <= 27 AND idade_obito_dias >= 7) AND (PESO >= 1500 AND PESO <= 1999)',
                ' GROUP BY res_codigo_adotado, ano_obito",
                        "fetch_size": 65000}
      }
    }')

request <- POST(url = endpoint, body = params, encode = "form")
df_mun3 <- convertRequestToDF(request)
names(df_mun3) <- c('codmunres', 'ano', 'obitos_7_27dias_1500_1999')
df_7_27dias_1500_1999 <- rbind(df_7_27dias_1500_1999, df_mun3)



head(df_7_27dias_1500_1999)

######## Número de óbitos de 7 a 27 dias de 2000 a 2499g

df_7_27dias_2000_2499 <- data.frame()


params = paste0('{
      "token": {
        "token": "',token,'"
      },
      "sql": {
        "sql": {"query": "SELECT res_codigo_adotado, ano_obito, COUNT(1)',
                ' FROM \\"datasus-sim\\"',
                ' WHERE (ano_obito >= 2012) AND (idade_obito_dias <= 27 AND idade_obito_dias >= 7) AND (PESO >= 2000 AND PESO <= 2499)',
                ' GROUP BY res_codigo_adotado, ano_obito",
                        "fetch_size": 65000}
      }
    }')

request <- POST(url = endpoint, body = params, encode = "form")
df_mun3 <- convertRequestToDF(request)
names(df_mun3) <- c('codmunres', 'ano', 'obitos_7_27dias_2000_2499')
df_7_27dias_2000_2499 <- rbind(df_7_27dias_2000_2499, df_mun3)



head(df_7_27dias_2000_2499)

######## Número de óbitos de 7 a 27 dias com mais de 2500g

df_7_27dias_mais2500 <- data.frame()


params = paste0('{
      "token": {
        "token": "',token,'"
      },
      "sql": {
        "sql": {"query": "SELECT res_codigo_adotado, ano_obito, COUNT(1)',
                ' FROM \\"datasus-sim\\"',
                ' WHERE (ano_obito >= 2012) AND (idade_obito_dias <= 27 AND idade_obito_dias >= 7) AND (PESO >= 2500)',
                ' GROUP BY res_codigo_adotado, ano_obito",
                        "fetch_size": 65000}
      }
    }')

request <- POST(url = endpoint, body = params, encode = "form")
df_mun3 <- convertRequestToDF(request)
names(df_mun3) <- c('codmunres', 'ano', 'obitos_7_27dias_mais2500')
df_7_27dias_mais2500 <- rbind(df_7_27dias_mais2500, df_mun3)



head(df_7_27dias_mais2500)

######## Número de nascidos vivos por municícpio de 2012 a 2021

df_nascidos <- data.frame()


params = paste0('{
      "token": {
        "token": "',token,'"
      },
      "sql": {
        "sql": {"query": "SELECT res_codigo_adotado, ano_nasc, COUNT(1)',
                ' FROM \\"datasus-sinasc\\"',
                ' WHERE (ano_nasc >= 2012)',
                ' GROUP BY res_codigo_adotado, ano_nasc",
                        "fetch_size": 65000}
      }
    }')

request <- POST(url = endpoint, body = params, encode = "form")
df_mun4 <- convertRequestToDF(request)
names(df_mun4) <- c('codmunres', 'ano', 'nascidos')
df_nascidos <- rbind(df_nascidos, df_mun4)



head(df_nascidos)

######## Número de nascidos vivos por municícpio de 2012 a 2021 com menos de 1500g

df_nascidos_menos1500 <- data.frame()


params = paste0('{
      "token": {
        "token": "',token,'"
      },
      "sql": {
        "sql": {"query": "SELECT res_codigo_adotado, ano_nasc, COUNT(1)',
                ' FROM \\"datasus-sinasc\\"',
                ' WHERE (ano_nasc >= 2012) AND (PESO < 1500)',
                ' GROUP BY res_codigo_adotado, ano_nasc",
                        "fetch_size": 65000}
      }
    }')

request <- POST(url = endpoint, body = params, encode = "form")
df_mun4 <- convertRequestToDF(request)
names(df_mun4) <- c('codmunres', 'ano', 'nascidos_menos1500')
df_nascidos_menos1500 <- rbind(df_nascidos_menos1500, df_mun4)



head(df_nascidos_menos1500)

######## Número de nascidos vivos por municícpio de 2012 a 2021 com de 1500g a 1999g

df_nascidos_1500_1999 <- data.frame()


params = paste0('{
      "token": {
        "token": "',token,'"
      },
      "sql": {
        "sql": {"query": "SELECT res_codigo_adotado, ano_nasc, COUNT(1)',
                ' FROM \\"datasus-sinasc\\"',
                ' WHERE (ano_nasc >= 2012) AND (PESO >=  1500 AND PESO<=1999)',
                ' GROUP BY res_codigo_adotado, ano_nasc",
                        "fetch_size": 65000}
      }
    }')

request <- POST(url = endpoint, body = params, encode = "form")
df_mun4 <- convertRequestToDF(request)
names(df_mun4) <- c('codmunres', 'ano', 'nascidos_1500_1999')
df_nascidos_1500_1999 <- rbind(df_nascidos_1500_1999, df_mun4)



head(df_nascidos_1500_1999)

######## Número de nascidos vivos por municícpio de 2012 a 2021 com de 2000g a 2499g

df_nascidos_2000_2499 <- data.frame()


params = paste0('{
      "token": {
        "token": "',token,'"
      },
      "sql": {
        "sql": {"query": "SELECT res_codigo_adotado, ano_nasc, COUNT(1)',
                ' FROM \\"datasus-sinasc\\"',
                ' WHERE (ano_nasc >= 2012) AND (PESO >=  2000 AND PESO<=2499)',
                ' GROUP BY res_codigo_adotado, ano_nasc",
                        "fetch_size": 65000}
      }
    }')

request <- POST(url = endpoint, body = params, encode = "form")
df_mun4 <- convertRequestToDF(request)
names(df_mun4) <- c('codmunres', 'ano', 'nascidos_2000_2499')
df_nascidos_2000_2499 <- rbind(df_nascidos_2000_2499, df_mun4)



head(df_nascidos_2000_2499)

######## Número de nascidos vivos por municícpio de 2012 a 2021 com peso maior ou igual a 2500g

df_nascidos_mais2500 <- data.frame()


params = paste0('{
      "token": {
        "token": "',token,'"
      },
      "sql": {
        "sql": {"query": "SELECT res_codigo_adotado, ano_nasc, COUNT(1)',
                ' FROM \\"datasus-sinasc\\"',
                ' WHERE (ano_nasc >= 2012) AND (PESO >=  2500)',
                ' GROUP BY res_codigo_adotado, ano_nasc",
                        "fetch_size": 65000}
      }
    }')

request <- POST(url = endpoint, body = params, encode = "form")
df_mun4 <- convertRequestToDF(request)
names(df_mun4) <- c('codmunres', 'ano', 'nascidos_mais2500')
df_nascidos_mais2500 <- rbind(df_nascidos_mais2500, df_mun4)



head(df_nascidos_mais2500)



######## Juntando as dataframes de número de óbitos de até 27 dias, entre 0 e 6 e entre 7 e 17

df_juncao1 <- left_join(df_27dias, df_6dias, by = c("codmunres", "ano"))
df_juncao2 <- left_join(df_juncao1, df_7_27dias, by = c("codmunres", "ano"))
df_juncao3 <- left_join(df_juncao2, df_27dias_menos1500, by = c("codmunres", "ano"))
df_juncao4 <- left_join(df_juncao3, df_27dias_1500_1999, by = c("codmunres", "ano"))
df_juncao5 <- left_join(df_juncao4, df_27dias_2000_2499, by = c("codmunres", "ano"))
df_juncao7 <- left_join(df_juncao5, df_27dias_mais2500, by = c("codmunres", "ano"))
df_juncao8 <- left_join(df_juncao7, df_6dias_menos1500, by = c("codmunres", "ano"))
df_juncao9 <- left_join(df_juncao8, df_6dias_1500_1999, by = c("codmunres", "ano"))
df_juncao10 <- left_join(df_juncao9, df_6dias_2000_2499, by = c("codmunres", "ano"))
df_juncao11 <- left_join(df_juncao10, df_6dias_mais2500, by = c("codmunres", "ano"))
df_juncao12 <- left_join(df_juncao11, df_7_27dias_menos1500, by = c("codmunres", "ano"))
df_juncao13 <- left_join(df_juncao12, df_7_27dias_1500_1999, by = c("codmunres", "ano"))
df_juncao14 <- left_join(df_juncao13, df_7_27dias_2000_2499, by = c("codmunres", "ano"))
df_juncao15 <- left_join(df_juncao14, df_7_27dias_mais2500, by = c("codmunres", "ano"))

######## Juntando as dataframes de número de óbitos com a de número de nascidos vivos
df_juncao16 <- left_join(df_nascidos, df_nascidos_menos1500, by = c("codmunres", "ano"))
df_juncao17 <- left_join(df_juncao16, df_nascidos_1500_1999, by = c("codmunres", "ano"))
df_juncao18 <- left_join(df_juncao17, df_nascidos_2000_2499, by = c("codmunres", "ano"))
df_juncao19 <- left_join(df_juncao18, df_nascidos_mais2500, by = c("codmunres", "ano"))
df_mort_neonatal <- left_join(df_juncao19, df_juncao15, by = c("codmunres", "ano"))


####### Juntando os dados às informações sobre os municípios


tabela_aux_municipios$codmunres <- as.character(tabela_aux_municipios$codmunres)
df_mort_neonatal_completo <- left_join(tabela_aux_municipios, df_mort_neonatal, by=c("codmunres"))

df_mort_neonatal_completo$obitos_27dias <- as.numeric(df_mort_neonatal_completo$obitos_27dias)
df_mort_neonatal_completo$obitos_6dias <- as.numeric(df_mort_neonatal_completo$obitos_6dias)
df_mort_neonatal_completo$obitos_7_27dias <- as.numeric(df_mort_neonatal_completo$obitos_7_27dias)
df_mort_neonatal_completo$nascidos <- as.numeric(df_mort_neonatal_completo$nascidos)
df_mort_neonatal_completo$obitos_27dias_menos1500 <- as.numeric(df_mort_neonatal_completo$obitos_27dias_menos1500)
df_mort_neonatal_completo$obitos_27dias_1500_1999 <- as.numeric(df_mort_neonatal_completo$obitos_27dias_1500_1999)
df_mort_neonatal_completo$obitos_27dias_2000_2499 <- as.numeric(df_mort_neonatal_completo$obitos_27dias_2000_2499)
df_mort_neonatal_completo$obitos_27dias_mais2500 <- as.numeric(df_mort_neonatal_completo$obitos_27dias_mais2500)
df_mort_neonatal_completo$obitos_6dias_menos1500 <- as.numeric(df_mort_neonatal_completo$obitos_6dias_menos1500)
df_mort_neonatal_completo$obitos_6dias_1500_1999 <- as.numeric(df_mort_neonatal_completo$obitos_6dias_1500_1999)
df_mort_neonatal_completo$obitos_6dias_2000_2499 <- as.numeric(df_mort_neonatal_completo$obitos_6dias_2000_2499)
df_mort_neonatal_completo$obitos_6dias_mais2500 <- as.numeric(df_mort_neonatal_completo$obitos_6dias_mais2500)
df_mort_neonatal_completo$obitos_7_27dias_menos1500 <- as.numeric(df_mort_neonatal_completo$obitos_7_27dias_menos1500)
df_mort_neonatal_completo$obitos_7_27dias_1500_1999 <- as.numeric(df_mort_neonatal_completo$obitos_7_27dias_1500_1999)
df_mort_neonatal_completo$obitos_7_27dias_2000_2499 <- as.numeric(df_mort_neonatal_completo$obitos_7_27dias_2000_2499)
df_mort_neonatal_completo$obitos_7_27dias_mais2500 <- as.numeric(df_mort_neonatal_completo$obitos_7_27dias_mais2500)
df_mort_neonatal_completo$nascidos_menos1500 <- as.numeric(df_mort_neonatal_completo$nascidos_menos1500)
df_mort_neonatal_completo$nascidos_1500_1999 <- as.numeric(df_mort_neonatal_completo$nascidos_1500_1999)
df_mort_neonatal_completo$nascidos_2000_2499 <- as.numeric(df_mort_neonatal_completo$nascidos_2000_2499)
df_mort_neonatal_completo$nascidos_mais2500 <- as.numeric(df_mort_neonatal_completo$nascidos_mais2500)

df_mort_neonatal_completo[is.na(df_mort_neonatal_completo)] <- 0

bloco7_mortalidade_neonatal <- df_mort_neonatal_completo %>%
  select(
    codmunres,
    municipio,
    ano,
    nascidos,
    nascidos_menos1500,
    nascidos_1500_1999,
    nascidos_2000_2499,
    nascidos_mais2500,
    obitos_27dias,
    obitos_6dias,
    obitos_7_27dias,
    obitos_27dias_menos1500,
    obitos_27dias_1500_1999,
    obitos_27dias_2000_2499,
    obitos_27dias_mais2500,
    obitos_6dias_menos1500,
    obitos_6dias_1500_1999,
    obitos_6dias_2000_2499,
    obitos_6dias_mais2500,
    obitos_7_27dias_menos1500,
    obitos_7_27dias_1500_1999,
    obitos_7_27dias_2000_2499,
    obitos_7_27dias_mais2500,
  )

#write.table(bloco7_mortalidade_neonatal, 'indicadores_bloco7_mortalidade_neonatal_2012-2021.csv', sep = ";", dec = ".", row.names = FALSE)
