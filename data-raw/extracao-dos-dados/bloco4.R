library(tidyverse)

#Criando um objeto que recebe os códigos dos municípios que utilizamos no painel
codigos_municipios <- read.csv("Databases/tabela_aux_municipios.csv") |>
  pull(codmunres)

#Criando um data.frame auxiliar que possui uma linha para cada combinação de município e ano
df_aux_municipios <- data.frame(codmunres = rep(codigos_municipios, each = length(2012:2022)), ano = 2012:2022)

#Criando o data.frame que irá receber todos os dados do bloco 4
df_bloco4 <- data.frame()

# Baixar os dados ano a ano
# Criar um vetor para armazenar os dados de cada ano
df_list <- list()

# Baixar os dados ano a ano
for (ano in 2012:2022) {
  df_ano <- microdatasus::fetch_datasus(year_start = ano, year_end = ano,
                                        information_system = "SINASC",
                                        vars = c("CODMUNRES"))

  # Adicionar a variável ANO ao dataframe
  df_ano$ano <- ano

  # Adicionar o dataframe à lista
  df_list[[ano - 2011]] <- df_ano
}

# Juntar os dataframes da lista em um único dataframe
df <- bind_rows(df_list)


df <- df %>%
  group_by(ano, CODMUNRES) %>%
  summarise(total_de_nascidos_vivos = n()) %>%
  rename(codmunres = CODMUNRES)

df <- df %>%
  mutate_if(is.character, as.numeric)



##Fazendo um left_join da base auxiliar de municípios com o data.frame que contém o total de nascidos vivos
df_bloco4 <- left_join(df_aux_municipios, df)

##Substituindo os NA's da coluna 'total_de_nascidos_vivos' por 0 (os NA's surgem quando um município não apresentou nascidos vivos num dado ano)
df_bloco4$total_de_nascidos_vivos[is.na(df_bloco4$total_de_nascidos_vivos)] <- 0


### Proporção de nascimentos por cesariana
df <- dataframe <- data.frame()

# Baixar os dados ano a ano
for (ano in 2012:2022) {
  df_ano <- microdatasus::fetch_datasus(year_start = ano, year_end = ano,
                                        information_system = "SINASC",
                                        vars = c("CODMUNRES", "PARTO"))

  # Adicionar a variável ANO ao dataframe
  df_ano$ano <- ano

  # Adicionar o dataframe à lista
  df_list[[ano - 2011]] <- df_ano
}

# Juntar os dataframes da lista em um único dataframe
df <- bind_rows(df_list)


df <- df |>
  filter(PARTO == 2) |>
  group_by(ano, CODMUNRES) |>
  summarise(mulheres_com_parto_cesariana = n()) |>
  rename(codmunres = CODMUNRES)


##Transformando as colunas que estão em caracter para numéricas
df <- df |> mutate_if(is.character, as.numeric)

##Juntando com o restante da base do bloco 4
df_bloco4 <- left_join(df_bloco4, df)

##Substituindo os NA's da coluna 'mulheres_com_parto_cesariana' por 0 (gerados após o left_join)
df_bloco4$mulheres_com_parto_cesariana[is.na(df_bloco4$mulheres_com_parto_cesariana)] <- 0



### df_robson

# Baixar os dados ano a ano
for (ano in 2012:2022) {
  df_ano <- microdatasus::fetch_datasus(year_start = ano, year_end = ano,
                                        information_system = "SINASC",
                                        vars = c("CODMUNRES", "TPROBSON"))

  # Adicionar a variável ANO ao dataframe
  df_ano$ano <- ano

  # Adicionar o dataframe à lista
  df_list[[ano - 2011]] <- df_ano
}

# Juntar os dataframes da lista em um único dataframe
df_robson <- bind_rows(df_list)


# Número de nascidos vivos de mulheres pertencentes ao grupo 1 de Robson
df <- dataframe <- data.frame()

# df <- microdatasus::fetch_datasus(year_start = 2022, year_end = 2022,
#                                   information_system = "SINASC",
#                                   vars = c("CODMUNRES", "TPROBSON"))

df <- df_robson |>
  filter(TPROBSON == "01") |>
  group_by(ano, CODMUNRES) |>
  summarise(mulheres_dentro_do_grupo_de_robson_1 = n()) |>
  rename(codmunres = CODMUNRES)


##Transformando as colunas que estão em caracter para numéricas
df <- df |> mutate_if(is.character, as.numeric)

##Juntando com o restante da base do bloco 4
df_bloco4 <- left_join(df_bloco4, df)

##Substituindo os NA's da coluna 'mulheres_dentro_do_grupo_de_robson_1' por 0 (gerados após o left_join)
df_bloco4$mulheres_dentro_do_grupo_de_robson_1[is.na(df_bloco4$mulheres_dentro_do_grupo_de_robson_1)] <- 0




# Número de nascidos vivos de mulheres pertencentes ao grupo 2 de Robson
df <- dataframe <- data.frame()

# df <- microdatasus::fetch_datasus(year_start = 2022, year_end = 2022,
#                                   information_system = "SINASC",
#                                   vars = c("CODMUNRES", "TPROBSON"))

df <- df_robson |>
  filter(TPROBSON == "02") |>
  group_by(ano, CODMUNRES) |>
  summarise(mulheres_dentro_do_grupo_de_robson_2 = n()) |>
  rename(codmunres = CODMUNRES)


##Transformando as colunas que estão em caracter para numéricas
df <- df |> mutate_if(is.character, as.numeric)

##Juntando com o restante da base do bloco 4
df_bloco4 <- left_join(df_bloco4, df)

##Substituindo os NA's da coluna 'mulheres_dentro_do_grupo_de_robson_2' por 0 (gerados após o left_join)
df_bloco4$mulheres_dentro_do_grupo_de_robson_2[is.na(df_bloco4$mulheres_dentro_do_grupo_de_robson_2)] <- 0

# Número de nascidos vivos de mulheres pertencentes ao grupo 3 de Robson
df <- dataframe <- data.frame()

# df <- microdatasus::fetch_datasus(year_start = 2022, year_end = 2022,
#                                   information_system = "SINASC",
#                                   vars = c("CODMUNRES", "TPROBSON"))

df <- df_robson |>
  filter(TPROBSON == "03") |>
  group_by(ano, CODMUNRES) |>
  summarise(mulheres_dentro_do_grupo_de_robson_3 = n()) |>
  rename(codmunres = CODMUNRES)


##Transformando as colunas que estão em caracter para numéricas
df <- df |> mutate_if(is.character, as.numeric)

##Juntando com o restante da base do bloco 4
df_bloco4 <- left_join(df_bloco4, df)

##Substituindo os NA's da coluna 'mulheres_dentro_do_grupo_de_robson_3' por 0 (gerados após o left_join)
df_bloco4$mulheres_dentro_do_grupo_de_robson_3[is.na(df_bloco4$mulheres_dentro_do_grupo_de_robson_3)] <- 0


# Número de nascidos vivos de mulheres pertencentes ao grupo 4 de Robson
df <- dataframe <- data.frame()

# df <- microdatasus::fetch_datasus(year_start = 2022, year_end = 2022,
#                                   information_system = "SINASC",
#                                   vars = c("CODMUNRES", "TPROBSON"))

df <- df_robson |>
  filter(TPROBSON == "04") |>
  group_by(ano, CODMUNRES) |>
  summarise(mulheres_dentro_do_grupo_de_robson_4 = n()) |>
  rename(codmunres = CODMUNRES)


##Transformando as colunas que estão em caracter para numéricas
df <- df |> mutate_if(is.character, as.numeric)

##Juntando com o restante da base do bloco 4
df_bloco4 <- left_join(df_bloco4, df)

##Substituindo os NA's da coluna 'mulheres_dentro_do_grupo_de_robson_4' por 0 (gerados após o left_join)
df_bloco4$mulheres_dentro_do_grupo_de_robson_4[is.na(df_bloco4$mulheres_dentro_do_grupo_de_robson_4)] <- 0


# Número de nascidos vivos de mulheres pertencentes ao grupo 5 de Robson
df <- dataframe <- data.frame()

# df <- microdatasus::fetch_datasus(year_start = 2022, year_end = 2022,
#                                   information_system = "SINASC",
#                                   vars = c("CODMUNRES", "TPROBSON"))

df <- df_robson |>
  filter(TPROBSON == "05") |>
  group_by(ano, CODMUNRES) |>
  summarise(mulheres_dentro_do_grupo_de_robson_5 = n()) |>
  rename(codmunres = CODMUNRES)


##Transformando as colunas que estão em caracter para numéricas
df <- df |> mutate_if(is.character, as.numeric)

##Juntando com o restante da base do bloco 4
df_bloco4 <- left_join(df_bloco4, df)

##Substituindo os NA's da coluna 'mulheres_dentro_do_grupo_de_robson_5' por 0 (gerados após o left_join)
df_bloco4$mulheres_dentro_do_grupo_de_robson_5[is.na(df_bloco4$mulheres_dentro_do_grupo_de_robson_5)] <- 0

# Número de nascidos vivos de mulheres pertencentes ao grupo  de Robson de 6 ao 9
df <- dataframe <- data.frame()

# df <- microdatasus::fetch_datasus(year_start = 2022, year_end = 2022,
#                                   information_system = "SINASC",
#                                   vars = c("CODMUNRES", "TPROBSON"))

df <- df_robson |>
  filter(TPROBSON >= "06" & TPROBSON <= "09") |>
  group_by(ano, CODMUNRES) |>
  summarise(mulheres_dentro_do_grupo_de_robson_6_ao_9 = n()) |>
  rename(codmunres = CODMUNRES)

##Transformando as colunas que estão em caracter para numéricas
df <- df |> mutate_if(is.character, as.numeric)

##Juntando com o restante da base do bloco 4
df_bloco4 <- left_join(df_bloco4, df)

##Substituindo os NA's da coluna 'mulheres_dentro_do_grupo_de_robson_6_ao_9' por 0 (gerados após o left_join)
df_bloco4$mulheres_dentro_do_grupo_de_robson_6_ao_9[is.na(df_bloco4$mulheres_dentro_do_grupo_de_robson_6_ao_9)] <- 0

# Número de nascidos vivos de mulheres pertencentes ao grupo 10 de Robson
df <- dataframe <- data.frame()

# df <- microdatasus::fetch_datasus(year_start = 2022, year_end = 2022,
#                                   information_system = "SINASC",
#                                   vars = c("CODMUNRES", "TPROBSON"))

df <- df_robson |>
  filter(TPROBSON == "10") |>
  group_by(ano, CODMUNRES) |>
  summarise(mulheres_dentro_do_grupo_de_robson_10 = n()) |>
  rename(codmunres = CODMUNRES)


##Transformando as colunas que estão em caracter para numéricas
df <- df |> mutate_if(is.character, as.numeric)

##Juntando com o restante da base do bloco 4
df_bloco4 <- left_join(df_bloco4, df)

##Substituindo os NA's da coluna 'mulheres_dentro_do_grupo_de_robson_10' por 0 (gerados após o left_join)
df_bloco4$mulheres_dentro_do_grupo_de_robson_10[is.na(df_bloco4$mulheres_dentro_do_grupo_de_robson_10)] <- 0


# Contribuição relativa de cada grupo de Robson na taxa global de cesariana

# Baixar os dados ano a ano
for (ano in 2012:2022) {
  df_ano <- microdatasus::fetch_datasus(year_start = ano, year_end = ano,
                                        information_system = "SINASC",
                                        vars = c("CODMUNRES", "TPROBSON", "PARTO"))

  # Adicionar a variável ANO ao dataframe
  df_ano$ano <- ano

  # Adicionar o dataframe à lista
  df_list[[ano - 2011]] <- df_ano
}

# Juntar os dataframes da lista em um único dataframe
df_cesariana <- bind_rows(df_list)

## Número de nascidos vivos de mães do grupo 1

df <- dataframe <- data.frame()

# df <- microdatasus::fetch_datasus(year_start = 2022, year_end = 2022,
#                                   information_system = "SINASC",
#                                   vars = c("CODMUNRES", "TPROBSON", "PARTO"))

df <- df_cesariana |>
  filter(TPROBSON == "01" & PARTO == 2) |>
  group_by(ano, CODMUNRES) |>
  summarise(total_cesariana_grupo_robson_1 = n()) |>
  rename(codmunres = CODMUNRES)

##Transformando as colunas que estão em caracter para numéricas
df <- df |> mutate_if(is.character, as.numeric)

##Juntando com o restante da base do bloco 4
df_bloco4 <- left_join(df_bloco4, df)

##Substituindo os NA's da coluna 'total_cesariana_grupo_robson_1' por 0 (gerados após o left_join)
df_bloco4$total_cesariana_grupo_robson_1[is.na(df_bloco4$total_cesariana_grupo_robson_1)] <- 0


## Número de nascidos vivos de mães do grupo 2

df <- dataframe <- data.frame()

# df <- microdatasus::fetch_datasus(year_start = 2022, year_end = 2022,
#                                   information_system = "SINASC",
#                                   vars = c("CODMUNRES", "TPROBSON", "PARTO"))

df <- df_cesariana |>
  filter(TPROBSON == "02" & PARTO == 2) |>
  group_by(ano, CODMUNRES) |>
  summarise(total_cesariana_grupo_robson_2 = n()) |>
  rename(codmunres = CODMUNRES)

##Transformando as colunas que estão em caracter para numéricas
df <- df |> mutate_if(is.character, as.numeric)

##Juntando com o restante da base do bloco 4
df_bloco4 <- left_join(df_bloco4, df)

##Substituindo os NA's da coluna 'total_cesariana_grupo_robson_2' por 0 (gerados após o left_join)
df_bloco4$total_cesariana_grupo_robson_2[is.na(df_bloco4$total_cesariana_grupo_robson_2)] <- 0


## Número de nascidos vivos de mães do grupo 3

df <- dataframe <- data.frame()

# df <- microdatasus::fetch_datasus(year_start = 2022, year_end = 2022,
#                                   information_system = "SINASC",
#                                   vars = c("CODMUNRES", "TPROBSON", "PARTO"))

df <- df_cesariana |>
  filter(TPROBSON == "03" & PARTO == 2) |>
  group_by(ano, CODMUNRES) |>
  summarise(total_cesariana_grupo_robson_3 = n()) |>
  rename(codmunres = CODMUNRES)

##Transformando as colunas que estão em caracter para numéricas
df <- df |> mutate_if(is.character, as.numeric)

##Juntando com o restante da base do bloco 4
df_bloco4 <- left_join(df_bloco4, df)

##Substituindo os NA's da coluna 'total_cesariana_grupo_robson_3' por 0 (gerados após o left_join)
df_bloco4$total_cesariana_grupo_robson_3[is.na(df_bloco4$total_cesariana_grupo_robson_3)] <- 0


## Número de nascidos vivos de mães do grupo 4

df <- dataframe <- data.frame()

# df <- microdatasus::fetch_datasus(year_start = 2022, year_end = 2022,
#                                   information_system = "SINASC",
#                                   vars = c("CODMUNRES", "TPROBSON", "PARTO"))

df <- df_cesariana |>
  filter(TPROBSON == "04" & PARTO == 2) |>
  group_by(ano, CODMUNRES) |>
  summarise(total_cesariana_grupo_robson_4 = n()) |>
  rename(codmunres = CODMUNRES)

##Transformando as colunas que estão em caracter para numéricas
df <- df |> mutate_if(is.character, as.numeric)

##Juntando com o restante da base do bloco 4
df_bloco4 <- left_join(df_bloco4, df)

##Substituindo os NA's da coluna 'total_cesariana_grupo_robson_4' por 0 (gerados após o left_join)
df_bloco4$total_cesariana_grupo_robson_4[is.na(df_bloco4$total_cesariana_grupo_robson_4)] <- 0


## Número de nascidos vivos de mães do grupo 5

df <- dataframe <- data.frame()

# df <- microdatasus::fetch_datasus(year_start = 2022, year_end = 2022,
#                                   information_system = "SINASC",
#                                   vars = c("CODMUNRES", "TPROBSON", "PARTO"))

df <- df_cesariana |>
  filter(TPROBSON == "05" & PARTO == 2) |>
  group_by(ano, CODMUNRES) |>
  summarise(total_cesariana_grupo_robson_5 = n()) |>
  rename(codmunres = CODMUNRES)

##Transformando as colunas que estão em caracter para numéricas
df <- df |> mutate_if(is.character, as.numeric)

##Juntando com o restante da base do bloco 4
df_bloco4 <- left_join(df_bloco4, df)

##Substituindo os NA's da coluna 'total_cesariana_grupo_robson_4' por 0 (gerados após o left_join)
df_bloco4$total_cesariana_grupo_robson_5[is.na(df_bloco4$total_cesariana_grupo_robson_5)] <- 0

## Número de nascidos vivos de mães dos grupos 6 a 9 conjuntamente
df <- dataframe <- data.frame()

# df <- microdatasus::fetch_datasus(year_start = 2022, year_end = 2022,
#                                   information_system = "SINASC",
#                                   vars = c("CODMUNRES", "TPROBSON", "PARTO"))

df <- df_cesariana |>
  filter(TPROBSON >= "06" & TPROBSON <= "09" & PARTO == 2) |>
  group_by(ano, CODMUNRES) |>
  summarise(total_cesariana_grupo_robson_6_ao_9 = n()) |>
  rename(codmunres = CODMUNRES)

##Transformando as colunas que estão em caracter para numéricas
df <- df |> mutate_if(is.character, as.numeric)

##Juntando com o restante da base do bloco 4
df_bloco4 <- left_join(df_bloco4, df)

##Substituindo os NA's da coluna 'total_cesariana_grupo_robson_6_ao_9' por 0 (gerados após o left_join)
df_bloco4$total_cesariana_grupo_robson_6_ao_9[is.na(df_bloco4$total_cesariana_grupo_robson_6_ao_9)] <- 0

## Número de nascidos vivos de mães do grupo 10

df <- dataframe <- data.frame()

# df <- microdatasus::fetch_datasus(year_start = 2022, year_end = 2022,
#                                   information_system = "SINASC",
#                                   vars = c("CODMUNRES", "TPROBSON", "PARTO"))

df <- df_cesariana |>
  filter(TPROBSON == "10" & PARTO == 2) |>
  group_by(ano, CODMUNRES) |>
  summarise(total_cesariana_grupo_robson_10 = n()) |>
  rename(codmunres = CODMUNRES)

##Transformando as colunas que estão em caracter para numéricas
df <- df |> mutate_if(is.character, as.numeric)

##Juntando com o restante da base do bloco 4
df_bloco4 <- left_join(df_bloco4, df)

##Substituindo os NA's da coluna 'total_cesariana_grupo_robson_4' por 0 (gerados após o left_join)
df_bloco4$total_cesariana_grupo_robson_10[is.na(df_bloco4$total_cesariana_grupo_robson_10)] <- 0

# Salvando a base de dados completa na pasta data-raw/csv -----------------
write.csv(df_bloco4, "data-raw/csv/indicadores_bloco4_assistencia_ao_parto_2012-2021.csv", row.names = FALSE)



