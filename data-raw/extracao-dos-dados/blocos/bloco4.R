
library(tidyverse)
library(data.table)
library(dplyr)

# Criando um objeto que recebe os códigos dos municípios que utilizamos no painel
codigos_municipios <- read.csv("data-raw/extracao-dos-dados/blocos/databases_auxiliares/tabela_aux_municipios.csv") |>
  pull(codmunres)

# Criando um data.frame auxiliar que possui uma linha para cada combinação de município e ano
df_aux_municipios <- data.frame(codmunres = rep(codigos_municipios, each = length(2012:2024)), ano = 2012:2024)

# Criando o data.frame que irá receber todos os dados do bloco 4
df_bloco4 <- data.frame()

# Baixar os dados ano a ano
# Criar um vetor para armazenar os dados de cada ano
df_list <- list()

# Baixar os dados ano a ano
for (ano in c(2012,2014:2022)) {
  df_ano <- microdatasus::fetch_datasus(year_start = ano, year_end = ano,
                                        information_system = "SINASC",
                                        vars = c("CODMUNRES", "TPROBSON", "PARTO"))

  # Adicionar a variável ANO ao dataframe
  df_ano$ano <- ano

  # Adicionar o dataframe à lista
  df_list[[ano - 2011]] <- df_ano
}

# TPROBSON não é definada para o ano de 2013 então a forma de baixar vai ser diferente
df_ano <- microdatasus::fetch_datasus(year_start = 2013, year_end = 2013,
                                      information_system = "SINASC",
                                      vars = c("CODMUNRES", "PARTO"))
df_ano$TPROBSON <- rep("NA", nrow(df_ano))
df_ano$ano <- 2013
df_list[[2]] <- df_ano

# Juntar os dataframes da lista em um único dataframe
df <- bind_rows(df_list)
rm(df_list)

df$TPROBSON <- as.numeric(df$TPROBSON)

# Dados ainda não consolidados
options(timeout=99999)

sinasc23 <- fread("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SINASC/DNOPEN23.csv", sep = ";")
sinasc23 <- sinasc23 |>
  mutate(ano = 2023) |>
  select(CODMUNRES, TPROBSON, PARTO, ano)

sinasc24 <- fread("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SINASC/DNOPEN24.csv", sep = ";")
sinasc24 <- sinasc24 |>
  mutate(ano = 2024) |>
  select(CODMUNRES, TPROBSON, PARTO, ano)

df_aux <- rbind(df, sinasc23, sinasc24)

rm(df, df_ano, sinasc23, sinasc24, codigos_municipios)

################################################################################
### Total de nascidos vivos
################################################################################

df <- df_aux |>
  select(ano, CODMUNRES) |>
  group_by(ano, CODMUNRES) |>
  summarise(total_de_nascidos_vivos = n()) |>
  rename(codmunres = CODMUNRES)

df <- df |>
  mutate_if(is.character, as.numeric)

# Fazendo um left_join da base auxiliar de municípios com o data.frame que contém o total de nascidos vivos
df_bloco4 <- left_join(df_aux_municipios, df)
rm(df_aux_municipios)

# Substituindo os NA's da coluna 'total_de_nascidos_vivos' por 0 (os NA's surgem quando um município não apresentou nascidos vivos num dado ano)
df_bloco4$total_de_nascidos_vivos[is.na(df_bloco4$total_de_nascidos_vivos)] <- 0

################################################################################
### Proporção de nascimentos por cesariana
################################################################################

df <- df_aux |>
  select(ano, CODMUNRES, PARTO) |>
  filter(PARTO == 2) |>
  group_by(ano, CODMUNRES) |>
  summarise(mulheres_com_parto_cesariana = n()) |>
  rename(codmunres = CODMUNRES)

# Transformando as colunas que estão em caracter para numéricas
df <- df |> mutate_if(is.character, as.numeric)

# Juntando com o restante da base do bloco 4
df_bloco4 <- left_join(df_bloco4, df)

# Substituindo os NA's da coluna 'mulheres_com_parto_cesariana' por 0 (gerados após o left_join)
df_bloco4$mulheres_com_parto_cesariana[is.na(df_bloco4$mulheres_com_parto_cesariana)] <- 0

################################################################################
### df_robson
################################################################################

### Número de nascidos vivos de mulheres pertencentes ao grupo 1 de Robson

df <- df_aux |>
  select(ano, CODMUNRES, TPROBSON) |>
  filter(TPROBSON == 1) |>
  group_by(ano, CODMUNRES) |>
  summarise(mulheres_dentro_do_grupo_de_robson_1 = n()) |>
  rename(codmunres = CODMUNRES)

# Transformando as colunas que estão em caracter para numéricas
df <- df |> mutate_if(is.character, as.numeric)

# Juntando com o restante da base do bloco 4
df_bloco4 <- left_join(df_bloco4, df)

# Substituindo os NA's da coluna 'mulheres_dentro_do_grupo_de_robson_1' por 0 (gerados após o left_join)
df_bloco4$mulheres_dentro_do_grupo_de_robson_1[is.na(df_bloco4$mulheres_dentro_do_grupo_de_robson_1)] <- 0

### Número de nascidos vivos de mulheres pertencentes ao grupo 2 de Robson

df <- df_aux |>
  select(ano, CODMUNRES, TPROBSON) |>
  filter(TPROBSON == 2) |>
  group_by(ano, CODMUNRES) |>
  summarise(mulheres_dentro_do_grupo_de_robson_2 = n()) |>
  rename(codmunres = CODMUNRES)

# Transformando as colunas que estão em caracter para numéricas
df <- df |> mutate_if(is.character, as.numeric)

# Juntando com o restante da base do bloco 4
df_bloco4 <- left_join(df_bloco4, df)

# Substituindo os NA's da coluna 'mulheres_dentro_do_grupo_de_robson_2' por 0 (gerados após o left_join)
df_bloco4$mulheres_dentro_do_grupo_de_robson_2[is.na(df_bloco4$mulheres_dentro_do_grupo_de_robson_2)] <- 0

### Número de nascidos vivos de mulheres pertencentes ao grupo 3 de Robson

df <- df_aux |>
  select(ano, CODMUNRES, TPROBSON) |>
  filter(TPROBSON == 3) |>
  group_by(ano, CODMUNRES) |>
  summarise(mulheres_dentro_do_grupo_de_robson_3 = n()) |>
  rename(codmunres = CODMUNRES)

# Transformando as colunas que estão em caracter para numéricas
df <- df |> mutate_if(is.character, as.numeric)

# Juntando com o restante da base do bloco 4
df_bloco4 <- left_join(df_bloco4, df)

# Substituindo os NA's da coluna 'mulheres_dentro_do_grupo_de_robson_3' por 0 (gerados após o left_join)
df_bloco4$mulheres_dentro_do_grupo_de_robson_3[is.na(df_bloco4$mulheres_dentro_do_grupo_de_robson_3)] <- 0

### Número de nascidos vivos de mulheres pertencentes ao grupo 4 de Robson

df <- df_aux |>
  select(ano, CODMUNRES, TPROBSON) |>
  filter(TPROBSON == 4) |>
  group_by(ano, CODMUNRES) |>
  summarise(mulheres_dentro_do_grupo_de_robson_4 = n()) |>
  rename(codmunres = CODMUNRES)

# Transformando as colunas que estão em caracter para numéricas
df <- df |> mutate_if(is.character, as.numeric)

# Juntando com o restante da base do bloco 4
df_bloco4 <- left_join(df_bloco4, df)

# Substituindo os NA's da coluna 'mulheres_dentro_do_grupo_de_robson_4' por 0 (gerados após o left_join)
df_bloco4$mulheres_dentro_do_grupo_de_robson_4[is.na(df_bloco4$mulheres_dentro_do_grupo_de_robson_4)] <- 0

### Número de nascidos vivos de mulheres pertencentes ao grupo 5 de Robson

df <- df_aux |>
  select(ano, CODMUNRES, TPROBSON) |>
  filter(TPROBSON == 5) |>
  group_by(ano, CODMUNRES) |>
  summarise(mulheres_dentro_do_grupo_de_robson_5 = n()) |>
  rename(codmunres = CODMUNRES)

# Transformando as colunas que estão em caracter para numéricas
df <- df |> mutate_if(is.character, as.numeric)

# Juntando com o restante da base do bloco 4
df_bloco4 <- left_join(df_bloco4, df)

# Substituindo os NA's da coluna 'mulheres_dentro_do_grupo_de_robson_5' por 0 (gerados após o left_join)
df_bloco4$mulheres_dentro_do_grupo_de_robson_5[is.na(df_bloco4$mulheres_dentro_do_grupo_de_robson_5)] <- 0

### Número de nascidos vivos de mulheres pertencentes ao grupo  de Robson de 6 ao 9

df <- df_aux |>
  select(ano, CODMUNRES, TPROBSON) |>
  filter(TPROBSON <= 9 & TPROBSON >= 6) |>
  group_by(ano, CODMUNRES) |>
  summarise(mulheres_dentro_do_grupo_de_robson_6_ao_9 = n()) |>
  rename(codmunres = CODMUNRES)

# Transformando as colunas que estão em caracter para numéricas
df <- df |> mutate_if(is.character, as.numeric)

# Juntando com o restante da base do bloco 4
df_bloco4 <- left_join(df_bloco4, df)

# Substituindo os NA's da coluna 'mulheres_dentro_do_grupo_de_robson_6_ao_9' por 0 (gerados após o left_join)
df_bloco4$mulheres_dentro_do_grupo_de_robson_6_ao_9[is.na(df_bloco4$mulheres_dentro_do_grupo_de_robson_6_ao_9)] <- 0

### Número de nascidos vivos de mulheres pertencentes ao grupo 10 de Robson

df <- df_aux |>
  select(ano, CODMUNRES, TPROBSON) |>
  filter(TPROBSON == "10") |>
  group_by(ano, CODMUNRES) |>
  summarise(mulheres_dentro_do_grupo_de_robson_10 = n()) |>
  rename(codmunres = CODMUNRES)

# Transformando as colunas que estão em caracter para numéricas
df <- df |> mutate_if(is.character, as.numeric)

# Juntando com o restante da base do bloco 4
df_bloco4 <- left_join(df_bloco4, df)

# Substituindo os NA's da coluna 'mulheres_dentro_do_grupo_de_robson_10' por 0 (gerados após o left_join)
df_bloco4$mulheres_dentro_do_grupo_de_robson_10[is.na(df_bloco4$mulheres_dentro_do_grupo_de_robson_10)] <- 0

################################################################################
### Contribuição relativa de cada grupo de Robson na taxa global de cesariana
################################################################################

### Número de nascidos vivos de mães do grupo 1

df <- df_aux |>
  filter((TPROBSON == 1) & PARTO == 2) |>
  group_by(ano, CODMUNRES) |>
  summarise(total_cesariana_grupo_robson_1 = n()) |>
  rename(codmunres = CODMUNRES)

# Transformando as colunas que estão em caracter para numéricas
df <- df |> mutate_if(is.character, as.numeric)

# Juntando com o restante da base do bloco 4
df_bloco4 <- left_join(df_bloco4, df)

# Substituindo os NA's da coluna 'total_cesariana_grupo_robson_1' por 0 (gerados após o left_join)
df_bloco4$total_cesariana_grupo_robson_1[is.na(df_bloco4$total_cesariana_grupo_robson_1)] <- 0

### Número de nascidos vivos de mães do grupo 2

df <- df_aux |>
  filter((TPROBSON == 2) & PARTO == 2) |>
  group_by(ano, CODMUNRES) |>
  summarise(total_cesariana_grupo_robson_2 = n()) |>
  rename(codmunres = CODMUNRES)

# Transformando as colunas que estão em caracter para numéricas
df <- df |> mutate_if(is.character, as.numeric)

# Juntando com o restante da base do bloco 4
df_bloco4 <- left_join(df_bloco4, df)

# Substituindo os NA's da coluna 'total_cesariana_grupo_robson_2' por 0 (gerados após o left_join)
df_bloco4$total_cesariana_grupo_robson_2[is.na(df_bloco4$total_cesariana_grupo_robson_2)] <- 0

### Número de nascidos vivos de mães do grupo 3

df <- df_aux |>
  filter((TPROBSON == 3) & PARTO == 2) |>
  group_by(ano, CODMUNRES) |>
  summarise(total_cesariana_grupo_robson_3 = n()) |>
  rename(codmunres = CODMUNRES)

# Transformando as colunas que estão em caracter para numéricas
df <- df |> mutate_if(is.character, as.numeric)

# Juntando com o restante da base do bloco 4
df_bloco4 <- left_join(df_bloco4, df)

# Substituindo os NA's da coluna 'total_cesariana_grupo_robson_3' por 0 (gerados após o left_join)
df_bloco4$total_cesariana_grupo_robson_3[is.na(df_bloco4$total_cesariana_grupo_robson_3)] <- 0

### Número de nascidos vivos de mães do grupo 4

df <- df_aux |>
  filter((TPROBSON == 4) & PARTO == 2) |>
  group_by(ano, CODMUNRES) |>
  summarise(total_cesariana_grupo_robson_4 = n()) |>
  rename(codmunres = CODMUNRES)

# Transformando as colunas que estão em caracter para numéricas
df <- df |> mutate_if(is.character, as.numeric)

# Juntando com o restante da base do bloco 4
df_bloco4 <- left_join(df_bloco4, df)

# Substituindo os NA's da coluna 'total_cesariana_grupo_robson_4' por 0 (gerados após o left_join)
df_bloco4$total_cesariana_grupo_robson_4[is.na(df_bloco4$total_cesariana_grupo_robson_4)] <- 0

### Número de nascidos vivos de mães do grupo 5

df <- df_aux |>
  filter((TPROBSON == 5) & PARTO == 2) |>
  group_by(ano, CODMUNRES) |>
  summarise(total_cesariana_grupo_robson_5 = n()) |>
  rename(codmunres = CODMUNRES)

# Transformando as colunas que estão em caracter para numéricas
df <- df |> mutate_if(is.character, as.numeric)

# Juntando com o restante da base do bloco 4
df_bloco4 <- left_join(df_bloco4, df)

# Substituindo os NA's da coluna 'total_cesariana_grupo_robson_4' por 0 (gerados após o left_join)
df_bloco4$total_cesariana_grupo_robson_5[is.na(df_bloco4$total_cesariana_grupo_robson_5)] <- 0

### Número de nascidos vivos de mães dos grupos 6 a 9 conjuntamente

df <- df_aux |>
  filter(TPROBSON >= 6 & TPROBSON <= 9 & PARTO == 2) |>
  group_by(ano, CODMUNRES) |>
  summarise(total_cesariana_grupo_robson_6_ao_9 = n()) |>
  rename(codmunres = CODMUNRES)

# Transformando as colunas que estão em caracter para numéricas
df <- df |> mutate_if(is.character, as.numeric)

# Juntando com o restante da base do bloco 4
df_bloco4 <- left_join(df_bloco4, df)

# Substituindo os NA's da coluna 'total_cesariana_grupo_robson_6_ao_9' por 0 (gerados após o left_join)
df_bloco4$total_cesariana_grupo_robson_6_ao_9[is.na(df_bloco4$total_cesariana_grupo_robson_6_ao_9)] <- 0

### Número de nascidos vivos de mães do grupo 10

df <- df_aux |>
  filter(TPROBSON == 10 & PARTO == 2) |>
  group_by(ano, CODMUNRES) |>
  summarise(total_cesariana_grupo_robson_10 = n()) |>
  rename(codmunres = CODMUNRES)

# Transformando as colunas que estão em caracter para numéricas
df <- df |> mutate_if(is.character, as.numeric)

# Juntando com o restante da base do bloco 4
df_bloco4 <- left_join(df_bloco4, df)

# Substituindo os NA's da coluna 'total_cesariana_grupo_robson_4' por 0 (gerados após o left_join)
df_bloco4$total_cesariana_grupo_robson_10[is.na(df_bloco4$total_cesariana_grupo_robson_10)] <- 0

# Salvando a base de dados completa na pasta data-raw/csv -----------------
write.csv(df_bloco4, "data-raw/csv/indicadores_bloco4_assistencia_ao_parto_2012-2024.csv", row.names = FALSE)


