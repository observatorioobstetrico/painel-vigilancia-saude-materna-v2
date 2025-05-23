library(microdatasus)
library(dplyr)
library(tidyr)
library(readr)
library(tidyverse)

#### Baixando os dados do sinasc de 2012-2022

anos <- c(2012, 2014:2023)
dados <- data.frame()

for (i in anos){

  df <- fetch_datasus(
    year_start = i,
    year_end = i,
    information_system = "SINASC",
    vars = c("CODMUNRES", "CODMUNNASC", 'CODESTAB', "LOCNASC",
             "PARTO", "IDADEMAE", "ESCMAE", "RACACORMAE",
             "TPROBSON", "PESO")
  )

  df <- df |> mutate(ANO = i)
  # data.table::fwrite(df, paste0("data-raw/csv/sinasc_", i, ".csv"))
  dados <- rbind(dados, df)
}

rm(df,anos,i)

## Não há a variável TPROBSON para 2013

df13 <- fetch_datasus(
  year_start = 2013,
  year_end = 2013,
  information_system = "SINASC",
  vars = c("CODMUNRES", "CODMUNNASC", 'CODESTAB', "LOCNASC",
           "PARTO", "IDADEMAE", "ESCMAE", "RACACORMAE", "PESO")
) |> mutate(ANO = 2013)

df13$TPROBSON <- rep("NA", nrow(df13))
# data.table::fwrite(df13, paste0("data-raw/csv/sinasc_2013.csv"))

## Juntando os dados do sinasc de 2013 aos dados dos outros anos

df_sinasc_consolidados <- rbind(dados, df13)
rm(df13)

#### Baixando os dados preliminares do sinasc 2023

options(timeout=99999)

#### Baixando os dados res do sinasc 2024

df24 <- data.table::fread(
  "https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SINASC/csv/SINASC_2024.csv",
  sep= ";") |>
  select("CODMUNRES", "CODMUNNASC", 'CODESTAB', "LOCNASC",
         "PARTO", "IDADEMAE", "ESCMAE", "RACACORMAE",
         "TPROBSON", "PESO") |> mutate(ANO = 2024)


#### Juntando os preliminares aos consolidados

df_sinasc_consolidados <- rbind(df_sinasc_consolidados, df24)

data.table::fwrite(df_sinasc_consolidados, "data-raw/csv/sinasc_2012_2024.csv",
                   row.names = FALSE)


