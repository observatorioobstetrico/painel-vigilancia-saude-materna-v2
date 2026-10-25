library(tidyverse)
library(data.table)
library(microdatasus)

################################################################################
# Municípios do painel
################################################################################

codigos_municipios <- read.csv(
  "data-raw/extracao-dos-dados/blocos/databases_auxiliares/tabela_aux_municipios.csv"
) |>
  pull(codmunres)

df_aux_municipios <- expand_grid(
  codmunres = codigos_municipios,
  ano = 2023:2026
)

################################################################################
# Baixar SINASC 2012–2024
################################################################################

df_list <- list()

# Baixar os dados ano a ano
for (ano in c(2023:2024)) {
  df_ano <- microdatasus::fetch_datasus(year_start = ano, year_end = ano,
                                        information_system = "SINASC",
                                        vars = c("CODMUNRES", "TPROBSON", "PARTO"))

  # Adicionar a variável ANO ao dataframe
  df_ano$ano <- ano

  # Adicionar o dataframe à lista
  df_list[[as.character(ano)]] <- df_ano
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

################################################################################
# SINASC 2025
################################################################################

options(timeout = 99999)

sinasc25 <- fread(
  "https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SINASC/csv/SINASC_2025_csv.zip",
  sep=";"
) |>
  mutate(ano = 2025) |>
  select(CODMUNRES,TPROBSON,PARTO,ano)

################################################################################
# SINASC 2026
################################################################################

options(timeout = 99999)

sinasc26 <- fread(
  "https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SINASC/csv/SINASC_2026_csv.zip",
  sep=";"
) |>
  mutate(ano = 2026) |>
  select(CODMUNRES,TPROBSON,PARTO,ano)

################################################################################
# Base principal
################################################################################

df_sinasc <- bind_rows(sinasc25, sinasc26)
df_aux <- bind_rows(
  df |> mutate(CODMUNRES = as.numeric(CODMUNRES)),
  df_sinasc |> mutate(CODMUNRES = as.numeric(CODMUNRES))
)

df_aux <- df_aux |>
  mutate(
    codmunres = as.numeric(CODMUNRES),
    TPROBSON = as.numeric(TPROBSON),
    PARTO = as.numeric(PARTO)
  ) |>
  select(-CODMUNRES)

################################################################################
# Total de nascidos vivos
################################################################################

df_total <- df_aux |>
  select(ano, codmunres) |>
  group_by(ano, codmunres) |>
  summarise(
    total_de_nascidos_vivos = n(),
    .groups = "drop"
  )

################################################################################
# Cesarianas
################################################################################

df_cesariana <- df_aux |>
  select(ano, codmunres, PARTO) |>
  filter(PARTO == 2) |>
  group_by(ano, codmunres) |>
  summarise(
    mulheres_com_parto_cesariana = n(),
    .groups = "drop"
  )

################################################################################
# Grupos de Robson
################################################################################

df_robson <- df_aux |>
  select(ano, codmunres, TPROBSON) |>
  group_by(ano, codmunres) |>
  summarise(
    mulheres_dentro_do_grupo_de_robson_1 = sum(TPROBSON == 1, na.rm = TRUE),
    mulheres_dentro_do_grupo_de_robson_2 = sum(TPROBSON == 2, na.rm = TRUE),
    mulheres_dentro_do_grupo_de_robson_3 = sum(TPROBSON == 3, na.rm = TRUE),
    mulheres_dentro_do_grupo_de_robson_4 = sum(TPROBSON == 4, na.rm = TRUE),
    mulheres_dentro_do_grupo_de_robson_5 = sum(TPROBSON == 5, na.rm = TRUE),
    mulheres_dentro_do_grupo_de_robson_6_ao_9 = sum(TPROBSON >= 6 & TPROBSON <= 9,
                                                    na.rm = TRUE),
    mulheres_dentro_do_grupo_de_robson_10 = sum(TPROBSON == 10, na.rm = TRUE),
    .groups = "drop"
  )

################################################################################
# Cesarianas por grupo de Robson
################################################################################

df_robson_cesariana <- df_aux |>
  filter(PARTO == 2) |>
  group_by(ano, codmunres) |>
  summarise(
    total_cesariana_grupo_robson_1 = sum(TPROBSON == 1, na.rm = TRUE),
    total_cesariana_grupo_robson_2 = sum(TPROBSON == 2, na.rm = TRUE),
    total_cesariana_grupo_robson_3 = sum(TPROBSON == 3, na.rm = TRUE),
    total_cesariana_grupo_robson_4 = sum(TPROBSON == 4, na.rm = TRUE),
    total_cesariana_grupo_robson_5 = sum(TPROBSON == 5, na.rm = TRUE),
    total_cesariana_grupo_robson_6_ao_9 = sum(TPROBSON >= 6 & TPROBSON <= 9,
                                              na.rm = TRUE),
    total_cesariana_grupo_robson_10 = sum(TPROBSON == 10, na.rm = TRUE),
    .groups = "drop"
  )

################################################################################
# Base final do bloco 4
################################################################################

df_total <- df_total |>
  mutate(
    codmunres = as.numeric(codmunres),
    ano = as.numeric(ano)
  )

df_cesariana <- df_cesariana |>
  mutate(
    codmunres = as.numeric(codmunres),
    ano = as.numeric(ano)
  )

df_robson <- df_robson |>
  mutate(
    codmunres = as.numeric(codmunres),
    ano = as.numeric(ano)
  )

df_robson_cesariana <- df_robson_cesariana |>
  mutate(
    codmunres = as.numeric(codmunres),
    ano = as.numeric(ano)
  )

df_bloco4 <- df_aux_municipios |>
  left_join(df_total, by = c("codmunres","ano")) |>

  left_join(df_cesariana, by = c("codmunres","ano")) |>
  left_join(df_robson, by = c("codmunres","ano")) |>
  left_join(df_robson_cesariana, by = c("codmunres","ano")) |>
  mutate(across(where(is.numeric), ~replace_na(.x,0))) |>
  mutate(across(everything(), as.numeric))

df_bloco4 <- df_bloco4 |>
  select(
    codmunres,
    ano,
    total_de_nascidos_vivos,
    mulheres_com_parto_cesariana,
    mulheres_dentro_do_grupo_de_robson_1,
    mulheres_dentro_do_grupo_de_robson_2,
    mulheres_dentro_do_grupo_de_robson_3,
    mulheres_dentro_do_grupo_de_robson_4,
    mulheres_dentro_do_grupo_de_robson_5,
    mulheres_dentro_do_grupo_de_robson_6_ao_9,
    mulheres_dentro_do_grupo_de_robson_10,
    total_cesariana_grupo_robson_1,
    total_cesariana_grupo_robson_2,
    total_cesariana_grupo_robson_3,
    total_cesariana_grupo_robson_4,
    total_cesariana_grupo_robson_5,
    total_cesariana_grupo_robson_6_ao_9,
    total_cesariana_grupo_robson_10
  )

################################################################################
# Salvar
################################################################################

write.csv(
  df_bloco4,
  "data-raw/csv/indicadores_bloco4_assistencia_ao_parto_2023-2026.csv",
  row.names = FALSE
)
